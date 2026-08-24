# SCRIPT SETUP -------------------------------------------------------------####

# Paths, seeds and output directories. Edit config.R, not this file.
source("config.R")

# Sourcing functions and packages
source("src.R")

# Random seed (see config.R; model_explain.R deliberately uses a different one)
run <- seed_train

# Output path for the fitted model, checked up front.
#
# Refuse to overwrite the published model. See README.md for details
model_pathname <- paste0(model_dir, "final_rf.rds")

if (
  file.exists(model_pathname) && !isTRUE(getOption("overwrite_final_model"))
) {
  stop(
    "Refusing to overwrite ",
    model_pathname,
    ", the published model.\n",
    "A retrained forest will differ from it - see 'Retraining' in README.md.\n",
    "To proceed anyway, move the existing file aside, or set:\n",
    "  options(overwrite_final_model = TRUE)",
    call. = FALSE
  )
}

# Importing data
train <- fread(require_file(train_data_file))
train_out <- fread(require_file(train_outcome_file))
train_bg <- fread(require_file(train_bg_file))

# DATA PREP ----------------------------------------------------------------####

# Applying the pre-processing
train_clean <-
  clean_df(train, train_bg) %>%
  merge(train_out, by = "nomem_encr") %>%
  mutate(new_child = factor(new_child)) %>%
  select(-nomem_encr, -intentionB)

# Free unused memory
rm(train, train_bg, train_out)

# Setting re-samples
set.seed(run)
folds <- vfold_cv(train_clean, v = 10, repeats = 3, strata = new_child)

# MODEL DEFINITION ---------------------------------------------------------####

# Model formula
rf_recipe <- recipe(formula = new_child ~ ., data = train_clean)

# Model Specification
rf_specs <-
  rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  set_engine(
    "ranger",
    probability = T,
    max.depth = tune("tree_depth"),
    regularization.factor = tune("lambda"),
    regularization.usedepth = T
  ) %>%
  set_mode("classification")

# Workflow definition
wf <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_specs)

# Hyper-parameter grid
# Setting ranges
range_tree <- c(1L, 5000L)
range_min_n <- c(1L, 40L)
range_mtry <- c(1L, 30L)
range_depth <- c(0, 30L)

# Updating parameters in the workflow obj
params <-
  wf %>%
  extract_parameter_set_dials() %>%
  update(trees = trees(range = range_tree)) %>%
  update(min_n = min_n(range = range_min_n)) %>%
  update(tree_depth = tree_depth()) %>%
  update(mtry = mtry(range = range_mtry))

# Generating the grid using space-filling design
set.seed(run)
grid <-
  params %>%
  grid_space_filling(type = "max_entropy", size = 250)

# MODEL TUNING -------------------------------------------------------------####
set.seed(run)

# Initializing parallel back-end.
#
# IMPORTANT: tune >= 2.0 removed foreach support entirely, and parallelism runs
# through mirai.
mirai::daemons(parallel::detectCores())

# defining metric set
train_metrics <-
  metric_set(f_meas, accuracy, precision, recall, specificity, sensitivity)

# Tuning procedure
tuning_time <- system.time({
  rf_tune <-
    wf %>%
    tune_race_anova(
      resamples = folds,
      grid = grid,
      control = control_race(
        verbose_elim = T,
        burn_in = 5,
        allow_par = T,
        event_level = "second"
      ),
      metrics = train_metrics
    )
})

# Races plot
plot_race(rf_tune) +
  labs(title = "Efficient tuning via Racing ANOVA", y = "F1 metric")

# Extracting results for F1 score
f_meas_results <-
  rf_tune %>%
  collect_metrics() %>%
  filter(.metric == "f_meas")

# Visualizing the best performing models' F1 scores
ggplot(
  f_meas_results,
  aes(
    x = .config,
    y = mean,
    ymin = mean - std_err,
    ymax = mean + std_err
  )
) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(
    title = "Tuning Results for F-measure",
    x = "Tuning Parameter Configuration",
    y = "F-measure (mean ± std_err)"
  ) +
  theme_minimal()

# Saving performance of the best performing model (F1 metric)
best_config <- select_best(rf_tune, metric = "f_meas")

train_metrics <-
  rf_tune %>%
  collect_metrics() %>%
  filter(.config == best_config$.config) %>%
  select(.metric, .estimator, mean)

metrics <- list(train_time = tuning_time, train_metrics = train_metrics)

res_pathname <- paste0(res_dir, "tuning_results.rds")
saveRDS(metrics, res_pathname)

# FINALIZE AND FIT TUNED MODEL ---------------------------------------------####

# Finalizing the workflow with tuned parameters
tuned_wf <-
  wf %>%
  finalize_workflow(best_config)

# Fitting the tuned model to the full training set.
set.seed(run)
tuned_fit <-
  tuned_wf %>%
  fit(train_clean)

# Saving the model.
save_bundle <-
  tuned_fit %>%
  butcher() %>%
  bundle()

saveRDS(save_bundle, model_pathname)

# Shutting down the mirai daemons started for the race.
mirai::daemons(0)
