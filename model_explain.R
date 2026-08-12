# SCRIPT SETUP -------------------------------------------------------------####
#

# Paths, seeds and output directories. Edit config.R, not this file.
source("config.R")

# Sourcing functions and packages
source("src.R")

# Parallel back-end.
doParallel::registerDoParallel(cores = detectCores())

# Random seed (see config.R; model_train.R deliberately uses a different one)
run <- seed_explain

# Number of repetitions (for permutations and shapley) comes from config.R as `n`

# Saving timing
timing <- list()

# Importing data
# Training data
train <- fread(require_file(train_data_file))
train_out <- fread(require_file(train_outcome_file))
train_bg <- fread(require_file(train_bg_file))

# Test data
test <- fread(require_file(test_data_file))
test_out <- fread(require_file(test_outcome_file))
test_bg <- fread(require_file(test_bg_file))

# DATA PREP ----------------------------------------------------------------####

# Applying the pre-processing
# Training data
train_clean <-
  clean_df(train, train_bg) %>%
  merge(train_out, by = "nomem_encr") %>%
  mutate(new_child = factor(new_child)) %>%
  select(-nomem_encr, -intentionB)

# Test data
test_clean <-
  clean_df(test, test_bg) %>%
  merge(test_out, by = "nomem_encr") %>%
  mutate(new_child = factor(new_child)) %>%
  select(-nomem_encr, -intentionB)

# Free unused memory
rm(train, train_bg, train_out, test, test_bg, test_out)


# LOADING MODEL ------------------------------------------------------------####

model_pathname <- paste0(model_dir, "final_rf.rds")
fit <- readRDS(model_pathname)
fit <- unbundle(fit)

# Restores $ordered for parsnip >= 1.6.0 for compatibility.
fit <- restore_parsnip_compat(fit)

# SELCTING FEATURES --------------------------------------------------------####

new_predictors <- c(
  "insta",
  "soc_lib"
)

rf_recipe <-
  recipe(formula = new_child ~ ., data = train_clean)

all_features <-
  rf_recipe$var_info %>%
  filter(role == "predictor") %>%
  pull(variable)

X_train <-
  train_clean %>%
  select(all_of(all_features))

X_test <-
  test_clean %>%
  select(all_of(all_features))

# MODEL-SPECIFIC (EMBEDDED) VARIABLE IMPORTANCE ----------------------------####

# setting up the explainer model
tuned_model <-
  fit %>%
  extract_spec_parsnip()

params <- tuned_model$eng_arg

## IMPURITY-BASED VIMP -----------------------------------------------------####
impurity_model <-
  tuned_model %>%
  set_engine("ranger", !!!params, importance = "impurity_corrected")

impurity_wf <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(impurity_model)


# fit the explainer model
set.seed(run)
registerDoRNG(seed = run)
time_embedd_impurity <- system.time({
  vi_impurity <-
    impurity_wf %>%
    fit(train_clean) %>% # fitting the entire training dataset
    extract_fit_parsnip() %>%
    vip(
      num_features = 40,
      geom = "col",
      scale = T,
      aesthetics = list(alpha = 0.8, fill = "midnightblue", base_size = 14),
      include_type = T
    )
})

# Saving time
timing[["embedded_impurity"]] <- unname(time_embedd_impurity[3])

# Plot object
vi_impurity_plot <-
  vi_impurity +
  theme_gray(base_size = 14)

vi_impurity_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_impurity_embedd.png"),
  plot = vi_impurity_plot,
  width = 8,
  height = 6,
  dpi = 250
)

## PERMUTATION-BASED VIMP --------------------------------------------------####
permutation_model <-
  tuned_model %>%
  set_engine("ranger", !!!params, importance = "permutation")

permutation_wf <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(permutation_model)

# fit the explainer model
set.seed(run)
registerDoRNG(seed = run)
time_embedd_permutation <- system.time({
  vi_permutation <-
    permutation_wf %>%
    fit(train_clean) %>% # fitting the entire training dataset
    extract_fit_parsnip() %>%
    vip(
      num_features = 40,
      scale = T,
      geom = "col",
      aesthetics = list(alpha = 0.8, fill = "midnightblue"),
      include_type = T
    )
})

# Saving time
timing[["embedded_permutation"]] <- unname(time_embedd_permutation[3])

# Plot object
vi_permutation_plot <-
  vi_permutation +
  theme_gray(base_size = 14)

vi_permutation_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_permutation_embedd.png"),
  plot = vi_permutation_plot,
  width = 8,
  height = 6,
  dpi = 250
)

# MODEL-AGNOSTIC VARIABLE IMPORTANCE ---------------------------------------####

## Variance-based approach ####
set.seed(run)
time_firm <- system.time({
  vi_firm <-
    vip(
      fit,
      num_features = 40,
      method = "firm",
      train = test_clean,
      target = 'new_child',
      scale = T,
      aesthetics = list(alpha = 0.8, fill = "indianred"),
      include_type = T
    )
})
# Saving time
timing[["FIRM"]] <- unname(time_firm[3])

# Plot object
vi_firm_plot <-
  vi_firm +
  theme_gray(base_size = 14)

vi_firm_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_firm.png"),
  plot = vi_firm_plot,
  width = 8,
  height = 6,
  dpi = 250
)

## Permutation based Approach ####

#### F1 as performance metric
registerDoRNG(seed = run)
time_perm_f1 <- system.time({
  vi_perm_f1 <-
    vip(
      fit,
      num_features = 40,
      method = "permute",
      train = test_clean,
      target = 'new_child',
      pred_wrapper = pred_class_cat,
      metric = yardstick::f_meas_vec,
      event_level = "second",
      smaller_is_better = F,
      aesthetics = list(fill = "forestgreen"),
      include_type = T,
      scale = T,
      nsim = n,
      parallelize_by = "repetitions",
      parallel = T
    )
})

# Saving time
timing[["agnostic_permutation_f1"]] <- unname(time_perm_f1[3])

# Plot object
vi_perm_f1_plot <-
  vi_perm_f1 +
  theme_gray(base_size = 14)

vi_perm_f1_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_perm_f1.png"),
  plot = vi_perm_f1_plot,
  width = 8,
  height = 6,
  dpi = 250
)

#### AUC as performance metric
registerDoRNG(seed = run)
time_perm_auc <- system.time({
  vi_perm_auc <-
    vip(
      fit,
      num_features = 40,
      method = "permute",
      train = test_clean,
      target = 'new_child',
      pred_wrapper = pred_class_num,
      metric = "roc_auc",
      event_level = "second",
      aesthetics = list(fill = "forestgreen"),
      include_type = T,
      scale = T,
      nsim = n,
      parallelize_by = "repetitions",
      parallel = T
    )
})

# Saving time
timing[["agnostic_permutation_auc"]] <- unname(time_perm_auc[3])

# Plot object
vi_perm_auc_plot <-
  vi_perm_auc +
  theme_gray(base_size = 14)

vi_perm_auc_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_perm_auc.png"),
  plot = vi_perm_auc_plot,
  width = 8,
  height = 6,
  dpi = 250
)

## Shapley-based Approach ####
registerDoRNG(seed = run)
time_vip_shap <- system.time({
  vi_shap <-
    vip(
      object = fit,
      num_features = 40,
      method = "shap",
      train = test_clean,
      pred_wrapper = pred_probs,
      aesthetics = list(alpha = 0.8, fill = "orange"),
      include_type = T,
      scale = T,
      nsim = n,
      parallel = T
    )
})

# Saving time
timing[["vip_shap"]] <- unname(time_vip_shap[3])

# Plot object
vi_shap_plot <-
  vi_shap +
  theme_gray(base_size = 14)

vi_shap_plot

# Saving plot
ggsave(
  paste0(figure_dir, "vip_shap.png"),
  plot = vi_shap_plot,
  width = 8,
  height = 6,
  dpi = 250
)

## COMPARISON ####

# extracting variable importance data from `vip` objects
embed_permutation_data <- vi_permutation$data %>%
  rename("Embedded_Impurity" = Importance)
embed_impurity_data <- vi_impurity$data %>%
  rename("Embedded_Permuation" = Importance)
vi_firm_data <- vi_firm$data %>%
  rename("FIRM" = Importance)
vi_perm_auc_data <- vi_perm_auc$data %>%
  rename("Permutation-AUC" = Importance) %>%
  select(-StDev)
vi_perm_f1_data <- vi_perm_f1$data %>%
  rename("Permutation-F1" = Importance) %>%
  select(-StDev)
vi_shap_data <- vi_shap$data %>%
  rename("Aggregated Shap" = Importance)


importance_data <- embed_permutation_data %>%
  full_join(embed_impurity_data, by = "Variable") %>%
  full_join(vi_firm_data, by = "Variable") %>%
  full_join(vi_perm_auc_data, by = "Variable") %>%
  full_join(vi_perm_f1_data, by = "Variable") %>%
  full_join(vi_shap_data, by = "Variable")

# computing summary statistic of importance measures
importance_data$tot <- rowSums(importance_data %>% select(-Variable))
importance_data$avg <- importance_data$tot / (ncol(importance_data) - 2)

# reordering the data
importance_data <- importance_data %>%
  arrange(desc(avg))

# formatting data
long_data <- importance_data %>%
  pivot_longer(
    cols = colnames(importance_data %>% select(-Variable, -tot, -avg)),
    names_to = "Metric",
    values_to = "Importance"
  ) %>%
  group_by(Variable) %>%
  arrange(Importance, .by_group = TRUE) %>%
  mutate(Metric = factor(Metric, levels = unique(Metric))) %>%
  ungroup()

long_data_summary <- long_data |>
  group_by(Variable) |>
  summarize(avg = mean(avg, na.rm = TRUE))

# summary plot
summary_all <- ggplot(
  long_data_summary,
  aes(x = reorder(Variable, avg), y = avg)
) +
  geom_bar(stat = "identity", alpha = 0.6) +
  coord_flip() + # Flip coordinates for horizontal bars
  theme_grey(base_size = 14) +
  labs(y = "Average VI", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_text(size = 10))

summary_all

ggsave(
  paste0(figure_dir, "summary_all.png"),
  plot = summary_all,
  width = 8,
  height = 6,
  dpi = 250
)

# extracting the var names
top5_vars <- importance_data %>%
  slice(1:5) %>%
  pull(Variable)


# Reshape data to long format
long_data_top5 <- long_data %>%
  filter(Variable %in% top5_vars)

# Importance x metric
single_metric_top5 <- ggplot(
  long_data_top5,
  aes(x = reorder(Variable, tot), y = Importance, fill = Metric)
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "black",
    linewidth = 0.2,
    alpha = 0.6
  ) +
  coord_flip() + # Flip coordinates for horizontal bars
  theme_grey(base_size = 14) +
  labs(y = "Importance", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_text(size = 10))

single_metric_top5

ggsave(
  paste0(figure_dir, "single_metric_top5.png"),
  plot = single_metric_top5,
  width = 8,
  height = 6,
  dpi = 250
)

# summary
summary_top5 <- ggplot(
  long_data_top5,
  aes(x = reorder(Variable, tot), y = avg)
) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.6) +
  coord_flip() + # Flip coordinates for horizontal bars
  theme_grey(base_size = 14) +
  labs(y = "Average VI", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_text(size = 12))

summary_top5

ggsave(
  paste0(figure_dir, "summary_top5.png"),
  plot = summary_top5,
  width = 8,
  height = 6,
  dpi = 250
)

# New predictors
long_data_new <- long_data %>%
  filter(Variable %in% new_predictors)

# Importance x metric
single_metric_new <- ggplot(
  long_data_new,
  aes(x = reorder(Variable, tot), y = Importance, fill = Metric)
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "black",
    linewidth = 0.2,
    alpha = 0.6
  ) +
  coord_flip() + # Flip coordinates for horizontal bars
  theme_grey(base_size = 14) +
  labs(y = "Importance Metric", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_text(size = 14))

single_metric_new

ggsave(
  paste0(figure_dir, "single_metric_new.png"),
  plot = single_metric_new,
  width = 8,
  height = 6,
  dpi = 250
)

# summary
summary_new <- ggplot(long_data_new, aes(x = reorder(Variable, tot), y = avg)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6, width = 0.6) +
  coord_flip() + # Flip coordinates for horizontal bars
  theme_grey(base_size = 12) +
  labs(y = "Average VI", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_text(size = 14))

summary_new

ggsave(
  paste0(figure_dir, "summary_new.png"),
  plot = summary_new,
  width = 8,
  height = 6,
  dpi = 250
)

# MARGINAL EFFECTS ---------------------------------------------------------####

# Define the explainer model
explainer <- explain_tidymodels(
  fit,
  test_clean,
  as.numeric(test_clean$new_child)
)


### For most important variable
time.dalex.top5 <- system.time({
  for (i in top5_vars) {
    set.seed(run)
    pd <- model_profile(
      explainer,
      variables = i,
      type = "partial",
      variable_type = 'categorical'
    )

    pd_plot <- ggplot_pdp_bar(pd, i)

    print(pd_plot)

    ggsave(
      paste0(figure_dir, "pd_plot_", i, ".png"),
      plot = pd_plot,
      width = 5,
      height = 5,
      dpi = 250
    )
  }
})

timing[['DALEX_top5']] <- unname(time.dalex.top5[3])


### For newly discovered predictors

time.dalex.new <- system.time({
  for (i in new_predictors) {
    set.seed(run)
    pd <- model_profile(
      explainer,
      variables = i,
      type = "partial",
      variable_type = 'categorical'
    )

    pd_plot <- ggplot_pdp_bar(pd, i)

    print(pd_plot)

    ggsave(
      paste0(figure_dir, "pd_plot_", i, ".png"),
      plot = pd_plot,
      width = 5,
      height = 5,
      dpi = 250
    )
  }
})

timing[['DALEX_new']] <- unname(time.dalex.new[3])

# INTERACTIONS STRENGHTS ---------------------------------------------------####

predictor <- Predictor$new(
  fit,
  data = X_test,
  y = test_clean$new_child,
  type = "prob",
  class = ".pred_1"
)


set.seed(run)
interaction.all.time <- system.time({
  interaction_all <- Interaction$new(predictor = predictor)
})

timing[["interaction_all_vars"]] <- interaction.all.time

interaction_plot <- interaction_all$plot() +
  theme_gray(base_size = 14) +
  theme(axis.text.y = element_text(size = 12))

interaction_plot

ggsave(
  paste0(figure_dir, "interactions.png"),
  plot = interaction_plot,
  width = 8,
  height = 6,
  dpi = 250
)

# Interactions for newly discovered predictors H-stat

interaction.new <- list()

int.new.time <- system.time({
  for (i in new_predictors) {
    set.seed(run)
    interaction_i <- Interaction$new(predictor = predictor, feature = i)
    interaction_plot_new <- interaction_i$plot() +
      theme(axis.text.y = element_text(size = 12)) +
      labs(x = paste0("Two-way interactions:", i))

    plot(interaction_plot_new)

    interaction.new[[i]] <- interaction_i$results

    ggsave(
      paste0(figure_dir, "interactions_", i, ".png"),
      plot = interaction_plot_new,
      width = 8,
      height = 7,
      dpi = 250
    )
  }
})

timing[['2-ways interaction new']] <- int.new.time

# Interaction PDPs + conditional PDPs

### insta
int_insta <- interaction.new$insta %>%
  arrange(desc(.interaction)) %>%
  filter(.interaction >= 0.09) %>%
  pull(.feature) %>%
  sub(":.*", "", .)

int_insta <- c(int_insta, "intention3B")

time.int.insta <- system.time({
  for (i in int_insta) {
    interaction_pdp <- FeatureEffect$new(
      predictor,
      feature = c(i, 'insta'),
      method = "pdp"
    )

    figure_int <- interaction_pdp$plot() +
      theme_grey(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 14))

    plot(figure_int)

    path1 <- paste0(figure_dir, "pd_int_", i, "_insta.png")
    ggsave(path1, plot = figure_int, width = 8, height = 6, dpi = 250)

    # Conditional PDP
    set.seed(run)
    cond_pd <- model_profile(
      explainer,
      variables = i,
      groups = "insta",
      type = "partial",
      variable_type = 'categorical'
    )

    cond_plot <- ggplot_pdp_bar(cond_pd, i, "insta")

    plot(cond_plot)

    path2 <- paste0(figure_dir, "pd_con_", i, "_insta.png")

    ggsave(path2, plot = cond_plot, width = 8, height = 6, dpi = 250)
  }
})

timing[['pdp_insta']] <- unname(time.int.insta[3])


### soc_lib
int_soclib <- interaction.new$soc_lib %>%
  arrange(desc(.interaction)) %>%
  filter(.interaction >= 0.09) %>%
  pull(.feature) %>%
  sub(":.*", "", .)

int_soclib <- c(int_soclib, "intention3B")

time.int.soclib <- system.time({
  for (i in int_soclib) {
    interaction_pdp <- FeatureEffect$new(
      predictor,
      feature = c(i, 'soc_lib'),
      method = "pdp"
    )

    figure_int <- interaction_pdp$plot() +
      theme_grey(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 14))

    plot(figure_int)

    path1 <- paste0(figure_dir, "pd_int_", i, "_soc_lib.png")
    ggsave(path1, plot = figure_int, width = 8, height = 6, dpi = 250)

    # Conditional PDP
    set.seed(run)
    cond_pd <- model_profile(
      explainer,
      variables = i,
      groups = "soc_lib",
      type = "partial",
      variable_type = 'categorical'
    )

    cond_plot <- ggplot_pdp_bar(cond_pd, i, "soc_lib") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        legend.text = element_text(size = 12)
      )

    plot(cond_plot)

    path2 <- paste0(figure_dir, "pd_con_", i, "_soc_lib.png")

    ggsave(path2, plot = cond_plot, width = 8, height = 6, dpi = 250)
  }
})

timing[['pdp_soc_lib']] <- unname(time.int.soclib[3])

# Saving timing results
# Saving evaluation metrics
res_pathname <- paste0(res_dir, "explain_timing.rds")
saveRDS(timing, res_pathname)
