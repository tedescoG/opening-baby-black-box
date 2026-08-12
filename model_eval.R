# SCRIPT SETUP -------------------------------------------------------------####

# Paths, seeds and output directories. Edit config.R, not this file.
source("config.R")

# Sourcing functions and packages
source("src.R")

# No seed is needed in this script: it only loads a fitted forest and calls
# predict(), which is deterministic. Nothing here draws random numbers.

# Importing data
test = fread(require_file(test_data_file))
test_out = fread(require_file(test_outcome_file))
test_bg = fread(require_file(test_bg_file))

# DATA PREP ----------------------------------------------------------------####

# Applying the pre-processing
test_clean = 
  clean_df(test, test_bg) %>%
  merge(test_out, by = "nomem_encr") %>%
  mutate(new_child = factor(new_child)) %>%
  select(-nomem_encr, -intentionB)

# Free unused memory
rm(test, test_bg, test_out)

# LOADING MODEL ------------------------------------------------------------####

model_pathname = paste0(model_dir,"final_rf.rds")
fit = readRDS(model_pathname)
fit = unbundle(fit)

# Restores $ordered for parsnip >= 1.6.0; see config.R for why.
fit = restore_parsnip_compat(fit)

# MODEL EVALUATION ---------------------------------------------------------####

# Defining evaluation metrics
eval_metrics =
  metric_set(f_meas, accuracy, precision, recall, specificity, sensitivity)

# Producing prediction for the holdout-set
test_predictions =
  predict(fit, new_data = test_clean) %>%
  bind_cols(test_clean)

# Computing evaluation metrics
metrics = eval_metrics(
  test_predictions,
  truth = new_child,
  estimate = .pred_class,
  event_level = "second"
)

# Saving evaluation metrics
res_pathname = paste0(res_dir,"eval_results.rds")
saveRDS(metrics, res_pathname)