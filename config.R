# Shared configuration for the analysis.
#
# This is the ONLY file you need to edit to run the replication package.
# Every script starts with source("config.R"), then source("src.R").
#
# Run the scripts from the project root, e.g.
#   Rscript model_eval.R
# or open the project in RStudio and source the script.

# Working directory ----------------------------------------------------------
# Convenience for RStudio users: jump to the directory of the active script.

if (
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Data location --------------------------------------------------------------
# EDIT THESE TWO PATHS.
#
# The PreFer / LISS microdata is NOT included in this repository.
# See the "Data availability" section of README.md for how to obtain it.
# Point the two variables below at the directories holding your own copy.
train_dir <- "~/path/to/PreFer/training_data"
holdout_dir <- "~/path/to/PreFer/holdout_data"

# Expected file names within those directories (can be modified).
train_data_file <- file.path(train_dir, "PreFer_train_data.csv")
train_outcome_file <- file.path(train_dir, "PreFer_train_outcome.csv")
train_bg_file <- file.path(train_dir, "PreFer_train_background_data.csv")

test_data_file <- file.path(holdout_dir, "PreFer_holdout_data.csv")
test_outcome_file <- file.path(holdout_dir, "PreFer_holdout_outcome.csv")
test_bg_file <- file.path(holdout_dir, "PreFer_holdout_background_data.csv")

# Output locations -----------------------------------------------------------
res_dir <- "results/" # tuning / evaluation metrics and timings (.rds)
model_dir <- "models/" # the bundled random forest (.rds)
figure_dir <- "figures/" # variable-importance and dependence plots (.png)

# Random seeds ---------------------------------------------------------------
# The published results were produced with 1234 for training and 61196 for the explanation stage.
# Keep the values for reproducing the results
seed_train <- 1234 # resampling, tuning grid, race, final fit
seed_explain <- 61196 # variable importance, PDPs, interaction strengths

# Number of repetitions for permutation- and Shapley-based importance.
n <- 30

# Helpers --------------------------------------------------------------------
# Fail early and legibly when a data file is missing, rather than deep inside
# fread() or the preprocessing.
require_file <- function(path) {
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop(
      "Data file not found:\n  ",
      path,
      "\n\nThe PreFer data is not included in this repository. Edit ",
      "`train_dir` / `holdout_dir` in config.R to point at your own copy ",
      "- see the 'Data availability' section of README.md.",
      call. = FALSE
    )
  }
  path
}

# Make sure the output directories exist.
for (d in c(res_dir, model_dir, figure_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Back-compatibility addition ----------------------------------------------------
# Restores a field that parsnip >= 1.6.0 expects on stored `model_fit` objects.
# models/final_rf.rds was serialised with an older parsnip, before `$ordered`
# existed; without this, predict() fails with
#   Error in if (ordered) "ordered" : argument is of length zero
# because predict_class.model_fit() calls factor(..., ordered = object$ordered).
# Purely additive: it restores the documented default (FALSE) and leaves the
# forest itself untouched, so predictions are identical.
restore_parsnip_compat <- function(fit) {
  if (is.null(fit$fit$fit$ordered)) {
    fit$fit$fit$ordered <- FALSE
  }
  fit
}
