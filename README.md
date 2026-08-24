# Opening the Baby Black Box: Explainability in Fertility Prediction from Classical Predictors to Social Media Use and Political Orientation

#### G. Tedesco, B. Arpino

Replication package for the article *"Opening the Baby Black Box: Explainability
in Fertility Prediction from Classical Predictors to Social Media Use and
Political Orientation"*.

The analysis trains a random forest to predict whether a respondent has a child
within a given window, then compares six variable-importance methods
(impurity, permutation, FIRM, permutation-F1, permutation-AUC and aggregated
Shapley) and examines partial dependence and two-way interaction strength for
the most important and two newly proposed predictors (`insta`, `soc_lib`).

---

## Data availability

**The data is not included in this repository.**

The analysis uses the PreFer Data Challenge data, derived from the LISS panel
(Longitudinal Internet studies for the Social Sciences, Centerdata / Tilburg
University). Access is granted under a data agreement and the terms do not permit
redistribution, so neither the training nor the holdout files are shipped here.

Researchers can request access through the PreFer / ODISSEI channels
(<https://preferdatachallenge.nl>) and via the LISS panel
(<https://www.lissdata.nl>). Once you have your own copy, point `config.R` at it.

The scripts expect these files:

| Variable in `config.R` | File |
|---|---|
| `train_dir`   | `PreFer_train_data.csv`, `PreFer_train_outcome.csv` |
| `train_dir`   | `PreFer_train_background_data.csv` (ships in `other_data/` — see note in `config.R`) |
| `holdout_dir` | `PreFer_holdout_data.csv`, `PreFer_holdout_outcome.csv`, `PreFer_holdout_background_data.csv` |


The fitted model `models/final_rf.rds` **is** included. 

---

## Dependencies

Set the project directory as the working directory and run:

```r
source("install_dependencies.R")
```

or from a shell, `Rscript install_dependencies.R`. It prints a table of installed versions and stops with an error if anything is missing.

### Special handling: `fastshap`

`fastshap` was **removed from CRAN on 2026-05-27**, so plain
`install.packages("fastshap")` no longer works. It is not optional — `src.R`
attaches it, and `vip(method = "shap")` calls `fastshap::explain()` internally.

`install_dependencies.R` handles this automatically.

---

## Structure

| Path | Contents |
|---|---|
| `config.R` | **The only file you need to edit.** Data paths, seeds, output directories. |
| `src.R` | All functions used by the analysis: preprocessing (`clean_df`), prediction wrappers, PDP plotting helpers. |
| `model_train.R` | Tunes and fits the random forest.|
| `model_eval.R` | Evaluates the fitted model on the holdout set. |
| `model_explain.R` | The variable-importance, partial-dependence and interaction study. |
| `install_dependencies.R` | Installs every required package. |
| `session_info.R` | Writes `session_info.txt`, the environment record. |
| `models/` | `final_rf.rds` — the fitted model used for all published results. |
| `results/` | Tuning metrics, holdout evaluation metrics, and timings (`.rds`). |
| `figures/` | All figures from the variable-importance study. |

---

## How to run it

1. Install dependencies (above).
2. Open `config.R` and set `train_dir` and `holdout_dir` to your copy of the data.
3. Run the scripts **from the project root**, either in RStudio or headless:

```sh
Rscript model_eval.R      # fast: loads the fitted model, evaluates on holdout
Rscript model_explain.R   # slow: the full variable-importance study
```

`model_eval.R` is the quickest way to confirm your environment is correct. It should reproduce `results/eval_results.rds` exactly:

| Metric | Value |
|---|---|
| F1 | 0.6974 |
| Accuracy | 0.8835 |
| Precision | 0.8281 |
| Recall / Sensitivity | 0.6023 |
| Specificity | 0.9642 |


**Run `model_explain.R` top to bottom in a fresh session.** Several blocks consume
random numbers, so results depend on the RNG stream being advanced in order.
Re-running individual chunks inside an existing session produces different permutation, Shapley and conditional-PDP output.

---

## Retraining

`model_train.R` refuses to overwrite `models/final_rf.rds` if it already exists. To retrain deliberately, move the existing file aside or set `options(overwrite_final_model = TRUE)`.

All published holdout results derive from the shipped model, and `model_eval.R` reproduces them exactly.

### Parallel back-ends

- `model_train.R` uses **mirai** (`mirai::daemons()`), because `tune` >= 2.0 no longer sees a foreach backend. Calling `registerDoParallel()`.
- `model_explain.R` uses **foreach/doRNG** (`registerDoParallel()`), because `vip parallelises over foreach.
---

## Computational environment

The published results were produced on a MacBook Pro (`Mac15,10`, Apple M3 Max, 14 CPU cores, 36 GB RAM) under macOS. Full detail in `session_info.txt`.

Session records provided:

- **`renv.lock`** — the exact package versions. To restore them:
  ```r
  install.packages("renv")
  renv::restore()
  ```
  This is the reliable route, and it pins `fastshap` 0.1.1.
- **`session_info.txt`** — R version, platform and every attached/loaded package version. Regenerate with `Rscript session_info.R`.


---

## License

The code in this repository is released under the [MIT License](LICENSE).
The PreFer / LISS microdata is **not** part of this repository and is not
covered by this license — it remains subject to the Centerdata / LISS data
agreement (see *Data availability*).

