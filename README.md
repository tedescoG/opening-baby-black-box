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

**The data is not included in this repository and cannot be redistributed.**

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

`.gitignore` excludes `holdout/` and `*.csv` so the microdata cannot be committed
by accident. Please keep it that way.

The fitted model `models/final_rf.rds` **is** included. A `ranger` probability
forest stores split thresholds and terminal-node predictions, not raw respondent
records, so it does not redistribute the underlying survey data.

---

## Dependencies

Set the project directory as the working directory and run:

```r
source("install_dependencies.R")
```

or from a shell, `Rscript install_dependencies.R`. It prints a table of installed
versions and stops with an error if anything is missing.

### One dependency needs special handling: `fastshap`

`fastshap` was **removed from CRAN on 2026-05-27**, so plain
`install.packages("fastshap")` no longer works. It is not optional — `src.R`
attaches it, and `vip(method = "shap")` calls `fastshap::explain()` internally.

`install_dependencies.R` handles this automatically, in two steps:

1. Installs **fastshap 0.1.1** (the last release, and the version used for the
   published results) from a dated Posit Package Manager snapshot that predates
   the archival. Pre-built binaries, no compiler required.
2. If that fails, falls back to building from the CRAN source archive. On macOS
   this additionally needs the official gfortran toolchain from
   <https://mac.r-project.org/tools/> — R's `Makeconf` unconditionally appends
   the Fortran runtime to the link line, so without `/opt/gfortran` the build
   fails with `ld: library not found for -lemutls_w`, even though fastshap itself
   is C++ only.

`renv.lock` also pins fastshap 0.1.1, so `renv::restore()` is the most reliable
route (see *Computational environment*).

---

## Structure

| Path | Contents |
|---|---|
| `config.R` | **The only file you need to edit.** Data paths, seeds, output directories. |
| `src.R` | All functions used by the analysis: preprocessing (`clean_df`), prediction wrappers, PDP plotting helpers. |
| `model_train.R` | Tunes and fits the random forest. See *Retraining* before running. |
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

`model_eval.R` is the quickest way to confirm your environment is correct. It
should reproduce `results/eval_results.rds` exactly:

| Metric | Value |
|---|---|
| F1 | 0.6974 |
| Accuracy | 0.8835 |
| Precision | 0.8281 |
| Recall / Sensitivity | 0.6023 |
| Specificity | 0.9642 |

(`recall` and `sensitivity` are the same quantity and are both reported because
the metric set lists both.)

**Run `model_explain.R` top to bottom in a fresh session.** Several blocks consume
random numbers, so results depend on the RNG stream being advanced in order.
Re-running individual chunks inside an existing session produces different — still
valid, but different — permutation, Shapley and conditional-PDP output.

### Random seeds

Seeds live in `config.R`. There are deliberately **two**, and they must not be
merged: `seed_train = 1234` for training, `seed_explain = 61196` for the
explanation stage. Collapsing them into one value would silently change the
explanation-stage results.

`model_eval.R` needs no seed — it only calls `predict()`, which is deterministic.

For the parallel importance methods, `model_explain.R` uses
`doRNG::registerDoRNG(seed = run)`. This makes each `%dopar%` stream reproducible
**and independent of the number of workers**, so `detectCores()` differing between
machines does not change the permutation or Shapley importances.

---

## Retraining

`models/final_rf.rds` and `results/tuning_results.rds` are **canonical artifacts**
and running `model_train.R` will not reproduce them. Two independent reasons:

1. The original run tuned under `tune` 1.x, which parallelised the race over
   `foreach`. `tune` >= 2.0 removed foreach support entirely and moved to `mirai`,
   a different execution and RNG path.
2. The final `fit()` was originally unseeded, so the forest depended on whatever
   RNG state the 250-configuration race left behind. A `set.seed()` has now been
   added, which makes future runs deterministic but cannot recover the original
   draw retroactively.

`model_train.R` therefore refuses to overwrite `models/final_rf.rds` if it already
exists. To retrain deliberately, move the existing file aside or set
`options(overwrite_final_model = TRUE)`.

All published holdout results derive from the shipped model, and `model_eval.R`
reproduces them exactly, so this does not limit verification of the reported
numbers.

### Parallel back-ends differ by script — on purpose

- `model_train.R` uses **mirai** (`mirai::daemons()`), because `tune` >= 2.0 no
  longer sees a foreach backend. Calling `registerDoParallel()` there is a silent
  no-op and the race would quietly run single-threaded.
- `model_explain.R` uses **foreach/doRNG** (`registerDoParallel()`), because `vip`
  still parallelises over foreach.

Please do not "harmonise" the two.

---

## Known provenance caveats

Recorded here for transparency:

- **Conditional partial-dependence figures.** The committed `figures/pd_con_*.png`
  and `results/explain_timing.rds` were produced before two missing seeds were
  added (the conditional `model_profile()` calls, which subsample with a default
  `N = 100`). They come from partial, interactive re-runs rather than one clean
  execution, which is also why `explain_timing.rds` holds 8 of the 12 timing
  entries the current script writes. The seeds are now in place; a single
  end-to-end run of `model_explain.R` regenerates all 12 timings and all
  conditional PDPs from one reproducible RNG stream. The unconditional PDPs, the
  importance rankings and the reported metrics are unaffected.
- **Figures are not pixel-identical across ggplot2 versions.** ggplot2 4.x is a
  major rewrite (S7 internals, theme-driven geom defaults), so regenerated figures
  differ cosmetically in legend spacing, fonts and bar outlines even when the
  underlying numbers match.
- **`.config` labels changed in `tune` 2.x** (`pre0_mod1_post0`, formerly
  `Preprocessor1_Model001`). `results/tuning_results.rds` carries the old labels.

---

## Computational environment

The published results were produced on a MacBook Pro (`Mac15,10`, Apple M3 Max,
14 CPU cores, 36 GB RAM) under macOS. Full detail in `session_info.txt`.

Two machine-readable records are included:

- **`renv.lock`** — the exact package versions. To restore them:
  ```r
  install.packages("renv")
  renv::restore()
  ```
  This is the reliable route, and it pins `fastshap` 0.1.1.
- **`session_info.txt`** — R version, platform and every attached/loaded package
  version. Regenerate with `Rscript session_info.R`.

A note on honesty about versions: the package versions used for the original
April-2025 run were never recorded and cannot now be reconstructed. `renv.lock`
captures the environment in which this package was **verified**, not the original
one. In that verified environment `model_eval.R` reproduces the published holdout
metrics exactly.

Approximate runtimes from `results/` (single machine, as configured above):
tuning ~3 min elapsed for the racing search; the embedded importance fits ~1–3 s
each; FIRM ~21 s; permutation and Shapley importance ~28–45 s each; full two-way
interaction strengths ~2 min; per-predictor interaction strengths ~7 min.

