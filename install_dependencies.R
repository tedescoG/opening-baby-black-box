# Installs every package required by the analysis.
#
# Usage (from the project root):
#   source("install_dependencies.R")
# or
#   Rscript install_dependencies.R
#
# An explicit mirror is set below: without it, install.packages() fails in a
# non-interactive session with "trying to use CRAN without setting a mirror".

CRAN <- "https://cloud.r-project.org"

packages <- c(
  # data handling
  "tidyverse",
  "data.table",
  # modelling
  "tidymodels",
  "dials",
  "finetune",
  "ranger",
  # model serialisation
  "bundle",
  "butcher",
  # parallel back-ends: mirai for tuning (tune >= 2.0), foreach/doRNG for vip
  "mirai",
  "doParallel",
  "doRNG",
  # explanation / visualisation
  "patchwork",
  "pdp",
  "vip",
  "iml",
  "DALEX",
  "DALEXtra",
  "shapviz",
  # infrastructure
  "rstudioapi", # optional convenience: auto-setwd inside RStudio
  "remotes", # needed to install fastshap from the CRAN archive
  "renv", # environment restore (renv.lock)
  "sessioninfo" # writes session_info.txt
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, " ...")
    install.packages(pkg, repos = CRAN)
  }
}

invisible(lapply(packages, install_if_missing))

# ---- fastshap ---------------------------------------------------------------
# fastshap was REMOVED from CRAN on 2026-05-27 ("issues were not corrected
# despite reminders"), so install.packages("fastshap") no longer works. It is
# not optional: src.R attaches it and vip(method = "shap") calls
# fastshap::explain() internally.
#
# Version 0.1.1 is the last release and the one used for the published results,
# so either route below reproduces the analysis exactly.
#
# Route 1 (preferred): a dated Posit Package Manager snapshot from before the
# archival still serves fastshap 0.1.1, including pre-built binaries for macOS
# and Windows. No compiler needed.
#
# Route 2 (fallback): build from the CRAN archive source. This needs a working
# compiler toolchain. On macOS it also needs the official gfortran toolchain
# from https://mac.r-project.org/tools/
P3M_SNAPSHOT <- "https://packagemanager.posit.co/cran/2026-05-01"

if (!requireNamespace("fastshap", quietly = TRUE)) {
  message(
    "Installing fastshap 0.1.1 (binary) from the Posit snapshot ",
    P3M_SNAPSHOT,
    " ..."
  )
  try(
    install.packages(
      "fastshap",
      repos = P3M_SNAPSHOT,
      type = if (
        .Platform$OS.type == "unix" &&
          Sys.info()[["sysname"]] == "Linux"
      ) {
        "source"
      } else {
        "binary"
      }
    ),
    silent = TRUE
  )
}

if (!requireNamespace("fastshap", quietly = TRUE)) {
  message(
    "Binary install failed; building fastshap 0.1.1 from the CRAN archive ..."
  )
  message(
    "(macOS: this needs the gfortran toolchain from https://mac.r-project.org/tools/)"
  )
  try(
    remotes::install_version(
      "fastshap",
      version = "0.1.1",
      repos = CRAN,
      upgrade = "never"
    ),
    silent = TRUE
  )
}

# ---- report -----------------------------------------------------------------
all_packages <- c(packages, "fastshap")

versions <- vapply(
  all_packages,
  function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) {
      "MISSING"
    })
  },
  character(1)
)

cat(
  "\nR version: ",
  R.version.string,
  "\n",
  "Platform:  ",
  R.version$platform,
  "\n\n",
  sep = ""
)
print(data.frame(package = all_packages, version = versions, row.names = NULL))

missing <- all_packages[versions == "MISSING"]
if (length(missing)) {
  cat("\n")
  stop(
    "Could not install: ",
    paste(missing, collapse = ", "),
    "\nThe analysis scripts will not run until these are available. ",
    "See the 'Dependencies' section of README.md.",
    call. = FALSE
  )
} else {
  cat("\nAll dependencies present.\n")
}
