# Records the computational environment used to run the analysis.
#
#   Rscript session_info.R
#
# Writes session_info.txt: R version, platform, and the version of every package
# attached or loaded by the analysis. Run it in the same session type you use for
# the analysis, so the record reflects what actually produced the results.
#
# This text file - together with renv.lock - is the environment record for the
# replication package. A screenshot of system settings is not a substitute: it
# cannot be diffed, searched, or used to restore anything.

source("config.R")
source("src.R")   # attaches every package the analysis uses

out = "session_info.txt"

header = c(
  "Computational environment for:",
  "Opening the Baby Black Box: Explainability in Fertility Prediction from Classical Predictors to Social Media Use and Political Orientation",
  "G. Tedesco, B. Arpino",
  "",
  paste("Recorded:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Working directory:", getwd()),
  ""
)

body = if (requireNamespace("sessioninfo", quietly = TRUE)) {
  capture.output(sessioninfo::session_info())
} else {
  # Fallback if sessioninfo is unavailable; less detailed but always present.
  capture.output(sessionInfo())
}

# Hardware. R exposes no portable way to read this, so shell out where we can and
# fall back to a clearly-marked placeholder otherwise.
sysctl = function(key) {
  if (Sys.info()[["sysname"]] != "Darwin") return(NA_character_)
  out = suppressWarnings(tryCatch(system2("sysctl", c("-n", key), stdout = TRUE,
                                          stderr = FALSE),
                                  error = function(e) NA_character_))
  if (length(out) != 1 || !nzchar(out)) NA_character_ else out
}

model = sysctl("hw.model")
cpu   = sysctl("machdep.cpu.brand_string")
mem   = sysctl("hw.memsize")
ram   = if (is.na(mem)) NA_character_ else
  paste0(round(as.numeric(mem) / 1024^3), " GB")

or_fill = function(x, hint) if (is.na(x)) paste0("[fill in: ", hint, "]") else x

hardware = c(
  "",
  "-- Hardware -------------------------------------------------------------------",
  paste(" Model                 ", or_fill(model, "e.g. MacBookPro21,1")),
  paste(" CPU                   ", or_fill(cpu,   "e.g. Apple M3, 14 cores")),
  paste(" CPU cores (detected)  ", parallel::detectCores()),
  paste(" RAM                   ", or_fill(ram,   "e.g. 36 GB")),
  paste(" Platform              ", R.version$platform),
  paste(" OS                    ", paste(Sys.info()[c("sysname", "release")], collapse = " "))
)

writeLines(c(header, body, hardware), out)
cat("Wrote", out, "\n")
