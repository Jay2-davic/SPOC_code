# eis_processing/run_pipeline.R
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(jsonlite)
  library(dplyr)
  library(tibble)
  library(utils)
})

# Resolve script directory robustly
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", args)
  if (length(i) > 0) return(dirname(normalizePath(sub("^--file=", "", args[i]))))
  getwd()
}
script_dir <- get_script_dir()

# All outputs will go in this directory:
data_dir <- file.path(script_dir, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Source shared helpers
fn_path <- file.path(script_dir, "..", "eis_cleaning", "functions.R")
if (!file.exists(fn_path)) stop("functions.R not found at: ", fn_path)
source(fn_path)  # provides: %||%, normalize_mpr_files, load_sample_ids_and_mpr_files, to_samples_json, to_mpr_long

# ---- CLI Options ----

suppressPackageStartupMessages({
  library(optparse)
  library(jsonlite)
  library(dplyr)
  library(tibble)
})

option_list <- list(
  make_option(c("-j", "--json"),      type = "character", help = "Input samples JSON, expects top-level 'samples'"),
  make_option(c("-b", "--base_dir"),  type = "character", help = "Base dir for relative mpr_files, not applied to absolute paths"),
  make_option(c("-o", "--output"),    type = "character", help = "Final output JSON path"),
  make_option(c("-c", "--csv"),       type = "character", help = "Optional debug CSV of sample_id,mpr_file"),
  # Python integration
  make_option(c("-s", "--cycle_py"),  type = "character", help = "Path to cycle_splitter.py"),
  make_option(c("-p", "--python"),    type = "character", help = "Python executable, else tries venv or PATH"),
  make_option(c("-v", "--venv"),      type = "character", help = "Python venv dir"),
  make_option(c("-r", "--results"),   type = "character", help = "Optional cycle_results.json written by Python"),
  make_option(c("-x", "--cycle_args"),type = "character", help = "Extra args to pass to cycle_splitter.py")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$json) || is.null(opt$output)) {
  stop("Usage: Rscript run_pipeline.R -j samples.json [-b base_dir] -o output.json [-c debug.csv] [-s cycle_splitter.py] [-p python|-v venv] [-r results.json] [-x extra_args]")
}

# ---- Path Expansion (all outputs go to data_dir) ----

default_base_dir <- script_dir
opt$json     <- path.expand(opt$json)
opt$output   <- file.path(data_dir, basename(opt$output))
opt$base_dir <- if (is.null(opt$base_dir) || !nzchar(opt$base_dir)) default_base_dir else path.expand(opt$base_dir)
if (!is.null(opt$csv))       opt$csv       <- file.path(data_dir, basename(opt$csv))
if (!is.null(opt$python))    opt$python    <- path.expand(opt$python)
if (!is.null(opt$venv))      opt$venv      <- path.expand(opt$venv)
if (!is.null(opt$cycle_py))  opt$cycle_py  <- path.expand(opt$cycle_py)
if (!is.null(opt$results))   opt$results   <- file.path(data_dir, basename(opt$results))

# ---- Early JSON Validation ----

if (!file.exists(opt$json)) stop("Input JSON not found: ", opt$json)
json_txt <- tryCatch(readChar(opt$json, file.info(opt$json)$size), error = function(e) e)
if (inherits(json_txt, "error")) stop("Failed to read JSON: ", conditionMessage(json_txt))
if (!jsonlite::validate(json_txt)) stop("Input JSON is invalid: ", opt$json)

# ---- Step 1: Build compact samples JSON in memory ----

res <- load_sample_ids_and_mpr_files(opt$json, base_dir = opt$base_dir)
df  <- res$df
upd_list <- to_samples_json(df)  # in-memory { samples: { id: { mpr_files: [...] } } }

cat(sprintf("Base dir: %s\n", opt$base_dir))
cat(sprintf("Loaded %d samples, %d with mpr_files\n", res$count, res$with_files))

# Optional CSV exports for granularity right away
mpr_long <- to_mpr_long(df, keep_empty = FALSE)
mpr_long$exists <- file.exists(mpr_long$mpr_file)
write.csv(mpr_long, file.path(data_dir, "mpr_long.csv"), row.names = FALSE)
write.csv(mpr_long[!mpr_long$exists, ], file.path(data_dir, "mpr_missing.csv"), row.names = FALSE)
write.csv(mpr_long[mpr_long$exists, c("sample_id", "mpr_file")], file.path(data_dir, "mpr_long_existing.csv"), row.names = FALSE)
cat(sprintf("Files present: %d, missing: %d\n", sum(mpr_long$exists), sum(!mpr_long$exists)))

# ---- Step 2: Run Python cycle splitter and capture stdout JSON ----

find_python <- function(py, venv) {
  if (!is.null(py) && nzchar(py) && file.exists(py)) return(py)
  if (!is.null(venv) && nzchar(venv)) {
    cand <- if (.Platform$OS.type == "windows") file.path(venv, "Scripts", "python.exe") else file.path(venv, "bin", "python")
    if (file.exists(cand)) return(cand)
  }
  "python"
}

run_cycle_splitter_system <- function(opt) {
  if (is.null(opt$cycle_py) || !nzchar(opt$cycle_py) || !file.exists(opt$cycle_py)) {
    stop("cycle_splitter.py not found: ", opt$cycle_py)
  }
  input_json <- opt$output
  py_exe <- find_python(opt$python, opt$venv)
  args <- c(opt$cycle_py, input_json)
  if (!is.null(opt$cycle_args) && nzchar(opt$cycle_args)) {
    args <- c(args, strsplit(opt$cycle_args, "\\s+")[[1]])
  }
  
  cat("Running Python via system2:\n")
  cat(sprintf("  %s %s\n", py_exe, paste(args, collapse = " ")))
  
  if (!is.null(opt$results) && nzchar(opt$results)) {
    dir.create(dirname(opt$results), recursive = TRUE, showWarnings = FALSE)
    status <- system2(command = py_exe, args = args, stdout = opt$results, stderr = TRUE)
    if (status != 0) stop("Python step failed with status ", status)
    cat(sprintf("Captured results JSON to %s\n", opt$results))
  } else {
    out_tmp <- tempfile("cycle_splitter_stdout_", fileext = ".json")
    status <- system2(command = py_exe, args = args, stdout = out_tmp, stderr = TRUE)
    if (status != 0) stop("Python step failed with status ", status)
    cat(sprintf("Python stdout captured to %s\n", out_tmp))
  }
}

if (!is.null(opt$cycle_py) && nzchar(opt$cycle_py)) {
  run_cycle_splitter_system(opt)
} else {
  cat("No cycle_splitter.py provided, skipping Python step.\n")
}

# ---- Step 3: Merge Python outputs into JSON ----

has_results_json <- !is.null(opt$results) && nzchar(opt$results) && file.exists(opt$results)

cat(sprintf("Merging mode: %s\n",
            if (has_results_json) "results JSON"
            else "none"
))

if (has_results_json) {
  # Option: Python wrote a results JSON
  merge_cycle_results_json(opt$output, opt$results, opt$output)
  cat(sprintf("Merged cycle results from %s into %s\n", opt$results, opt$output))
  
  # Flatten results to CSV for inspection
  resj <- jsonlite::fromJSON(opt$results, simplifyVector = FALSE)
  results <- resj$results %||% list()
  
  rows <- list()
  for (sid in names(results)) {
    per_file <- results[[sid]]
    if (is.null(per_file)) next
    for (fname in names(per_file)) {
      item <- per_file[[fname]] %||% list()
      rows[[length(rows) + 1]] <- data.frame(
        sample_id       = sid,
        file_basename   = fname,
        cycles          = as.integer(item$cycles %||% NA_integer_),
        cycle_count     = length(item$cycle_data %||% list()),
        stringsAsFactors = FALSE
      )
    }
  }
  results_flat <- if (length(rows) > 0) dplyr::bind_rows(rows) else
    tibble::tibble(sample_id = character(), file_basename = character(), cycles = integer(),
                   cycle_count = integer())
  csv_results_path <- file.path(data_dir, "results_flat.csv")
  write.csv(results_flat, csv_results_path, row.names = FALSE)
  cat(sprintf("Wrote results CSV: %s (%d rows)\n", csv_results_path, nrow(results_flat)))
  
} else {
  cat("No results JSON provided; JSON contains filepaths only.\n")
}

# ---- Step 4: Write final JSON once ----

jsonlite::write_json(upd_list, opt$output, pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Wrote final JSON to %s\n", opt$output))

# Optional debug CSV requested explicitly
if (!is.null(opt$csv)) {
  dir.create(dirname(opt$csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(mpr_long, opt$csv, row.names = FALSE)
  cat(sprintf("Wrote debug CSV to %s (%d rows)\n", opt$csv, nrow(mpr_long)))
}

cat("Pipeline done.\n")