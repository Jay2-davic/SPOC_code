# eis_cleaning/functions.R

suppressPackageStartupMessages({
  library(jsonlite)
  library(tibble)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Detect Windows absolute paths: C:\... or \\server\share
is_absolute_path <- function(p) {
  if (is.null(p) || !nzchar(p)) return(FALSE)
  grepl("^[A-Za-z]:", p) || grepl("^\\\\\\\\", p)
}

# Replace NBSP with space; turn backslashes into forward slashes, dedupe slashes
sanitize_path <- function(p) {
  if (is.null(p)) return(NA_character_)
  p <- gsub("\u00A0", " ", p)     # NBSP -> space
  p <- gsub("\\\\", "/", p)       # backslashes -> forward slashes
  p <- gsub("//+", "/", p)        # dedupe
  trimws(p)
}

# Normalize mpr_files to character vector; apply base_dir only to relative paths
normalize_mpr_files <- function(x, base_dir = NULL) {
  if (is.null(x)) return(character(0))
  v <- unlist(x, use.names = FALSE)
  v <- vapply(v, sanitize_path, character(1))
  if (!is.null(base_dir) && nzchar(base_dir)) {
    base_dir <- sanitize_path(base_dir)
    v <- vapply(v, function(p) if (is_absolute_path(p)) p else sanitize_path(file.path(base_dir, p)), character(1))
  }
  unique(v)
}

# Load minimal df: sample_id and list-column mpr_files
load_sample_ids_and_mpr_files <- function(json_path, base_dir = NULL) {
  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  samples <- raw$samples %||% list()
  if (length(samples) == 0) {
    return(list(
      df = tibble(sample_id = character(), mpr_files = list()),
      count = 0L, with_files = 0L
    ))
  }
  rows <- lapply(names(samples), function(sid) {
    mpr <- samples[[sid]][["mpr_files"]]
    tibble(sample_id = sid, mpr_files = list(normalize_mpr_files(mpr, base_dir = base_dir)))
  })
  df <- dplyr::bind_rows(rows)
  list(df = df, count = nrow(df), with_files = sum(lengths(df$mpr_files) > 0))
}

# Convert df back into { "samples": { id: { mpr_files: [...] } } }
to_samples_json <- function(df) {
  samples_list <- setNames(
    lapply(seq_len(nrow(df)), function(i) list(mpr_files = df$mpr_files[[i]])),
    df$sample_id
  )
  list(samples = samples_list)
}

# Long view: one row per file
to_mpr_long <- function(df, keep_empty = FALSE) {
  out <- dplyr::bind_rows(lapply(seq_len(nrow(df)), function(i) {
    files <- df$mpr_files[[i]]
    if (length(files) == 0 && !keep_empty) return(NULL)
    tibble(sample_id = df$sample_id[i], mpr_file = files)
  }))
  if (is.null(out)) tibble(sample_id = character(), mpr_file = character()) else out
}

# Merge Python results JSON into the samples JSON under mpr_results
# Expects results_json:
# { "results": { "KR_01": { "file1.mpr": { "cycles": 3, "cycle_csv": [...] }, ... }, ... } }
merge_cycle_results_json <- function(updated_json_path, cycle_results_json, output_json_path) {
  upd <- jsonlite::fromJSON(updated_json_path, simplifyVector = FALSE)
  res <- jsonlite::fromJSON(cycle_results_json, simplifyVector = FALSE)
  results <- res$results %||% list()
  if (length(results) == 0) {
    warning("No 'results' in cycle_results.json. Skipping merge.")
    jsonlite::write_json(upd, output_json_path, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(NULL))
  }
  for (sid in names(upd$samples)) {
    upd$samples[[sid]]$mpr_results <- upd$samples[[sid]]$mpr_results %||% list()
    if (!is.null(results[[sid]])) {
      for (fname in names(results[[sid]])) {
        upd$samples[[sid]]$mpr_results[[fname]] <- results[[sid]][[fname]]
      }
    }
  }
  jsonlite::write_json(upd, output_json_path, pretty = TRUE, auto_unbox = TRUE)
}

# Attach cycle CSVs from a directory by matching basename before _RUN_
# Writes under mpr_results[basename.mpr] = { cycles, cycle_csv }
attach_cycle_dir_results <- function(updated_json_path, cycle_dir, output_json_path) {
  upd <- jsonlite::fromJSON(updated_json_path, simplifyVector = FALSE)
  if (!dir.exists(cycle_dir)) {
    warning("cycle_dir does not exist: ", cycle_dir)
    jsonlite::write_json(upd, output_json_path, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(NULL))
  }
  csvs <- list.files(cycle_dir, pattern = "_RUN_\\d+\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) {
    warning("No cycle CSVs found in: ", cycle_dir)
    jsonlite::write_json(upd, output_json_path, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(NULL))
  }
  base_map <- split(csvs, sub("_RUN_\\d+\\.csv$", "", basename(csvs)))
  for (sid in names(upd$samples)) {
    upd$samples[[sid]]$mpr_results <- upd$samples[[sid]]$mpr_results %||% list()
    files <- upd$samples[[sid]]$mpr_files %||% character(0)
    for (p in files) {
      base <- sub("\\.mpr$", "", basename(p), ignore.case = TRUE)
      if (!is.null(base_map[[base]])) {
        upd$samples[[sid]]$mpr_results[[paste0(base, ".mpr")]] <- list(
          cycles = length(base_map[[base]]),
          cycle_csv = as.list(normalizePath(base_map[[base]], winslash = "/", mustWork = FALSE))
        )
      }
    }
  }
  jsonlite::write_json(upd, output_json_path, pretty = TRUE, auto_unbox = TRUE)
}