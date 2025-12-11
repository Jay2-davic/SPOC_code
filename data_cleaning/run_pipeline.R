suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  source("data_cleaning/functions.R")  # assumes all helpers are here
})

options(warn = -1)

# --- PARAMETERS ---
# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Usage: Rscript run_pipeline.R <excel_dir> <output_path>
if (length(args) < 2) {
  stop("Usage: Rscript run_pipeline.R <excel_dir> <output_path>")
}

excel_dir   <- args[1]
output_path <- args[2]

# You can still hardcode these, or make them arguments if needed:
mpr_cols <- c("eis_mpr", "x2eis_mpr", "x3eis_mpr", "x4eis_mpr")
prefix   <- "General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\"
# --- 1. Load and Clean Excel Files ---
excel_files <- list.files(excel_dir, pattern = "\\.xls[x]?$", full.names = TRUE)

all_cleaned <- lapply(excel_files, function(file) {
  df <- readxl::read_excel(file)
  poly3_cleaning(df)  # robust cleaning: drops bad cols, merges dups, normalizes NA
})

# --- 2. Auto-detect numeric columns (defensive) ---
all_colnames <- unique(unlist(lapply(all_cleaned, names)))
guess_numeric <- function(col, dfs, threshold = 0.8) {
  vals <- unlist(lapply(dfs, function(df) if (col %in% names(df)) df[[col]] else NULL), use.names = FALSE)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  suppressWarnings({
    nums <- as.numeric(vals)
  })
  mean(!is.na(nums)) >= threshold
}
numeric_cols_auto <- all_colnames[sapply(all_colnames, guess_numeric, dfs = all_cleaned, threshold = 0.8)]

# --- 3. Harmonize types (never coerce chemistry/material fields) ---
all_cleaned <- lapply(all_cleaned, function(df) {
  convert_types(df, numeric_cols_auto)
})

# --- 4. Consolidate Dataframes ---
all_cleaned_df <- bind_rows(all_cleaned)

# --- 5. Syringe Renaming and Additive Combining ---
all_cleaned_df <- rename_syringe_columns(all_cleaned_df)

# --- 6. Add explicit material_1 columns for forward compatibility ---
all_cleaned_df <- augment_syringe_material_1(all_cleaned_df)

# --- 7. Ensure all expected syringe columns exist (typed NA) ---
all_cleaned_df <- ensure_syringe_columns(all_cleaned_df)

# --- 8. Ensure sample_ID column exists and standardized ---
all_cleaned_df <- ensure_sample_id(all_cleaned_df)

# --- 9. Clean mpr paths for portability ---
all_cleaned_df <- clean_mpr_paths(all_cleaned_df, mpr_cols, prefix)

# --- 10. Feature engineering/calculations ---
all_cleaned_df <- finalize_samples(all_cleaned_df)

# --- 11. Add stable row order for best-row summarization ---
all_cleaned_df <- all_cleaned_df %>%
  mutate(.row_order = row_number())

# --- 12. Summarize samples: pick most complete row per sample ---
samples_grouped <- summarize_samples_bestrow(all_cleaned_df, mpr_cols)

# --- 13. Export to JSON ---
append_or_write_samples_json_atomic(
  new_samples_df = samples_grouped,
  output_path    = output_path,
  merge_strategy = "append_only"  # options: "append_only", "prefer_existing", "prefer_new"
)

cat(glue::glue("Pipeline complete. Output saved to {output_path}\n"))