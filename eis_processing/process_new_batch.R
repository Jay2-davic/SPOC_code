# eis_cleaning/process_new_batch.R
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(jsonlite)
  source("eis_processing/functions.R")
})

option_list <- list(
  make_option(c("-j", "--json"),     type = "character", help = "Input samples JSON"),
  make_option(c("-b", "--base_dir"), type = "character", help = "Base dir for relative mpr_files"),
  make_option(c("-o", "--output"),   type = "character", help = "Output JSON path"),
  make_option(c("-c", "--csv"),      type = "character", help = "Optional CSV path sample_id,mpr_file")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$json) || is.null(opt$output)) {
  stop("Usage: Rscript eis_processing/process_new_batch.R -j samples.json [-b base_dir] -o output.json [-c debug.csv]")
}

opt$json   <- path.expand(opt$json)
opt$output <- path.expand(opt$output)
if (!is.null(opt$base_dir)) opt$base_dir <- path.expand(opt$base_dir)
if (!is.null(opt$csv))      opt$csv      <- path.expand(opt$csv)

res <- load_sample_ids_and_mpr_files(opt$json, base_dir = opt$base_dir)
df  <- res$df
cat(sprintf("Loaded %d samples, %d with mpr_files\n", res$count, res$with_files))

dir.create(dirname(opt$output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(to_samples_json(df), opt$output, pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Wrote JSON to %s\n", opt$output))

if (!is.null(opt$csv)) {
  dir.create(dirname(opt$csv), recursive = TRUE, showWarnings = FALSE)
  mpr_long <- to_mpr_long(df, keep_empty = FALSE)
  write.csv(mpr_long, opt$csv, row.names = FALSE)
  cat(sprintf("Wrote CSV to %s (%d rows)\n", opt$csv, nrow(mpr_long)))
}