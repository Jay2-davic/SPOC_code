#!/usr/bin/env Rscript

#' EIS Processing Pipeline
#'
#' @description
#' Complete pipeline for EIS file processing, Python analysis,
#' and integration with polymer electrolyte database.

suppressPackageStartupMessages({
  require(tidyverse)
  require(future)
  require(reticulate)
  require(glue)
})

future::multicore(workers = 4)

source("../shared/config.R")
source("functions.R")
source("../data_cleaning/functions.R")

paths <- get_config()

message(glue("EIS Processing v{paths$version_number}"))

tryCatch({
  # Load and combine data
  historical <- jsonlite::read_json("historical/outputs/historical_samples.json", simplifyVector = TRUE)
  active <- jsonlite::read_json("data_cleaning/outputs/active_samples.json", simplifyVector = TRUE)
  df_combined <- dplyr::bind_rows(data.frame(historical$samples), data.frame(active$samples))
  
  # Run EIS pipeline steps
  check_finished_files(paths$user_name)
  update_conversion_dictionary(paths$user_name)
  EIS_files <- update_data_dictionary(paths$user_name)
  
  df_combined <- clean_eis_filepaths(df_combined, paths$user_name)
  df_combined <- rename_and_map_files(df_combined, EIS_files)
  copy_eis_files(df_combined)
  
  # Python processing
  reticulate::use_condaenv(
    glue::glue('C:\\Users\\{paths$user_name}\\AppData\\Local\\miniforge3\\envs\\EIS_processing'), #change
    required = TRUE
  )
  reticulate::source_python("python/cycle_splitter.py")
  reticulate::source_python("python/autoprocess.py")
  
  # Compile and save
  eis_full <- compile_eis_files(paths$user_name)
  weather <- load_weather_data()
  
  combined_eis <- integrate_eis_data(df_combined, eis_full)
  combined_eis <- calculate_ionic_conductivity(combined_eis)
  combined_eis <- filter_eis_data(combined_eis)
  combined_eis <- dplyr::left_join(combined_eis, weather, by = "date_printed")
  
  output_dir <- "eis_processing/outputs"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  save_mpr_files_json(
    combined_eis |> dplyr::distinct(sample_ID, new_filename, .keep_all = TRUE),
    file.path(output_dir, "mpr_files.json")
  )
  save_cycles_json(combined_eis, file.path(output_dir, "cycles.json"))
  
  message(glue("✓ Complete: {n_distinct(combined_eis$new_filename)} MPR files, {nrow(combined_eis)} cycles\n"))
  quit(status = 0)
  
}, error = function(e) {
  message(glue("Error: {e$message}"))
  quit(status = 1)
})