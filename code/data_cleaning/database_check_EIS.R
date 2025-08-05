# ============================================================================
# EIS File Status Checker
# 
# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte
#
# Description: Checks the database for finished EIS (Electrochemical Impedance
#              Spectroscopy) files by comparing raw data files with analyzed
#              results. Creates a status report indicating which files have
#              been processed.
#
# Version: 1.0
#
# Required packages:
#   - tidyverse: for data manipulation
#   - glue: for string interpolation
#   - stringr: for string manipulation
#   - stringi: for advanced string operations
#   - readr: for writing CSV files
# ============================================================================

# Load required packages -------------------------------------------------------
library(tidyverse)
library(glue)
library(stringr)
library(stringi)
library(readr)

# File Processing Functions ----------------------------------------------------

# Extract and clean file names from paths
extract_file_names <- function(filepath_vector, pattern_to_remove) {
  filepath_vector |>
    stringr::str_extract(pattern = '[^/]+$') |>  # Extract filename from path
    stringi::stri_replace_all_regex(pattern_to_remove, '')  # Remove extensions
}

# Collect MPR files from directory
collect_mpr_files <- function(directory_path) {
  tibble::tibble(
    filepath = list.files(directory_path, full.names = TRUE)
  ) |>
    dplyr::mutate(
      names = extract_file_names(filepath, '.csv')
    )
}

# Collect finished MPR files
collect_finished_files <- function(directory_path) {
  tibble::tibble(
    filepath_finished = list.files(
      directory_path,
      full.names = TRUE,
      pattern = '.csv'
    )
  ) |>
    dplyr::mutate(
      names = extract_file_names(
        filepath_finished, 
        '[.]csv|__|_randles|randles'
      )
    )
}

# Merge and flag files
merge_and_flag_files <- function(mpr_files, mpr_files_finished) {
  dplyr::full_join(mpr_files, mpr_files_finished) |>
    dplyr::mutate(
      file_done = dplyr::case_when(
        # Files present in finished directory are marked as FALSE (done)
        is.na(filepath_finished) == FALSE ~ FALSE,
        TRUE ~ TRUE  # Files not in finished directory are marked as TRUE (not done)
      )
    ) 
    #dplyr::select(filepath, file_done)
}

# Main Function ----------------------------------------------------------------

# Check EIS files for completion status
EIS_check_for_finished_files <- function(save_output = TRUE,
                                         return_data = FALSE) {
  # Define directory paths
  mpr_directory <- glue::glue(
    '~/Git/SPOC_code/data/processed_eis/'
  )
  
  # directory where mpr results are stored
  mpr_analyzed <- glue::glue(
    '~/Git/SPOC_code/data/eis_extracted/'
  ) 
  
  output_path <- glue::glue(
    '~/Git/SPOC_code/data/database/EIS_full_files.csv'
  )
  
  # Check if directories exist
  if (!dir.exists(mpr_directory)) {
    stop(glue::glue("MPR directory not found: {mpr_directory}"))
  }
  
  if (!dir.exists(mpr_analyzed)) {
    stop(glue::glue("Analyzed files directory not found: {mpr_analyzed}"))
  }
  
  # Collect and process files
  message("Collecting MPR files...")
  mpr_files <- collect_mpr_files(mpr_directory)
  
  message("Collecting finished files...")
  mpr_files_finished <- collect_finished_files(mpr_analyzed)
  
  # Merge and create status report
  message("Merging and flagging files...")
  mpr_merge <- merge_and_flag_files(mpr_files, mpr_files_finished)
  
  # Calculate summary statistics
  total_files <- nrow(mpr_merge)
  finished_files <- sum(!mpr_merge$file_done)
  pending_files <- sum(mpr_merge$file_done)
  
  # Print summary
  message("\n=== EIS File Status Summary ===")
  message(glue::glue("Total files: {total_files}"))
  message(glue::glue("Finished files: {finished_files}"))
  message(glue::glue("Pending files: {pending_files}"))
  message(glue::glue("Completion rate: {round(finished_files/total_files * 100, 1)}%"))
  
  # Save output if requested
  if (save_output) {
    readr::write_csv(mpr_merge, output_path)
    message(glue::glue("\nStatus report saved to: {output_path}"))
  }
  
  # Return data if requested
  if (return_data) {
    return(mpr_merge)
  }
}

# Wrapper function for backward compatibility ----------------------------------

# Simple wrapper matching original function call
check_eis_files <- function() {
  
  # Run the main function
  EIS_check_for_finished_files(
    save_output = TRUE,
    return_data = TRUE
  )
}

# Usage Examples ---------------------------------------------------------------

# # Option 1: Use the simple wrapper (matches original usage)
check_eis_files()
# 
# # Option 2: Use main function with custom parameters
# results <- EIS_check_for_finished_files(
#   user_name = "jimenez45",
#   save_output = TRUE,
#   return_data = TRUE
# )
# 
# # Option 3: Just get the data without saving
# data_only <- EIS_check_for_finished_files(
#   save_output = FALSE,
#   return_data = TRUE
# )

# Clean up workspace -----------------------------------------------------------
# rm(list = ls())