# ============================================================================
# MPR File Conversion Data Dictionary
# 
# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte
#
# Description: Creates a data dictionary to map converted CSV files back to 
#              their parent MPR (EC-Lab) files. This maintains traceability
#              between original experimental data and processed files.
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

# Extract base filename from path
extract_base_filename <- function(filepath, extension_to_remove) {
  filepath |>
    stringr::str_extract(pattern = '[^/]+$') |>  # Extract filename from path
    stringi::stri_replace_all_regex(extension_to_remove, '')  # Remove extension
}

# Process MPR files
process_mpr_files <- function(directory_path) {
  # Check if directory exists
  if (!dir.exists(directory_path)) {
    stop(glue::glue("MPR directory not found: {directory_path}"))
  }
  
  # Get all MPR files
  mpr_files <- tibble::tibble(
    filepath = list.files(
      directory_path,
      pattern = '\\.mpr$',
      full.names = TRUE
    )
  )
  
  # Check if any files found
  if (nrow(mpr_files) == 0) {
    warning(glue::glue("No MPR files found in: {directory_path}"))
    return(tibble::tibble())
  }
  
  # Extract filenames
  mpr_files |>
    dplyr::mutate(
      new_filename = extract_base_filename(filepath, '\\.mpr')
    )
}

# Process converted CSV files
process_converted_files <- function(directory_path) {
  # Check if directory exists
  if (!dir.exists(directory_path)) {
    stop(glue::glue("Converted files directory not found: {directory_path}"))
  }
  
  # Get all CSV files
  csv_files <- tibble::tibble(
    file_conv = list.files(
      directory_path,
      pattern = '\\.csv$',
      full.names = TRUE
    )
  )
  
  # Check if any files found
  if (nrow(csv_files) == 0) {
    warning(glue::glue("No CSV files found in: {directory_path}"))
    return(tibble::tibble())
  }
  
  # Extract filenames and remove run number suffix
  csv_files |>
    dplyr::mutate(
      new_filename = stringr::str_extract(file_conv, pattern = '[^/]+$') |>
        stringr::str_remove(pattern = '_RUN_\\d+\\.csv')
    )
}

# Create mapping between MPR and CSV files
create_file_mapping <- function(mpr_data, csv_data) {
  # Join on the cleaned filename
  mapping <- dplyr::full_join(
    csv_data, 
    mpr_data, 
    by = "new_filename"
  ) |>
    dplyr::select(filepath, file_conv) |>
    # Keep only matched files
    dplyr::filter(!is.na(filepath))
  
  # Add summary information
  n_mpr <- nrow(mpr_data)
  n_csv <- nrow(csv_data)
  n_matched <- nrow(mapping)
  n_unmatched_mpr <- sum(is.na(mapping$file_conv))
  n_unmatched_csv <- n_csv - n_matched
  
  attr(mapping, "summary") <- list(
    n_mpr_files = n_mpr,
    n_csv_files = n_csv,
    n_matched = n_matched,
    n_unmatched_mpr = n_unmatched_mpr,
    n_unmatched_csv = n_unmatched_csv,
    match_rate = round(n_matched / max(n_mpr, n_csv) * 100, 1)
  )
  
  return(mapping)
}

# Main Function ----------------------------------------------------------------

# Create conversion data dictionary
conversion_dataDictionary <- function(save_output = TRUE,
                                      return_data = FALSE,
                                      verbose = TRUE) {
  
  # specify folder directory where cleaned raw eis files are located
  mpr_directory <- glue::glue(
    # '~/Git/SPOC_code/data/raw_eis/'
    '~/Database of polymer electrolytes/mpr_new_names'
  )
  
  # specify folder directory to save the processed files
  converted_directory <- glue::glue(
    # '~/Git/SPOC_code/data/processed_eis/'
    '~/Database of polymer electrolytes/eis_results'
  ) 
  
  output_path <- glue::glue(
    '~/Git/SPOC_code/data/database/database_conversion_dictionary.csv'
  )
  
  # Process files
  if (verbose) message("Processing MPR files...")
  mpr_data <- process_mpr_files(mpr_directory)
  
  if (verbose) message("Processing converted CSV files...")
  csv_data <- process_converted_files(converted_directory)
  
  # Create mapping
  if (verbose) message("Creating file mapping...")
  mapping <- create_file_mapping(mpr_data, csv_data)
  
  # Get summary
  summary_info <- attr(mapping, "summary")
  
  # Print summary
  if (verbose) {
    message("\n=== Conversion Dictionary Summary ===")
    message(glue::glue("MPR files found: {summary_info$n_mpr_files}"))
    message(glue::glue("CSV files found: {summary_info$n_csv_files}"))
    message(glue::glue("Successfully matched: {summary_info$n_matched}"))
    message(glue::glue("Match rate: {summary_info$match_rate}%"))
    
    if (summary_info$n_unmatched_csv > 0) {
      warning(glue::glue("{summary_info$n_unmatched_csv} CSV files could not be matched to MPR files"))
    }
  }
  
  # Save output if requested
  if (save_output) {
    readr::write_csv(mapping, output_path)
    if (verbose) {
      message(glue::glue("\nData dictionary saved to: {output_path}"))
    }
  }
  
  # Return data if requested
  if (return_data) {
    return(mapping)
  }
}

# Additional Analysis Functions ------------------------------------------------

# Find unmatched files
find_unmatched_files <- function(user_name = NULL) {
  # Set default user name if not provided
  if (is.null(user_name)) {
    user_name <- Sys.info()["user"]
  }
  
  # Define directory paths
  mpr_directory <- glue::glue(
    '~/Git/SPOC_code/data/raw_eis/'
  )
  
  converted_directory <- glue::glue(
    '~/Git/SPOC_code/data/processed_eis/'
  ) 
  
  # Process files
  mpr_data <- process_mpr_files(mpr_directory)
  csv_data <- process_converted_files(converted_directory)
  
  # Find unmatched
  unmatched_mpr <- mpr_data |>
    dplyr::anti_join(csv_data, by = "new_filename")
  
  unmatched_csv <- csv_data |>
    dplyr::anti_join(mpr_data, by = "new_filename")
  
  return(list(
    unmatched_mpr = unmatched_mpr,
    unmatched_csv = unmatched_csv
  ))
}

# Validate dictionary consistency
validate_dictionary <- function(dictionary_path) {
  # Read dictionary
  dict <- readr::read_csv(dictionary_path, show_col_types = FALSE)
  
  # Check for duplicates
  duplicated_mpr <- dict |>
    dplyr::group_by(filepath) |>
    dplyr::filter(n() > 1)
  
  duplicated_csv <- dict |>
    dplyr::group_by(file_conv) |>
    dplyr::filter(n() > 1)
  
  # Check file existence
  mpr_exists <- file.exists(dict$filepath)
  csv_exists <- file.exists(dict$file_conv)
  
  return(list(
    n_entries = nrow(dict),
    has_duplicated_mpr = nrow(duplicated_mpr) > 0,
    has_duplicated_csv = nrow(duplicated_csv) > 0,
    duplicated_mpr = duplicated_mpr,
    duplicated_csv = duplicated_csv,
    missing_mpr_files = sum(!mpr_exists),
    missing_csv_files = sum(!csv_exists),
    missing_files = dict[!mpr_exists | !csv_exists, ]
  ))
}

# Wrapper function for backward compatibility ----------------------------------

# Simple wrapper matching original function call
create_conversion_dictionary <- function() {
  
  # Run the main function
  conversion_dataDictionary(
    save_output = TRUE,
    return_data = FALSE,
    verbose = TRUE
  )
}

# Usage Examples ---------------------------------------------------------------

# Use the simple wrapper (matches original usage)
create_conversion_dictionary()


# Clean up workspace -----------------------------------------------------------
# rm(list = ls())