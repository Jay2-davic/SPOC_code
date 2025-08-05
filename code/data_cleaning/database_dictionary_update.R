# ============================================================================
# EIS Database Dictionary Update
#
# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte
#
# Description: Updates the database dictionary to map processed EIS files
#              back to their parent MPR (EC-Lab) files. This maintains
#              traceability between original experimental data and processed files.
#
# Version: 2.0
#
# Required packages:
#   - tidyverse: for data manipulation
#   - glue: for string interpolation
#   - stringr: for string manipulation
#   - stringi: for advanced string operations
#   - readr: for writing CSV files
#   - fs: for file system operations
# ============================================================================

# Load required packages -------------------------------------------------------
library(tidyverse)
library(glue)
library(stringr)
library(stringi)
library(readr)
library(fs)

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
  mpr_files <- tibble::tibble(filepath = list.files(directory_path, pattern = '\\.mpr$', full.names = TRUE))
  
  # Check if any files found
  if (nrow(mpr_files) == 0) {
    warning(glue::glue("No MPR files found in: {directory_path}"))
    return(tibble::tibble())
  }
  
  # Extract filenames
  mpr_files |>
    dplyr::mutate(new_filename = extract_base_filename(filepath, '\\.mpr'))
}

# Process converted CSV files
process_converted_files <- function(directory_path) {
  # Check if directory exists
  if (!dir.exists(directory_path)) {
    stop(glue::glue("Converted files directory not found: {directory_path}"))
  }
  
  # Get all CSV files
  csv_files <- tibble::tibble(file_conv = list.files(directory_path, pattern = '\\.csv$', full.names = TRUE))
  
  # Check if any files found
  if (nrow(csv_files) == 0) {
    warning(glue::glue("No CSV files found in: {directory_path}"))
    return(tibble::tibble())
  }
  
  # Extract filenames and remove run number suffix
  csv_files |>
    dplyr::mutate(
      new_filename = stringr::str_extract(file_conv, pattern = '[^/]+$'),
      new_filename = stringr::str_remove(new_filename, pattern = '_RUN_\\d+\\.csv')
    )
}

# Create mapping between MPR and CSV files
create_file_mapping <- function(mpr_data, csv_data) {
  # Join on the cleaned filename
  mapping <- dplyr::full_join(csv_data, mpr_data, by = "new_filename") |>
    dplyr::select(filepath, file_conv) |>
    # Keep only matched files
    dplyr::filter(!is.na(filepath))
  
  return(mapping)
}

# Main Processing --------------------------------------------------------------

# Define directory paths using updated structure
mpr_directory <- glue::glue('~/Git/SPOC_code/data/raw_eis/')
mpr_analyzed <- glue::glue('~/Git/SPOC_code/data/split_cycle/')

# Process MPR files from raw directory
mpr_data <- process_mpr_files(mpr_directory)

# Process converted CSV files from processed directory
csv_data <- process_converted_files(mpr_analyzed) 

# Create file mapping
EIS_conv <- create_file_mapping(mpr_data, csv_data) |>
  dplyr::rename(mpr_files = filepath) |>
  dplyr::rename(mpr_files_cycle = file_conv) |>
  dplyr::mutate(
    mpr_files = stringi::stri_replace_all_regex(mpr_files, '\\\\', '/'),
    mpr_files_cycle = stringi::stri_replace_all_regex(mpr_files_cycle, '\\\\', '/'),
    mpr_files_cycle = stringi::stri_replace_all_regex(mpr_files_cycle, '9_pegmea_1_pegda', '9_PEGMEA_1_PEGDA'),
    mpr_files = stringi::stri_replace_all_regex(mpr_files, '9_pegmea_1_pegda', '9_PEGMEA_1_PEGDA'),
    merge = mpr_files |> basename() |> tools::file_path_sans_ext()
  )

# Load processed EIS files from data dictionary directory
EIS_df <- tibble::tibble(filename =
                           list.files('~/Git/SPOC_code/data/database_dictionary/', full.names = TRUE)) |>
  # does not include the actual dictionary
  dplyr::filter(stringr::str_detect(filename, '/database_dictionary/database_dictionary') == FALSE)

# Load and process EIS files
EIS_files <- dplyr::mutate(furrr::future_map_dfr(EIS_df$filename, readr::read_csv)) |>
  dplyr::filter(stringr::str_detect(orig_filename, '[.]mpr') == TRUE) |>
  # omitting Michell's results until cleaned up
  dplyr::filter(stringr::str_detect(orig_filename, 'Michell') == FALSE) |>
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    ~ stringi::stri_replace_all_regex(., '\\\\', '/')
  )) |>
  # text fixes as found
  dplyr::mutate(
    orig_filename = stringi::stri_replace_all_regex(orig_filename, '18,86', '12,86'),
    merge = sub(".*/", "", new_filename),
    merge = tools::file_path_sans_ext(merge)
  ) |>
  dplyr::full_join(EIS_conv) |>
  dplyr::select(-merge) |>
  dplyr::mutate(
    mpr_files = stringi::stri_replace_all_regex(mpr_files, r'(~/Git/SPOC_code/data/raw_eis/)', '')
  )

# Write updated database dictionary
readr::write_csv(EIS_files,
                 glue::glue('~/Git/SPOC_code/data/database/database_dictionary.csv'))