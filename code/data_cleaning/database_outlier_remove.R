# ============================================================================
# Statistical and Data Processing Functions
# 
# Description: Collection of statistical and data processing functions for 
#              polymer electrolyte data analysis. Outliers are removed from 
#              their 2 sigma from mean.
#
# Author: J. Jimenez
# Version: 1.0
#
# Required packages:
#   - dplyr: for data manipulation
#   - stats: for statistical functions
#   - stringi: for string manipulation
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(stats)
library(stringi)

# Data Preprocessing Functions -------------------------------------------------

# Filter problematic dates from database
filter_problematic_dates <- function(df) {
  df |>
    dplyr::filter(
      date_printed != '2024-07-22',
      date_printed > '2024-01-01',
      date_printed != '2024-09-26',
      date_printed != '2024-08-15',
      date_printed != '2024-05-06',
      date_printed != '2024-10-07',
      date_printed != '2024-03-04'
    )
}

# Apply standard data transformations
apply_data_transformations <- function(df) {
  df |>
    dplyr::mutate(
      # Fix formulation naming inconsistency
      fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation, '4.28', '4.29'),
      # Handle humidity values for different print types
      humidity = dplyr::case_when(
        print_type == 'Coin cell' ~ 0.00001,
        TRUE ~ average_hum
      ),
      temp = dplyr::case_when(
        print_type == 'Coin cell' ~ 22,
        TRUE ~ average_temp
      ),
      # Add validation labels for specific dates
      validation = dplyr::case_when(
        date_printed == '2024-06-27' ~ 'YES',
        date_printed == '2024-08-16' ~ 'YES',
        TRUE ~ 'NO'
      )
    )
}

# Apply conductivity filters
apply_conductivity_filters <- function(df) {
  df |>
    dplyr::filter(
      dplyr::case_when(
        fcomp_salt_wt_pct != 0 ~ ionic_conductivity_final >= 10^-7,
        TRUE ~ TRUE
      ),
      fcomp_additive != 'AlO3'
    )
}

# Complete preprocessing pipeline
preprocess_database <- function(df) {
  df |>
    filter_problematic_dates() |>
    apply_data_transformations() |>
    apply_conductivity_filters() |>
    dplyr::relocate(ionic_conductivity_final, .before = print_type)
}

# Data Selection and Processing Functions --------------------------------------

# Select data frame based on type
select_df <- function(df, type) {
  if (type == 'coin' || type == 'Coin') {
    return(df |> dplyr::filter(print_type == 'Coin cell'))
  } else if (type == 'PCB' || type == 'PCB Print') {
    return(df |> dplyr::filter(print_type == 'PCB print'))
  } else {
    return(df)
  }
}

# Z-score based outlier removal
z_score_outlier_remove <- function(df, type, threshold) {
  selected_df <- select_df(df = df, type = type)
  
  outlier_info <- selected_df |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    dplyr::mutate(
      log_ic = log10(ionic_conductivity_final),
      z_score = (log_ic - mean(log_ic)) / stats::sd(log_ic)
    ) |>
    dplyr::filter(abs(z_score) <= threshold) |>
    dplyr::select(-z_score)
  
  return(outlier_info)
}

# Data Processing Helper Functions ---------------------------------------------

# Apply outlier removal to database with filtering
apply_outlier_removal <- function(final_db) {
  
  # Process main dataset (coin cell data)
  final_db_outlier <- final_db |>
    apply_data_transformations() |>
    # Initial filtering
    dplyr::filter(
      !date_printed <= '2024-02-02',
      validation == 'NO',
      fab_method != 3
    ) |>
    # Remove specific problematic samples
    dplyr::filter(
      !(fcomp_salt_wt_pct == 20 & date_printed == '2024-09-23'),
      date_printed != '2024-08-15'
    ) |>
    # Apply outlier removal using z-score (2 sigma threshold)
    z_score_outlier_remove('coin', 2) |>
    # Combine with PCB data after outlier removal
    dplyr::bind_rows(
      final_db |>
        dplyr::filter(
          !(fcomp_salt_wt_pct == 20 & print_type == 'PCB print'),
          ionic_conductivity_final <= 10^-3
        ) |>
        z_score_outlier_remove('PCB', 2)
    )
  
  return(final_db_outlier)
}

# Filter coin cell data
filter_coin_data <- function(df) {
  df |>
    dplyr::filter(
      !date_printed <= '2024-02-02',
      validation == 'NO',
      fab_method != 3,
      !(fcomp_salt_wt_pct == 20 & date_printed == '2024-09-23'),
      date_printed != '2024-08-15'
    )
}

# Filter PCB data
filter_pcb_data <- function(df) {
  df |>
    dplyr::filter(
      !(fcomp_salt_wt_pct == 20 & print_type == 'PCB print'),
      ionic_conductivity_final <= 10^-3
    )
}

# Process database with separated filtering and outlier removal
process_database_clean <- function(final_db) {
  # Process coin cell data
  coin_data <- final_db |>
    filter_coin_data() |>
    z_score_outlier_remove('coin', 2)
  
  # Process PCB data  
  pcb_data <- final_db |>
    filter_pcb_data() |>
    z_score_outlier_remove('PCB', 2)
  
  # Combine results
  final_db_outlier <- dplyr::bind_rows(coin_data, pcb_data)
  
  return(final_db_outlier)
}