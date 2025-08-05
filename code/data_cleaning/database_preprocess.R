# ============================================================================
# Data Processing Main Script
# 
# Description: Main script for processing polymer electrolyte database using
#              statistical functions for outlier removal and data cleaning
#
# Author: [Your Name]
# Date: [Date]
# Version: 1.0
#
# Required packages:
#   - ggplot2: for plotting
#   - tidyverse: for data manipulation
#   - svDialogs: for dialog boxes
#   - magrittr: for pipe operators
#   - readr: for reading/writing CSV files
#   - stringi: for string manipulation
#   - glue: for string interpolation
# ============================================================================

# =============================================================================
# LIBRARY LOADING AND SETUP
# =============================================================================

# Load required libraries
require(ggplot2)
require(tidyverse)
require(svDialogs)
require(magrittr)
require(glue)
require(stringi)
require(readr)

# Set directory paths as variables
code_dir <- '~/Git/SPOC_code/code'
data_dir <- '~/Git/SPOC_code/data/database'

# # Load custom functions
# source(glue::glue('{git_directory}2401-GridSearch_functions-jimenez45.R'))
# source(glue::glue('{git_directory}2310-PlotlyTemplate_PlotlyExport.R'))

# Load statistical functions
# UPDATE THIS PATH to where you saved the statistical functions file
source(glue::glue('{code_dir}/data_cleaning/database_outlier_remove.R'))

# =============================================================================
# DATA LOADING, PREPROCESSING, OUTLIER REMOVAL
# =============================================================================

# Load combined PE database
df_PE_combined <- readr::read_csv(
  glue::glue('{data_dir}/pe_database.csv')
)

# Load and preprocess final database using the preprocessing function
final_db <- df_PE_combined |>
  preprocess_database() |>
  apply_outlier_removal()

# =============================================================================
# DATA EXPORT FOR MACHINE LEARNING
# =============================================================================

# Define output directory
output_directory <- '~/Git/SPOC_code/output/'

# Create output directory if it doesn't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Export cleaned database
readr::write_csv(
  final_db, 
  file.path(data_dir, 'PE_database_cleaned.csv')
)

# Export coin cell data for ML training
write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'Coin cell') |>
    dplyr::select(
      fcomp_formulation,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    ),
  file.path(data_dir, 'PE_MLtraining_LiTFSI_coin.csv'),
  row.names = FALSE
)

# Export PCB data for ML training
write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'PCB print') |>
    dplyr::select(
      fcomp_formulation,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    ),
  file.path(data_dir, 'PE_MLtraining_LiTFSI_PCB.csv'),
  row.names = FALSE
)
