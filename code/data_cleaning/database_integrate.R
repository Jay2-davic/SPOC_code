# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Polyelectrolyte

# =============================================================================
# SETUP
# =============================================================================

require(future)
future::multicore(workers = 4)

# =============================================================================
# CONFIGURATION - Update paths here for different environments
# =============================================================================

# DEBUG PATHS (uncomment for local debugging)
BASE_GIT_PATH <- '~/Git/23-spoc_code'
# ONEDRIVE_BASE <- 'C:\\Users\\{user_name}\\OneDrive - LLNL'
# CONDA_ENV_PATH <- 'C:\\Users\\{user_name}\\AppData\\Local\\miniforge3\\envs\\EIS_processing'

# PRODUCTION PATHS (set these for formal deployment)
data_destination <- '~/Git/SPOC_code/data/database/'  # Update this path for production
CONDA_ENV_PATH <- glue::glue('C:\\Users\\{user_name}\\AppData\\Local\\miniforge3\\envs\\EIS_processing')  # Update this path for production


# =============================================================================
# DATA PROCESSING PIPELINE
# =============================================================================

# Source all required scripts
source('~/Git/SPOC_code/code/data_cleaning/database_historical_clean.R') # performs data cleaning and integration of excel files 
source('~/Git/SPOC_code/code/eis_cleaning/2310-WRAPPER_EIS_extraction_jimenez45.R') # performs file name cleaning on EIS files, wrapper function for any updates and new files/cleaning script
source('~/Git/SPOC_code/code/data_cleaning/database_check_EIS.R')
source('~/Git/SPOC_code/code/data_cleaning/data_dictionary_conversion.R')
source('~/Git/SPOC_code/code/data_cleaning/database_dictionary_update.R')
source('~/Git/SPOC_code/code/data_cleaning/database_rename_files.R')

# Setup Python environment
reticulate::use_condaenv(CONDA_ENV_PATH, required = TRUE)
reticulate::source_python(file.path(BASE_GIT_PATH, 'EIS_Analysis', '2312-EIS_cycle_splitter_jimenez45.py'))
reticulate::source_python(file.path(BASE_GIT_PATH, 'EIS_Analysis', '2507-autoprocess_EIS.py'))

# Compile EIS files
source(file.path(BASE_GIT_PATH, 'EIS_Analysis', '2311-EIScompile_fileMatch.R'))

# =============================================================================
# DATA TRANSFORMATION
# =============================================================================

# Modify combined files
df_PE_combined_2 <- df_PE_combined |>
  dplyr::distinct(mpr_files_cycle, sample_ID, .keep_all = TRUE) |>
  dplyr::mutate(date_printed = as.character(date_printed),
                mpr_files = as.character(mpr_files),
                legacy_mpr = as.character(legacy_mpr),
                mpr_files_cycle = as.character(mpr_files_cycle)) |>
  dplyr::rename(new_filename = mpr_files,
                orig_filename = legacy_mpr) |>
  dplyr::full_join(eis_full, by = 'mpr_files_cycle') |>
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with('resistance'),
      ~ dplyr::case_when(print_type == 'PCB print' |
                           print_type == 'Bulk cast' ~ NA,
                         TRUE ~ .)
    ),
    mpr_files_cycle = stringi::stri_replace_all_regex(
      mpr_files_cycle,
      'eis_results',
      'mpr_new_files_converted'),
    date_printed = as.Date(date_printed)
  ) |>
  dplyr::filter(is.na(eis_mpr) != TRUE) |>
  dplyr::distinct(sample_ID, mpr_files_cycle, resistance, .keep_all = TRUE)

# Calculate ionic conductivity
combined_df <- df_PE_combined_2 |>
  dplyr::mutate(
    resistance = dplyr::case_when(
      is.na(resistanceAVG_o) != TRUE ~ resistanceAVG_o,
      TRUE ~ resistance
    ),
    ionic_conductivity_final = dplyr::case_when(
      print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      is.na(mpr_files_cycle) == TRUE | print_type == 'Coin cell' &
        is.na(resistance) == TRUE ~ ionic_conductivity_s_cm,
      is.na(FittedValue.R1) == FALSE ~ 0.025 / (FittedValue.R1 * 0.25^2)
    ),
    ionic_conductivity_final_geom_factor_applied = dplyr::case_when(
      print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      is.na(mpr_files_cycle) == TRUE &
        is.na(resistance) == TRUE ~ ionic_conductivity_s_cm,
      is.na(mpr_files_cycle) == FALSE ~ 4.35757/FittedValue.R1
    )
  ) |>
  dplyr::filter(film_thickness >= 0.008 | is.na(film_thickness) == TRUE & is.na(resistanceAVG_o) == TRUE) |>
  dplyr::distinct(ionic_conductivity_final, fcomp_formulation, .keep_all = TRUE) |>
  dplyr::filter(ionic_conductivity_final != 0) |>
  dplyr::mutate(
    fcomp_salt_wt_pct = signif(fcomp_salt_wt_pct, digit = 4),
    fcomp_additive_wt_pct = signif(fcomp_additive_wt_pct, digit = 2)
  )

# adding the temperature and humidity
weather <- readr::read_csv('~/Git/SPOC_code/data/database/livermore_weather.csv') |>
  dplyr::mutate(date = lubridate::as_date(datetime),
                time = lubridate::hour(datetime)) |>
  dplyr::mutate(work_time = 
                  dplyr::case_when(
                    time >= 9 & time <= 17 ~ 'work',
                    TRUE ~ 'off'
                  )
  ) |>
  dplyr::group_by(work_time, date) |>
  dplyr::mutate(average_hum = mean(hourly_relative_humidity_2m),
                average_temp = mean(hourly_temperature)) |>
  dplyr::ungroup() |>
  dplyr::rename(date_printed = date) |>
  dplyr::filter(work_time == 'work') |>
  dplyr::select(-c(datetime, hourly_relative_humidity_2m, hourly_temperature, time, work_time)) |>
  dplyr::distinct()

# =============================================================================
# FINAL DATABASE PREPARATION
# =============================================================================

# Select columns and filter for final database
final_db <- combined_df |>
  dplyr::filter(
    date_printed >= as.Date('2023-10-22') &
      fcomp_salt != 'NaTFSI' |
      fcomp_additive_wt_pct == '0' &
      fcomp_salt_wt_pct == '0' & fcomp_additive2 != 'Ethanol' |
      fcomp_salt == 'LiTFSI' &
      fcomp_additive_wt_pct == '0' & fcomp_additive2 != 'Ethanol'
  ) |>
  dplyr::select(
    c(
      sample_ID, fcomp_formulation, ionic_conductivity_final,
      ionic_conductivity_final_geom_factor_applied, date_printed,
      fab_method, print_type, fcomp_additive, fcomp_additive_wt_pct,
      fcomp_salt, fcomp_salt_wt_pct, fcomp_additive2, resistance,
      area_cm2, film_thickness, film_quality, mpr_files_cycle,
      RMSE_real, RMSE_imag, head_count, mu, Circuits, 
      InitialGuess.R0, InitialGuess.R1, InitialGuess.Wo1_0, 
      InitialGuess.Wo1_1, InitialGuess.C1, FittedValue.R0, 
      FittedValue.R1, FittedValue.Wo1_0, FittedValue.Wo1_1, 
      FittedValue.C1, FittedValue.R0_stDev, FittedValue.R1_stDev, 
      FittedValue.Wo1_0_stDev, FittedValue.Wo1_1_stDev, 
      FittedValue.C1_stDev, humidity, temperature
    )
  ) |>
  dplyr::group_by(ionic_conductivity_final, fcomp_formulation) |>
  dplyr::distinct() |>
  dplyr::left_join(weather)

# =============================================================================
# OUTPUT FILES
# =============================================================================

# Define output paths
db_output_path <- '~/Git/SPOC_code/data/database/pe_database.csv'

# Save for manuscript
write.csv(
  final_db |>
    dplyr::select(
      c(
        sample_ID, fcomp_formulation, ionic_conductivity_final,
        ionic_conductivity_final_geom_factor_applied, date_printed,
        print_type, fcomp_additive, fcomp_additive_wt_pct,
        fcomp_salt, fcomp_salt_wt_pct, resistance, area_cm2,
        film_thickness, film_quality, RMSE_real, RMSE_imag,
        head_count, InitialGuess.R0, InitialGuess.R1,
        InitialGuess.Wo1_0, InitialGuess.Wo1_1, InitialGuess.C1,
        FittedValue.R0, FittedValue.R1, FittedValue.Wo1_0,
        FittedValue.Wo1_1, FittedValue.C1, FittedValue.R0_stDev,
        FittedValue.R1_stDev, FittedValue.Wo1_0_stDev,
        FittedValue.Wo1_1_stDev, FittedValue.C1_stDev,
        humidity, temperature
      )
    ) |>
    dplyr::filter(
      date_printed != '2024-07-22',
      date_printed > '2024-01-01',
      date_printed != '2024-09-26',
      date_printed != '2024-08-15',
      date_printed != '2024-05-06',
      date_printed != '2024-10-07',
      date_printed != '2024-03-04'
    ) |>
    dplyr::mutate(
      fcomp_formulation = stringi::stri_replace_all_regex(
        fcomp_formulation, '4.28', '4.29'
      ),
      humidity = dplyr::case_when(
        print_type == 'Coin cell' ~ 0.00001,
        TRUE ~ humidity / 100
      ),
      print_type = stringr::str_replace_all(print_type, 'Bulk cast', 'PCB print')
    ) |>
    dplyr::filter(
      dplyr::case_when(
        fcomp_salt_wt_pct != 0 ~ ionic_conductivity_final >= 10^-7,
        TRUE ~ TRUE
      ),
      fcomp_additive != 'AlO3'
    ) |>
    dplyr::relocate(ionic_conductivity_final, .before = print_type),
  db_output_path
)


# Cleanup
rm(list = ls())
gc()
