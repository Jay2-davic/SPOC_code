# ============================================================================
# Polyelectrolyte Data Processing
#
# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte
#
# Description: Updates and copies new MPR files into the data directory.
#              Processes EIS data paths and creates file mappings for
#              polyelectrolyte experiments.
#
# Version: 1.0
# ============================================================================

# updates and copies new mpr files into the
EIS_files <- readr::read_csv('~/Git/SPOC_code/data/database/EIS_full_files.csv') 
df <- readr::read_csv('~/Git/SPOC_code/data/database/historical_files.csv') |>
  dplyr::mutate(
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '\\\\', '/'),
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, 'none.mpr', 'none'),
    # removes the user name and replaces it with a variable string
    # EIS_mpr =  dplyr::case_when(
    #   stringr::str_detect(EIS_mpr, 'Users') == FALSE &
    #     EIS_mpr != 'none' ~ stringi::stri_replace_all_regex(EIS_mpr, dirname(EIS_mpr), '~/Git/SPOC_code/data/raw_eis'),
    #   TRUE ~ EIS_mpr
    # ),
    # EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '[.][.]', '.')
  ) |>
  # combines with the full data
  dplyr::rename(orig_filename = EIS_mpr) %>%
  dplyr::full_join(EIS_files) %>%
  dplyr::distinct(orig_filename, sample_ID, .keep_all = TRUE) |>
  dplyr::rename(legacy_mpr = orig_filename, mpr_files = new_filename)

# extracts files and copies into new folder
purrr::map2(df$legacy_mpr, df$mpr_files, .f = file.copy)

rm(EIS_files, EIS_df)