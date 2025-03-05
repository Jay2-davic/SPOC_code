# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte

# checks the database for finished files

EIS_check_for_finished_files <- function() {
  require(tidyverse)
  
  mpr_directory <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_files_converted')
  
  mpr_analyzed <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_results_from_Jay_2')
  
  # collects and labels the split EIS files
  mpr_files <- tibble::tibble(filepath = list.files(mpr_directory,
                                                    full.names = TRUE)) %>%
    dplyr::mutate(
      names = filepath,
      names = stringr::str_extract(names, pattern = '[^/]+$'),
      names = stringi::stri_replace_all_regex(names,
                                              '.csv',
                                              '')
    )
  # collects already finished EIS files for flagging
  mpr_files_finished <-
    tibble::tibble(filepath_finished = list.files(mpr_analyzed,
                                                  full.names = TRUE,
                                                  pattern = '.csv')) %>%
    dplyr::mutate(
      names = filepath_finished,
      names = stringr::str_extract(names, pattern = '[^/]+$'),
      names = stringi::stri_replace_all_regex(names,
                                              '[.]csv|__|_randles|randles',
                                              '')
    )
  
  # merge the two dfs and create a flag for files present in both
  mpr_merge <- dplyr::full_join(mpr_files,
                                mpr_files_finished) %>%
    dplyr::mutate(file_done = dplyr::case_when(# labels unfinished files as FALSE
      is.na(filepath_finished) == FALSE ~ FALSE,
      TRUE ~ TRUE)) %>%
    dplyr::select(c(filepath, file_done))
  
  readr::write_csv(
    mpr_merge,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\EIS_full_files.csv')
  )
}

EIS_check_for_finished_files()

rm(EIS_check_for_finished_files)