# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte

# makes a data dictionary to refer the converted files back to parent .mpr files

conversion_dataDictionary <- function() {
  require(tidyverse)
  
  new_files <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names')
  # retrieves all the .mpr files in the folder
  eis_files <- tibble::tibble(filepath = list.files(new_files,
                                                    pattern = '.mpr',
                                                    full.names = TRUE)) %>%
    # extracts the base name
    dplyr::mutate(
      new_filename = stringr::str_extract(filepath, pattern = '[^/]+$'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '.mpr',
                                                     '')
    )
  
  conv_files <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_files_converted')
  
  eis_conv <- tibble::tibble(file_conv = list.files(conv_files,
                                                    pattern = '.csv',
                                                    full.names = TRUE)) %>%
    # extracts the base name
    dplyr::mutate(
      new_filename = stringr::str_extract(file_conv, pattern = '[^/]+$'),
      new_filename = stringr::str_remove(new_filename, pattern = '_RUN_\\d+\\.csv')
    )
  
  df <-
    dplyr::full_join(eis_conv, eis_files) %>%
    dplyr::select(filepath, file_conv) %>%
    dplyr::filter(!is.na(filepath) == TRUE)
  
  readr::write_csv(
    df,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\database_conversion_dictionary.csv')
  )
}

conversion_dataDictionary()

rm(conversion_dataDictionary)