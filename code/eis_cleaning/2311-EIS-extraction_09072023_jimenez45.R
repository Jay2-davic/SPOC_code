#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09082023* revised from 09072023

cleaning_09072023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
  # loads tidyverse & functions
  require(tidyverse)
  source(
    '~/Git/polyelectrolyte/polyelectrolyte/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
  )
  # destination folder for converted files
  new_folder_struct_main <-
    'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
  # location of EIS files
  folder_struct <-
    r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Katherine Updates\09072023 1 pegda 9 pegmea 30wt litfsi 15 aerosil 380)'
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct) 
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename =
        stringi::stri_replace_all_regex(orig_filename,
                                        pattern = '1pegda 9 pegmea ',
                                        replacement = '09072023_9_PEGMEA_1_PEGDA_'),
      
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = 'aerosil 380 ',
                                        replacement = 'Aerosil380_'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = 'wt ',
                                        replacement = '_'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = 'Litfsi|litfsi',
                                        replacement = 'LiTFSI'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = ' _',
                                        replacement = '_'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = '2-2',
                                        replacement = 'RUN_1'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = '4_1',
                                        replacement = '4_RUN_2'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = '4_2',
                                        replacement = '4_RUN_1'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = '2_2',
                                        replacement = '2_RUN_1'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = '4_3',
                                        replacement = '4_RUN_3'),
      new_filename = dplyr::case_when(
        stringr::str_detect(new_filename, 'A2|A4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                             '12,86',
                                                                                             '0'),
        stringr::str_detect(new_filename, 'B2|B4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               '12,86',
                                                                                               '2,14'),
        stringr::str_detect(new_filename, 'C2|C4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                             '12,86',
                                                                                             '4,29'),
        stringr::str_detect(new_filename, 'D2|D4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                             '12,86',
                                                                                             '6,43'),
        stringr::str_detect(new_filename, 'E2|E4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                             '12,86',
                                                                                             '8,56'),
        stringr::str_detect(new_filename, 'F2|F4') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                             '12,86',
                                                                                             '10,71'),
        TRUE ~ new_filename)
      ) %>%
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
    dplyr::select(c(orig_filename, new_filename))
  # saves the dataframe as a csv file
  readr::write_csv(
    new_files,
    r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr files\data_dictionary\09082023 1 pegda 9 pegmea 15wt aerosil 380 30 wt litfsi_database_dictionary.csv)'
  )
  #return(new_files)
    })
  })
}

cleaning_09072023_files(user_name)
rm(cleaning_09072023_files)