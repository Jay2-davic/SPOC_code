#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 06302023*

cleaning_06302023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse & functions
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )
      # location of the EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\06302023 9 pegmea 1 pegda 15 wt aerosil 90'
        )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '9 pegmea 1 pegda 1,875 wt aerosil 90',
              replacement = '9_pegmea_1_pegda_0_Aerosil90_0_LiTFSI'
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 3,75 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_2,14_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 5,625 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_4,29_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 7,5 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_6,43_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 9,375 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_8,57_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 11,25 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 11',
            replacement = '9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 13,125 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_12,85_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt aerosil 90',
            replacement = '9_pegmea_1_pegda_15_Aerosil90_0_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '  ',
            replacement = '_'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = ' ',
            replacement = '_'
          )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = 'LiTFSI',
            replacement = 'LiTFSI_1_1'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            '9_pegmea_1_pegda_',
            '06302023_9_PEGMEA_1_PEGDA_'
          )
        )
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\06302023 9 pegmea 1 pegda 15 wt aerosil 90_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_06302023_files(user_name)

rm(cleaning_06302023_files)
