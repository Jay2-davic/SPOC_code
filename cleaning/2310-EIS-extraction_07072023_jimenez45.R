#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 07072023*

cleaning_07072023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse & functions
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      # location of EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\070752023 9 pegmea 1 pegda 10 wt Aerosil 90'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '9 pegmea 1 pegda 9,375 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_9,33_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 8,75 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_8,67_Aerosil380_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 8,125 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_8_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 7,5 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_7,33_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 6,875 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_6,67_Aerosil380_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 6,25 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_6_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 5,625 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_5,33_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 10 wt aerosil 90',
              replacement = glue::glue('9_pegmea_1_pegda_10_Aerosil90_0_LiTFSI')
            )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea_1_pegda',
            replacement = '07072023_9_PEGMEA_1_PEGDA'
          )
        ) %>%
        dplyr::select(c(orig_filename, new_filename))
      
      # do not rerun
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\070752023 9 pegmea 1 pegda 10 wt Aerosil 90_database_dictionary.csv'
        )
      )
      return(new_files)
    })
  })
}

cleaning_07072023_files(user_name)
rm(cleaning_07072023_files)
