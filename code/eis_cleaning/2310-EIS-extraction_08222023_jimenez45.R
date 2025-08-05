#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 08222023

cleaning_08222023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\08222023 1 pegda 9 pegmea 15 wt Al2O3 15 wt litfsi'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          subfolder = stringr::str_extract(orig_filename, pattern = '([^/\\\\]+)(?=[/\\\\][^/\\\\]*$|$)')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '9 pegmea 1 pegda 0 wt al2o3 15 wt litfsi',
              replacement = glue::glue('9_pegmea_1_pegda_0_AlO3_15_LiTFSI_{subfolder}')
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_10,71_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,86 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_12,86_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_15_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_2,14_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_4,29_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,42 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_6,42_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_8,57_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,43 wt al2o3 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_6,42_AlO3_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2',
            replacement = glue::glue('9_pegmea_1_pegda_2,14_AlO3_15_LiTFSI_7')
          )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea_1_pegda',
            replacement = '08222023_9_PEGMEA_1_PEGDA'
          )
        ) %>%
        dplyr::select(-subfolder)
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\08222023 1 pegda 9 pegmea 15 wt Al2O3 15 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_08222023_files(user_name)
rm(cleaning_08222023_files)
