#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 06292023*

cleaning_08042023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse & functions
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )  # location of EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\08042023 1pegda 9 pegmea 25 wt tio2 15 wt litfsi'
        )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          subfolder = stringr::str_extract(orig_filename, pattern = '([^/\\\\]+)(?=[/\\\\][^/\\\\]*$|$)')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '9 pegmea 1 pegda 0 wt tio2 15 wt litfsi',
              replacement = glue::glue('9_pegmea_1_pegda_0_TiO2_15_LiTFSI_{subfolder}')
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_10,71_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 14,29 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_14,29_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 17,86 wt tio2 10,71 ',
            replacement = glue::glue(
              '9_pegmea_1_pegda_17,86_TiO2_10,71_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 21.43 wt tio2 12',
            replacement = glue::glue('9_pegmea_1_pegda_17,86_TiO2_12_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 17,86 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_17,86_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 21,43 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_21,43_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 21,43 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_21,43_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 21.43 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_21,43_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 25 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_25_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 25 wt tio2 15wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_25_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 3,57 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_3,57_TiO2_15_LiTFSI_{subfolder}')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 7,14 wt tio2 15 wt litfsi',
            replacement = glue::glue('9_pegmea_1_pegda_7,14_TiO2_15_LiTFSI_{subfolder}')
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
            pattern = '9_pegmea_1_pegda',
            replacement = '08042023_9_PEGMEA_1_PEGDA'
          )
        ) %>%
        dplyr::select(-subfolder)
      # saves dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\08042023 1pegda 9 pegmea 25 wt tio2 15 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_08042023_files(user_name)
rm(cleaning_08042023_files)
