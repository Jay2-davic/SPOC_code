#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 08162023*

cleaning_08162023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\08162023 1 pegda 9 pegmea 5wt aerosil 380 15wt  litfsi'
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
              pattern = '9 pegmea 1 pegda 0 wt aerosil 380 15 wt litfsi',
              replacement = glue::glue(
                '9_PEGMEA_1_PEGDA_0_Aerosil380_15_LiTFSI_{subfolder}'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 0_',
            #big problem
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_0,71_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 0.mps',
            #big problem
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_0,71_Aerosil380_15_LiTFSI_{subfolder}.mps'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 1,43 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_1,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,86 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_2,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14 aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,86 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_2,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 3,57 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_3,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 5 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_5_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 0,71 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_0,71_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '2,71 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue('')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4',
            replacement = glue::glue(
              '9_PEGMEA_1_PEGDA_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '3,71 wt aerosil 380 15 wt litfsi.',
            replacement = ''
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            '9 pegmea 1 pegda 11,25 wt liftsi',
            '9_PEGMEA_1_PEGDA_0_none_11,25_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            '9 pegmea 1 pegda 13,125 wt liftsi',
            '9_PEGMEA_1_PEGDA_0_none_13,125_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            '9 pegmea 1 pegda 15 wt liftsi',
            '9_PEGMEA_1_PEGDA_0_none_15_LiTFSI'
          )
        ) %>%
        
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_PEGMEA_1_PEGDA',
            replacement = '08162023_9_PEGMEA_1_PEGDA'
          ),
          # last minute correction found upon loading entire database
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = ',29 wt aerosil 90 15 wt litfsi',
            replacement = '_C01'
          )
        ) %>%
        dplyr::select(-subfolder)
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\08162023 1 pegda 9 pegmea 5wt aerosil 380 15wt  litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_08162023_files(user_name)
rm(cleaning_08162023_files)
