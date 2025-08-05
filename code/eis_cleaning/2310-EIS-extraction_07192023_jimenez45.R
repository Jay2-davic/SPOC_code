#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 07192023*

cleaning_07192023_files <- function(user_name) {
  # loads tidyverse & functions
  suppressMessages({
    suppressWarnings({
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )  # location of EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\07192023 9pegmea 1 pegda 15wt aerosil 90 15wt liftsi'
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
          subfolder = stringr::str_extract(orig_filename, pattern = '([^/\\\\]+)(?=[/\\\\][^/\\\\]*$|$)')
        ) %>%
        dplyr::mutate(
          subfolder = stringi::stri_replace_all_regex(subfolder,
                                                      pattern =
                                                        '1b',
                                                      replacement =
                                                        '5')
        ) %>%
        dplyr::mutate(
          subfolder = stringi::stri_replace_all_regex(subfolder,
                                                      pattern =
                                                        '2b',
                                                      replacement =
                                                        '6')
        ) %>%
        dplyr::mutate(
          subfolder = stringi::stri_replace_all_regex(subfolder,
                                                      pattern =
                                                        '3b',
                                                      replacement =
                                                        '7')
        ) %>%
        dplyr::mutate(
          subfolder = stringi::stri_replace_all_regex(subfolder,
                                                      pattern =
                                                        '4b',
                                                      replacement =
                                                        '8')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '  ',
              replacement = ' '
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 0wt aerosil 90 0wt liftsi',
              replacement = glue::glue('9_pegmea_1_pegda_0_Aerosil90_0_LiTFSI_{subfolder}')
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71wt aerosil 90 10,71wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_10,71_Aerosil90_10,71_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,85wt aerosil 90 12,85wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,85_Aerosil90_12,85_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15wt aerosil 90 15wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_15_Aerosil90_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14wt aerosil 90 2,14wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_2,14_Aerosil90_2,14_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,28wt aerosil 90 4,28wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,28_Aerosil90_4,28_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '18,57',
            replacement = glue::glue('8,57')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,42wt aerosil 90 6,42wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,42_Aerosil90_6,42_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57wt aerosil 90 8,57wt liftsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil90_8,57_LiTFSI_{subfolder}'
            )
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
        dplyr::select(-subfolder) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea_1_pegda',
            replacement = '07192023_9_PEGMEA_1_PEGDA'
          )
        )
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\07192023 9pegmea 1 pegda 15wt aerosil 90 15wt liftsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_07192023_files(user_name)

rm(cleaning_07192023_files)
