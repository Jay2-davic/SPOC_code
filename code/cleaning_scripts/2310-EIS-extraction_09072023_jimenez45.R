#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

######## SUPERCEDED ########

### Cleaning EIS file on 09072023* S
cleaning_09072023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\09072023 1 pegda 9 pegmea 30wt litfsi 15 aerosil 380'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = 'i_D',
              replacement = 'i_RUN_D'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = 'i_G',
              replacement = 'i_RUN_G'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_E',
              replacement = 'i_RUN_E'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_D',
              replacement = 'i_RUN_D'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_C',
              replacement = 'i_RUN_C'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_B',
              replacement = 'i_RUN_B'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_A',
              replacement = 'i_RUN_A'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'i_F',
              replacement = 'i_RUN_F'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_H',
              replacement = '_RUN_H'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '2-2',
              replacement = '2'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '2_2|4_1',
              replacement = '1'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '4_2',
              replacement = '2'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '4_3',
              replacement = '3'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '1pegda 9 pegmea ',
              replacement = '09072023_9_PEGMEA_1_PEGDA_'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'litfsi ',
              replacement = 'LiTFSI'
            ),
          # new_filename =
          #   stringi::stri_replace_all_regex(new_filename,
          #                                   pattern = 'i_A|i_B|i_C|i_D|i_E|i_F|i_G|i_H',
          #                                   replacement = 'i'),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = ' wt ',
              replacement = '_'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'aerosil 380 ',
              replacement = 'Aerosil380_'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'Litfsi',
              replacement = 'LiTFSI'
            )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea_1_pegda',
            replacement = '09072023_9_PEGMEA_1_PEGDA'
          )
        ) %>%
        dplyr::select(c(orig_filename, new_filename))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\09072023 1 pegda 9 pegmea 30wt litfsi 15 aerosil 380_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

#cleaning_09072023_files(user_name)
rm(cleaning_09072023_files)