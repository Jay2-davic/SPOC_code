#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 06292023*

cleaning_06292023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\06292023 9pegmea 1pegda 15 wt aerosil 90'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '  ',
              replacement = ' '
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 1,875 wt aerosil 90',
              replacement = '9_PEGMEA_1_PEGDA_1,875_Aerosil90_0_LiTFSI'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 3,75 wt aerosil 90',
              replacement = '9_PEGMEA_1_PEGDA_3,75_Aerosil90_0_LiTFSI'
            ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 5,625 wt aerosil 90',
            replacement = '9_PEGMEA_1_PEGDA_5,625_Aerosil90_0_LiTFSI'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 7,5 wt aerosil 90',
            replacement = '9_PEGMEA_1_PEGDA_7,5_Aerosil90_0_LiTFSI'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 9,375 wt aerosil 90',
            replacement = '9_PEGMEA_1_PEGDA_9,375_Aerosil90_0_LiTFSI'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 11,25 wt aerosil 90',
            replacement = '9_PEGMEA_1_PEGDA_11,75_Aerosil90_0_LiTFSI'
          ),
          new_filename = stringr::str_extract(new_filename, pattern = '[^\\\\]+$'),
          new_filename = paste0(new_folder_struct_main, '\\', new_filename),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = 'LiTFSI',
            replacement = 'LiTFSI_1_1'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '06292023  ',
            replacement = '06292023_'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '06292023 ',
            replacement = '06292023_'
          ),
        )
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\06292023 9pegmea 1pegda 15 wt aerosil 90_database_dictionary.csv'
        )
      )
    })
  })
  # for debugging 
  #return(new_files)
}

cleaning_06292023_files(user_name)

rm(cleaning_06292023_files)
