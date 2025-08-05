#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 06062023*

cleaning_06062023_files <- function(user_name) {
  # loads tidyverse & functions
  suppressMessages({
    suppressWarnings({
      require(tidyverse)
      # loads the EIS_extraction function from the Git folder
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\06062023 9 pegda 1 pegmea 15 wt aerosil 90'
        )
      # extracts the file paths from EIS files
      orig_files <- EIS_finder(folder_struct)
      # format the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              '9 pegmea 1 pegda ',
              '06062023_9_PEGMEA_1_PEGDA_'
            ),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' wt aerosil 90_C01',
                                            '_Aerosil90_0_none')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringr::str_extract(new_filename,
                                 pattern = '[^/]+$'),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        )
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\06062023 9 pegda 1 pegmea 15 wt aerosil 90_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_06062023_files(user_name)

rm(cleaning_06062023_files)
