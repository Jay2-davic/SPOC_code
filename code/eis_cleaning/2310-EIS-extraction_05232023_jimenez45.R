#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 05232023*

cleaning_05232023_files <- function(user_name) {
  # loads tidyverse & functions
  suppressWarnings({
    suppressMessages({
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names')
      # location of EIS files
      folder_struct <-
        glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\05232023 PEGDA PEGMEA 15 WT LIFTSI')
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # format the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              '9 pegmea 1 pegda ',
              '05232023_9_PEGMEA_1_PEGDA_0_none_'
            ),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' wt liftsi_C01',
                                            '_LiTFSI')
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
        glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\05232023 PEGDA PEGMEA 15 WT LIFTSI_database_dictionary.csv')
      )
      # for debugging output
      #return(new_files)
    })
  })
}

cleaning_05232023_files(user_name)

rm(cleaning_05232023_files)
