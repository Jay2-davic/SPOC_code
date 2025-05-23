#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09082023*

cleaning_09082023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\09082023 1 pegda 9 pegmea 15wt aerosil 380 30 wt litfsi'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '1pegda 9 pegmea ',
              replacement = '09082023_9_PEGMEA_1_PEGDA_'
            ),
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
        dplyr::mutate(
          new_filename =
            stringr::str_extract(new_filename,
                                 pattern = '[^/]+$'),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        ) %>%
        dplyr::select(c(orig_filename, new_filename))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\09082023 1 pegda 9 pegmea 15wt aerosil 380 30 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_09082023_files(user_name)
rm(cleaning_09082023_files)