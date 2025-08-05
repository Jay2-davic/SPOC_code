#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 05092023*

cleaning_05092023_files <- function(user_name) {
  # loads tidyverse & functions
  suppressWarnings({
    require(tidyverse)
    source('~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R')
    # destination folder for converted files
    new_folder_struct_main <-
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
      )
    # location of EIS files
    folder_struct <-
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\05092023 9 pegmea 1 pegda  5wt Cab-O-Sil EH%'
      )
    # extracts the file paths of EIS files
    orig_files <- EIS_finder(folder_struct)
    # formats the names
    new_files <- orig_files %>%
      dplyr::mutate(
        new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                       '5wt',
                                                       '5 wt'),
        new_filename = stringi::stri_replace_all_regex(new_filename,
                                                       ' EH%',
                                                       ''),
        new_filename = stringi::stri_replace_all_regex(
          new_filename,
          '/9 pegmea 1 pegda ',
          '/05092023_9_PEGMEA_1_PEGDA_'
        ),
        new_filename = stringi::stri_replace_all_regex(
          new_filename,
          ' wt Cab-O-Sil.mpr',
          '_Cab-O-Sil_0_LiTFSI.mpr'
        ),
        meas = dplyr::if_else(
          stringr::str_detect(new_filename,
                              'second batch') == TRUE,
          '2',
          '1'
        )
      ) %>%
      dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
      dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
      dplyr::select(-c(meas))
    # saves the dataframe as a csv file
    readr::write_csv(
      new_files,
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\5092023 9 pegmea 1 pegda  5wt Cab-O-Sil EH%_database_dictionary.csv'
      )
    )
  })
  #return(new_files)
}

cleaning_05092023_files(user_name)

rm(cleaning_05092023_files)
