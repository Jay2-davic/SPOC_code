#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 05122023*

cleaning_05122023_files <- function(user_name) {
  # loads tidyverse & functions
  require(tidyverse)
  source(
    '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
  )
  # destination folder for converted files
  new_folder_struct_main <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names')
  # location of EIS files
  folder_struct <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\05162023 9 pegmea 1 pegda 20 wt Cab-O-Sil EH%')
  
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename =
        stringi::stri_replace_all_regex(
          orig_filename,
          '9 pegmea 1 pegda 18,6 wt Cab-O-Sil EH%.mpr',
          '05162023_9_PEGMEA_1_PEGDA_18,6_Cab-O-Sil_0_none.mpr'
        ),
      new_filename =
        stringi::stri_replace_all_regex(
          new_filename,
          '9 pegmea 1 pegda 20 wt Cab-O-Sil EH%.mpr',
          '05162023_9_PEGMEA_1_PEGDA_20_Cab-O-Sil_0_none'
        )
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\05162023 9 pegmea 1 pegda 20 wt Cab-O-Sil EH%_database_dictionary.csv')
  )
  return(new_files)
}

cleaning_05122023_files(user_name)

rm(cleaning_05122023_files)
