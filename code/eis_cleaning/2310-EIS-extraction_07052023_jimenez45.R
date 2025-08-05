#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 07052023*

cleaning_07052023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
  # loads tidyverse & functions
  require(tidyverse)
  source(
    '~/Git/polyelectrolyte/polyelectrolyte/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
  )
  # destination folder for converted files
  new_folder_struct_main <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names')
  # location of EIS files
  folder_struct <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\07052023 9 pegmea 1pegda 15wt Tio2')
  # extracts the file paths for EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename =
        stringi::stri_replace_all_regex(
          orig_filename,
          '9 pegmea 1 pegda ',
          '07052023_9_PEGMEA_1_PEGDA_'
        ),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        ' wt Tio2_C01',
                                        '_TiO2_0_none')
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\07052023 9 pegmea 1pegda 15wt Tio2_database_dictionary.csv')
  )
  #return(new_files)
    })
  })
}

cleaning_07052023_files(user_name)

rm(cleaning_07052023_files)
