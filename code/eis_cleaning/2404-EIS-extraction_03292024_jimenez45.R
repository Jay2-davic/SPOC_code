#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 03292024

cleaning_03132024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\03292024_9_PEGMEA_1_PEGDA_10_LiTFSI_X_Aerosil380')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        '/',
        '_'
      ),
      new_filename = dplyr::case_when(
        stringr::str_detect(new_filename, '1-1|1-2|1-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '15'),
        stringr::str_detect(new_filename, '2-1|2-2|2-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '12,85'),
        stringr::str_detect(new_filename, '3-1|3-2|3-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '10,71'),
        stringr::str_detect(new_filename, '4-1|4-2|4-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '8,56'),
        stringr::str_detect(new_filename, '5-1|5-2|5-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '6,42'),
        stringr::str_detect(new_filename, '6-1|6-2|6-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '4,28'),
        stringr::str_detect(new_filename, '7-1|7-2|7-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '2,14'),
        stringr::str_detect(new_filename, '8-1|8-2|8-3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, 'X', '0'),
        TRUE ~ new_filename
      )
    ) %>%
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '03292024.*')) %>%
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
    dplyr::select(c(orig_filename,
                    new_filename))
      
  # saves the dataframe as a csv file
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\03292024_9_PEGMEA_1_PEGDA_10_LiTFSI_X_Aerosil380_database_dictionary.csv')
  )
  return(new_files)
}

cleaning_03132024_files(user_name)

rm(cleaning_03132024_files)
