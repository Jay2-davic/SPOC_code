#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#data cleaning of EIS files from 10232023*

cleaning_10232023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse and its functions
      require(tidyverse)
      # loads EIS file finder
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )
      # loads directory where renamed files are going
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      
      # loads directory where the files are located
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\10232023  9 pegmea 1 pegda 15 wt Aerosil 380 (vary)'
        )
      
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                         '[A-Z]\\d+ ',
                                                         'none_0'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '  | |wt| _ ',
                                                         '_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '9_pegmea_1_pegda',
                                                         '9_PEGMEA_1_PEGDA'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_aerosil_380',
                                                         'Aerosil380'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '__',
                                                         '_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_0_',
                                                         '_0_RUN_'),
          
        ) %>%
        # missing formulation information for each cell
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\10232023  9 pegmea 1 pegda 15 wt Aerosil 380 (vary)_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_10232023_files(user_name)
rm(cleaning_10232023_files)