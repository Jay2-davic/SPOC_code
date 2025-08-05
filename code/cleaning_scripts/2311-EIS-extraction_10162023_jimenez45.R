#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 10162023*

cleaning_10162023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\10162023 9 pegmea 1 pegda 15 wt aerosil 380 ( constant) 30 wt Litfsi'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                         '  9 pegmea 1 pegda ',
                                                         '_9_PEGMEA_1_PEGDA_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '0wt',
                                                         '0 wt'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         ' wt aerosil 380 ',
                                                         '_Aerosil380_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         ' _ ',
                                                         '_'),
          # some names have two spaces
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '  ',
                                                         ' '),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         ' wt litfsi ',
                                                         '_LiTFSI_')
        ) %>%
        tidyr::separate(new_filename,
                        into = c('a', 'name'),
                        sep = '[/]') %>%
        dplyr::mutate(new_filename =
                        glue::glue(new_folder_struct_main, '\\{name}')) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\10162023 9 pegmea 1 pegda 15 wt aerosil 380 ( constant) 30 wt Litfsi_database_dictionary.csv'
        )
      )
      # return(new_files)
    })
  })
}

cleaning_10162023_files(user_name)
rm(cleaning_10162023_files)