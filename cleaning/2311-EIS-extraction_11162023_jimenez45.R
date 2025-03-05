#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 11162023*

cleaning_11162023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\11162023_9_PEGMEA_1_PEGDA_10LiFTSI_15Aerosil380'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            orig_filename,
            '9PEGMEA_1PEGDA_10LiTFSI_',
            '11162023_9_PEGMEA_1_PEGDA_'
          ),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'Ae380',
                                                         '_Aerosil380'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'pt',
                                                         ','),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'Aerosil380',
                                                         'Aerosil380_10_LiTFSI'),
          file_ext = stringr::str_extract(new_filename,
                                          '[^.]+$')
        ) %>%
        tidyr::separate(new_filename,
                        into = c('a', 'name', 'c'),
                        sep = '[/]') %>%
        dplyr::mutate(new_filename =
                        glue::glue(new_folder_struct_main, '\\{name}.{file_ext}')) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\11162023_9_PEGMEA_1_PEGDA_10LiFTSI_15Aerosil380_varyAerosil_0to15_AllData_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_11162023_files(user_name)
rm(cleaning_11162023_files)
