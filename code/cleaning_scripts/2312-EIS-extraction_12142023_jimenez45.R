#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 12142023*

cleaning_12142023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\12142023_9_PEGMEA_1_PEGDA_X_Aerosil380_20_LiTFSI\\EIS_rawfiles'
        )
      
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = dplyr::case_when(
            stringr::str_detect(orig_filename, 'Board 2') == TRUE ~ stringi::stri_replace_all_regex(orig_filename, 'H3', 'H2'),
            TRUE ~ orig_filename
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'RUN',
                                                         'RUN_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '__',
                                                         '_'),
          new_filename = dplyr::case_when(
            stringr::str_detect(new_filename, 'A_A') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_A',
                                                                                               'A_15_Aerosil380_20_LiTFSI_A'),
            stringr::str_detect(new_filename, 'A_B') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_B',
                                                                                               'A_12,85_Aerosil380_20_LiTFSI_B'),
            stringr::str_detect(new_filename, 'A_C') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_C',
                                                                                               'A_10,71_Aerosil380_20_LiTFSI_C'),
            stringr::str_detect(new_filename, 'A_D') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_D',
                                                                                               'A_8,56_Aerosil380_20_LiTFSI_D'),
            stringr::str_detect(new_filename, 'A_E') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_E',
                                                                                               'A_6,42_Aerosil380_20_LiTFSI_E'),
            stringr::str_detect(new_filename, 'A_F') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_F',
                                                                                               'A_4,28_Aerosil380_20_LiTFSI_F'),
            stringr::str_detect(new_filename, 'A_G') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_G',
                                                                                               'A_2,14_Aerosil380_20_LiTFSI_G'),
            stringr::str_detect(new_filename, 'A_H') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                               'A_H',
                                                                                               'A_0_Aerosil380_20_LiTFSI_H'),
            stringr::str_detect(new_filename, 'Board 2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                                   'H3',
                                                                                                   'H2'),
            TRUE ~ new_filename
            
          )
          
        ) %>%
        # missing formulation information for each cell
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\12142023_9_PEGMEA_1_PEGDA_X_Aerosil380_20_LiTFSI_database_dictionary.csv'
        )
      )
    })
  })
}

cleaning_12142023_files(user_name)
rm(cleaning_12142023_files)
