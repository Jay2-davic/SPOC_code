#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09182023*

cleaning_01182024_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\01182024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI'
        )
      
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                         '01182024',
                                                         '01182024_9_PEGMEA_1_PEGDA'),
          new_filename = dplyr::case_when(
            stringr::str_detect(new_filename, 'A2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'A2',
                                                                                              '0_Aerosil380_10_LiTFSI_A2'),
            stringr::str_detect(new_filename, 'B2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'B2',
                                                                                              '2,14_Aerosil380_10_LiTFSI_B2'),
            stringr::str_detect(new_filename, 'C2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'C2',
                                                                                              '4,28_Aerosil380_10_LiTFSI_C2'),
            stringr::str_detect(new_filename, 'D2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'D2',
                                                                                              '6,42_Aerosil380_10_LiTFSI_D2'),
            stringr::str_detect(new_filename, 'E2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'E2',
                                                                                              '8,56_Aerosil380_10_LiTFSI_E2'),
            stringr::str_detect(new_filename, 'F2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'F2',
                                                                                              '10,71_Aerosil380_10_LiTFSI_F2'),
            stringr::str_detect(new_filename, 'G2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'G2',
                                                                                              '12,86_Aerosil380_10_LiTFSI_G2'),
            stringr::str_detect(new_filename, 'H2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'H2',
                                                                                              '15_Aerosil380_10_LiTFSI_H2'),
            stringr::str_detect(new_filename, 'A3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'A3',
                                                                                              '0_Aerosil380_10_LiTFSI_A3'),
            stringr::str_detect(new_filename, 'B1') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'B1',
                                                                                              '2,14_Aerosil380_10_LiTFSI_B1'),
            stringr::str_detect(new_filename, 'C3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'C3',
                                                                                              '4,28_Aerosil380_10_LiTFSI_C3'),
            stringr::str_detect(new_filename, 'D3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'D3',
                                                                                              '6,42_Aerosil380_10_LiTFSI_D3'),
            stringr::str_detect(new_filename, 'E3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'E3',
                                                                                              '8,56_Aerosil380_10_LiTFSI_E3'),
            stringr::str_detect(new_filename, 'F3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'F3',
                                                                                              '10,71_Aerosil380_10_LiTFSI_F3'),
            stringr::str_detect(new_filename, 'G3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'G3',
                                                                                              '12,86_Aerosil380_10_LiTFSI_G3'),
            stringr::str_detect(new_filename, 'H3') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'H3',
                                                                                              '15_Aerosil380_10_LiTFSI_H3'),
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\01182024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI_database_dictionary.csv'
        )
      )
    })
  })
}

cleaning_01182024_files(user_name)
rm(cleaning_01182024_files)
