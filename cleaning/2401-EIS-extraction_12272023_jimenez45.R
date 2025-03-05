#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 12272023*

cleaning_12272023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\12272023_9_PEGMEA_1_PEGDA_15Aerosil380_varyLiTFSI_0to30_AllData'
        )
      
      orig_files <- EIS_finder(folder_struct)
      
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                         'RUN',
                                                         'RUN_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '__',
                                                         '_'),
          new_filename = dplyr::case_when(
            stringr::str_detect(new_filename, 'A2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'A2',
                                                                                              '15_Aerosil380_0_LiTFSI_A2'),
            stringr::str_detect(new_filename, 'B2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'B2',
                                                                                              '15_Aerosil380_4,29_LiTFSI_B2'),
            stringr::str_detect(new_filename, 'C2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'C2',
                                                                                              '15_Aerosil380_8,57_LiTFSI_C2'),
            stringr::str_detect(new_filename, 'D2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'D2',
                                                                                              '15_Aerosil380_12,86_LiTFSI_D2'),
            stringr::str_detect(new_filename, 'E2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'E2',
                                                                                              '15_Aerosil380_17,14_LiTFSI_E2'),
            stringr::str_detect(new_filename, 'F2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'F2',
                                                                                              '15_Aerosil380_21,43_LiTFSI_F2'),
            stringr::str_detect(new_filename, 'G2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'G2',
                                                                                              '15_Aerosil380_25,71_LiTFSI_G2'),
            stringr::str_detect(new_filename, 'H2') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                              'H2',
                                                                                              '15_Aerosil380_30_LiTFSI_H2'),
            TRUE ~ new_filename
          ),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '12282023',
                                                         '12272023')
          
        ) %>%
        # missing formulation information for each cell
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\12272023_9_PEGMEA_1_PEGDA_15Aerosil380_varyLiTFSI_0to30_AllData_database_dictionary.csv'
        )
      )
    })
  })
}

cleaning_12272023_files(user_name)
rm(cleaning_12272023_files)
