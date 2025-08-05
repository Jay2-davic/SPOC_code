#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 08172023*

cleaning_08172023_files <- function(user_name) {
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
      folder_structure_1 <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\08172023 Coin cells 1 pegda 9 pegmea 5 wt aerosil 380 15 wt litfsi'
        )
      folder_structure_2 <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\08172023 coin cells 1 pegda 9 pegmea 15 wt litfsi'
        )
      folder_structure_3 <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\08172023 coin cells 1 pegda 9 pegmea 15 wt litfsi 25 wt tio2 0.29mm'
        )
      
      orig_files_1 <- EIS_finder(folder_structure_1)
      orig_files_2 <- EIS_finder(folder_structure_2)
      orig_files_3 <- EIS_finder(folder_structure_3)
      
      new_files <- orig_files_1 %>%
        dplyr::bind_rows(orig_files_2,
                         orig_files_3) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(orig_filename,
                                            'pegamea',
                                            'pegmea'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '0.29mm/1',
                                            '0.29mm/A 1'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '1pegda',
                                            '1 pegda'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '5wt',
                                            '5 wt'),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              ' 1 pegda 9 pegmea 25 wt tio2 15 wt litfsi',
              '_08172023_9_PEGMEA_1_PEGDA_25_tio2_15_LiTFSI'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              ' 1 pegda 9 pegmea 15 wt litfsi',
              '_08172023_9_PEGMEA_1_PEGDA_0_none_15_LiTFSI'
            ),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              ' 1 pegda 9 pegmea 5 wt aerosil 380 15 wt litfsi',
              '_08172023_9_PEGMEA_1_PEGDA_5_Aerosil380_15_LiTFSI'
            ),
          meas =
            stringr::str_extract(new_filename, pattern = '[^/]+$'),
          meas =
            stringr::str_extract(meas,
                                 '^(.*?)_'),
          string_to_remove =
            stringr::str_extract(meas,
                                 '^(.*?)_'),
          meas =
            stringi::stri_replace_all_regex(meas,
                                            '_',
                                            ''),
          meas =
            factor(meas) |>
            as.numeric(),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            string_to_remove,
                                            ''),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'LiTFSI',
                                            glue::glue('LiTFSI_{meas}')),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '_LiTFSC11',
                                            '_LiTFSI_C11'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'tio2',
                                            'TiO2'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'PEGME1',
                                            'PEGMEA_1'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'PEGD25',
                                            'PEGDA_25'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'PEGD0',
                                            'PEGDA_0')
        ) %>%
        # removes the folder names and replaces it with the destination folder
        dplyr::mutate(
          new_filename =
            stringr::str_extract(new_filename, pattern = '[^/]+$'),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        ) %>%
        dplyr::select(-c(string_to_remove, meas))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\08172023 Coin cells 1 pegda 9 pegmea 5 wt aerosil 380 15 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_08172023_files(user_name)
rm(cleaning_08172023_files)