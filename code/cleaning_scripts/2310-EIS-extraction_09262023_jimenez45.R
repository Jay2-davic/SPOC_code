#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09262023*

cleaning_09262023_files <- function(user_name) {
  suppressWarnings({
    suppressMessages({
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\09262023 9pegmea 1 pegda 15wt aerosil 380 7.5 wt litfsi'
        )
      # extracts the file paths from EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              '09262023  9 pegmea 1 pegda ',
              '09262023_9_PEGMEA_1_PEGDA_'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' wt aerosil 380 ',
                                            '_Aerosil380_')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'wt litfsi ',
                                            '_LiTFSI_')
        ) %>%
        dplyr::mutate(new_filename =
                        stringi::stri_replace_all_regex(new_filename,
                                                        ' litfsi ',
                                                        '_LiTFSI_')) %>%
        dplyr::mutate(new_filename =
                        stringi::stri_replace_all_regex(new_filename,
                                                        ' ',
                                                        '')) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              'Aerosil380_A4',
              'Aerosil380_7,5_LiTFSI_A4'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '_7[.]',
                                            '_7,5_LiTFSI_A4_4.')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '_7_',
                                            '_7,5_LiTFSI_A4_4_')
        ) %>%
        dplyr::mutate(new_filename =
                        stringi::stri_replace_all_regex(new_filename,
                                                        '22',
                                                        '3')) %>%
        dplyr::mutate(file =
                        stringr::str_extract(new_filename,
                                             '[^.]+$')) %>%
        dplyr::mutate(
          new_filename =
            stringr::str_extract(new_filename,
                                 pattern = '[^/]+$'),
          grouping = stringr::str_extract(new_filename,
                                          pattern = '[^/]+$')
        ) %>%
        tidyr::separate(
          grouping,
          into = c('a', 'b', 'c',
                   'd', 'e', 'f',
                   'g', 'h', 'i',
                   'j'),
          sep = '_'
        ) %>%
        dplyr::mutate(new_filename =
                        stringi::stri_replace_all_regex(new_filename,
                                                        glue::glue('_{j}_'),
                                                        '_')) %>%
        dplyr::group_by(file, f, h, j) %>%
        dplyr::mutate(meas = as.numeric(factor(new_filename))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'LITFSI',
                                                         glue::glue('LiTFSI_{meas}')),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        ) %>%
        dplyr::select(c(orig_filename,
                        new_filename))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\09262023 9pegmea 1 pegda 15wt aerosil 380 7,5 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_09262023_files(user_name)
rm(cleaning_09262023_files)