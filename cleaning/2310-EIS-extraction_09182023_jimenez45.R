#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09182023*

cleaning_09182023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse & functions
      require(tidyverse)
      source(
        '~/Git/polyelectrolyte/polyelectrolyte/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      # location of EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\09182023 1 pegda 9 pegmea 15 wt litfsi 15 aerosil 380'
        )
      # extracts the file path for EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(orig_filename,
                                            '\\d+,\\d+_',
                                            '07052023_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '1 PEGDA 9 PEGMEA ',
                                            '9_PEGMEA_1_PEGDA_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' WT |WT ',
                                            '_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'AEROSIL 380 ',
                                            'Aerosil380_'),
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
                                            'PEGME1_PEGD15',
                                            'PEGMEA_1_PEGDA_15'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'LITFSC',
                                            glue::glue('LITFSI_C')),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'LITFSD',
                                            glue::glue('LITFSI_D'))
        ) %>%
        dplyr::mutate(
          new_filename =
            stringr::str_extract(new_filename,
                                 pattern = '[^/]+$'),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        ) %>%
        # grouping by formulation
        dplyr::mutate(grouping =
                        stringr::str_extract(new_filename,
                                             pattern = '[^\\\\]+$')) %>%
        dplyr::mutate(type =
                        stringr::str_extract(new_filename,
                                             pattern = '[^.]+$')) %>%
        tidyr::separate(
          grouping,
          into = c('a', 'b', 'c',
                   'd', 'e', 'f',
                   'g', 'h', 'i'),
          sep = '_'
        ) %>%
        dplyr::group_by(f, type) %>%
        dplyr::mutate(meas = as.numeric(factor(meas))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(new_filename = stringi::stri_replace_all_regex(new_filename,
                                                                     'LITFSI',
                                                                     glue::glue('LiTFSI_{meas}')))  %>%
        dplyr::select(-c(a, b, c, d, e, g, h, i, string_to_remove, f, type, meas))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\09182023 1 pegda 9 pegmea 15 wt litfsi 15 aerosil 380_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_09182023_files(user_name)
rm(cleaning_09182023_files)
