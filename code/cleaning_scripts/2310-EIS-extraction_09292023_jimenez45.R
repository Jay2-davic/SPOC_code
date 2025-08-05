#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09292023*

cleaning_09292023_files <- function(user_name) {
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
          'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\09292023 9 pegmea 1pegda 15 wt aerosil 380 22.5 wt litfsi'
        )
      # extracts the file paths for EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(orig_filename,
                                            '  9 pegmea 1 pegda ',
                                            '_9_PEGMEA_1_PEGDA_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' wt aerosil 380 ',
                                            '_Aerosil380_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' litfsi ',
                                            '_LiTFSI_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'H _',
                                            'H1 _'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'G _',
                                            'G1 _'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' _ ',
                                            '_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            ' _',
                                            '_'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            'B 3',
                                            'B3'),
          new_filename =
            stringi::stri_replace_all_regex(new_filename,
                                            '_ ',
                                            '_'),
          subfolder =
            dplyr::if_else(
              stringr::str_detect(new_filename,
                                  pattern = '10092023') == TRUE,
              2,
              1
            ),
          new_filename =
            stringr::str_extract(new_filename,
                                 pattern = '[^/]+$'),
          type =
            stringr::str_extract(new_filename,
                                 pattern = '[^.]+$'),
          pos =
            stringr::str_extract(new_filename,
                                 pattern = '[A-Z]\\d+'),
          string_to_delete =
            stringr::str_extract(new_filename,
                                 pattern = '_[A-Z]\\d+_\\d+'),
        ) %>%
        dplyr::mutate(
          meas =
            stringr::str_extract(string_to_delete,
                                 pattern = '_\\d'),
          meas =
            stringi::stri_replace_all_regex(meas,
                                            '_',
                                            ''),
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              glue::glue('{string_to_delete}'),
              glue::glue('_{pos}_{meas}')
            ),
          new_filename =
            paste0(new_folder_struct_main, '\\', new_filename)
        ) %>%
        dplyr::select(c(orig_filename, new_filename))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\09292023 9 pegmea 1pegda 15 wt aerosil 380 22,5 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_09292023_files(user_name)
rm(cleaning_09292023_files)
