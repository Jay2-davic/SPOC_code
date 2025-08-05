#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 07252023*

cleaning_07252023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
      # loads tidyverse & functions
      require(tidyverse)
      source(
        '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
      )  # location of EIS files
      folder_struct <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\07252023 9 pegmea 1 pegda 15 wt TiO2 15 wt litfsi print 1'
        )
      folder_struct2 <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\07252023 9 pegmea 1 pegda 15wt Tio2 15 wt Litfsi print 2'
        )
      # destination folder for converted files
      new_folder_struct_main <-
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct) %>%
        dplyr::bind_rows(EIS_finder(folder_struct2))
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '9 pegmea 1 pegda 0wt tio2 0wt liftsi',
              replacement = '9_pegmea_1_pegda_0_TiO2_0_LiTFSI'
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71wt tio2 10,71liftsi',
            replacement = '9_pegmea_1_pegda_10,71_TiO2_10,71_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2.14wt tio2 2',
            replacement = '9_pegmea_1_pegda_2,14_TiO2_2,14_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 428wt tio2 4,28liftsi',
            replacement = '9_pegmea_1_pegda_4,28_TiO2_4,28_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,42wt tio2 6,42liftsi',
            replacement = '9_pegmea_1_pegda_6,42_TiO2_6,42_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57wt tio2 8,57liftsi',
            replacement = '9_pegmea_1_pegda_8,57_TiO2_8,57_LiTFSI'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '  ',
            replacement = '_'
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = ' ',
            replacement = '_'
          )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea',
            replacement = '07252023_9_pegmea'
          ),
          new_filename = stringi::stri_replace_all_regex(
            new_filename ,
            pattern = 'pegmea_1_pegda',
            replacement = 'PEGMEA_1_PEGDA'
          )
        )
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\07252023 1pegda 9pegmea 15wt liftsi 15wt tio2_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_07252023_files(user_name)
rm(cleaning_07252023_files)
