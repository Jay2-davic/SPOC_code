#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09012023*

cleaning_09012023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\091223 1 pegda 9 pegmea 15 wt aerosil 380'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              orig_filename,
              pattern = '1pegda 9 pegmea',
              replacement = '9 pegmea 1 pegda'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_1-2',
              replacement = '&2&'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_2-2',
              replacement = '&2&'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_2',
              replacement = '&1&'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_1',
              replacement = '&3&'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_3',
              replacement = '&2&'
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '_4',
              replacement = '&4&'
            )
        ) %>%
        tidyr::separate(
          col = new_filename,
          into = c('new_filename', 'meas', 'C'),
          sep = '&'
        ) %>%
        tidyr::separate(
          col = new_filename,
          into = c('new_filename', 'pos'),
          sep = '_'
        ) %>%
        dplyr::mutate(new_filename = glue::glue('{new_filename}_{meas}_{pos}{C}')) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_A',
              replacement = glue::glue('06152023_9_pegmea_1_pegda_0_Aerosil90_0_LiTFSI_1_A')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_A',
              replacement = glue::glue('06152023_9_pegmea_1_pegda_0_Aerosil90_0_LiTFSI_2_A')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_A',
              replacement = glue::glue('06152023_9_pegmea_1_pegda_0_Aerosil90_0_LiTFSI_3_A')
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_B',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_2,14_Aerosil90_0_LiTFSI_1_B'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_B',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_2,14_Aerosil90_0_LiTFSI_2_B'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_B',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_2,14_Aerosil90_0_LiTFSI_3_B'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_4_B',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_2,14_Aerosil90_0_LiTFSI_4_B'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_C',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_4,28_Aerosil90_0_LiTFSI_1_C'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_C',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_4,28_Aerosil90_0_LiTFSI_2_C'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_C',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_4,28_Aerosil90_0_LiTFSI_3_C'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_D',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_6,43_Aerosil90_0_LiTFSI_1_D'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_D',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_6,43_Aerosil90_0_LiTFSI_2_D'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_D',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_6,43_Aerosil90_0_LiTFSI_3_D'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_E',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_8,57_Aerosil90_0_LiTFSI_1_E'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_E',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_8,57_Aerosil90_0_LiTFSI_2_E'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_E',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_8,57_Aerosil90_0_LiTFSI_3_E'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_F',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI_1_F'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_F',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI_2_F'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_F',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI_3_F'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_4_F',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_10,71_Aerosil90_0_LiTFSI_4_F'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_G',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_12,86_Aerosil90_0_LiTFSI_1_G'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_G',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_12,86_Aerosil90_0_LiTFSI_2_G'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_G',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_12,86_Aerosil90_0_LiTFSI_3_G'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_1_H',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_15_Aerosil90_0_LiTFSI_1_H'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_2_H',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_15_Aerosil90_0_LiTFSI_2_H'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  9 pegmea 1 pegda 15 wt aerosil 90_3_H',
              replacement = glue::glue(
                '06152023_9_pegmea_1_pegda_15_Aerosil90_0_LiTFSI_3_H'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '06152023  ',
              replacement = glue::glue('06152023_')
            )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '06152023',
            replacement = glue::glue('09122023')
          )
        ) %>%
        dplyr::filter(
          !stringr::str_detect(new_filename,
                               pattern = '09122023_9 pegmea 1 pegda 15 wt aerosil 90_NA_B1NA')
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = 'pegmea_1_pegda',
              replacement = 'PEGMEA_1_PEGDA'
            )
        ) %>%
        dplyr::select(c(orig_filename, new_filename))
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\091223 1 pegda 9 pegmea 15 wt aerosil 380_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_09012023_files(user_name)
rm(cleaning_09012023_files)