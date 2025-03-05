#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 06292023*

cleaning_08302023_files <- function(user_name) {
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Katherine Updates\\08302023'
        )
      # extracts the file paths of the EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          subfolder = stringr::str_extract(orig_filename, pattern = '([^/\\\\]+)(?=[/\\\\][^/\\\\]*$|$)')
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            orig_filename,
            pattern = '1pegda 9 pegmea',
            replacement = '9 pegmea 1 pegda'
          )
        ) %>%
        dplyr::mutate(
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 4,29wt aerosil 380 15 wt litfsi 2'
            ),
            '7',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2'
            ),
            '10',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2 3'
            ),
            '7',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2 4'
            ),
            '8',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2'
            ),
            '12',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '5/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2'
            ),
            '11',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '3/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2'
            ),
            '9',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 2,14 wt aerosil 380 15 wt litfsi 2'
            ),
            '7',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 12,57wt aerosil 380 15 wt litfsi_'
            ),
            '7',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 10,71 wt aerosil 380 15 wt litfsi 2'
            ),
            '10',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 6,43 wt aerosil 380 15 wt litfsi 2'
            ),
            '7',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2 3'
            ),
            '13',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 4,29 wt aerosil 380 15 wt litfsi 2'
            ),
            '8',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 15 wt aerosil 380 15 wt litfsi 2 4'
            ),
            '14',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '4/1pegda 9 pegmea 0 wt aerosil 380 15 wt litfsi 2'
            ),
            '10',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '6/1pegda 9 pegmea 0 wt aerosil 380 15 wt litfsi 2'
            ),
            '11',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '6/1pegda 9 pegmea 2,14 wt aerosil 380 15 wt litfsi 2'
            ),
            '10',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '1/1pegda 9 pegmea 12,57wt aerosil 380 15 wt litfsi'
            ),
            '8',
            subfolder
          ),
          subfolder = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              '2/1pegda 9 pegmea 4,29 wt aerosil 380 15 wt litfsi'
            ),
            '9',
            subfolder
          )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 0 wt aerosil 380 15 wt litfsi 2',
              replacement = glue::glue(
                '9_pegmea_1_pegda_0_Aerosil380_15_LiTFSI_{subfolder}'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename =
            stringi::stri_replace_all_regex(
              new_filename,
              pattern = '9 pegmea 1 pegda 0 wt aerosil 380 15 wt litfsi',
              replacement = glue::glue(
                '9_pegmea_1_pegda_0_Aerosil380_15_LiTFSI_{subfolder}'
              )
            )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 04wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_0_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4.mps',
            replacement = glue::glue('9 pegmea 1 pegda 4,29 aerosil 380 15 wt litfsi.mps')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4_',
            replacement = glue::glue('2/9 pegmea 1 pegda 4,29 aerosil 380 15 wt litfsi_')
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 0wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_0_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 1,43 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 2,14 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_2,14_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,86 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,86 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 3,57 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 5 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_5_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,56 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt aerosil 380 15 wt litfsi 2 4',
            replacement = glue::glue(
              '9_pegmea_1_pegda_15_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt aerosil 380 15 wt litfsi 2 3',
            replacement = glue::glue(
              '9_pegmea_1_pegda_15_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_15_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 15 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_15_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_10,71_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_10,71_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 10,71  wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_10,71_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,86 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,56 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,43wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,43 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 6,43 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 12,57wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_12,86_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57  wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,42  wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_6,43_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 8,57wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_8,57_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 wt aerosil 380 15 wt litfsi 2',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9 pegmea 1 pegda 4,29 wt aerosil 380 15 wt litfsi',
            replacement = glue::glue(
              '9_pegmea_1_pegda_4,29_Aerosil380_15_LiTFSI_{subfolder}'
            )
          )
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            new_filename,
            pattern = '9_pegmea_1_pegda',
            replacement = '08302023_9_PEGMEA_1_PEGDA'
          )
        ) %>%
        dplyr::select(-subfolder)
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\08302023 9 pegmea 1 pegda 15 wt aerosil 380 15 wt litfsi_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

cleaning_08302023_files(user_name)
rm(cleaning_08302023_files)
