#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 12122023*

cleaning_12122023_files <- function(user_name) {
  suppressMessages({
    suppressWarnings({
  # loads tidyverse & functions
  require(tidyverse)
  source(
    '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
  )
  # destination folder for converted files
  new_folder_struct_main <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names')
  # location of EIS files
  folder_struct <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\12122023_9_PEGMEA_1_PEGDA_X_Aerosil380_30_LiTFSI')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        'A1',
        '15_Aerosil380_30_LiTFSI_A1'
      ), 
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'A3',
        '15_Aerosil380_30_LiTFSI_A3'
      ), 
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'A5',
        '15_Aerosil380_30_LiTFSI_A5'
      ), 
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'B1',
        '12,85_Aerosil380_30_LiTFSI_B1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'B3',
        '12,85_Aerosil380_30_LiTFSI_B3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'B5',
        '12,85_Aerosil380_30_LiTFSI_B5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'C1',
        '10,71_Aerosil380_30_LiTFSI_C1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'C3',
        '10,71_Aerosil380_30_LiTFSI_C3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'C5',
        '10,71_Aerosil380_30_LiTFSI_C5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'D1',
        '8,56_Aerosil380_30_LiTFSI_D1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'D3',
        '8,56_Aerosil380_30_LiTFSI_D3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'D5',
        '8,56_Aerosil380_30_LiTFSI_D5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'E1',
        '6,42_Aerosil380_30_LiTFSI_E1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'E3',
        '6,42_Aerosil380_30_LiTFSI_E3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'E5',
        '6,42_Aerosil380_30_LiTFSI_E5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'F1',
        '4,28_Aerosil380_30_LiTFSI_F1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'F3',
        '4,28_Aerosil380_30_LiTFSI_F3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'F5',
        '4,28_Aerosil380_30_LiTFSI_F5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'G1',
        '2,14_Aerosil380_30_LiTFSI_G1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'G3',
        '2,14_Aerosil380_30_LiTFSI_G3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'G5',
        '2,14_Aerosil380_30_LiTFSI_G5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'H1',
        '0_Aerosil380_30_LiTFSI_H1'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'H3',
        '0_Aerosil380_30_LiTFSI_H3'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'H5',
        '0_Aerosil380_30_LiTFSI_H5'
      ),
      file_ext = stringr::str_extract(
        new_filename,
        '[^.]+$'
      ) 
    ) %>%
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
    dplyr::select(
      c(orig_filename,
        new_filename)
    )
   
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\12122023_9_PEGMEA_1_PEGDA_X_Aerosil380_30_LiTFSI_database_dictionary.csv')
  )
  
  # return(new_files)
    })
  })
}

cleaning_12122023_files(user_name)
rm(cleaning_12122023_files)