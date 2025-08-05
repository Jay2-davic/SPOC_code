#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 11012023*

cleaning_11012023_files <- function(user_name) {
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\11012023_CoinCells_9_PEGMEA_1_PEGDA_30LiFTSI_15Aerosil380_varyAerosil_0to15_AllData\\EIS Files')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        '9PEGMEA_1PEGDA_30LiTFSI',
        '11012023_9_PEGMEA_1_PEGDA'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'Ae380',
        '_Aerosil380_30_LiTFSI'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'pt',
        ','
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        '1,_setting_8-2',
        '8-4'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        '1,settiung_8-3',
        '8-5'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        '1_partsetiing_8-1',
        '8-6'
      ),
    ) %>%
    dplyr::mutate(file_ext =
                    stringr::str_extract(new_filename,
                                         '[^.]+$')) %>%
    dplyr::mutate(filename = new_filename) %>%
    tidyr::separate(
      filename,
      into = c('a', 'b', 'c',
               'd'),
      sep = '/'
    ) %>%
    tidyr::separate(
      c,
      into = c('a', 'run', 'c'),
      sep = "[-._]"
    ) %>%
    dplyr::mutate(file = 
                    glue::glue(
                      '{b}_{c}_RUN_{run}'
                    )) %>%
    dplyr::mutate(
      new_filename =
        glue::glue(new_folder_struct_main, '\\{file}.{file_ext}')
    ) %>%
    dplyr::select(c(orig_filename,
                    new_filename))
  
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\11012023_CoinCells_9_PEGMEA_1_PEGDA_30LiFTSI_15Aerosil380_varyAerosil_0to15_AllData.csv')
  )
  # return(new_files)
    })
  })
}

cleaning_11012023_files(user_name)
rm(cleaning_11012023_files)
  