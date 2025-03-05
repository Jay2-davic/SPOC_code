#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 11172023*

cleaning_11172023_files <- function(user_name) {
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\11172023_9_PEGMEA_1_PEGDA_20LiFTSI_15Aerosil380')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        'Row 1|Row 3',
        '11172023_'
      ), 
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        ' 9PEGMEA_1PEGDA_20LiTFSI',
        '9_PEGMEA_1_PEGDA_20_LiTFSI'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'Ae380',
        '_Aerosil380'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        'pt',
        ','
      ),
      new_filename = dplyr::case_when(
        stringr::str_detect(orig_filename, 
                            '11_17_14') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                  'Aerosil380',
                                                                                  'Aerosil380_20_LiTFSI_RUN_1'),
        stringr::str_detect(orig_filename, 
                            '11_17_15') == TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                                                                  'Aerosil380',
                                                                                  'Aerosil380_20_LiTFSI_RUN_2'),
        TRUE ~ stringi::stri_replace_all_regex(new_filename,
                                               'Aerosil380',
                                               'Aerosil380_20_LiTFSI_RUN_3')
      ),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     'A_20_LiTFSI',
                                                     'A'),
      file_ext = stringr::str_extract(
        new_filename,
        '[^.]+$'
      ),
      new_filename = stringr::str_remove(
        new_filename,
        '/pin.*$|/PIN.*$|/redo.*$|/Redo.*$|/print.*$'
      ),
      new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$'),
      new_filename = glue::glue(new_folder_struct_main, '/{new_filename}.{file_ext}')
    ) %>%
    dplyr::select(
      c(orig_filename,
        new_filename)
    )
  
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\11172023_9_PEGMEA_1_PEGDA_20LiFTSI_15Aerosil380_varyAerosil_0to15_AllData_database_dictionary.csv')
  )
  
  #return(new_files)
    })
  })
}
  
cleaning_11172023_files(user_name)
rm(cleaning_11172023_files)