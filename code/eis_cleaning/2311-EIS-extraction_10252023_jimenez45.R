#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 10262023*

cleaning_10252023_files <- function(user_name) {
  suppressWarnings({
    suppressMessages({
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\10252023')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        ' 1 pegda 9 pegmea ',
        '_9_PEGMEA_1_PEGDA_'
      ),
      # adding colon for separatimg
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        ' wt Litfsi ',
        '_LiTFSI:'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        ' wt litfsi ',
        '_LiTFSI:'
      ),
      # changes towards standard nomenclature
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        ' wt aerosil [(]constant[)] ',
        '_Aerosil380_'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        ' wt aerosil 380 [(]constant[)] ',
        '_Aerosil380_'
      ),
      new_filename = stringi::stri_replace_all_regex(
        new_filename,
        '[(]vary[)]',
        ''
      ),
      file = new_filename,
      file_ext = 
        stringr::str_extract(
          new_filename,
          '[^.]+$'
        ),
      file = 
        dplyr::case_when(
          stringr::str_detect(
            # labels all the X2 as 7.5 
            file,
            '3[/]A2_|3[/]B2_|3[/]C2_|3[/]D2_|3[/]E2_|3[/]F2_|3[/]G2_|3[/]H2_'
          ) ~ stringi::stri_replace_all_regex(
            file,
            '10252023[/]',
            '10252023/10252023_9_PEGMEA_1_PEGDA_8_Aerosil380_30_LiTFSI:/'
          ),
          stringr::str_detect(
            # labels all the X2 as 7.5 
            file,
            '3[/]A1_|3[/]B1_|3[/]C1_|3[/]D1_|3[/]A4_|3[/]H4_'
          ) ~ stringi::stri_replace_all_regex(
            file,
            '10252023[/]',
            '10252023/10252023_9_PEGMEA_1_PEGDA_15_Aerosil380_30_LiTFSI:/'
          ),
          TRUE ~ file
        )
    ) %>%
    # extracting cell information to tie the formulation into the file name
    tidyr::separate(
      file,
      into = c('file', 'b'),
      sep = ':/'
    ) %>%
    tidyr::separate(
      b,
      into = c('loc', 'run'),
      sep = '[_.]'
    ) %>%
    dplyr::mutate(
      new_filename = 
        glue::glue(
          '{file}_{loc}_{run}.{file_ext}'
        )
    ) %>%
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
    dplyr::select(
      c(
      orig_filename,
      new_filename
    ))
  
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\10252023_database_dictionary.csv')
  )
  # return(new_files)
    })
  })
}

cleaning_10252023_files(user_name)
rm(cleaning_10252023_files)
