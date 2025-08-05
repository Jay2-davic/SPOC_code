#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 10262023*

cleaning_10262023_files <- function(user_name) {
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\10262023 1 pegda 9 pegmea 30wt litfsi (vary)')
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
        '30wt litfsi ',
        '30_LiTFSI:'
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
        dplyr::case_when(
          loc != 'pin0' ~ glue::glue(
            '{file}_{run}.{file_ext}'),
          TRUE ~ glue::glue(
            '{file}_2.{file_ext}'
          )
        ),
      # adding formulation information based on the column position or cell location
      new_filename = 
        dplyr::case_when(
          loc == 'A2' | run == 'column0' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '0_none'),
          loc == 'B2' | run == 'column1' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '4,29_LiTFSI'),
          loc == 'C2' | run == 'column2' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '8,57_LiTFSI'),
          loc == 'D2' | run == 'column3' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '12,86_LiTFSI'),
          loc == 'E2' | run == 'column4' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '17,14_LiTFSI'),
          loc == 'F2' | run == 'column5' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '21,43_LiTFSI'),
          loc == 'G2' | run == 'column2' ~ stringi::stri_replace_all_regex(
            new_filename,
            '30_LiTFSI',
            '25,71_LiTFSI'),
          TRUE ~ new_filename,
        ),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = 'none',
                                        replacement = 'none_RUN'),
      new_filename =
        stringi::stri_replace_all_regex(new_filename,
                                        pattern = 'LiTFSI',
                                        replacement = 'LiTFSI_RUN'),
      
    ) %>%
    # tidyr::separate(
    #   file,
    #   into = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
    #   sep = '\\\\'
    # ) %>%
    dplyr::mutate(
      new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$'),
      new_filename =
        glue::glue(new_folder_struct_main, '\\{new_filename}')
    ) %>%
    dplyr::select(
      c(orig_filename, new_filename)
    )
  
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\10262023 1 pegda 9 pegmea 30wt litfsi (vary)_database_dictionary.csv')
  )
  # return(new_files)
    })
  })
}
  
cleaning_10262023_files(user_name)
rm(cleaning_10262023_files)
