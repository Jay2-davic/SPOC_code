#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 04012022*

cleaning_04012022_files <- function(user_name) {
  # loads tidyverse & functions
  suppressMessages({
    suppressWarnings({
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
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Michell Updates\\1_DIW\\1_Prints_on_Stainless_Steel_Spacers\\040122_1PEGDA 9PEGMEA LITFSI_0-50wt%'
        )
      # extracts the file paths of EIS files
      orig_files <- EIS_finder(folder_struct)
      # formats the names
      new_files <- orig_files %>%
        dplyr::mutate(
          new_filename = stringi::stri_replace_all_regex(
            orig_filename,
            '1PEGDA_9PEGMEA_',
            '9_PEGMEA_1_PEGDA_0_none_'
          ),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'SALT_',
                                                         '_LiTFSI_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '\\d{1,3}POLYMER_',
                                                         ''),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_EIS_04_01_22',
                                                         ''),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         'R\\d{1}C\\d{1}_',
                                                         '040122_'),
          # removes stray date at the end of file names
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_040722',
                                                         ''),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_BC_7M-1',
                                                         ''),
          # corrects the date measurement 040722 for some samples using the orig filename
          new_filename = dplyr::if_else(
            stringr::str_detect(orig_filename,
                                '_040722'),
            stringi::stri_replace_all_regex(new_filename,
                                            '040122',
                                            '040722'),
            new_filename
          ),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_C\\d{2}',
                                                         ''),
          # adds replicate information
          new_filename = dplyr::if_else(
            stringr::str_detect(orig_filename,
                                'Run1'),
            stringi::stri_replace_all_regex(new_filename,
                                            'LiTFSI_',
                                            'LiTFSI_1_'),
            stringi::stri_replace_all_regex(new_filename,
                                            'LiTFSI_',
                                            'LiTFSI_2_')
          ),
          # corrects for 0% LiTFSI that has a 3rd replicate
          new_filename = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              'Run1/R1C1_1PEGDA_9PEGMEA_0SALT_100POLYMER_60C_EIS_04_01_22_BC_7M-1_C13'
            ),
            stringi::stri_replace_all_regex(new_filename,
                                            'LiTFSI_1',
                                            'LiTFSI_3'),
            new_filename
          ),
          new_filename = dplyr::if_else(
            stringr::str_detect(
              orig_filename,
              'Run1/R1C1_1PEGDA_9PEGMEA_0SALT_100POLYMER_RT_EIS_04_01_22_BC_7M-1'
            ),
            stringi::stri_replace_all_regex(new_filename,
                                            'LiTFSI_1',
                                            'LiTFSI_3'),
            new_filename
          ),
          # divide the LiTFSI by 2 to reflect wt%
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_20_',
                                                         '_10_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_40_',
                                                         '_20_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_60_',
                                                         '_30_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_80_',
                                                         '_40_'),
          new_filename = stringi::stri_replace_all_regex(new_filename,
                                                         '_100_',
                                                         '_50_'),
        ) %>%
        dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
        dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename))
      
      # saves the dataframe as a csv file
      readr::write_csv(
        new_files,
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\040122_1PEGDA 9PEGMEA LITFSI_0-50wt%_database_dictionary.csv'
        )
      )
      #return(new_files)
    })
  })
}

# cleaning_04012022_files(user_name)

rm(cleaning_04012022_files)
