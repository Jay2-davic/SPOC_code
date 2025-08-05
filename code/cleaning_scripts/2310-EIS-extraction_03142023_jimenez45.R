#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 03132023

cleaning_03142023_files <- function(user_name) {
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Michell Updates\\1_DIW\\2_Prints_on_PCB\\03142023_PAL_PETMP')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(orig_filename,
                                                     ' print',
                                                     '_'),
      # labels the base polymer formulation information
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023 ',
                                                     '03142023_5_PAL_5_PEDGME_'),
      # labels each additive and salt %
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     'Row 1_8',
                                                     '0_none_0_LiTFSI_H1'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023_5_PAL_5_PEDGME_Row 1_7',
                                                     '03132023_5_PAL_5_PEDGME_0_none_0_LiTFSI_H1'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023_5_PAL_5_PEDGME_Row 1_6',
                                                     '0314023_5_PAL_5_PEDGME_0,19_EO_5,7_LiTFSI_F1'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023_5_PAL_5_PEDGME_Row 1_5',
                                                     '0313023_5_PAL_5_PEDGME_0,19_EO_5,7_LiTFSI_E1'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023_5_PAL_5_PEDGME_Row 1_4',
                                                     '0314023_5_PAL_5_PEDGME_0,16_EO_4,8_LiTFSI_D1'),
      new_filename = stringi::stri_replace_all_regex(new_filename,
                                                     '03142023_5_PAL_5_PEDGME_Row 1_3',
                                                     '0314023_5_PAL_5_PEDGME_0,16_EO_4,8_LiTFSI_D1'),
      
    )
  
  # saves the dataframe as a csv file
  # readr::write_csv(
  #   new_files,
  #   glue::glue(C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\03142023_PAL_PETMP_database_dictionary.csv)'
  # )
  #return(new_files)
}
  
#cleaning_03142023_files(user_name)

rm(cleaning_03142023_files)