#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 03132023

cleaning_03132023_files <- function(user_name) {   
  # loads tidyverse & functions
  require(tidyverse)
  source(
    '~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R'
  )
  # destination folder for converted files
  new_folder_struct_main <-
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
  # location of EIS files
  folder_struct <-
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Michell Updates\\1_DIW\\2_Prints_on_PCB\\03132023_PAL_PETMP')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        ' print',
        '_'
      )
    )
      
  # saves the dataframe as a csv file
  # readr::write_csv(
  #   new_files,
  #   r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr files\data_dictionary\03132023_PAL_PETMP_database_dictionary.csv)'
  # )
  #return(new_files)
}

#cleaning_03132023_files()

rm(cleaning_03132023_files)
