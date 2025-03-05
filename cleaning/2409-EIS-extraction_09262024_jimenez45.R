#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09252024

cleaning_09262024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\20240926_1,4_PEGMEA_1_Thiolene_1_PEO_12_LiTFSI_194_ANPHA_1,5_TEMPO')
  # extracts the file paths of EIS files
  cleaned_data <- EIS_cleaner_thiolene(folder_struct,
                              new_folder_struct_main,
              salt = 'LiTFSI',
              additive = 'PEO',
              wt_salt_max_1 = 12,
              wt_salt_max_2 = 12,
              wt_additive_max_1 = 1,
              wt_additive_max_2 = 0)
  
  # saves the dataframe as a csv file
  readr::write_csv(
    cleaned_data,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\20240926_1,4_PEGMEA_1_Thiolene_1_PEO_12_LiTFSI_194_ANPHA_1,5_TEMPO_database_dictionary.csv)')
  )
}

cleaning_09262024_files(user_name)

rm(cleaning_09262024_files)

