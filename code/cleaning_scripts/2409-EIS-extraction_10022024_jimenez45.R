#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 09252024

cleaning_10022024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\20241002_9_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI_100ppm_Inhibitor')
  # extracts the file paths of EIS files
  cleaned_data <- EIS_cleaner(folder_struct,
                              new_folder_struct_main,
              salt = 'LiTFSI',
              additive = 'Aerosil380',
              wt_salt_max_1 = 0,
              wt_salt_max_2 = 0,
              wt_additive_max_1 = 15,
              wt_additive_max_2 = 15) |>
    dplyr::mutate(new_filename = stringi::stri_replace_all_regex(new_filename, '2024', '10022024'))
  
  # saves the dataframe as a csv file
  readr::write_csv(
    cleaned_data,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\20241002_9_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI_100ppm_Inhibitor_database_dictionary.csv')
  )
}

cleaning_10022024_files(user_name)

rm(cleaning_10022024_files)

