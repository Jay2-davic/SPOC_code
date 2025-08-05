#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 10042024

cleaning_10042024_files <- function(user_name) {
  # loads tidyverse & functions
  require(tidyverse)
  source('~/Git/23-spoc_code/EIS_Cleaner/2310-FUNCTION_EIS_extraction_jimenez45.R')
  # destination folder for converted files
  new_folder_struct_main <-
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_names'
    )
  # location of EIS files
  folder_struct <-
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\20241004_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI_100ppm_Inhibitor'
    )
  # extracts the file paths of EIS files
  cleaned_data <- EIS_cleaner(
    folder_struct,
    new_folder_struct_main,
    salt = 'LiTFSI',
    additive = 'Aerosil380',
    wt_salt_max_1 = 10,
    wt_salt_max_2 = 10,
    wt_additive_max_1 = 15,
    wt_additive_max_2 = 0
  ) |>
    dplyr::mutate(new_filename = stringi::stri_replace_all_regex(new_filename, '2024', '10042024')) |>
    dplyr::mutate(column = stringr::str_extract(new_filename, '(?<=LiTFSI_)([A-Z])(?=1_)')) |>
    # relabels repeat names in order
    dplyr::group_by(new_filename) |>
    dplyr::mutate(
      count = row_number(),
      new_filename = dplyr::case_when(
        count == 1 ~ stringr::str_replace(
          new_filename,
          'LiTFSI_[A-Z]1_',
          glue::glue('LiTFSI_{column}{count}_')
        ),
        count == 2 ~ stringr::str_replace(
          new_filename,
          'LiTFSI_[A-Z]1_',
          glue::glue('LiTFSI_{column}{count}_')
        ),
        count == 3 ~ stringr::str_replace(
          new_filename,
          'LiTFSI_[A-Z]1_',
          glue::glue('LiTFSI_{column}{count}_')
        ),
        count == 4 ~ stringr::str_replace(
          new_filename,
          'LiTFSI_[A-Z]1_',
          glue::glue('LiTFSI_{column}{count}_')
        ),
        count == 5 ~ stringr::str_replace(
          new_filename,
          'LiTFSI_[A-Z]1_',
          glue::glue('LiTFSI_{column}{count}_')
        )
      )
    ) |>
    dplyr::select(-c(column, count))
  
  # saves the dataframe as a csv file
  readr::write_csv(
    cleaned_data,
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\20241004_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI_100ppm_Inhibitor_database_dictionary.csv'
    )
  )
}

cleaning_10042024_files(user_name)

rm(cleaning_10042024_files)
