#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 04162024

cleaning_04162024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\EIS_Silverman1\\04162024_Board103_20wtNaTFSI_vary_0to10wtAE380')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files |>
    dplyr::mutate(
      new_filename = stringr::str_replace(orig_filename, '/pin(\\d+)_column(\\d+)_row(\\d+)_2024_4_(\\d+)_(\\d+)_(\\d+)_(\\d+)', ''),
      new_filename = stringr::str_replace(new_filename, 'RUN', 'RUN_'),
      # calculate the wt percent based on the varied component
      wt_salt_max_1 = 29,
      wt_salt_max_2 = 25,
      wt_additive_max_1 = 14,
      wt_additive_max_2 = 11,
      wt_pct_vary_salt = dplyr::case_when(
        stringr::str_detect(new_filename, '_A(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 1, 3),
        stringr::str_detect(new_filename, '_B(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.857 + wt_salt_max_2 * 0.143, 3),
        stringr::str_detect(new_filename, '_C(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.714 + wt_salt_max_2 * 0.286, 3),
        stringr::str_detect(new_filename, '_D(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.571 + wt_salt_max_2 * 0.429, 3),
        stringr::str_detect(new_filename, '_E(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.429 + wt_salt_max_2 * 0.571, 3),
        stringr::str_detect(new_filename, '_F(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.286 + wt_salt_max_2 * 0.714, 3),
        stringr::str_detect(new_filename, '_G(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.143 + wt_salt_max_2 * 0.857, 3),
        stringr::str_detect(new_filename, '_H(\\d+)_') == TRUE ~ signif(wt_salt_max_2 * 0, 3)
      ),
      wt_pct_vary_additive = dplyr::case_when(
        stringr::str_detect(new_filename, '_A(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 1, 3),
        stringr::str_detect(new_filename, '_B(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.857 + wt_additive_max_2 * 0.143, 3),
        stringr::str_detect(new_filename, '_C(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.714 + wt_additive_max_2 * 0.286, 3),
        stringr::str_detect(new_filename, '_D(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.571 + wt_additive_max_2 * 0.429, 3),
        stringr::str_detect(new_filename, '_E(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.429 + wt_additive_max_2 * 0.571, 3),
        stringr::str_detect(new_filename, '_F(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.286 + wt_additive_max_2 * 0.714, 3),
        stringr::str_detect(new_filename, '_G(\\d+)_') == TRUE ~ signif(wt_additive_max_1 * 0.143 + wt_additive_max_2 * 0.857, 3),
        stringr::str_detect(new_filename, '_H(\\d+)_') == TRUE ~ signif(wt_additive_max_2 * 1, 3)
      ),
      wt_pct_vary_salt = format(wt_pct_vary_salt, decimal.mark = ','),
      wt_pct_vary_salt = stringi::stri_replace_all_regex(wt_pct_vary_salt, ' ', ''),
      wt_pct_vary_additive = format(wt_pct_vary_additive, decimal.mark = ','),
      wt_pct_vary_additive = stringi::stri_replace_all_regex(wt_pct_vary_additive, ' ', ''),
      # removes stray space
      new_filename = dplyr::case_when(
        stringr::str_detect(new_filename, '_A(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_A(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_A\\1_')),
        stringr::str_detect(new_filename, '_B(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_B(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_B\\1_')),
        stringr::str_detect(new_filename, '_C(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_C(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_C\\1_')),
        # different because file DUSB0 exists
        stringr::str_detect(new_filename, '_D(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_D(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_D\\1_')),
        stringr::str_detect(new_filename, '_E(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_E(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_E\\1_')),
        stringr::str_detect(new_filename, '_F(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_F(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_F\\1_')),
        stringr::str_detect(new_filename, '_G(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_G(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_G\\1_')),
        stringr::str_detect(new_filename, '_H(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_H(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_NaTFSI_H\\1_')),
        TRUE ~ new_filename
      ),
    )  |>
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) |>
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) |>
    dplyr::select(c(orig_filename,
                    new_filename))
  
  # saves the dataframe as a csv file
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\04162024_Board103_20wtNaTFSI_vary_0to10wtAE380_database_dictionary.csv)')
  )
  return(new_files)
}

cleaning_04162024_files(user_name)

rm(cleaning_04162024_files)
