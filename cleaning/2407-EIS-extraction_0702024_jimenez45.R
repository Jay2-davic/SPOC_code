#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 05282024

cleaning_07052024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\20240702_9_PEGMEA_1_PEGDA_0to15_Aerosil380_10_LiTFSI_100ppm_Inhibitor')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files |>
    dplyr::mutate(
      new_filename = stringr::str_replace(orig_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_([A-Z])(\\d+)_', ''),
      new_filename = stringi::stri_replace_all_regex(new_filename, '2024_', '2024_9_PEGMEA_1_PEGDA_'),
      new_filename = stringr::str_replace(new_filename, 'RUN', 'RUN_'),
      # calculate the wt percent based on the varied component
      wt_salt_max_1 = 0,
      wt_salt_max_2 = 20,
      wt_additive_max_1 = 10,
      wt_additive_max_2 = 10,
      wt_pct_vary_salt = dplyr::case_when(
        stringr::str_detect(new_filename, '_A(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 1, 3),
        stringr::str_detect(new_filename, '_B(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.857 + wt_salt_max_2 * 0.143, 3),
        stringr::str_detect(new_filename, '_C(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.714 + wt_salt_max_2 * 0.286, 3),
        stringr::str_detect(new_filename, '_D(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.571 + wt_salt_max_2 * 0.429, 3),
        stringr::str_detect(new_filename, '_E(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.429 + wt_salt_max_2 * 0.571, 3),
        stringr::str_detect(new_filename, '_F(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.286 + wt_salt_max_2 * 0.714, 3),
        stringr::str_detect(new_filename, '_G(\\d+)_') == TRUE ~ signif(wt_salt_max_1 * 0.143 + wt_salt_max_2 * 0.857, 3),
        stringr::str_detect(new_filename, '_H(\\d+)_') == TRUE ~ signif(wt_salt_max_2 * 1, 3)
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
        stringr::str_detect(new_filename, '_A(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_A(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_A\\1_')),
        stringr::str_detect(new_filename, '_B(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_B(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_B\\1_')),
        stringr::str_detect(new_filename, '_C(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_C(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_C\\1_')),
        # different because file DUSB0 exists
        stringr::str_detect(new_filename, '_D(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_D(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_D\\1_')),
        stringr::str_detect(new_filename, '_E(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_E(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_E\\1_')),
        stringr::str_detect(new_filename, '_F(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_F(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_F\\1_')),
        stringr::str_detect(new_filename, '_G(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_G(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_G\\1_')),
        stringr::str_detect(new_filename, '_H(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_H(\\d)_', glue::glue('_{wt_pct_vary_additive}_Aerosil380_{wt_pct_vary_salt}_LiTFSI_H\\1_')),
        TRUE ~ new_filename
      ),
    ) |>
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) |>
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) |>
    dplyr::select(c(orig_filename,
                    new_filename))
  
  # saves the dataframe as a csv file
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\20240702_9_PEGMEA_1_PEGDA_0to15_Aerosil380_10_LiTFSI_100ppm_Inhibitor_database_dictionary.csv)')
  )
  return(new_files)
}

cleaning_07052024_files(user_name)

rm(cleaning_07052024_files)
