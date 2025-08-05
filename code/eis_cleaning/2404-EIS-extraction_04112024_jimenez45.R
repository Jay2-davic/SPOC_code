#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### Cleaning EIS file on 04112024

cleaning_04112024_files <- function(user_name) {   
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
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\04112024_Board101_10wtNaTFSI_vary_0to10AE380')
  # extracts the file paths of EIS files
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files %>%
    dplyr::mutate(
      new_filename = stringi::stri_replace_all_regex(
        orig_filename,
        '/11April2024|/04112024',
        '/04112024_9_PEGMEA_1_PEGDA'
      ),
      # calculate the wt percent based on the varied component
      wt_pct_vary = dplyr::case_when(
        stringr::str_detect(new_filename, '_A3_') == TRUE ~ signif(10 * 1, 3),
        stringr::str_detect(new_filename, '_B3_') == TRUE ~ signif(10 * 0.857, 3),
        stringr::str_detect(new_filename, '_C3_') == TRUE ~ signif(10 * 0.714, 3),
        stringr::str_detect(new_filename, '_D3_') == TRUE ~ signif(10 * 0.571, 3),
        stringr::str_detect(new_filename, '_E3_') == TRUE ~ signif(10 * 0.429, 3),
        stringr::str_detect(new_filename, '_F3_') == TRUE ~ signif(10 * 0.286, 3),
        stringr::str_detect(new_filename, '_G3_') == TRUE ~ signif(10 * 0.143, 3),
        stringr::str_detect(new_filename, '_H3_') == TRUE ~ signif(10 * 0, 3)
      ),
      # change decimals to commas
      wt_pct_vary = format(wt_pct_vary, decimal.mark = ','),
      # removes stray space
      wt_pct_vary = stringi::stri_replace_all_regex(wt_pct_vary, ' ', ''),
      # adds salt and additive wt pcts
      new_filename = dplyr::case_when(
        stringr::str_detect(new_filename, '_A3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_A3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_A3')),
        stringr::str_detect(new_filename, '_B3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_B3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_B3')),
        stringr::str_detect(new_filename, '_C3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_C3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_C3')),
        stringr::str_detect(new_filename, '_D3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_D3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_D3')),
        stringr::str_detect(new_filename, '_E3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_E3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_E3')),
        stringr::str_detect(new_filename, '_F3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_F3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_F3')),
        stringr::str_detect(new_filename, '_G3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_G3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_G3')),
        stringr::str_detect(new_filename, '_H3') == TRUE ~ stringi::stri_replace_all_regex(new_filename, '_H3', glue::glue('_{wt_pct_vary}_Aerosil380_10_NaTFSI_H3')),
        TRUE ~ new_filename
      ),
      new_filename = stringi::stri_replace_all_regex(new_filename, 'RUN', 'RUN_')
    ) %>%
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) %>%
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) %>%
    dplyr::select(c(orig_filename,
                    new_filename))
      
  # saves the dataframe as a csv file
  readr::write_csv(
    new_files,
    glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\04112024_Board101_10wtNaTFSI_vary_0to10AE380_database_dictionary.csv')
  )
  return(new_files)
}

cleaning_04112024_files(user_name)

rm(cleaning_04112024_files)
