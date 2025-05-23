#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

### function for extracting EIS-related files in a folder into a dataframe

EIS_finder <- function(file_path){
  # loads tidyverse package
  require(tidyverse)
  
  # saves listed files as a dataframe
  df <- 
    tibble::tibble(
      orig_filename =
        list.files(file_path,
                   # these are the file outputs from BioLogic
                   pattern = 'mpr|mps|sta|mgr',
                   # full paths for easy querying
                   full.names = TRUE,
                   # just in case there are extra folders inside where more files
                   # exist
                   recursive = TRUE))
  return(df)
}

EIS_cleaner <- function(folder_struct,
                        new_folder_struct_main,
                        salt,
                        additive,
                        wt_salt_max_1,
                        wt_salt_max_2,
                        wt_additive_max_1,
                        wt_additive_max_2) {
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files |>
    dplyr::mutate(
      new_filename = stringr::str_replace(orig_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_([a-zA-Z])(\\d+)|RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)', ''),
      new_filename = stringr::str_replace(new_filename, '2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)', '2024'),
      new_filename = stringr::str_replace(new_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)', 'RUN\\1'),
      new_filename = stringr::str_replace(new_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)_', 'RUN\\1'),
      #new_filename = stringr::str_replace(new_filename, '(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)', ''),
      new_filename = stringr::str_replace(new_filename, '__', '_'),
      #new_filename = stringr::str_replace(new_filename, '08012024', '08022024'),
      new_filename = stringi::stri_replace_all_regex(new_filename, '2024_', '2024_9_PEGMEA_1_PEGDA_'),
      new_filename = stringr::str_replace(new_filename, 'RUN', 'RUN_'),
      # calculate the wt percent based on the varied component
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
        stringr::str_detect(new_filename, '_A(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_A(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_A\\1_')),
        stringr::str_detect(new_filename, '_B(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_B(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_B\\1_')),
        stringr::str_detect(new_filename, '_C(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_C(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_C\\1_')),
        # different because file DUSB0 exists
        stringr::str_detect(new_filename, '_D(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_D(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_D\\1_')),
        stringr::str_detect(new_filename, '_E(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_E(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_E\\1_')),
        stringr::str_detect(new_filename, '_F(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_F(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_F\\1_')),
        stringr::str_detect(new_filename, '_G(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_G(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_G\\1_')),
        stringr::str_detect(new_filename, '_H(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_H(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_H\\1_')),
        TRUE ~ new_filename
      ),
    ) |>
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) |>
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename)) |>
    dplyr::select(c(orig_filename,
                    new_filename,
                    ))
  
  return(new_files)
}

EIS_cleaner_thiolene <- function(folder_struct,
                        new_folder_struct_main,
                        salt,
                        additive,
                        wt_salt_max_1,
                        wt_salt_max_2,
                        wt_additive_max_1,
                        wt_additive_max_2) {
  orig_files <- EIS_finder(folder_struct)
  # formats the names
  new_files <- orig_files |>
    dplyr::mutate(
      new_filename = stringr::str_replace(orig_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_([a-zA-Z])(\\d+)|RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)', ''),
      new_filename = stringr::str_replace(new_filename, '2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)', '2024'),
      new_filename = stringr::str_replace(new_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)', 'RUN\\1'),
      new_filename = stringr::str_replace(new_filename, 'RUN(\\d+)/2024_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)_pin(\\d+)_column(\\d+)_row(\\d+)_', 'RUN\\1'),
      #new_filename = stringr::str_replace(new_filename, '(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)', ''),
      new_filename = stringr::str_replace(new_filename, '__', '_'),
      #new_filename = stringr::str_replace(new_filename, '08012024', '08022024'),
      new_filename = stringr::str_replace(new_filename, 'RUN', 'RUN_'),
      # calculate the wt percent based on the varied component
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
        stringr::str_detect(new_filename, '_A(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_A(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_A\\1_')),
        stringr::str_detect(new_filename, '_B(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_B(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_B\\1_')),
        stringr::str_detect(new_filename, '_C(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_C(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_C\\1_')),
        # different because file DUSB0 exists
        stringr::str_detect(new_filename, '_D(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_D(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_D\\1_')),
        stringr::str_detect(new_filename, '_E(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_E(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_E\\1_')),
        stringr::str_detect(new_filename, '_F(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_F(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_F\\1_')),
        stringr::str_detect(new_filename, '_G(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_G(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_G\\1_')),
        stringr::str_detect(new_filename, '_H(\\d)_') == TRUE ~ stringr::str_replace(new_filename, '_H(\\d)_', glue::glue('_{wt_pct_vary_additive}_{additive}_{wt_pct_vary_salt}_{salt}_H\\1_')),
        TRUE ~ new_filename
      ),
    ) |>
    # missing formulation information for each cell
    dplyr::mutate(new_filename = stringr::str_extract(new_filename, pattern = '[^/]+$')) |>
    dplyr::mutate(new_filename = paste0(new_folder_struct_main, '\\', new_filename),
                  new_filename = stringi::stri_replace_all_regex(new_filename, '2024_', '2024_1,4_PEGMEA_1_Thiolene_6,5_PAL_'),
    ) |>
    dplyr::select(c(orig_filename,
                    new_filename,
    ))
  
  return(new_files)
}
