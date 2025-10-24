# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte


# updates the database dictionary

EIS_df <- tibble::tibble(
  filename =
    fs::dir_ls(
      glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary'),
      type = 'file',
      full_path = TRUE
    )
) |>
  # does not include the actual dictionary
  dplyr::filter(stringr::str_detect(filename, '/database_dictionary') == FALSE)

# loads conversion data dictionary
EIS_conv <- 
  readr::read_csv(glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\database_conversion_dictionary.csv')) |>
  dplyr::rename(mpr_files = filepath) |>
  dplyr::rename(mpr_files_cycle = file_conv) |>
  dplyr::mutate(mpr_files = stringi::stri_replace_all_regex(mpr_files,
                                                            '\\\\',
                                                            '/'),
                mpr_files_cycle = stringi::stri_replace_all_regex(mpr_files_cycle,
                                                                  '\\\\',
                                                                  '/'),
                mpr_files_cycle = stringi::stri_replace_all_regex(mpr_files_cycle,
                                                                  '9_pegmea_1_pegda',
                                                                  '9_PEGMEA_1_PEGDA'),
                mpr_files = stringi::stri_replace_all_regex(mpr_files,
                                                            '9_pegmea_1_pegda',
                                                            '9_PEGMEA_1_PEGDA'),
                merge = mpr_files |> basename() |> tools::file_path_sans_ext(),
                dplyr::across(
                  dplyr::everything(),
                  ~ stringi::stri_replace_all_regex(.,
                                                    '\\\\',
                                                    '/'))
  )

# loads processed EIS files
EIS_files <-
  dplyr::mutate(furrr::future_map_dfr(EIS_df$filename,
                                      readr::read_csv)) |>
  dplyr::filter(stringr::str_detect(orig_filename, '[.]mpr|[.]csv') == TRUE) |>
  # omitting Michell's results until cleaned up
  dplyr::filter(stringr::str_detect(orig_filename, 'Michell') == FALSE) |>
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    ~ stringi::stri_replace_all_regex(.,
                                      '\\\\',
                                      '/')
  )) |>
  # text fixes as found
  dplyr::mutate(
    orig_filename =
      stringi::stri_replace_all_regex(
        orig_filename,
        '18,86',
        '12,86'
      ),
    merge = sub(".*/", "", new_filename),
    merge = tools::file_path_sans_ext(merge)
  ) |> 
  dplyr::full_join(EIS_conv) |>
  dplyr::select(-merge) |>
  dplyr::mutate(mpr_files = stringi::stri_replace_all_regex(mpr_files, r'(C:/Users/jimenez45/OneDrive - LLNL)', ''),
                mpr_files = as.character(mpr_files))

# writes a csv file for the database dictionary
readr::write_csv(EIS_files,
                 glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary\\database_dictionary.csv'))