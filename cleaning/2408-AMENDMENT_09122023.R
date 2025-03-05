files <- r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_names)'
files2 <- r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_files_converted)'
files3 <- r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_results_from_Jay_2)'


change_name <- tibble::tibble(
  orig_path = list.files(files,
                         full.names = TRUE)
) |>
  # dplyr::filter(stringr::str_detect(orig_path,
  #                                   '09122023_9_pegmea_1_pegda')) |>
  # dplyr::mutate(
  #   new_path = stringi::stri_replace_all_regex(orig_path,
  #                                              'pegmea_1_pegda',
  #                                              'PEGMEA_1_PEGDA')
  # )
  dplyr::filter(stringr::str_detect(orig_path,
                                    '07252023|09072023|08162023|08172023|07222023')) |>
  dplyr::mutate(
    new_path = stringi::stri_replace_all_regex(orig_path,
                                               'pegmea_1_pegda',
                                               'PEGMEA_1_PEGDA'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '0wt',
                                               '0_wt'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               'wt_tio2',
                                               '_TiO2'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '__',
                                               '_'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '_randles',
                                               '__randles')
  )

change_name2 <- tibble::tibble(
  orig_path = list.files(files2,
                         full.names = TRUE)
) |>
  # dplyr::filter(stringr::str_detect(orig_path,
  #                                   '09122023')) |>
  # dplyr::mutate(
  #   new_path = stringi::stri_replace_all_regex(orig_path,
  #                                              'pegmea_1_pegda|PEGMEA_1_PEGDA',
  #                                              'PEGMEA_1_PEGDA')
  # )
  dplyr::filter(stringr::str_detect(orig_path,
                                    '07252023|09072023|08162023|08172023|07222023')) |>
  dplyr::mutate(
    new_path = stringi::stri_replace_all_regex(orig_path,
                                               'pegmea_1_pegda',
                                               'PEGMEA_1_PEGDA'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '0wt',
                                               '0_wt'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               'wt_tio2',
                                               '_TiO2'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '__',
                                               '_'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '_randles',
                                               '__randles')
  )

change_name3 <- tibble::tibble(
  orig_path = list.files(files3,
                         full.names = TRUE)
) |>
  # dplyr::filter(stringr::str_detect(orig_path,
  #                                   '09122023')) |>
  # dplyr::mutate(
  #   new_path = stringi::stri_replace_all_regex(orig_path,
  #                                              'pegmea_1_pegda|PEGMEA_1_PEGDA',
  #                                              'PEGMEA_1_PEGDA')
  # )
  dplyr::filter(stringr::str_detect(orig_path,
                                    '07252023|09072023|08162023|08172023|07222023')) |>
  dplyr::mutate(
    new_path = stringi::stri_replace_all_regex(orig_path,
                                               'pegmea_1_pegda',
                                               'PEGMEA_1_PEGDA'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '0wt',
                                               '0_wt'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               'wt_tio2',
                                               '_TiO2'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '__',
                                               '_'),
    new_path = stringi::stri_replace_all_regex(new_path,
                                               '_randles',
                                               '__randles')
  )


change_all <- dplyr::bind_rows(change_name,
                               change_name2,
                               change_name3)

file.rename(change_all$orig_path, change_all$new_path)
