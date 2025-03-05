#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Digital Twins (SI)
#Subproject: Polymer Electrolyte

# loads relevant packages

require(dplyr)
require(tidyr)
require(tibble)
require(readr)
require(purrr)
require(furrr)
require(stringr)
require(stringi)
require(tools)

future::multicore(workers = 3)

# loads data dictionary from sorted files
data_dictionary <-
  suppressMessages({
    tibble::tibble(
      csv = list.files(
        glue::glue(
          'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary'
        ),
        pattern = '_database_dictionary',
        recursive = TRUE,
        full.names = TRUE
      )
    ) |>
      #dplyr::mutate(result = furrr::future_map(csv, readr::read_csv))
      # about 4x faster
      dplyr::mutate(result = furrr::future_map(csv, data.table::fread))
  })

EIS_cleanNames <-
  # extracts the data_dictionary into a dataframe
  furrr::future_map_dfr(data_dictionary$result, purrr::pluck) |>
  # edits naming oversights
  dplyr::mutate(new_filename =
                  stringi::stri_replace_all_regex(
                    new_filename,
                    glue::glue(
                      'C:/Users/{user_name}/OneDrive - LLNL/General - High-Throughput Polymer Electrolytes DIW/Katherine Updates/06292023_9pegmea 1pegda 15 wt aerosil 90/'
                    ),
                    ''
                  )) |>
  # adds extension to a file missing its extension
  dplyr::mutate(
    new_filename =
      stringi::stri_replace_all_regex(
        new_filename,
        '05162023_9_PEGMEA_1_PEGDA_20_Cab-O-Sil_0_none',
        '05162023_9_PEGMEA_1_PEGDA_20_Cab-O-Sil_0_none.mgr'
      )
  ) |>
  # extracts the file path and extension
  dplyr::mutate(mpr_names =
                  stringr::str_extract(new_filename, pattern = '[^\\\\]+$')) |>
  # labels files based on file extensions
  dplyr::mutate(file_ext =
                  stringr::str_extract(new_filename, pattern = '[^.]+$')) |>
  # extracts the base name without extension
  dplyr::mutate(
    base_name =
      tools::file_path_sans_ext(mpr_names),
    name =
      stringr::str_remove(base_name, '_C0.*'),
    base_name = stringr::str_remove(base_name, 'mpr_new_names/'),
    dplyr::across(dplyr::everything(~ stringr::str_replace(., '/', '\\\\')))
  ) |>
  # uses the last column made to extract metadata information
  # tidyr::separate(
  #   name,
  #   sep = '_',
  #   into = c(
  #     'date_printed',
  #     'material1Rat',
  #     'material1',
  #     'material2Rat',
  #     'material2',
  #     'additiveWtPct',
#     'additive',
#     'saltWtPct',
#     'salt'
#   )
# ) |>
# TODO LATER include the other extensions
dplyr::filter(file_ext == 'mpr') |>
  dplyr::select(-mpr_names)
# modifies the mpr file names to the actual sample for merging
# dplyr::mutate(mpr_names =
#                 stringi::stri_replace_all_regex(mpr_names,
#                                                 '.mpr',
#                                                 ''),
# mpr_names =
#   stringr::str_remove(mpr_names, '_RUN_.*')
#)
# dplyr::mutate(additive = dplyr::if_else(additiveWtPct == '0',
#                                         'none',
#                                         additive)) |>
# dplyr::mutate(salt = dplyr::if_else(saltWtPct == '0',
#                                     'none',
#                                     salt)) |>
# # capitalizes all polymer material names
# dplyr::mutate(material1 =
#                 toupper(material1),
#               material2 =
#                 toupper(material2),
#               additiveWtPct =
#                 stringi::stri_replace_all_regex(
#                   additiveWtPct,
#                   ',',
#                   '.'
#                 ),
#               saltWtPct =
#                 stringi::stri_replace_all_regex(
#                   saltWtPct,
#                   ',',
#                   '.'
#                 ))

#updates the database dictionary into a csv file
readr::write_csv(
  EIS_cleanNames,
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\database_dictionary.csv'
  )
)

rm(data_dictionary)

###

# loads the folder where the processed EIS files are located
eis_filepath <-
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_files_converted'
  )

# loads the EIS files into a dataframe
# eis_results_df <-
#   tibble::tibble(eis_files =
#                    list.files(eis_filepath,
#                               pattern = '.csv',
#                               full.names = TRUE))
#
# eis_results_df1 <- eis_results_df |>
#   # removes the parent path
#   dplyr::mutate(mpr_names =
#                   stringr::str_extract(eis_files,
#                                        pattern = '[^/]+$')) |>
#   # removes the extension
#   dplyr::mutate(mpr_names =
#                   tools::file_path_sans_ext(mpr_names)) |>
#   # edits the 2 misnamed files to convention
#   dplyr::mutate(mpr_names =
#                   stringi::stri_replace_all_regex(mpr_names,
#                                                   'none1',
#                                                   'none_RUN_1')) |>
#   dplyr::mutate(mpr_names =
#                   stringi::stri_replace_all_regex(mpr_names,
#                                                   'none2',
#                                                   'none_RUN_2')) |>
#   # extracts metadata replicates from name into dataframe
#   tidyr::separate(
#     mpr_names,
#     sep = "_RUN_",
#     into = c(
#       'mpr_names',
#       'run'
#     )
#   ) |>
#   # extracts metadata formulation into dataframe
#   dplyr::mutate(mpr_split = mpr_names) |>
#   tidyr::separate(
#     mpr_split,
#     sep = '_',
#     into = c(
#       'date_printed',
#       'material1Rat',
#       'material1',
#       'material2Rat',
#       'material2',
#       'additiveWtPct',
#       'additive',
#       'saltWtPct',
#       'salt'
#     )
#   ) |>
#   dplyr::mutate(saltWtPct = stringi::stri_replace_all_regex(saltWtPct,
#                                                             ',',
#                                                             '.')) |>
#   dplyr::mutate(additiveWtPct = stringi::stri_replace_all_regex(additiveWtPct,
#                                                                 ',',
#                                                                 '.')) |>
#   # relabels additive as none is 0 wt% (adds to baseline training)
#   dplyr::mutate(additive = dplyr::if_else(additiveWtPct == '0',
#                                           'none',
#                                           additive)) |>
#   dplyr::mutate(salt = dplyr::if_else(saltWtPct == '0',
#                                       'none',
#                                       salt)) |>
#   dplyr::mutate(mpr_names = stringr::str_remove(mpr_names,
#                                                 '_\\d+$')) |>
#   # capitalizes all polymer material names
#   dplyr::mutate(material1 =
#                   toupper(material1),
#                 material2 =
#                   toupper(material2)) |>
#   dplyr::select(-run)
####

eis_results_file <-
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_results_from_Jay_2'
  )

eis_results_df2 <-
  tibble::tibble(filepath =
                   list.files(eis_results_file,
                              pattern = 'csv|png',
                              full.names = TRUE)) |>
  dplyr::mutate(file_path =
                  stringi::stri_replace_all_regex(filepath,
                                                  pattern = '_randles',
                                                  '')) |>
  dplyr::mutate(file_ext =
                  stringr::str_extract(file_path,
                                       pattern = '[^.]+$')) |>
  dplyr::mutate(mpr_names =
                  stringr::str_extract(filepath,
                                       pattern = '[^/]+$')) |>
  dplyr::mutate(
    base_name =
      stringr::str_remove(mpr_names,
                          pattern = '_randles.*|__randles.*|_Bode.*|_Lin.*|_.png*')
  ) |>
  dplyr::mutate(
    mpr_names =
      stringi::stri_replace_all_regex(mpr_names,
                                      '_.csv',
                                      '.csv'),
    mpr_names =
      stringi::stri_replace_all_regex(mpr_names,
                                      '__',
                                      '_'),
    mpr_names =
      stringr::str_remove(mpr_names,
                          '_randles.*|_Bode.*|_Lin.*|_.png*')
  ) |>
  dplyr::group_by(mpr_names) |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::ungroup() |>
  # dplyr::mutate(base_name =
  #                 tools::file_path_sans_ext(mpr_names),
  #               # labels each data by their file
  #               file_type =
  #                 dplyr::case_when(
  #                   stringr::str_detect(filepath, 'Bode') == TRUE & stringr::str_detect(file_ext, 'png') == TRUE ~ 'bode',
  #                   stringr::str_detect(filepath, 'Lin') == TRUE & stringr::str_detect(file_ext, 'png') == TRUE ~ 'Lin',
  #                   stringr::str_detect(filepath, 'randles') == TRUE & stringr::str_detect(file_ext, 'csv') == TRUE ~ 'csv_file',
  #                   stringr::str_detect(filepath, 'randles') == TRUE & stringr::str_detect(file_ext, 'png') == TRUE & stringr::str_detect(filepath, 'Lin|Bode') != TRUE ~ 'bode',
  #                 )) |>
  dplyr::select(-c(file_path, file_ext)) |>
  tidyr::pivot_wider(names_from = row, values_from = c(filepath)) |>
  dplyr::rename(
    csv_file = '1',
    bode_file = '2',
    linKK_file = '3',
    nyquist_file = '4'
  ) |>
  # fixes random file extension errors
  dplyr::mutate(csv_file =
                  stringi::stri_replace_all_regex(
                    csv_file,
                    '.png',
                    '.csv'
                  ),
                csv_file = 
                  stringi::stri_replace_all_regex(
                    csv_file,
                    'BodePlot',
                    ''
                  )) 

# read and convert numerical value to double

# read_and_convert <- function(csv_file) {
#   df <- readr::read_csv(csv_file,
#                         progress = TRUE,
#                         show_col_types = FALSE) |>
#     dplyr::mutate(
#       dplyr::across(
#         dplyr::contains('stDev'),
#         as.numeric
#       )
#     )
# }
#
read_and_convert <- function(csv_file) {
  df <-
    data.table::fread(csv_file, fill = TRUE, check.names = TRUE) |>
    dplyr::mutate(dplyr::across(dplyr::contains('stDev'),
                                as.numeric))
}


# tictoc::tic()
#
# results <- eis_results_df3 |>
#   dplyr::slice(20) |>
#   dplyr::filter(is.na(eis_mpr) != TRUE) |>
#   dplyr::mutate(
#     furrr::future_map_dfr(
#       #furrr::future_map(
#       eis_mpr,
#       suppressMessages(
#         purrr::possibly(read_and_convert,
#                         otherwise = NA,
#                         quiet = FALSE))
#     ),
#     run = as.double(run)
#   )
#
# tictoc::toc()


tictoc::tic()

results <- eis_results_df2 |>
  dplyr::filter(is.na(csv_file) != TRUE) |>
  dplyr::mutate(
    furrr::future_map_dfr(#furrr::future_map(
      csv_file,
      read_and_convert),
    base_name = stringr::str_replace(mpr_names, '_C01.*', '_C01'),
    base_name = stringr::str_remove(base_name, 'mpr_new_names/'),
    base_name = dplyr::case_when(
      stringr::str_detect(base_name, 'Cab') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '03292024') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '07052023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '11162023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '08172023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '08162023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '08222023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '09082023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '09262023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(base_name, '10252023') == TRUE ~ stringr::str_remove(base_name, '_RUN.*'),
      stringr::str_detect(
        base_name,
        '11172023|06062023|04152024|05232023|06022023|09072023_9_PEGMEA_1_PEGDA_12,86_Aerosil380_30_LiTFSI_RUN_G1_RUN'
      ) == TRUE ~ stringr::str_sub(base_name, end = -7),
      TRUE ~ base_name
    )
  ) |>
  dplyr::select(-mpr_names) |>
  dplyr::rename(mpr_files_cycle = EIS_file) |>
  dplyr::distinct(csv_file, .keep_all = TRUE)
#change such that it removes only the second RUN

tictoc::toc()


# results <- eis_results_df3 |>
#   dplyr::filter(is.na(eis_mpr) != TRUE) |>
#   dplyr::mutate(
#     result = furrr::future_map(
#     #furrr::future_map(
#       eis_mpr,
#       suppressMessages(
#         purrr::possibly(readr::read_csv,
#                       otherwise = NA,
#                       quiet = FALSE)),
#       .progress = TRUE,
#       show_col_types = FALSE
#     ),
#     run = as.double(run)
#   )

eis_full <-
  dplyr::full_join(#eis_results_df1,
    EIS_cleanNames,
    results,
    by = 'base_name') |>
  # dplyr::full_join(results) |>
  dplyr::select(-c(file_ext,
                   name,
                   base_name)) |>
  dplyr::mutate(
    EIS_mpr =
      stringi::stri_replace_all_regex(
        orig_filename,
        glue::glue('C:/Users/{user_name}/OneDrive - LLNL'),
        ''
      ),
    dplyr::across(dplyr::everything(),
      ~ stringi::stri_replace_all_regex(.x,
                                 '[\\\\]',
                                 '/')
    )
    ,
    dplyr::across(
      !dplyr::matches('file|mpr'),
      ~ as.numeric(.x)
    ),
    mpr_files_cycle =
      stringr::str_remove(csv_file, '__randles')
  ) |>
  # # correction for 10042024 samples
  # dplyr::mutate(orig_filename = 
  #                 dplyr::case_when(
  #                   stringr::str_detect(orig_filename, '20241004') == TRUE ~ stringi::stri_replace_all_regex(orig_filename, '/2024_\\d+_\\d+_\\d+_\\d+_\\d+_[A-Z]\\d+_RUN\\d+/', '/')
  #                 )) |>
  dplyr::distinct()

rm(
  eis_mpr,
  eis_png,
  eis_results_df,
  eis_results_df1,
  eis_results_df2,
  EIS_cleanNames,
  eis_results_df3,
  results,
  eis_filepath,
  eis_results_file
)

### task join all three dataframes
