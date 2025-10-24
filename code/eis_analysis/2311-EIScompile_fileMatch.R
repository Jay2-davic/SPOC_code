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

plan(multisession, workers = parallel::detectCores() - 1)

# loads data dictionary from sorted files
data_dictionary <-
  suppressMessages({
    tibble::tibble(
      csv = list.files(
        glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr files\\data_dictionary'), 
        pattern = '_database_dictionary',
        recursive = TRUE,
        full.names = TRUE
      )
    ) %>%
      dplyr::mutate(result = purrr::map(csv, readr::read_csv))
  })

EIS_cleanNames <-
  # extracts the data_dictionary into a dataframe
  purrr::map_dfr(data_dictionary$result, purrr::pluck) %>%
  # edits naming oversights
  dplyr::mutate(
    new_filename =
      stringi::stri_replace_all_regex(
        new_filename,
        glue::glue('C:/Users/{user_name}/OneDrive - LLNL/General - High-Throughput Polymer Electrolytes DIW/Katherine Updates/06292023_9pegmea 1pegda 15 wt aerosil 90/'),
        ''
      )
  ) %>%
  # adds extension to a file missing its extension
  dplyr::mutate(
    new_filename =
      stringi::stri_replace_all_regex(
        new_filename,
        '05162023_9_PEGMEA_1_PEGDA_20_Cab-O-Sil_0_none',
        '05162023_9_PEGMEA_1_PEGDA_20_Cab-O-Sil_0_none.mgr'
      )
  ) %>%
  # extracts the file path and extension
  dplyr::mutate(mpr_names =
                  stringr::str_extract(new_filename, pattern = '[^\\\\]+$')) %>%
  # labels files based on file extensions
  dplyr::mutate(file_ext =
                  stringr::str_extract(new_filename, pattern = '[^.]+$')) %>%
  # extracts the base name without extension
  dplyr::mutate(base_name =
                  tools::file_path_sans_ext(mpr_names)) %>%
  # uses the last column made to extract metadata information
  tidyr::separate(
    base_name,
    sep = '_',
    into = c(
      'date_printed',
      'material1Rat',
      'material1',
      'material2Rat',
      'material2',
      'additiveWtPct',
      'additive',
      'saltWtPct',
      'salt'
    )
  ) %>%
  # TODO LATER include the other extensions
  dplyr::filter(file_ext == 'mpr') %>%
  # modifies the mpr file names to the actual sample for merging
  dplyr::mutate(mpr_names =
                  stringi::stri_replace_all_regex(mpr_names,
                                                  '_RUN.*',
                                                  '')) %>%
  dplyr::mutate(additive = dplyr::if_else(additiveWtPct == '0',
                                          'none',
                                          additive)) %>%
  dplyr::mutate(salt = dplyr::if_else(saltWtPct == '0',
                                      'none',
                                      salt)) %>%
  # capitalizes all polymer material names
  dplyr::mutate(material1 =
                  toupper(material1),
                material2 = 
                  toupper(material2),
                additiveWtPct =
                  stringi::stri_replace_all_regex(
                    additiveWtPct,
                    ',',
                    '.'
                  ),
                saltWtPct = 
                  stringi::stri_replace_all_regex(
                    saltWtPct,
                    ',',
                    '.'
                  ))

#updates the database dictionary into a csv file
readr::write_csv(
  EIS_cleanNames,
  glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\database_dictionary.csv')
)

rm(data_dictionary)

###

# loads the folder where the processed EIS files are located
eis_filepath <-
  glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\mpr_new_files_converted')

# loads the EIS files into a dataframe
eis_results_df <-
  tibble::tibble(eis_files =
                   list.files(eis_filepath,
                              pattern = '.csv',
                              full.names = TRUE))

eis_results_df1 <- eis_results_df %>%
  # removes the parent path
  dplyr::mutate(mpr_names =
                  stringr::str_extract(eis_files,
                                       pattern = '[^/]+$')) %>%
  # removes the extension
  dplyr::mutate(mpr_names =
                  tools::file_path_sans_ext(mpr_names)) %>%
  # edits the 2 misnamed files to convention
  dplyr::mutate(mpr_names =
                  stringi::stri_replace_all_regex(mpr_names,
                                                  'none1',
                                                  'none_RUN_1')) %>%
  dplyr::mutate(mpr_names =
                  stringi::stri_replace_all_regex(mpr_names,
                                                  'none2',
                                                  'none_RUN_2')) %>%
  # extracts metadata replicates from name into dataframe
  tidyr::separate(
    mpr_names,
    sep = "_RUN_",
    into = c(
      'mpr_names',
      'run'
    )
  ) |>
  dplyr::mutate(
    run = stringr::str_replace_all(run, '_C01|C01', '')
  ) %>%
  # extracts metadata formulation into dataframe
  dplyr::mutate(mpr_split = mpr_names) %>%
  tidyr::separate(
    mpr_split,
    sep = '_',
    into = c(
      'date_printed',
      'material1Rat',
      'material1',
      'material2Rat',
      'material2',
      'additiveWtPct',
      'additive',
      'saltWtPct',
      'salt'
    )
  ) %>%
  dplyr::mutate(saltWtPct = stringi::stri_replace_all_regex(saltWtPct,
                                                            ',',
                                                            '.')) %>%
  dplyr::mutate(additiveWtPct = stringi::stri_replace_all_regex(additiveWtPct,
                                                                ',',
                                                                '.')) %>%
  # relabels additive as none is 0 wt% (adds to baseline training)
  dplyr::mutate(additive = dplyr::if_else(additiveWtPct == '0',
                                          'none',
                                          additive)) %>%
  dplyr::mutate(salt = dplyr::if_else(saltWtPct == '0',
                                      'none',
                                      salt)) %>%
  dplyr::mutate(mpr_names = stringr::str_remove(mpr_names,
                                                '_\\d+$')) %>%
  # capitalizes all polymer material names
  dplyr::mutate(material1 =
                  toupper(material1),
                material2 = 
                  toupper(material2)) %>%
  dplyr::mutate(run = as.double(
    dplyr::if_else(
      run == 'C01',
      '1',
      run
    )))

####

eis_results_file <-
  glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\eis_results')

eis_results_df2 <-
  tibble::tibble(filepath =
                   list.files(eis_results_file,
                              pattern = 'csv|png',
                              full.names = TRUE)) %>%
  dplyr::mutate(
    file_path =
      stringi::stri_replace_all_regex(
        filepath,
        pattern = '_randles',
        ''
      )
  ) %>%
  dplyr::mutate(file_ext =
                  stringr::str_extract(file_path,
                                       pattern = '[^.]+$')) %>%
  dplyr::mutate(mpr_names =
                  stringr::str_extract(filepath,
                                       pattern = '[^/]+$')) %>%
  dplyr::mutate(mpr_names =
                  stringi::stri_replace_all_regex(
                    mpr_names,
                    '_.csv',
                    '.csv'
                  )) %>%
  dplyr::mutate(base_name =
                  tools::file_path_sans_ext(mpr_names)) %>%
  dplyr::select(-file_path)

eis_png <- eis_results_df2 %>%
  dplyr::filter(
    file_ext == 'png'
  ) %>% 
  dplyr::rename(
    eis_png = filepath
  ) %>%
  dplyr::select(
    -c(file_ext,
       mpr_names)
  ) %>%
  dplyr::mutate(
    base_name =
      stringi::stri_replace_all_regex(
        base_name,
        '_randles',
        ''
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'RUN_2_',
        'RUN_2'
      )
  )

eis_mpr <- eis_results_df2 %>%
  dplyr::filter(
    file_ext == 'csv'
  ) %>%
  dplyr::rename(
    eis_mpr = filepath
  ) %>%
  dplyr::select(
    -c(file_ext,
       mpr_names)
  ) %>%
  dplyr::mutate(
    base_name =
      stringi::stri_replace_all_regex(
        base_name,
        '_randles',
        ''
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'RUN_2_',
        'RUN_2'
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'RUN_1_',
        'RUN_1'
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'RUN_3_',
        'RUN_3'
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'none1',
        'none_RUN_1'
      ),
    base_name = 
      stringi::stri_replace_all_regex(
        base_name,
        'none2',
        'none_RUN_2'
      )
  )

eis_results_df3 <- 
  dplyr::full_join(
    eis_mpr,
    eis_png
  ) %>%
  tidyr::separate(
    base_name,
    sep = '_RUN_',
    into = c(
      'mpr_names',
      'run'
    )
  ) |>
  dplyr::mutate(
    run = stringr::str_replace_all(run, '_C01|C01', '')
  )

results <- eis_results_df3 %>%
  dplyr::filter(!is.na(eis_mpr)) %>%
  dplyr::mutate(
    data = furrr::future_map(
      eis_mpr,
      ~ {
        tryCatch({
          df <- data.table::fread(.x, 
                                  data.table = FALSE,
                                  fill = TRUE,
                                  blank.lines.skip = TRUE,
                                  strip.white = TRUE)
          
          df$source_file <- basename(.x)
          df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
          
          return(df)
        }, error = function(e) {
          # Return NULL instead of error tibble for failed reads
          return(NULL)
        })
      },
      .options = furrr_options(seed = TRUE, packages = c("data.table", "dplyr", "tibble")),
      .progress = TRUE
    ),
    run = suppressWarnings(as.double(run))
  ) %>%
  # Filter out NULL/failed results before unnesting
  dplyr::filter(!purrr::map_lgl(data, is.null)) %>%
  tidyr::unnest(data) |>
  type.convert(as.is = FALSE, na.strings = c("", "NA", "NULL"))


eis_full <-
  dplyr::full_join(eis_results_df1,
                   EIS_cleanNames) %>%
  dplyr::full_join(results) %>%
  dplyr::select(
    -c(
      file_ext,
      EIS_file,
      date_printed,
      mpr_names,
    )
  ) %>%
  # dplyr::mutate(EIS_mpr =
  #                 stringi::stri_replace_all_regex(
  #                   orig_filename,
  #                   glue::glue('C:/Users/{user_name}/OneDrive - LLNL'),
  #                   ''
  #                 ),
  #               EIS_mpr = 
  #                 stringi::stri_replace_all_regex(
  #                   EIS_mpr,
  #                   '\\\\',
  #                   '/'
  #                 )) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.character), ~ stringi::stri_replace_all_regex(.x, '\\\\', '/')
    ),
    eis_files = as.character(eis_files),
    new_filename = as.character(new_filename),
    orig_filename = as.character(orig_filename)
  ) |>
  dplyr::rename(
    mpr_files_cycle = eis_files
  ) |>
  dplyr::distinct(eis_mpr, .keep_all = TRUE)

rm(eis_mpr, eis_png, eis_results_df,
   eis_results_df1, eis_results_df2, EIS_cleanNames,
   eis_results_df3, results, eis_filepath,
   eis_results_file)

### task join all three dataframes
