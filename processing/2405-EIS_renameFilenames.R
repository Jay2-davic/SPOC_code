#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

# updates and copies new mpr files into the 
df_PE_combined <- df_PE_combined %>%
  dplyr::mutate(
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr,
                                              '\\\\',
                                              '/'),
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr,
                                              'none.mpr',
                                              'none'),
    # removes the user name and replaces it with a variable string
    EIS_mpr =  dplyr::case_when(
      stringr::str_detect(EIS_mpr, 'Users') == FALSE ~   stringi::stri_replace_all_regex(EIS_mpr,
                                                                                         '[/]General|General',
                                                                                         glue::glue('C:/Users/{user_name}/OneDrive - LLNL/General')),
      TRUE ~ EIS_mpr),
    EIS_mpr = stringi::stri_replace_all_regex(
      EIS_mpr,
      '[.][.]',
      '.'
    )
  ) %>%
  # combines with the full data
  dplyr::rename(orig_filename = EIS_mpr) %>%
  dplyr::full_join(EIS_files) %>%
  dplyr::rename(legacy_mpr = orig_filename,
                mpr_files = new_filename)

# extracts files and copies into new folder 
purrr::map2(
  df_PE_combined$legacy_mpr, 
  df_PE_combined$mpr_files,
  .f = file.copy
)

rm(EIS_files, EIS_df)