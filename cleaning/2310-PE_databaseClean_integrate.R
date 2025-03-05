#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

# opens the excel file, sheet 2
{
  require(xlsx)
  require(tidyverse)
  excel_file <-
    xlsx::read.xlsx(
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Michell Updates\\1_DIW\\Screening data Preliminary.xls'
      ),
      sheetIndex = 3
    ) |>
    dplyr::filter(dplyr::row_number() != 1) |>
    dplyr::mutate(sample_ID = paste('MM', row_number(), sep = "_"))
  
  excel_file2 <-
    xlsx::read.xlsx(
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-rivadeneirav1_PEdatabase_v0.xlsx'
      ),
      sheetIndex = 1
    ) |>
    dplyr::mutate(
      SPE_thickness1_mm = dplyr::case_when(
        is.na(SPE_thickness1_mm) == TRUE ~ SPE_thicknessAVG_mm,
        TRUE ~ SPE_thickness1_mm
      )
    ) |>
    # expands replicate measurements as one row instead of multiple columns
    tidyr::pivot_longer(
      cols = dplyr::contains('EIS_mpr'),
      names_to = 'delete',
      values_to = 'EIS_mpr'
    ) |>
    dplyr::select(-c(
      delete,
      resistanceAVG_o,
      SPE_thicknessAVG_mm,
      SPE_thicknessAVG_cm
    )) |>
    dplyr::filter(!is.na(EIS_mpr) | dplyr::row_number() == 1) |>
    tidyr::pivot_longer(
      cols = dplyr::contains('resistance'),
      names_to = 'delete',
      values_to = 'resistance'
    ) |>
    dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
    # removes rows with empty thickness and mpr values (bad samples)
    dplyr::filter(is.na(EIS_mpr) == FALSE &
                    is.na(film_thickness) == FALSE) |>
    dplyr::mutate(film_thickness = film_thickness / 10) |>
    dplyr::distinct()
  
  excel_file3 <-
    xlsx::read.xlsx(
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\Solid State 10232023-rivadeneirav1_PEdatabase_v0.xlsx'
      ),
      sheetIndex = 1
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::contains('EIS_mpr'),
      names_to = 'delete',
      values_to = 'EIS_mpr'
    ) |>
    dplyr::select(-c(delete, resistanceAVG_o)) |>
    #dplyr::filter(!is.na(EIS_mpr) | dplyr::row_number() == 1) |>
    tidyr::pivot_longer(
      cols = dplyr::contains('resistance'),
      names_to = 'delete',
      values_to = 'resistance'
    ) |>
    dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
    # removes rows with empty thickness and mpr values (bad samples)
    dplyr::filter(is.na(film_thickness) == FALSE |
                    print_type == 'Coin cell') |>
    dplyr::mutate(film_thickness = film_thickness / 10) |>
    dplyr::distinct()
  
  excel_file4 <-
    xlsx::read.xlsx(
      glue::glue(
        'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2304-15_PEdatabase_NaTFSI.xlsx'
      ),
      sheetIndex = 1
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::contains('EIS_mpr'),
      names_to = 'delete',
      values_to = 'EIS_mpr'
    ) |>
    dplyr::select(-c(delete, resistanceAVG_o)) |>
    #dplyr::filter(!is.na(EIS_mpr) | dplyr::row_number() == 1) |>
    tidyr::pivot_longer(
      cols = dplyr::contains('resistance'),
      names_to = 'delete',
      values_to = 'resistance'
    ) |>
    dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
    # removes rows with empty thickness and mpr values (bad samples)
    dplyr::filter(is.na(film_thickness) == FALSE |
                    print_type == 'Coin cell') |>
    dplyr::mutate(film_thickness = film_thickness / 10) |>
    dplyr::rename(temperature = 'temperature..F.',
                  humidity = 'humidity....') |>
    dplyr::distinct()
}



###Begin code

polyelectrolyte_DataCleaning <- function(excel_file) {
  require(tidyverse)
  suppressWarnings({
    bulkClean_excel_file <- excel_file |>
      # clean the column names
      janitor::clean_names() |>
      # removing first row
      dplyr::filter(sample_number != is.na(.data)) |>
      # removes empty data based on value of date_printed
      dplyr::filter(date_printed != is.na(.data))
    
    bulkClean_excel_file1 <- bulkClean_excel_file |>
      ### naming convention as follows:
      # polymer1_MW_ratio_polymer2_MW_ratio_electrolyte1_MW_electrolyte2_wt%
      
      # rename columns for less wordy names
      dplyr::rename('material_a' = 'material_a_linear_actuator_1') |>
      dplyr::rename('material_b' = 'material_b_linear_actuator_2') |>
      # removes parenthesis in the values
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(., '\\(|\\)', '')
      )) |>
      # handling individual polymer components (finding common namings)
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(.,
                                          '0.9 PEGMEA 550, ',
                                          'PEGMEA_550_0.9_')
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(.,
                                          '0.1 PEGDA 250',
                                          'PEGDA_250_0.1___')
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(
          .,
          'PAL, Pentaerythrytol tetrakis3mercaptopropionate, PEGDME',
          'PAL__0.5_PEGDME__0.5_' #weights are 50/50, arbitrarily to get final ratios 5/31/23
        )
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(.,
                                          ', \\[EO\\]\\/LiTFSI ',
                                          # formerly 'EO_500_LiTFSI_') 5/31/2023
                                          'EO_1_LiTFSI_')
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(
          .,
          'Diallyl carbonate, ETTMP thiol ',
          'diallylCarbonate__0.5_ETTMPthiol__0.5_' #weights are 50/50, arbitrarily to get final ratios 5/31/23
        )
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringi::stri_replace_all_regex(
          .,
          'Allyl Carbonate, ETTMP',
          'allylCarbonate__0.5_ETTMP__0.5_'
        ) #weights are 50/50, arbitrarily to get final ratios 5/31/23
      )) |>
      # takes the string pattern '## wt% aaa' converts to 'aaa_##'
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringr::str_replace(.,
                               ',(\\d+) wt% (\\w+)',
                               '\\2_\\1')
      )) |>
      # same as above but finds the names with an added space in the beginning
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringr::str_replace(.,
                               ', (\\d+) wt% (\\w+)',
                               '\\2_\\1')
      )) |>
      # renames the chamber feed ratios, removes the 'Comp# ='
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ stringr::str_remove_all(.,
                                  'Comp1 =|Comp2 =|Comp1=|Comp2=')
      )) |>
      # removes spaces and commas from programmed_composition to '_'
      dplyr::mutate(
        programmed_composition =
          stringi::stri_replace_all_regex(programmed_composition,
                                          ',',
                                          '_')
      ) |>
      dplyr::mutate(
        programmed_composition =
          stringi::stri_replace_all_regex(programmed_composition,
                                          ' ',
                                          '')
      ) |>
      dplyr::mutate(
        programmed_composition =
          stringi::stri_replace_all_regex(programmed_composition,
                                          '0.968',
                                          '0.968_')
      ) |>
      # cleans the value names in print_type
      dplyr::mutate(
        print_type =
          stringi::stri_replace_all_regex(print_type,
                                          'Coin Cell |Coin cell |coin cell',
                                          'Coin cell')
      ) |>
      dplyr::mutate(print_type =
                      stringi::stri_replace_all_regex(print_type,
                                                      'PCB print ',
                                                      'PCB print'))
    
    # splits the naming convention and formats all of the columns
    bulkClean_excel_file2 <- bulkClean_excel_file1 |>
      tidyr::separate(
        col = material_a,
        into = c(
          'material1a',
          'material1aMW',
          'material1aRat',
          'material2a',
          'material2aMW',
          'material2aRat',
          'additive_a',
          # formerly 'electrolyte1a',
          'wt_pctAdd_a',
          # formerly 'wt_pct1a',
          'salt2a',
          'wt_pctSalt2a'
        ),
        sep = '_'
      ) |>
      tidyr::separate(
        col = material_b,
        into = c(
          'material1b',
          'material1bMW',
          'material1bRat',
          'material2b',
          'material2bMW',
          'material2bRat',
          'additive_b',
          # formerly 'electrolyte1b',
          'wt_pctAdd_b',
          # formerly 'wt_pct1b',
          'salt2b',
          'wt_pctSalt2b'
        ),
        sep = '_'
      ) |>
      tidyr::separate(
        col = programmed_composition,
        into = c('Comp1_flowRate',
                 'Comp2_flowRate'),
        sep = '_'
      ) |>
      # replaces blank cells with NAs
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  ~ dplyr::na_if(., " "))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  ~ dplyr::na_if(., ""))) |>
      # formatting numerical columns if any columns contain numerical-only strings
      dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
      dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(
        c("^[0-9]+$|[.]"), .
      ))),
      ~ as.double(as.character(.)))) |>
      dplyr::mutate(area_cm2 =
                      area_mm2 * 0.01)
    
    bulkClean_excel_file2 <- bulkClean_excel_file2 |>
      # formatting the date into date-time
      dplyr::mutate(date_printed = lubridate::ymd(date_printed)) |>
      # new column name 5/18/2023
      dplyr::rename(SPE_thickness_mm = thickness_mm,
                    sample_ID = sample_id) |>
      # changes the data type
      dplyr::mutate(ratio_eo_li = as.double(ratio_eo_li))
  })
  
  return(bulkClean_excel_file2)
}

#cleaning the second dataset
poly2_cleaning <- function(excel_file) {
  require(tidyverse)
  df_eis <- excel_file |>
    # saves the EIS_mpr as a separate df (because it messes the rest of the code)
    dplyr::transmute(EIS_mpr = as.character(EIS_mpr))
  df <- excel_file |>
    # converts the dataframe as a tibble
    tibble::as_tibble() |>
    # removes the first two columns
    dplyr::select(-c(1, EIS_mpr)) |>
    # replaces NAs as a string to proper NAs
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ stringi::stri_replace_all_regex(., "NA", NA)
    )) |>
    # dplyr::mutate(dplyr::across(dplyr::everything(),
    #                             ~ dplyr::na_if(., ""))) |>
    # formatting numerical columns if any columns contain numerical-only strings
    dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(
      c("^[0-9]+$|[.]"), .
    ))),
    ~ as.double(as.character(.)))) |>
    # cleaning newer data column names
    dplyr::rename(additive_a = Additive) |>
    dplyr::rename(additive_b = Additive.) |>
    dplyr::rename(wt_pctAdd_a = wt_pctAdd) |>
    dplyr::rename(wt_pctAdd_b = wt_pctAdd.1) |>
    # changes the date into a date-time type
    dplyr::mutate(date_printed = as.Date(date_printed))  |>
    # binds the df_eis row removed earlier
    dplyr::bind_cols(df_eis) |>
    # removes empty data based on value of date_printed
    dplyr::filter(date_printed != is.na(.data)) |>
    # cleans the value names in print_type
    dplyr::mutate(print_type =
                    stringi::stri_replace_all_regex(print_type,
                                                    'coin cell',
                                                    'Coin cell')) |>
    # changes the data type since it is a character
    dplyr::mutate(ratio_eo_li = as.double(ratio_eo_li))
  
}

poly3_cleaning <- function(excel_file_) {
  require(tidyverse)
  df_eis <- excel_file_  |>
    # saves the EIS_mpr as a separate df (because it messes the rest of the code)
    dplyr::transmute(EIS_mpr = as.character(EIS_mpr))
  df <- excel_file_ |>
    # converts the dataframe as a tibble
    tibble::as_tibble() |>
    # removes the first two columns
    dplyr::select(-c(1, EIS_mpr)) |>
    # replaces NAs as a string to proper NAs
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ stringi::stri_replace_all_regex(., "NA", NA)
    )) |>
    # formatting numerical columns if any columns contain numerical-only strings
    dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(
      c("^[0-9]+$|[.]"), .
    ))),
    ~ as.double(as.character(.)))) |>
    # changes the date into a date-time type
    dplyr::mutate(date_printed = as.Date(date_printed))  |>
    # binds the df_eis row removed earlier
    dplyr::bind_cols(df_eis) |>
    # removes empty data based on value of date_printed
    dplyr::filter(date_printed != is.na(.data)) |>
    # cleans the value names in print_type
    dplyr::mutate(print_type =
                    stringi::stri_replace_all_regex(print_type,
                                                    'coin cell',
                                                    'Coin cell')) |>
    # changing the data types
    dplyr::mutate(wt_pctSalt2b = as.double(wt_pctSalt2b)) |>
    dplyr::mutate(ratio_eo_li = as.double(ratio_eo_li))
}

# add cleaning function for area and variable types
PE_addArea_changeVarType <- function(data) {
  require(tidyverse)
  data |>
    # converts area_mm2 to area_cm2
    tibble::as_tibble() |>
    # removes empty data based on value of date_printed
    dplyr::filter(date_printed != is.na(.data)) |>
    # changes new variable into numerics
    dplyr::mutate(
      wt_pct_add_a = as.double(wt_pctAdd_a),
      wt_pct_add_b = as.double(wt_pctAdd_b),
      temperature = as.double(temperature),
      humidity = as.double(humidity)
    )
}

# collects the df and data type changes
PE_collectDF_changeVarType <- function(data) {
  require(tidyverse)
  data |>
    # rbind changes reverts some types back
    #plyr::rbind.fill(df_PE2) |>
    dplyr::mutate(EIS_mpr = as.character(EIS_mpr)) |>
    dplyr::mutate(
      wt_pct_add_a = as.double(wt_pct_add_a),
      wt_pct_add_b = as.double(wt_pct_add_b),
      temperature = as.double(temperature),
      humidity = as.double(humidity)
    )
}

PE_column_cleaning <- function(data) {
  require(tidyverse)
  data |>
    dplyr::mutate(
      Comp1_flowRate =
        dplyr::case_when(
          (
            print_type == 'Coin cell' &
              date_printed < as.Date('2023-10-22')
          ) ~ as.numeric('1'),
          (
            print_type == 'Bulk cast' &
              date_printed < as.Date('2023-10-22') & date_printed > as.Date('2024-01-01')
          ) ~ as.numeric('1'),
          TRUE ~ Comp1_flowRate
        ),
      Comp2_flowRate =
        dplyr::case_when(
          (
            print_type == 'Coin cell' &
              date_printed < as.Date('2023-10-22')
          ) ~ as.numeric('0'),
          (
            print_type == 'Bulk cast' &
              date_printed < as.Date('2023-10-22') & date_printed > as.Date('2024-01-01')
          ) ~ as.numeric('0'),
          TRUE ~ Comp2_flowRate
        ),
      # SPE_thickness1_cm =
      #   SPE_thickness1_mm * 0.1,
      # SPE_thickness2_cm =
      #   SPE_thickness2_mm * 0.1
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character) & !names('sample_ID') ,
      ~ dplyr::if_else(is.na(.),
                       'none',
                       .)
    ))
}

# add final composition information
PE_fcomp_adding <- function(data) {
  require(tidyverse)
  data |>
    # fills in for additive formulations (if one column is missing, then use second column name)
    dplyr::mutate(fcomp_additive =
                    dplyr::if_else(
                      additive_a != 'none',
                      additive_a,
                      dplyr::if_else(additive_b != 'none',
                                     additive_b,
                                     'none')
                    )) |>
    # labels missing values as none
    dplyr::mutate(fcomp_salt =
                    dplyr::if_else(
                      salt2a != 'none',
                      salt2a,
                      dplyr::if_else(salt2b != 'none',
                                     salt2b,
                                     'none')
                    )) |>
    # fills missing numerical values as 0
    dplyr::mutate(dplyr::across(
      c(wt_pctAdd_a,
        wt_pctSalt2a,
        wt_pctAdd_b,
        wt_pctSalt2b),
      ~ dplyr::if_else(is.na(.),
                       0,
                       .)
    )) |>
    # calculates the final additive composition based on piston flow rates
    dplyr::mutate(
      fcomp_additive_wt_pct = (wt_pctAdd_a * Comp1_flowRate + wt_pctAdd_b * Comp2_flowRate),
      digit = 3,
      fcomp_additive_wt_pct = as.double(fcomp_additive_wt_pct)
    ) |>
    # # calculates final composition of the salt based on piston flow rates
    # dplyr::mutate(test1 = wt_pctSalt2a * Comp1_flowRate,
    #               test2 = wt_pctSalt2b * Comp2_flowRate,
    #               test3 = test1 + test2) |>
    dplyr::mutate(
      fcomp_salt_wt_pct = wt_pctSalt2a * Comp1_flowRate + wt_pctSalt2b * Comp2_flowRate,
      digit = 3,
      fcomp_salt_wt_pct = as.double(fcomp_salt_wt_pct)
    ) |>
    # calculates the area in cms
    dplyr::mutate(area_cm2 = signif(dplyr::if_else(is.na(area_cm2),
                                                   0,
                                                   area_cm2)),
                  digit = 3) |>
    # fills in purge_time_s as 240 after april 2023 (SOP implemented)
    dplyr::mutate(purge_time_s =
                    dplyr::if_else(date_printed > '2023-04-01',
                                   240,
                                   purge_time_s)) |>
    # labeling final comp as material1a since they are the same order in the naming conventions
    dplyr::mutate(fcomp_mat1 = material1a) |>
    # making a compositional ratio  based on flow rate and polymer ratio
    dplyr::mutate(fcomp_mat1 = material1a) |>
    dplyr::mutate(fcomp_mat1_ratio = signif(dplyr::if_else(
      fcomp_mat1 == 'none',
      0,
      dplyr::if_else(
        print_type == 'PCB print' | print_type == 'Bulk cast',
        #bulk cast additives set to 1 (since unknown amount used)
        material1aRat * Comp1_flowRate +
          material1bRat * Comp2_flowRate,
        dplyr::if_else(print_type == 'Coin cell',
                       material1aRat,
                       0.5)
      )
    )), digits = 2) |>
    #   dplyr::case_when(
    #     fcomp_mat1 == 'none' ~ 0,
    #     print_type == 'PCB print' | print_type == 'Bulk cast' ~ material1aRat * Comp1_flowRate + material1bRat * Comp2_flowRate,
    #     print_type == 'Coin cell' ~ material1aRat,
    #     TRUE ~ 0.5),
    #   digits = 3)) |>
    dplyr::mutate(fcomp_mat2 = material2a) |>
    dplyr::mutate(fcomp_mat2_ratio = signif(
      dplyr::if_else(
        fcomp_mat2 == 'none',
        0,
        dplyr::if_else(
          print_type == 'PCB print' | print_type == 'Bulk cast',
          material2aRat * Comp1_flowRate +
            material2bRat * Comp2_flowRate,
          dplyr::if_else(print_type == 'Coin cell',
                         material2aRat,
                         0.5)
        )
      ),
      digits = 2
    )) |>
    dplyr::mutate(fcomp_salt_wt_pct = signif(dplyr::if_else(
      is.na(fcomp_salt) == T,
      0,
      dplyr::if_else(
        print_type == 'PCB print' | print_type == 'Bulk cast',
        #bulk cast additives set to 1 (since unknown amount used)
        (wt_pctSalt2a * Comp1_flowRate +
           wt_pctSalt2b * Comp2_flowRate),
        dplyr::if_else(
          print_type == 'Coin cell' ,
          wt_pctSalt2a,
          wt_pctSalt2a +
            wt_pctSalt2b
        )
      )
    ), digits = 2)) |>
    dplyr::relocate(ionic_conductivity_s_cm, .after = fcomp_salt_wt_pct) |>
    #dplyr::select(-EIS_mpr) |>
    dplyr::mutate(
      film_quality = dplyr::if_else(
        notes == 'none',
        'transparent',
        dplyr::if_else(
          str_detect(notes, 'white|opaque') == T,
          'opaque',
          'transparent'
        ),
        
      ),
      fcomp_salt = dplyr::if_else(fcomp_salt_wt_pct == 0,
                                  'none',
                                  fcomp_salt),
      fcomp_additive = dplyr::if_else(fcomp_additive_wt_pct == 0,
                                      'none',
                                      fcomp_additive)
    ) |>
    #fixes the numbering of the sample numbers
    #dplyr::mutate(sample_number = as.numeric(rownames(fcomp_additive))) |>
    # coercion of numbers to proper digits
    dplyr::mutate(
      fcomp_additive_wt_pct =
        dplyr::case_when(
          fcomp_additive_wt_pct == 1.43000 ~ 1.4860,
          fcomp_additive_wt_pct == 2.14000 ~ 2.14290,
          fcomp_additive_wt_pct == 2.86000 ~ 2.85710,
          fcomp_additive_wt_pct == 4.28000 ~ 4.28570,
          fcomp_additive_wt_pct == 6.42000 ~ 6.42860,
          fcomp_additive_wt_pct == 8.56000 |
            fcomp_additive_wt_pct == 8.57000 ~ 8.57140,
          fcomp_additive_wt_pct == 10.71000 ~ 10.7140,
          fcomp_additive_wt_pct == 12.85000 |
            fcomp_additive_wt_pct == 8.56000 ~ 12.85700,
          TRUE ~ fcomp_additive_wt_pct
        )
    ) |>
    #corrects for significant figs in the wt percentages
    dplyr::mutate(
      fcomp_additive_wt_pct = signif(fcomp_additive_wt_pct, digits = 3),
      fcomp_salt_wt_pct = signif(fcomp_salt_wt_pct, digits = 3)
    ) |>
    dplyr::mutate(
      fcomp_formulation =
        glue::glue(
          '{fcomp_mat1_ratio} {fcomp_mat1}, {fcomp_mat2_ratio} {fcomp_mat2}, {fcomp_additive_wt_pct} {fcomp_additive}, {fcomp_salt_wt_pct} {fcomp_salt}'
        )
    ) |>
    dplyr::select(
      c(
        # sample_number,
        date_printed,
        print_type,
        purge_time_s,
        Comp1_flowRate,
        Comp2_flowRate,
        fab_method,
        notes,
        film_thickness,
        # resistance1_o,
        # resistance2_o,
        # resistance3_o,
        resistance,
        resistanceAVG_o,
        fcomp_mat1,
        fcomp_mat1_ratio,
        fcomp_mat2,
        fcomp_mat2_ratio,
        fcomp_additive,
        fcomp_additive_wt_pct,
        fcomp_salt,
        fcomp_salt_wt_pct,
        ionic_conductivity_s_cm,
        film_quality,
        fcomp_formulation,
        EIS_mpr,
        area_cm2,
        sample_ID,
        temperature,
        humidity
      )
    ) |>
    dplyr::relocate(c(fcomp_formulation, ionic_conductivity_s_cm, print_type),
                    .after = date_printed)
}

### AMENDMENTS ###
PE_AMEND_ethanolAddition <- function(data) {
  require(tidyverse)
  data |>
    dplyr::mutate(
      nile_red =
        dplyr::case_when(
          # labels nile red from 2023-05-22 to 2023-10-23 and when no nile red is getting pumped out
          date_printed > as.Date('2023-05-22') &
            date_printed < as.Date('2023-10-23') &
            fcomp_additive_wt_pct != '0' ~ TRUE,
          TRUE ~ FALSE
        ),
      fcomp_additive2 =
        dplyr::case_when(
          # labels nile red from 2023-05-22 to 2023-10-23 and when no ethanol is getting pumped out
          date_printed > as.Date('2023-05-22') &
            date_printed < as.Date('2023-10-23') &
            fcomp_additive_wt_pct != '0' ~ 'Ethanol',
          TRUE ~ 'none'
        ),
      fcomp_additive2_wt_pct =
        dplyr::case_when(# labels nile red from 2023-05-22 to 2023-10-23 and when no ethanol is getting pumped out
          fcomp_additive2 == 'Ethanol' ~ '20',
          TRUE ~ '0'),
    )
}

PE_AMEND_eis_filepathFill <- function(data) {
  require(tidyverse)
  data |>
    dplyr::mutate(
      EIS_mpr =
        dplyr::case_when(
          # adds partial file extension for the remaining 2023-10-26 files
          date_printed == '2023-10-26' &
            stringr::str_detect(EIS_mpr, 'pin') == TRUE ~ glue::glue(
              r'({stringi::stri_replace_all_regex(EIS_mpr, '\\_C01[.]mpr', '')})'
            ),
          # adds partial file extension for the remaining 2023-10-23
          TRUE ~ EIS_mpr
        ),
      temperature = as.double(temperature),
      humidity = as.numeric(humidity)
    ) |>
    # fixes the _c01 string issue
    dplyr::mutate(EIS_mpr =
                    stringi::stri_replace_all_regex(EIS_mpr,
                                                    '_C01\\\\pin',
                                                    '\\\\pin'))
}


# cleans the first excel sheet
df_PE1 <- polyelectrolyte_DataCleaning(excel_file) |>
  # column formating to reflect the other datasets
  dplyr::mutate(
    SPE_thicknessAVG_cm = SPE_thickness_mm * 0.1,
    temperature = NA,
    #temperature = as.double(temperature),
    humidity = NA,
    #humidity = as.numeric(humidity)
  ) |>
  dplyr::rename(resistanceAVG_o = resistance_o) |>
  dplyr::select(-c(
    area_mm2,
    SPE_thickness_mm,
    ratio_eo_li,
    x_li_tfsi,
    additive,
    additive_wt,
    temperature,
    humidity
  ))

# cleans the ss studies
df_PE3 <- poly3_cleaning(excel_file3) |>
  dplyr::mutate(temperature = as.double(temperature),
                humidity = as.numeric(humidity)) |>
  dplyr::select(
    -c(
      electrolyte1a,
      wt_pct1a,
      electrolyte1b,
      wt_pct1b,
      ratio_eo_li,
      x_li_tfsi,
      additive,
      additive_wt,
      temperature,
      humidity
    )
  )

# cleans the Na-ion studies
df_PE4 <- poly3_cleaning(excel_file4) |>
  dplyr::select(
    -c(
      electrolyte1a,
      wt_pct1a,
      electrolyte1b,
      wt_pct1b,
      ratio_eo_li,
      x_li_tfsi,
      additive,
      additive_wt,
    )
  ) |>
  # dplyr::rename(temperature = 4,
  #               humidity = 5) |>
  dplyr::mutate(
    resistance = as.numeric(resistance),
    EIS_mpr = as.character(EIS_mpr),
    temperature = as.double(temperature),
    humidity = as.numeric(humidity) * 100,
    notes = as.character(notes)
  )

df_PE2 <- poly2_cleaning(excel_file2) |>
  dplyr::mutate(temperature = as.double(temperature),
                humidity = as.numeric(humidity)) |>
  dplyr::select(
    -c(
      electrolyte1a,
      wt_pct1a,
      electrolyte1b,
      wt_pct1b,
      ratio_eo_li,
      x_li_tfsi,
      additive,
      additive_wt,
      temperature,
      humidity
    )
  )

# AMENDMENT correcting EIS filepaths to new location
PE_AMEND_eis_correctedFilepath <- function(data) {
  require(dplyr)
  require(stringi)
  data |>
    dplyr::mutate(
      # changes the 08222023 files
      EIS_mpr =
        stringi::stri_replace_all_regex(
          EIS_mpr,
          'Katherine Updates\\\\08222023',
          'Database of polymer electrolytes/mpr files/08222023'
        ),
      EIS_mpr =
        stringi::stri_replace_all_regex(
          EIS_mpr,
          'Katherine Updates\\\\091223',
          'Database of polymer electrolytes/mpr files/091223'
        ),
      
    )
}

# extracts the excel file 2
df_PE_combined <- df_PE4 |>
  dplyr::bind_rows(df_PE2, df_PE3, df_PE1) |>
  PE_addArea_changeVarType() |>
  PE_collectDF_changeVarType() |>
  PE_column_cleaning() |>
  PE_fcomp_adding() |>
  PE_AMEND_ethanolAddition() |>
  #PE_AMEND_eis_filepathFill() |>
  PE_AMEND_eis_correctedFilepath() |>
  #dplyr::mutate(sample_number = 1:nrow(fcomp_)) |>
  dplyr::mutate(
    EIS_mpr =
      dplyr::if_else(
        !str_detect(EIS_mpr,
                    '[.]mpr'),
        glue::glue('{EIS_mpr}.mpr'),
        EIS_mpr
      ),
    EIS_mpr =
      stringi::stri_replace_all_regex(EIS_mpr,
                                      '\\\\',
                                      '/'),
    # querires single character spaces in excel (' ' does not work at all)
    EIS_mpr =
      stringi::stri_replace_all_regex(EIS_mpr,
                                      '\\s',
                                      ' '),
    EIS_mpr = 
      stringi::stri_replace_all_regex(EIS_mpr,
                                      '.* LLNL',
                                      '')
  )
# removes everything but the collected df
# rm(list = setdiff(ls(), c("df_PE_combined", "user_name"))) |>
#   gc()

# look for duplicates in composition

# dupes_finding <- test |>
#   dplyr::select(c(fcomp_mat1, fcomp_mat1_ratio, fcomp_mat2, fcomp_mat2_ratio, fcomp_additive, fcomp_additive_wt_pct, fcomp_salt, fcomp_salt_wt_pct))

#saving the versions
# write.csv(
#   df_PE_combined,
#   r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\2305-jimenez45_PEdatabase_v17.csv)'
# )
