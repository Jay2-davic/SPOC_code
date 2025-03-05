#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte

require(future)
future::multicore(workers = 4)
# adds a user name functionality to access own directory
user_name <-
  svDialogs::dlg_input("Enter your OUN", Sys.info()["user"])$res
version <-
  svDialogs::dlg_input("What version is this database?", '')$res

# collects the formulation database information
source('~/Git/23-spoc_code/PE_DatabaseCleaning/2310-PE_databaseClean_integrate.R')
# runs the EIS file cleaner
source('~/Git/23-spoc_code/EIS_Cleaner/2310-WRAPPER_EIS_extraction_jimenez45.R')
# updates the EIS files for new files
source('~/Git/23-spoc_code/EIS_Analysis/2312-EIS_check_for_finished_files.R')
# updates the converted data dictionary (to related converted files back to parent mpr file)
source('~/Git/23-spoc_code/EIS_Analysis/2312-EIS_conversion_dataDictionary.R')
source('~/Git/23-spoc_code/EIS_Analysis/2405-EIS_dataDictionaryUpdate.R')
source('~/Git/23-spoc_code/EIS_Analysis/2405-EIS_renameFilenames.R')
# converts the EIS files into individual measurements
reticulate::use_condaenv(glue::glue('C:\\Users\\{user_name}\\AppData\\Local\\miniforge3\\envs\\EIS_processing'),
                         required = TRUE)
reticulate::source_python('~/Git/23-spoc_code/EIS_Analysis/2312-EIS_cycle_splitter_jimenez45.py')
#(user_name)
# # compiles the EIS files
source('~/Git/23-spoc_code/EIS_Analysis/2311-EIScompile_fileMatch.R')

# modifies the combined files
df_PE_combined_2 <- df_PE_combined |>
  dplyr::rename(new_filename = mpr_files,
                orig_filename = legacy_mpr) |>
  dplyr::full_join(eis_full,
                   relationship = 'many-to-many'
                   ) |>
  dplyr::mutate(dplyr::across(
    dplyr::starts_with('resistance'),
    ~ dplyr::case_when(print_type == 'PCB print' |
                         print_type == 'Bulk cast' ~ NA,
                       TRUE ~ .)
  ),
  mpr_files_cycle = stringi::stri_replace_all_regex(
    mpr_files_cycle,
    'mpr_results_from_Jay_2',
    'mpr_new_files_converted')
  ) |>
  dplyr::distinct(sample_ID, new_filename, resistance, .keep_all = TRUE)

combined_df <- df_PE_combined_2 |>
  # calculates ionic conductivity
  dplyr::mutate(
    resistance = dplyr::case_when(is.na(resistanceAVG_o) != TRUE ~ resistanceAVG_o, # if resistance is blank take values from resistanceAVG
                                  TRUE ~ resistance),
    ionic_conductivity_final = dplyr::case_when(
      # coin cell ionic conductivity = l / (R_b * A)
      #print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      # no mpr files, take existing ionic_conductivity value
      is.na(mpr_files_cycle) == TRUE &
        is.na(resistance) == TRUE ~ ionic_conductivity_s_cm,
      # PCB prints ionic conductivity = 0.025 cm / (R_b * l * 0.25 cm)
      # is.na(csv_file) == FALSE ~ 0.025 / (FittedValue.R1 * film_thickness * 0.25^2) 
      is.na(csv_file) == FALSE ~ 0.025 / (FittedValue.R1 * 0.25^2)
    ),
    ionic_conductivity_final_alt = dplyr::case_when(
      # coin cell ionic conductivity = l / (R_b * A)
      #print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      print_type == 'Coin cell' ~ film_thickness / (resistance * area_cm2),
      # no mpr files, take existing ionic_conductivity value
      is.na(mpr_files_cycle) == TRUE &
        is.na(resistance) == TRUE ~ ionic_conductivity_s_cm,
      # PCB prints ionic conductivity = 0.025 cm / (R_b * l * 0.25 cm)
      # is.na(csv_file) == FALSE ~ 0.025 / (FittedValue.R1 * film_thickness * 0.25) 
      is.na(csv_file) == FALSE ~ 4.35757/FittedValue.R1
    )
  ) |>
  # remove high ionic conductivity based on very thin samples (0.006?)
  dplyr::filter(film_thickness >= 0.008 | is.na(film_thickness) == TRUE & is.na(resistanceAVG_o) == TRUE) |>
  # removes non-unique ionic conductivity values
  dplyr::distinct(ionic_conductivity_final, fcomp_formulation, .keep_all = TRUE) |>
  # removes zero ionic conductivities
  dplyr::filter(ionic_conductivity_final != 0) |>
  # rounds the wt percentages to combine together
  dplyr::mutate(
    fcomp_salt_wt_pct = signif(fcomp_salt_wt_pct, digit = 4),
    fcomp_additive_wt_pct = signif(fcomp_additive_wt_pct, digit = 2)
  )

# selecting columns for final db
final_db <- combined_df |>
  # filters only the solid polyelectrolyte for paper
  dplyr::filter(
    # filters samples after 10/23/2023
    date_printed >= as.Date('2023-10-22') &
      fcomp_salt != 'NaTFSI' |
      # filters prior samples that have 0 salt and additive
      fcomp_additive_wt_pct == '0' &
      fcomp_salt_wt_pct == '0' & fcomp_additive2 != 'Ethanol' |
      # filters zero values that is not ethanol containing and have 0% additive
      fcomp_salt == 'LiTFSI' &
      fcomp_additive_wt_pct == '0' & fcomp_additive2 != 'Ethanol'
  ) |>
  dplyr::select(
    c(
      sample_ID,
      fcomp_formulation,
      ionic_conductivity_final,
      ionic_conductivity_final_alt,
      date_printed,
      fab_method,
      print_type,
      fcomp_additive,
      fcomp_additive_wt_pct,
      fcomp_salt,
      fcomp_salt_wt_pct,
      fcomp_additive2,
      area_cm2,
      film_thickness,
      film_quality,
      mpr_files_cycle,
      bode_file,
      linKK_file,
      nyquist_file,
      RMSE_real,
      RMSE_imag,
      head_count,
      mu,
      Circuits,
      InitialGuess.R0,
      InitialGuess.R1,
      InitialGuess.Wo1_0,
      InitialGuess.Wo1_1,
      InitialGuess.C1,
      FittedValue.R0,
      FittedValue.R1,
      FittedValue.Wo1_0,
      FittedValue.Wo1_1,
      FittedValue.C1,
      FittedValue.R0_stDev,
      FittedValue.R1_stDev,
      FittedValue.Wo1_0_stDev,
      FittedValue.Wo1_1_stDev,
      FittedValue.C1_stDev,
      humidity,
      temperature
    )
  ) |>
  dplyr::group_by(ionic_conductivity_final, fcomp_formulation) |>
  dplyr::distinct()

# saves the final db version
write.csv(
  final_db,
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version}.csv'
  )
)

# saving the versions
write.csv(
  combined_df,
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version}_FULL.csv'
  )
)
write.csv(
  final_db |>
    dplyr::select(
      c(
        sample_ID,
        fcomp_formulation,
        ionic_conductivity_final,
        date_printed,
        fab_method,
        print_type,
        fcomp_additive,
        fcomp_additive_wt_pct,
        fcomp_salt,
        fcomp_salt_wt_pct,
        fcomp_additive2,
        area_cm2,
        film_thickness,
        film_quality,
        RMSE_real,
        RMSE_imag,
        head_count,
        mu,
        Circuits,
        InitialGuess.R0,
        InitialGuess.R1,
        InitialGuess.Wo1_0,
        InitialGuess.Wo1_1,
        InitialGuess.C1,
        FittedValue.R0,
        FittedValue.R1,
        FittedValue.Wo1_0,
        FittedValue.Wo1_1,
        FittedValue.C1,
        FittedValue.R0_stDev,
        FittedValue.R1_stDev,
        FittedValue.Wo1_0_stDev,
        FittedValue.Wo1_1_stDev,
        FittedValue.C1_stDev,
        humidity,
        temperature
      )
    ) |>
    dplyr::filter(
      date_printed != '2024-07-22',
      date_printed > '2024-01-01',
      date_printed != '2024-09-26',
      date_printed != '2024-08-15',
      date_printed != '2024-05-06',
      date_printed != '2024-10-07',
      date_printed != '2024-03-04'
    ) |>
    dplyr::mutate(
      fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation,
                                                          '4.28',
                                                          '4.29'),
      humidity = dplyr::case_when(print_type == 'Coin cell' ~ 0.00001,
                                  TRUE ~ humidity / 100),
      # adds validation labels to misformulated prints
      validation = dplyr::case_when(
        date_printed == '2024-06-27' ~ 'YES',
        date_printed == '2024-08-16' ~ 'YES',
        TRUE ~ 'NO'
      )
    ) |>
    dplyr::filter(
      dplyr::case_when(
        fcomp_salt_wt_pct != 0 ~ ionic_conductivity_final >= 10 ^ -7,
        TRUE ~ TRUE
      ),
      fcomp_additive != 'AlO3'
    ) |>
    dplyr::relocate(ionic_conductivity_final, .before = print_type)
  ,
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\ForSharing\\2510-jimenez45_PEdatabase_v2.29.csv'
  )
) 

rm(list = ls())
gc()
