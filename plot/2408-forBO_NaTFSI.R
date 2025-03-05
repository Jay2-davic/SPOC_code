#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Polyelectrolyte
#For saving ML dataset for BO - LiTFSI

require(ggplot2)
require(tidyverse)
require(svDialogs)
require(magrittr)

user_name <-
  svDialogs::dlg_input("Enter your OUN", Sys.info()["user"])$res
version_number <- 
  svDialogs::dlg_input("Enter version number (use commas for decimals)", '2,19')$res

# loads the functions
source('~/Git/23-spoc_code/PE_plotTemplates/2401-GridSearch_functions-jimenez45.R')

# output paper figures in the manuscript folder
folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Jay\\forJune\\NaTFSI\\'
)

final_db <- readr::read_csv(
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version_number}_FULL.csv'
  )
) |>
  # dplyr::mutate(fcomp_formulation =
  # stringr::str_replace(fcomp_formulation, '4.28', '4.29')) |>
  dplyr::filter(date_printed != '2024-07-22',
                date_printed > '2024-02-01',
                fcomp_salt == 'NaTFSI' |
                  fcomp_salt == 'none') |>
  dplyr::mutate(fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation,
                                                                    '4.28',
                                                                    '4.29'),
                humidity = dplyr::case_when(
                  print_type == 'Coin cell' ~ 0.00001,
                  TRUE ~ humidity/100
                ),
                # adds validation labels to misformulated prints
                validation = dplyr::case_when(
                  date_printed == '2024-06-27' ~ 'YES',
                  date_printed == '2024-08-16' ~ 'YES',
                  TRUE ~ 'NO'
                )) |>
  dplyr::filter(
    dplyr::case_when(fcomp_salt_wt_pct != 0 ~ ionic_conductivity_final >= 10^-7,
                     TRUE ~ TRUE)
  )

{
  final_db_outlier <- final_db |>
    dplyr::filter(!date_printed <= '2024-02-02',
                  validation == 'NO',
                  fab_method != 3) |>
    # removes outliers outside 2 sigmas
    z_score_outlier_remove('coin', 1.5) |>
    dplyr::bind_rows(z_score_outlier_remove(final_db, 'PCB', 1.5)) |>
    dplyr::filter(
      fcomp_additive_wt_pct <= 10,
      !fcomp_additive_wt_pct %in% c(7.5, 2.1, 6.4)) |>
    dplyr::select(
      fcomp_formulation,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    )
  }

{
  # grid search of parameters before outlier removal
  # select_df(df = final_db, type = 'coin') |>
  #   gridSearch_plotter('Coin')
  # grid search of parameters after outlier removal
  select_df(df = final_db_outlier, type = 'coin') |>
    gridSearch_plotter_NaTFSI('Coin')
  # grid search of parameters before outlier removal
  # plotdf <-select_df(df = final_db, type = 'PCB') |>
  #   gridSearch_plotter('PCB')
  # grid search of parameters after outlier removal
  select_df(df = final_db_outlier, type = 'PCB') |>
    # removes the cross formulations
    #dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
    gridSearch_plotter_NaTFSI('PCB')
}

select_df(df = final_db_outlier, type = 'coin')  |>
  # removes the cross formulations
  dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
  boxplot_plotter('Coin Cell')
select_df(df = final_db_outlier, type = 'PCB') |> 
  boxplot_plotter('PCB Print')

#saving the version for ML
write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'Coin cell'),
  glue::glue('{folder_destination}\\PE_MLtraining_v{version_number}_NaTFSI_coin.csv')
)

write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'PCB print'),
  glue::glue('{folder_destination}\\PE_MLtraining_v{version_number}_NaTFSI_PCB.csv')
)
