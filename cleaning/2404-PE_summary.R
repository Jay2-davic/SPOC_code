require(ggplot2)
require(tidyverse)
require(svDialogs)

user_name <-
  svDialogs::dlg_input("Enter your OUN", Sys.info()["user"])$res

boxplot_outlier_remove <- function(df) {
  df |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    # log transform the outliers
    dplyr::mutate(
      log_ic = log10(ionic_conductivity_final)
    ) |>
    dplyr::summarise(outlier = list(log_ic[log_ic < quantile(log_ic, 0.25) - 1.5 * IQR(log_ic) |
                                             log_ic > quantile(log_ic, 0.75) + 1.5 * IQR(log_ic)])) |>
    dplyr::ungroup() |>
    dplyr::left_join(select_df(df = df, type = type)) |>
    dplyr::mutate(outlier_label = ifelse(ionic_conductivity_final %in% unlist(outlier),
                                         'YES',
                                         "NO"))
}

combined_df <-
  readr::read_csv(
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v2,26_FULL.csv'
    )
  )

final_db_outlier <- 
  combined_df |>
  dplyr::filter(
    !fcomp_salt_wt_pct %in% c(7.5, 15)
  ) |>
  dplyr::group_by(fcomp_formulation, print_type) |>
  # log transform the outliers
  dplyr::mutate(
    log_ic = log10(ionic_conductivity_final)
  ) |>
  dplyr::summarise(outlier = list(log_ic[log_ic < quantile(log_ic, 0.25) - 1.5 * IQR(log_ic) |
                                           log_ic > quantile(log_ic, 0.75) + 1.5 * IQR(log_ic)])) |>
  dplyr::ungroup() |>
  dplyr::left_join(combined_df) |>
  dplyr::mutate(outlier_label = ifelse(outlier == 'numeric(0)',
                                       'NO',
                                       'YES')) |>
  dplyr::filter(outlier_label == 'NO')

combined_df <- combined_df |>
  dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type, 'Bulk cast', 'PCB print'))
# 
# {final_db_outlier <- boxplot_outlier_remove(combined_df) |>
#     dplyr::bind_rows(boxplot_outlier_remove(combined_df)) |>
#     dplyr::select(-c(outlier, '...4')) |>
#     dplyr::filter(outlier_label == 'NO')
#   }

# finds # of total samples
summarize_df <- combined_df %>%
  dplyr::summarize(total_samples = dplyr::n_distinct(sample_ID),
                   total_IC_meas = dplyr::n_distinct(ionic_conductivity_final,),
                   total_unique_formulations = dplyr::n_distinct(sample_ID),
  ) %>%
  dplyr::mutate(print_type = 'All', .before = total_samples)

summarize_df2 <- combined_df %>%
  dplyr::group_by(print_type) %>%
  dplyr::summarize(total_samples = dplyr::n_distinct(sample_ID),
                   total_IC_meas = dplyr::n_distinct(ionic_conductivity_final),
                   total_unique_formulations = dplyr::n_distinct(sample_ID)) %>%
  dplyr::ungroup()

summarize_df3 <- combined_df %>%
  dplyr::group_by(fcomp_salt, print_type) %>%
  dplyr::summarize(total_samples = dplyr::n_distinct(sample_ID),
                   total_IC_meas = dplyr::n_distinct(ionic_conductivity_final),
                   total_unique_formulations = dplyr::n_distinct(sample_ID))

summarize_all <- summarize_df %>%
  dplyr::bind_rows(summarize_df2) %>%
  gt::gt() %>%
  gt::tab_header(gt::md('Summary of PE Project')) %>%
  gt::cols_label(
    print_type = gt::md('**Print Type**'),
    total_samples = gt::md('**# of Samples**'),
    total_IC_meas = gt::md('**# of Ionic <br> Conductivity Measurements**'),
    total_unique_formulations = gt::md('**# of Unique<br>Samples**')
  ) %>%
  print()
# finds # of total coin cell samples

summarize_salts <- summarize_df3 %>%
  gt::gt() %>%
    gt::tab_header(gt::md('Summary of PE Project by Salt')) %>%
    gt::cols_label(
      fcomp_salt = gt::md('**Salt Added**'),
      print_type = gt::md('**Print Type**'),
      total_samples = gt::md('**# of Samples**'),
      total_IC_meas = gt::md('**# of Ionic <br> Conductivity Measurements**'),
      total_unique_formulations = gt::md('**# of Unique<br>Samples**')
    ) %>%
    print()

summarize_performance <- combined_df %>%
  dplyr::filter(!fcomp_salt == 'none', 
                !print_type == 'Bulk cast',
                ionic_conductivity_final < 1,
                !fcomp_salt == 'NaTFSI' | !fcomp_salt_wt_pct > 20,
                !fcomp_additive2 == 'Ethanol') %>%
  dplyr::group_by(print_type, fcomp_salt) %>%
  dplyr::arrange(
    dplyr::desc(ionic_conductivity_final)) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::select(fcomp_formulation, ionic_conductivity_final) %>%
  dplyr::mutate(ionic_conductivity_final = scales::scientific(ionic_conductivity_final, digits = 3)) %>%
  gt::gt() %>%
  gt::tab_header(gt::md('Summary of Highest PE for PCB and Coin')) %>%
  gt::cols_label(
    fcomp_formulation = gt::md('**Formulation**'),
    print_type = gt::md('**Print Type**'),
    fcomp_salt = gt::md('**Salt Added**'),
    ionic_conductivity_final = gt::md('**Ionic Conductivity <br> S/cm**')
  ) %>%
  print()


  