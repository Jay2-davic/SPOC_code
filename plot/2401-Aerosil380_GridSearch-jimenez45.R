#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Digital Twins (SI)
#Subproject: Polyelectrolyte Screening
# loads the database

#loads OUN to replace user name
require(ggplot2)
require(tidyverse)
require(svDialogs)
require(magrittr)

# user_name <-
#   svDialogs::dlg_input("Enter your OUN", Sys.info()["user"])$res
# version_number <-
#   svDialogs::dlg_input("Enter version number (use commas for decimals)", '2,19')$res

user_name <- 'jimenez45'
version_number <- '2,29'

# loads the functions
source('~/Git/23-spoc_code/PE_plotTemplates/2401-GridSearch_functions-jimenez45.R')
# loads the plotly plotter
source('~/Git/23-spoc_code/PE_plotTemplates/2310-PlotlyTemplate_PlotlyExport.R')

# output paper figures in the manuscript folder
folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs_corr\\'
)

df_PE_combined <- readr::read_csv(
  glue::glue(
    'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version_number}_FULL.csv'
  )
)

final_db <-
  readr::read_csv(
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version_number}.csv'
    )
  ) |>
  # dplyr::mutate(fcomp_formulation =
  # stringr::str_replace(fcomp_formulation, '4.28', '4.29')) |>
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


# finds outliers in each group then combines the dataframe again
# {
#   final_db_outlier <-
#     boxplot_outlier_remove(final_db |> dplyr::filter(fab_method != 3), 'coin') |>
#     dplyr::bind_rows(boxplot_outlier_remove(final_db, 'PCB')) |>
#     dplyr::select(-c(outlier, '...4')) |>
#     dplyr::filter(!date_printed <= '2024-02-02',
#                   outlier_label == 'NO',
#                   validation == 'NO')
#   }

{
  final_db_outlier <- final_db |>
    dplyr::filter(!date_printed <= '2024-02-02',
                  validation == 'NO',
                  fab_method != 3,) |>
    dplyr::filter(
      !(fcomp_salt_wt_pct == 20 & date_printed == '2024-09-23'),
      #!(fcomp_salt_wt_pct == 10 & date_printed != '2024-05-07'),
      date_printed != '2024-08-15'
    ) |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    # removes outliers outside 2 sigmas
    z_score_outlier_remove('coin', 2) |>
    dplyr::bind_rows(z_score_outlier_remove(final_db |>
                                              dplyr::filter(
                                                !(
                                                  fcomp_salt_wt_pct == 20 &
                                                    #date_printed <= '2024-09-24' &
                                                    print_type == 'PCB print'
                                                ),
                                                ionic_conductivity_final <= 10^-3
                                              ) , 'PCB', 2))
  
  readr::write_csv(final_db_outlier,
                   # dplyr::select(
                   #   c(fcomp_salt_wt_pct, fcomp_additive_wt_pct,ionic_conductivity_final)
                   # ),
                   glue::glue('{folder_destination}/2410-PE_database.csv'))
  
  }
write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'Coin cell') |>
    dplyr::select(
      fcomp_formulation,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    ),
  glue::glue('{folder_destination}\\PE_MLtraining_v{version_number}_LiTFSI_coin.csv')
)

write.csv(
  final_db_outlier |>
    dplyr::filter(print_type == 'PCB print') |>
    dplyr::select(
      fcomp_formulation,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    ),
  glue::glue('{folder_destination}\\PE_MLtraining_v{version_number}_LiTFSI_PCB.csv')
)
{
  gg_add_violin <- ggplot2::ggplot(
    data = df_PE_combined |>
      # filters before the SS studies
      dplyr::filter(
        date_printed >= '2023-10-23' |
          date_printed != '2023-06-06' &
          date_printed != '2023-06-02' &
          date_printed != '2023-06-15'
      ) |>
      dplyr::filter(
        !(fcomp_additive == 'Aerosil 380' & date_printed >= '2024-01-01')
      ) |>
      dplyr::mutate(
        fcomp_additive = stringi::stri_replace_all_regex(fcomp_additive, 'AlO3', 'Al2O3')
      ) |>
      dplyr::filter(
        print_type != 'Coin cell',!(fcomp_additive == 'Al2O3' &
                                      FittedValue.R1 <= 1e6)
      ) |> # filters Al2O3 with bad fits
      # dplyr::filter(!(fcomp_additive == 'Aerosil 380' & fcomp_salt_wt_pct != 0),
      #               !(fcomp_additive == 'TiO2' & fcomp_salt_wt_pct != 0),
      #               !(fcomp_additive == 'Aerosil 90' & fcomp_salt_wt_pct != 0),
      #               !(fcomp_additive == 'AlO3' & FittedValue.R1 <= 1e6)) |>
      dplyr::filter(fcomp_salt_wt_pct == 0) |>
      dplyr::filter(fcomp_additive != 'EO', fcomp_additive != 'none') |>
      #dplyr::filter(fcomp_salt == 'none') |>
      # removes 5e-6 unrealistic values
      # dplyr::filter(ionic_conductivity_final <= 5e-6,
      #               ionic_conductivity_final >= 1e-12) |>
      dplyr::mutate(
        fcomp_additive = stringi::stri_replace_all_regex(fcomp_additive, 'aerosil', 'Aerosil'),
        fcomp_additive = stringi::stri_replace_all_regex(fcomp_additive, 'EH%', 'EH5'),
        
      ),
    #z_score_outlier_remove('PCB', 1.5),
    ggplot2::aes(
      y = ionic_conductivity_final,
      x = fcomp_additive |> as.factor(),
      group = fcomp_additive |> as.factor(),
      color = fcomp_additive |> as.factor()
    )
  ) +
    cowplot::theme_half_open() +
    #cowplot::background_grid(minor = 'y') +
    ggplot2::annotate(
      "rect",
      xmin = 0.5,
      xmax = 1.5,
      ymin = 1e-9,
      ymax = 5e-6,
      fill = "yellow",
      alpha = 0.3
    ) +
    ggplot2::geom_violin(
      #width = 1.25,
      size = 0.9,
      ggplot2::aes(fill = fcomp_additive),
      scale = 'width',
      alpha = 0.7
    ) +
    ggplot2::geom_boxplot(size = .25,
                          width = .15,
                          color = 'black') +
    # geom_jitter(size = 1,
    #             width = 0.01) +
    #facet_grid(. ~ fcomp_additive) +
    ggplot2::scale_y_continuous(
      trans = 'log10',
      limits = c(1e-9, 1e-5),
      breaks = c(1e-5, 1e-6, 1e-7, 1e-8, 1e-9)
    ) +
    ggplot2::labs(
                  y = 'Ionic Conductivity (S/cm)',
                  title = 'b) Comparing SPEs with varying filler compositions') +
    #ggplot2::ylim(c(1e-9, 1e-4)) +
    ggplot2::theme(
      legend.position = 'none',
      plot.title.position = 'plot',
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(
        size = 12,
        angle = 0,
        vjust = 0.7
      ),
      strip.text = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11.35)
    ) +
    ggplot2::scale_x_discrete(labels = c(
      expression('Ae380'),
      expression('Ae90'),
      expression(Al[2] * O[3]),
      expression('EH5'),
      expression(TiO[2])
    )) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::scale_color_viridis_d()
  
  ggplot2::ggsave(
    gg_add_violin,
    filename = glue::glue('{folder_destination}2305-Additive_Screening_violin.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  # clear the memory
  rm('gg_add_violin')
}

{
  # grid search of parameters before outlier removal
  # select_df(df = final_db, type = 'coin') |>
  #   gridSearch_plotter('Coin')
  # grid search of parameters after outlier removal
  select_df(df = final_db_outlier, type = 'coin') |>
    # removes the cross formulations
    dplyr::filter(!fcomp_additive_wt_pct %in% c(2.9, 5, 5.7, 10, 7.1)) |>
    gridSearch_plotter('Coin')
  # grid search of parameters before outlier removal
  # plotdf <-select_df(df = final_db, type = 'PCB') |>
  #   gridSearch_plotter('PCB')
  # grid search of parameters after outlier removal
  ggploter <- select_df(df = final_db_outlier, type = 'PCB') |>
    # removes the cross formulations
    dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
    gridSearch_plotter('PCB')
}

#plot of the other additive formulations
# ggplot2::ggplot(
#   data = final_db |> dplyr::filter(
#     fcomp_salt_wt_pct == '0' & fcomp_additive != 'none',
#     date_printed != '2023-06-02' &
#       date_printed != '2023-06-06'
#   ) |>
#     dplyr::mutate(
#       fcomp_additive = stringi::stri_replace_all_regex(fcomp_additive, 'aerosil 380', 'Aerosil 380')
#     ),
#   aes(x = fcomp_additive_wt_pct,
#       y = ionic_conductivity_final,
#       color = fcomp_additive)
# ) +
#   geom_point(size = 4) +
#   facet_wrap(. ~ fcomp_additive,
#              ncol = 5,
#              scales = 'fixed') +
#   theme(legend.position = 'none') +
#   scale_y_continuous(trans = 'log10') +
#   labs(x = ('Additive (wt%)'),
#        y = expression('Ionic Conductivity (' ~ 'S' ~ '/cm)'))

# contour plot

# look at the mean for each grid METHOD1
# mean_final_db <- final_db |>
#   dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct) |>
#   dplyr::mutate(
#     group_mean = mean(ionic_conductivity_final),
#     group_median = stats::median(ionic_conductivity_final)
#   ) |>
#   dplyr::distinct(fcomp_additive_wt_pct, fcomp_salt_wt_pct)
# ggplot2::ggplot(
#   data = final_db,
#   aes(x = fcomp_salt_wt_pct,
#       y = fcomp_additive_wt_pct,
#       fill = ionic_conductivity_final)
# ) +
#   stat_density2d() +
#   geom_jitter(size = 4,
#               alpha = 0.2)

### plots heatmaps based on median/mean with coin cells or pcbs

# coin cell

# select_df(df = final_db, type = 'coin') |>
#   heatmap_mean('Coin Cell')

# select_df(df = final_db_outlier, type = 'coin')  |>
#   # removes the cross formulations
#   dplyr::filter(!fcomp_additive_wt_pct %in% c(2.9, 5, 5.7, 10, 7.1)) |>
#   heatmap_mean('Coin Cell')
# # select_df(df = final_db, type = 'coin') |>
# #   heatmap_median('Coin Cell')
# select_df(df = final_db_outlier, type = 'coin')  |>
#   # removes the cross formulations
#   dplyr::filter(!fcomp_additive_wt_pct %in% c(2.9, 5, 5.7, 10, 7.1)) |>
#   heatmap_median('Coin Cell')

# pcb boards

select_df(df = final_db_outlier, type = 'PCB') |>
  dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
  heatmap_mean('PCB Print')

# #select_df(df = final_db, type = 'PCB') |>
# #heatmap_median('PCB Print')
# select_df(df = final_db_outlier, type = 'PCB') |>
#   # removes the cross formulations
#   #dplyr::filter(fcomp_salt_wt_pct %in% c(0, 10, 20, 30),!fcomp_additive_wt_pct %in% c(5, 5.7, 10, 7.5)) |>
#   dplyr::filter(#fcomp_salt_wt_pct %in% c(0, 10, 20, 30) |!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
#     heatmap_median('PCB Print'))

# plots the boxplots after outlier removal
select_df(df = final_db_outlier, type = 'coin')  |>
  # removes the cross formulations
  dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
  boxplot_plotter('Coin Cell')
select_df(df = final_db_outlier, type = 'PCB') |>
  #dplyr::filter(#fcomp_salt_wt_pct %in% c(0, 10, 20, 30) |!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1)) |>
    boxplot_plotter('PCB Print')

# select unique values for coin cells
select_df(df = final_db_outlier, type = 'coin') |>
  dplyr::slice_max(ionic_conductivity_final, n = 60) |>
  dplyr::distinct(fcomp_formulation, .keep_all = TRUE) |>
  dplyr::select(fcomp_formulation, ionic_conductivity_final, date_printed) |>
  dplyr::slice_max(ionic_conductivity_final, n = 5) |>
  gt::gt() |>
  gt::tab_header(title = 'Coin Cell Top 5')

# total stats
pe_stats <- tibble::tibble(
  'Amount of Unique Samples (Coin Cells)' = select_df(df = df_PE_combined, type = 'coin') |>
    dplyr::distinct(sample_ID) |>
    dplyr::n_distinct(),
  'Amount of Unique Samples (SPOC)' = select_df(df = df_PE_combined, type = 'PCB') |>
    dplyr::distinct(sample_ID) |>
    dplyr::n_distinct(),
  'Amount of Coin Cell Measurements' = select_df(df = df_PE_combined, type = 'coin') |>
    dplyr::n_distinct('ionic_conductivity_final'),
  'Amount of SPOC Measurements' = select_df(df = df_PE_combined, type = 'PCB') |>
    dplyr::n_distinct('ionic_conductivity_final'),
) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = ' ',
    values_to = '  '
  ) |>
  gt::gt()

# select unique values for PCB prints

select_df(df = final_db, type = 'PCB') |>
  dplyr::slice_max(ionic_conductivity_final, n = 60) |>
  dplyr::distinct(fcomp_formulation, .keep_all = TRUE) |>
  dplyr::select(fcomp_formulation, ionic_conductivity_final, date_printed) |>
  dplyr::slice_max(ionic_conductivity_final, n = 5) |>
  gt::gt() |>
  gt::tab_header(title = 'PCB Print Top 5')

### summary of # of observations per formulation for each type

{
  tab1 <- select_df(df = final_db_outlier, type = 'PCB') |>
    dplyr::count(fcomp_formulation,
                 fcomp_salt_wt_pct,
                 fcomp_additive_wt_pct,
                 .drop = FALSE) |>
    dplyr::rename('# of PCB Measurements' = n)
  tab1_1 <-
    select_df(df = final_db_outlier, type = 'PCB') |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::mutate(PCB_count = dplyr::n_distinct(sample_ID)) |>
    dplyr::group_by(sample_ID, fcomp_formulation) |>
    dplyr::ungroup() |>
    dplyr::distinct(fcomp_formulation,
                    fcomp_salt_wt_pct,
                    fcomp_additive_wt_pct,
                    PCB_count)
  
  tab1 <- tab1 |>
    dplyr::full_join(tab1_1) |>
    dplyr::rename('# of PCB Samples' = PCB_count)
  rm(tab1_1)
  
  tab2 <- select_df(df = final_db_outlier, type = 'coin') |>
    dplyr::count(fcomp_formulation,
                 fcomp_salt_wt_pct,
                 fcomp_additive_wt_pct) |>
    dplyr::rename('# of Coin Cell Measurements' = n)
  
  tab2_1 <-
    select_df(df = final_db_outlier, type = 'coin') |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::mutate(cc_count = dplyr::n_distinct(sample_ID)) |>
    dplyr::group_by(sample_ID, fcomp_formulation) |>
    dplyr::ungroup() |>
    dplyr::distinct(fcomp_formulation,
                    fcomp_salt_wt_pct,
                    fcomp_additive_wt_pct,
                    cc_count)
  
  
  tab2 <- tab2 |>
    dplyr::full_join(tab2_1) |>
    dplyr::rename('# of Coin Cells' = cc_count)
  rm(tab2_1)
  
  # rearranged based on additive wt pct values for neatness
  mergedTab <- tab1 |> dplyr::full_join(tab2) |>
    dplyr::arrange(fcomp_additive_wt_pct)
  
  # 0% LiTFSI table summary of formulations
  mergedTab |>
    dplyr::filter(fcomp_salt_wt_pct == '0') |>
    dplyr::select(-c(fcomp_salt_wt_pct, fcomp_additive_wt_pct)) |>
    gt::gt() |>
    gt::tab_header(gt::md('Number of Measurements at **0% LiTFSI**')) |>
    print()
  
  # 10% LiTFSI table summary of formulations
  mergedTab |>
    dplyr::filter(fcomp_salt_wt_pct == '10') |>
    dplyr::select(-c(fcomp_salt_wt_pct, fcomp_additive_wt_pct)) |>
    gt::gt() |>
    gt::tab_header(gt::md('Number of Measurements at **10% LiTFSI**')) |>
    print()
  
  # 20% LiTFSI table summary of formulations
  mergedTab |>
    dplyr::filter(fcomp_salt_wt_pct == '20') |>
    dplyr::select(-c(fcomp_salt_wt_pct, fcomp_additive_wt_pct)) |>
    gt::gt() |>
    gt::tab_header(gt::md('Number of Measurements at **20% LiTFSI**')) |>
    print()
  
  # 30% LiTFSI table summary of formulations
  mergedTab |>
    dplyr::filter(fcomp_salt_wt_pct == '30') |>
    dplyr::select(-c(fcomp_salt_wt_pct, fcomp_additive_wt_pct)) |>
    gt::gt() |>
    gt::tab_header(gt::md('Number of Measurements at **30% LiTFSI**')) |>
    print()
  }

#anova results per salt formulation
mergedTab2 <- select_df(final_db_outlier, type = 'coin') |>
  dplyr::full_join(select_df(final_db, type = 'PCB')) |>
  dplyr::mutate(log_IC = log10(ionic_conductivity_final))

mergedTab2_0 <-
  select_df(final_db_outlier, type = 'coin') |>
  dplyr::full_join(select_df(final_db, type = 'PCB')) |>
  dplyr::mutate(log_IC = log10(ionic_conductivity_final)) |>
  dplyr::filter(fcomp_salt_wt_pct %in% 0)

mergedTab2_10 <-
  select_df(final_db_outlier, type = 'coin') |>
  dplyr::full_join(select_df(final_db, type = 'PCB')) |>
  dplyr::mutate(log_IC = log10(ionic_conductivity_final)) |>
  dplyr::filter(fcomp_salt_wt_pct %in% 10)

mergedTab2_20 <-
  select_df(final_db_outlier, type = 'coin') |>
  dplyr::full_join(select_df(final_db, type = 'PCB')) |>
  dplyr::mutate(log_IC = log10(ionic_conductivity_final)) |>
  dplyr::filter(fcomp_salt_wt_pct %in% 20)

mergedTab2_30 <-
  select_df(final_db_outlier, type = 'coin') |>
  dplyr::full_join(select_df(final_db, type = 'PCB')) |>
  dplyr::mutate(log_IC = log10(ionic_conductivity_final)) |>
  dplyr::filter(fcomp_salt_wt_pct %in% 30)

### run the glm for each iteration

# qqplots
# {
#   qq_0 <-
#     ggpubr::ggqqplot(
#       mergedTab2,
#       x = 'log_IC',
#       #color = 'print_type',
#       panel.labs = list(print_type = c('Coin Cell', 'PCB Print')),
#       facet.by = 'print_type',
#       xlab = 'Sample Quantile',
#       ylab = 'Theoretical Quantile'
#     )
#   qq_10 <-
#     ggpubr::ggqqplot(
#       mergedTab2_10,
#       x = 'log_IC',
#       #color = 'print_type',
#       panel.labs = list(print_type = c('Coin Cell', 'PCB Print')),
#       facet.by = 'print_type',
#       xlab = 'Sample Quantile',
#       ylab = 'Theoretical Quantile'
#     )
#   qq_20 <-
#     ggpubr::ggqqplot(
#       mergedTab2_20,
#       x = 'log_IC',
#       #color = 'print_type',
#       panel.labs = list(print_type = c('Coin Cell', 'PCB Print')),
#       facet.by = 'print_type',
#       xlab = 'Sample Quantile',
#       ylab = 'Theoretical Quantile'
#     )
#   # qq_30 <- ggpubr::ggqqplot(mergedTab2_30, x = 'log_IC', #color = 'print_type',
#   #                           panel.labs = list(print_type = c('Coin Cell', 'PCB Print')),
#   #                           facet.by = 'print_type',
#   #                           xlab = 'Sample Quantile',
#   #                           ylab = 'Theoretical Quantile')
#
#   ggplot2::ggsave(
#     qq_0,
#     filename = glue::glue('{folder_destination}2304-0_pct_qq_plot.png'),
#     dpi = 600,
#     width = 4,
#     height = 3,
#     bg = 'white'
#   )
#
#   ggplot2::ggsave(
#     qq_10,
#     filename = glue::glue('{folder_destination}2304-10_pct_qq_plot.png'),
#     dpi = 600,
#     width = 4,
#     height = 3,
#     bg = 'white'
#   )
#
#   ggplot2::ggsave(
#     qq_20,
#     filename = glue::glue('{folder_destination}2304-20_pct_qq_plot.png'),
#     dpi = 600,
#     width = 4,
#     height = 3,
#     bg = 'white'
#   )
#
#   ggplot2::ggsave(
#     qq_30,
#     filename = glue::glue('{folder_destination}2304-30_pct_qq_plot.png'),
#     dpi = 600,
#     width = 4,
#     height = 3,
#     bg = 'white'
#   )
#
#   rm(qq_0, qq_10, qq_20, qq_30)
#   }

# shapiro-wilk test summaries

# {
#   summary_table_10 <- mergedTab2_10 |>
#     dplyr::group_by(print_type, fcomp_salt_wt_pct) |>
#     dplyr::summarise(
#       statistic = stats::shapiro.test(log_IC)$statistic,
#       p.value = stats::shapiro.test(log_IC)$p.value
#     ) |>
#     dplyr::ungroup()
#   summary_table_0 <- mergedTab2_0 |>
#     dplyr::group_by(print_type, fcomp_salt_wt_pct) |>
#     dplyr::summarise(
#       statistic = stats::shapiro.test(log_IC)$statistic,
#       p.value = stats::shapiro.test(log_IC)$p.value
#     ) |>
#     dplyr::ungroup()
#   summary_table_20 <- mergedTab2_20 |>
#     dplyr::group_by(print_type, fcomp_salt_wt_pct) |>
#     dplyr::summarise(
#       statistic = stats::shapiro.test(log_IC)$statistic,
#       p.value = stats::shapiro.test(log_IC)$p.value
#     ) |>
#     dplyr::ungroup()
#   summary_table_30 <- mergedTab2_30 |>
#     dplyr::group_by(print_type, fcomp_salt_wt_pct) |>
#     dplyr::summarise(
#       statistic = stats::shapiro.test(log_IC)$statistic,
#       p.value = stats::shapiro.test(log_IC)$p.value
#     ) |>
#     dplyr::ungroup()
#   summary_table_all <- mergedTab2 |>
#     dplyr::group_by(print_type) |>
#     dplyr::summarise(
#       statistic = stats::shapiro.test(log_IC)$statistic,
#       p.value = stats::shapiro.test(log_IC)$p.value
#     ) |>
#     dplyr::mutate(fcomp_salt_wt_pct = 'all') |>
#     dplyr::ungroup()
#
#   summary_SW <- summary_table_all |>
#     rbind(summary_table_0,
#           summary_table_10,
#           summary_table_20,
#           summary_table_30)
#
#   readr::write_csv(summary_SW,
#                    glue::glue('{folder_destination}2304-SWnormalitySummary.csv'))
#
#   rm(
#     summary_table_all,
#     summary_table_0,
#     summary_table_10,
#     summary_table_20,
#     summary_table_30
#   )
#   }

# function to save plots for glm models with low-pressure
# glm_moisture_pe_summary <- function(data) {
#   # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variables
#   data_name <- deparse(substitute(data))
#   model_pe <-
#     stats::glm(
#       log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct * high_pressure,
#       # log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct * high_pressure,
#       #log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct,
#       data = data |>
#         dplyr::mutate(
#           Pressure = as.numeric(Pressure),
#           DP = as.numeric(DP),
#           # print_type = as.factor(print_type),
#           # print_type = case_when(print_type == 'Coin cell' ~ 0,
#           #                        print_type == 'PCB print' ~ 1),
#           # print_type = as.numeric(print_type) |> as.factor()) |>
#           filter(print_type == 'PCB print'),
#           family = gaussian(link = 'identity')
#         )
#     )
#       #results <- data.table::data.table(anova_pe[[1]])
#       residuals_pe <- stats::residuals(model_pe)
#       stats <- model_pe |>
#         gtsummary::tbl_regression() |>
#         gtsummary::as_gt() |>
#         gt::tab_source_note(gt::md(glue::glue(
#           'From {data_name}'
#         )))
#       print(summary(model_pe))
#       print(stats)
#       #generates a qq-plot of the residuals (determine model performance with real values)
#       qq_plot <- ggpubr::ggqqplot(residuals_pe,
#                                   xlab = 'Sample Quantile',
#                                   ylab = 'Theoretical Quantile')
#
#       glm_plot <-
#         ggiraphExtra::ggPredict(model_pe)
#       ggplot2::ggsave(
#         qq_plot,
#         filename = glue::glue(
#           '{folder_destination}2304-{data_name}_moisture_QQresidual_plot.png'
#         ),
#         dpi = 600,
#         width = 4,
#         height = 3,
#         bg = 'white'
#       )
#       return(model_pe)
# }

# function to save plots for glm models with just print type, additive% and salt%
lm_pe_summary <- function(data) {
  # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variables
  data_name <- deparse(substitute(data))
  model_pe <-
    stats::glm(
      #log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity,
      #log_IC ~ print_type + fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity |> as.numeric(),
      log_IC ~ print_type + fcomp_additive_wt_pct + fcomp_salt_wt_pct,
      data = data |>
        dplyr::mutate(
          print_type = as.factor(print_type),
          log_IC = log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
        ),
      family = gaussian(link = 'identity')
    )
  #results <- data.table::data.table(anova_pe[[1]])
  residuals_pe <- stats::residuals(model_pe)
  
  # predict the model with the data
  preds <-
    stats::predict(model_pe, data, type = 'response', se.fit = TRUE)
  data$fit <- preds$fit
  data$se <- preds$se
  
  predplot <- ggplot2::ggplot(
    data |>
      dplyr::mutate(
        print_type = as.factor(print_type),
        log_IC = log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
      ),
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = log_IC,
      group = print_type,
      color = print_type
    )
  ) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        y = preds$fit,
        ymin = fit - 1.96 * preds$se,
        ymax = fit + 1.96 * preds$se
      ),
      fill = 'gray',
      alpha = 0.5
    ) +
    ggplot2::geom_line(ggplot2::aes(y = preds$fit),
                       size = 0.75) +
    ggplot2::facet_wrap(. ~ fcomp_additive_wt_pct, nrow = 5, ncol = 5)
  
  print(predplot)
}
  
  
  #print(pred_plot |> plot(show_data = TRUE))
  
  #generates a qq-plot of the residuals (determine model performance with real values)
  #   qq_plot <- ggpubr::ggqqplot(residuals_pe,
  #                               xlab = 'Sample Quantile',
  #                               ylab = 'Theoretical Quantile')
  #   # outputs regression statistics
  #   stats <- model_pe |>
  #     gtsummary::tbl_regression() |>
  #     gtsummary::as_gt() |>
  #     gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  #   glm_plot <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_salt_wt_pct', 'fcomp_additive_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #   glm_plot2 <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_additive_wt_pct', 'fcomp_salt_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #
  #   summary(model_pe) |> print()
  #
  #   results_df <-
  #     broom::tidy(model_pe)
  #   summary_df <-
  #     broom::glance(model_pe)
  #
  #   # save the results of the models
  #   arrow::write_csv_arrow(results_df,
  #                          sink = glue::glue('{folder_destination}2304-{data_name}_LMcoef_results.csv'))
  #
  #
  #   arrow::write_csv_arrow(summary_df,
  #                          sink = glue::glue('{folder_destination}2304-{data_name}_LMan_results.csv'))
  #   # saves plot
  #   ggplot2::ggsave(
  #     qq_plot,
  #     filename = glue::glue(
  #       '{folder_destination}2304-{data_name}_LM_QQresidual_plot.png'
  #     ),
  #     dpi = 600,
  #     width = 4,
  #     height = 3,
  #     bg = 'white'
  #   )
  #   return(model_pe)
  # }
  #
  # lm_sum <- lm_pe_summary(final_db_outlier)
  
  # glm_pe_summary_inv <- function(data, degree = 1) {
  #   # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variable
  #   data_name <- deparse(substitute(data))
  #   model_pe <-
  #     stats::glm(
  #       #log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity,
  #       #log_IC ~ print_type + fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity |> as.numeric(),
  #       log_IC ~ print_type * fcomp_additive_wt_pct * poly(fcomp_salt_wt_pct, degree),
  #       data = data |>
  #         dplyr::mutate(
  #           print_type = as.factor(print_type),
  #           log_IC = -log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
  #         ),
  #       family = Gamma(link = 'identity')
  #     )
  #   #results <- data.table::data.table(anova_pe[[1]])
  #   residuals_pe <- stats::residuals(model_pe)
  #   #generates a qq-plot of the residuals (determine model performance with real values)
  #   qq_plot <- ggpubr::ggqqplot(residuals_pe,
  #                               xlab = 'Sample Quantile',
  #                               ylab = 'Theoretical Quantile')
  #   # outputs regression statistics
  #   stats <- model_pe |>
  #     gtsummary::tbl_regression() |>
  #     gtsummary::as_gt() |>
  #     gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  #   glm_plot <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_salt_wt_pct', 'fcomp_additive_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #   glm_plot2 <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_additive_wt_pct', 'fcomp_salt_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #   
  #   summary(model_pe) |> print()
  #   
  #   
  #   
  #   results_df <-
  #     broom::tidy(model_pe)
  #   summary_df <-
  #     broom::glance(model_pe)
  #   
  #   # save the results of the models
  #   arrow::write_csv_arrow(
  #     results_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GLMcoef_degree_{degree}_results.csv'
  #     )
  #   )
  #   
  #   
  #   arrow::write_csv_arrow(
  #     summary_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GLMan_degree_{degree}_results.csv'
  #     )
  #   )
  #   
  #   # saves plot
  #   ggplot2::ggsave(
  #     qq_plot,
  #     filename = glue::glue(
  #       '{folder_destination}2304-{data_name}_degree-{degree}_QQresidual_plot.png'
  #     ),
  #     dpi = 600,
  #     width = 4,
  #     height = 3,
  #     bg = 'white'
  #   )
  #   return(model_pe)
  # }
  # 
  # # function to save plots for glm models with just print type, additive% and salt%
  # glm_pe_summary <- function(data, degree = 1) {
  #   # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variable
  #   data_name <- deparse(substitute(data))
  #   model_pe <-
  #     stats::glm(
  #       #log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity,
  #       #log_IC ~ print_type + fcomp_additive_wt_pct * fcomp_salt_wt_pct * humidity |> as.numeric(),
  #       log_IC ~ print_type * fcomp_additive_wt_pct * poly(fcomp_salt_wt_pct, degree),
  #       data = data |>
  #         dplyr::mutate(
  #           print_type = as.factor(print_type),
  #           log_IC = -log10(ionic_conductivity_final) |> as.numeric() # negative to allow usage of Gamma link function
  #         ),
  #       family = Gamma(link = 'identity')
  #     )
  #   #results <- data.table::data.table(anova_pe[[1]])
  #   residuals_pe <- stats::residuals(model_pe)
  #   
  #   # predict the model with the data
  #   # pred_plot <- ggeffects::ggpredict(model_pe)
  #   #
  #   # print(pred_plot |> plot(show_data = TRUE))
  #   
  #   #generates a qq-plot of the residuals (determine model performance with real values)
  #   qq_plot <- ggpubr::ggqqplot(residuals_pe,
  #                               xlab = 'Sample Quantile',
  #                               ylab = 'Theoretical Quantile')
  #   # outputs regression statistics
  #   stats <- model_pe |>
  #     gtsummary::tbl_regression() |>
  #     gtsummary::as_gt() |>
  #     gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  #   # glm_plot <-
  #   #   ggeffects::ggpredict(model_pe,
  #   #                        c('fcomp_salt_wt_pct', 'fcomp_additive_wt_pct')) |> plot(rawdata = TRUE,
  #   #                                                                                 facets = TRUE)
  #   # glm_plot2 <-
  #   #   ggeffects::ggpredict(model_pe,
  #   #                        c('fcomp_additive_wt_pct', 'fcomp_salt_wt_pct')) |> plot(rawdata = TRUE,
  #   #                                                                                 facets = TRUE)
  #   
  #   summary(model_pe) |> print()
  #   
  #   # defines the predictions in the df
  #   preds <-
  #     stats::predict.glm(model_pe, data, type = 'response', se.fit = TRUE)
  #   data$fit <- preds$fit
  #   data$se <- preds$se
  #   
  #   predplot <- ggplot2::ggplot(
  #     data |>
  #       dplyr::mutate(
  #         print_type = as.factor(print_type),
  #         log_IC = -log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
  #       ),
  #     ggplot2::aes(
  #       x = fcomp_additive_wt_pct,
  #       y = log_IC,
  #       group = print_type,
  #       color = print_type
  #     )
  #   ) +
  #     ggplot2::geom_point(size = 1) +
  #     ggplot2::geom_ribbon(
  #       ggplot2::aes(
  #         y = preds$fit,
  #         ymin = fit - 1.96 * preds$se,
  #         ymax = fit + 1.96 * preds$se
  #       ),
  #       fill = 'gray',
  #       alpha = 0.5
  #     ) +
  #     ggplot2::geom_line(ggplot2::aes(y = preds$fit),
  #                        size = 0.75) +
  #     ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct, nrow = 5, ncol = 5)
  #   
  #   print(predplot)
  #   
  #   results_df <-
  #     broom::tidy(model_pe)
  #   summary_df <-
  #     broom::glance(model_pe)
  #   
  #   # save the results of the models
  #   arrow::write_csv_arrow(
  #     results_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GLMcoef_degree_{degree}_results.csv'
  #     )
  #   )
  #   
  #   
  #   arrow::write_csv_arrow(
  #     summary_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GLMan_degree_{degree}_results.csv'
  #     )
  #   )
  #   
  #   # saves plot
  #   ggplot2::ggsave(
  #     qq_plot,
  #     filename = glue::glue(
  #       '{folder_destination}2304-{data_name}_degree-{degree}_QQresidual_plot.png'
  #     ),
  #     dpi = 600,
  #     width = 4,
  #     height = 3,
  #     bg = 'white'
  #   )
  #   return(model_pe)
  # }
  # 
  # glm_deg1 <- glm_pe_summary(final_db_outlier, degree = 1)
  # glm_deg2 <- glm_pe_summary(final_db_outlier, degree = 2)
  # 
  # # gams
  # gam_pe_summary <- function(data) {
  #   # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variables
  #   data_name <- deparse(substitute(data))
  #   model_pe <-
  #     mgcv::gam(
  #       #log_IC ~ s(fcomp_additive_wt_pct, bs="cc", by = print_type |> as.factor()) + s(fcomp_salt_wt_pct, bs="cc", by = print_type |> as.factor()),
  #       log_IC ~ s(
  #         fcomp_additive_wt_pct,
  #         bs = 'cr',
  #         k = 10,
  #         by = print_type
  #       ) + s(
  #         fcomp_salt_wt_pct,
  #         bs = 'cr',
  #         k = 10,
  #         by = print_type
  #       ) + print_type + ti(fcomp_salt_wt_pct, fcomp_additive_wt_pct, k = 10),
  #       #log_IC ~ s(fcomp_additive_wt_pct, by = print_type |> as.factor(), k = 12) + s(fcomp_salt_wt_pct, by = print_type |> as.factor(), k = 10),
  #       data = data |>
  #         dplyr::mutate(
  #           log_IC = -log10(ionic_conductivity_final),
  #           # converting the values into positive so that Gamma function can work
  #           print_type = print_type |> as.factor()
  #         ),
  #       family = Gamma(link = 'identity'),
  #       method = 'REML'
  #     )
  #   #results <- data.table::data.table(anova_pe[[1]])
  #   residuals_pe <- mgcv::residuals.gam(model_pe)
  #   
  #   # predict the model with the data
  #   preds <-
  #     mgcv::predict.gam(model_pe, data, type = 'response', se.fit = TRUE)
  #   data$fit <- preds$fit
  #   data$se <- preds$se
  #   
  #   predplot1 <- ggplot2::ggplot(
  #     data |>
  #       dplyr::mutate(
  #         print_type = as.factor(print_type),
  #         log_IC = -log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
  #       ),
  #     ggplot2::aes(
  #       x = fcomp_salt_wt_pct,
  #       y = log_IC,
  #       group = print_type,
  #       color = print_type
  #     )
  #   ) +
  #     ggplot2::geom_point(size = 1) +
  #     ggplot2::geom_ribbon(
  #       ggplot2::aes(
  #         y = preds$fit,
  #         ymin = fit - 1.96 * preds$se,
  #         ymax = fit + 1.96 * preds$se
  #       ),
  #       fill = 'gray',
  #       alpha = 0.5
  #     ) +
  #     ggplot2::geom_line(ggplot2::aes(y = preds$fit),
  #                        size = 0.75) +
  #     ggplot2::facet_wrap(. ~ fcomp_additive_wt_pct,
  #                         nrow = 5,
  #                         ncol = 5)
  #   
  #   predplot2 <- ggplot2::ggplot(
  #     data |>
  #       dplyr::mutate(
  #         print_type = as.factor(print_type),
  #         log_IC = -log10(ionic_conductivity_final) # negative to allow usage of Gamma link function
  #       ),
  #     ggplot2::aes(
  #       x = fcomp_additive_wt_pct,
  #       y = log_IC,
  #       group = print_type,
  #       color = print_type
  #     )
  #   ) +
  #     ggplot2::geom_point(size = 1) +
  #     ggplot2::geom_ribbon(
  #       ggplot2::aes(
  #         y = preds$fit,
  #         ymin = fit - 1.96 * preds$se,
  #         ymax = fit + 1.96 * preds$se
  #       ),
  #       fill = 'gray',
  #       alpha = 0.5
  #     ) +
  #     ggplot2::geom_line(ggplot2::aes(y = preds$fit),
  #                        size = 0.75) +
  #     ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct, nrow = 5, ncol = 5)
  #   
  #   print(predplot2)
  #   
  #   #generates a qq-plot of the residuals (determine model performance with real values)
  #   qq_plot <- ggpubr::ggqqplot(residuals_pe,
  #                               xlab = 'Sample Quantile',
  #                               ylab = 'Theoretical Quantile')
  #   # outputs regression statistics
  #   stats <- model_pe |>
  #     gtsummary::tbl_regression() |>
  #     gtsummary::as_gt() |>
  #     gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  #   glm_plot <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_salt_wt_pct', 'fcomp_additive_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #   glm_plot2 <-
  #     ggeffects::ggpredict(model_pe,
  #                          c('fcomp_additive_wt_pct', 'fcomp_salt_wt_pct')) |> plot(rawdata = TRUE,
  #                                                                                   facets = TRUE)
  #   
  #   results_df <-
  #     broom::tidy(model_pe)
  #   summary_df <-
  #     broom::glance(model_pe)
  #   
  #   summary(model_pe) |> print()
  #   # saves plot
  #   ggplot2::ggsave(
  #     qq_plot,
  #     filename = glue::glue(
  #       '{folder_destination}2304-{data_name}_gam_QQresidual_plot.png'
  #     ),
  #     dpi = 600,
  #     width = 4,
  #     height = 3,
  #     bg = 'white'
  #   )
  #   
  #   # save the results of the models
  #   arrow::write_csv_arrow(
  #     results_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GAMcoef_results.csv'
  #     )
  #   )
  #   # save the results of the models
  #   arrow::write_csv_arrow(
  #     results_df,
  #     sink = glue::glue(
  #       '{folder_destination}2304-{data_name}_GAMan_results.csv'
  #     )
  #   )
  #   
  #   return(model_pe)
  # }
  # 
  # test <- gam_pe_summary(final_db_outlier)
  # # SW of the residuals
  # 
  # # # {
  # #   summary_table <-
  # #     tibble::tibble(
  # #       # moist_statistic = stats::shapiro.test(glm_moisture_pe_summary(mergedTab2_10))$statistic,
  # #       # moist_p.value = stats::shapiro.test(glm_moisture_pe_summary(mergedTab2_10))$p.value,
  # #       statistic = stats::shapiro.test(glm_pe_summary(final_db_outlier))$statistic,
  # #       p.value = stats::shapiro.test(glm_pe_summary(final_db_outlier))$p.value,
  # #       fcomp_salt_wt_pct = c(0, 20, 30, 10)
  # #     )
  # 
  # #stats::shapiro.test(data = mergedTab2, grouping = 'print_type', response = 'log_IC')
  # 
  # anova_pe_log <-
  #   stats::aov(
  #     log10(ionic_conductivity_final) ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct * sample_ID,
  #     data = mergedTab2
  #   )
  # summary(anova_pe_log)
  # residuals_pe_log <- stats::residuals(anova_pe_log)
  # stats::shapiro.test(residuals_pe_log)
  # plot(residuals_pe_log)
  # 
  # ggpubr::ggqqplot(
  #   data = mergedTab2 |> mutate(fcomp_additive_wt_pct = as.factor(fcomp_additive_wt_pct)),
  #   x = 'ionic_conductivity_final',
  #   color = 'fcomp_additive_wt_pct'
  # ) +
  #   theme(legend.position = 'none') +
  #   facet_wrap(fcomp_salt_wt_pct ~ ., scales = 'free')
  # ggpubr::ggqqplot(
  #   data = mergedTab2 |> mutate(
  #     fcomp_additive_wt_pct = as.factor(fcomp_additive_wt_pct),
  #     log_IC = log10(ionic_conductivity_final)
  #   ),
  #   x = 'ionic_conductivity_final',
  #   color = 'fcomp_additive_wt_pct'
  # ) +
  #   theme(legend.position = 'none') +
  #   facet_wrap(fcomp_salt_wt_pct ~ ., scales = 'free')
  #
  # TUKEY <- stats::TukeyHSD(x = anova_pe_log, conf.level = 0.95)
  #
  # plot(TUKEY)
  
  # bland-altman plot
  {
    #df_pick <- final_db
    df_pick <- final_db_outlier
    
    ba_plot_tab1 <- select_df(df_pick, type = 'coin') |>
      dplyr::group_by(fcomp_formulation) |>
      dplyr::mutate(avg_gold = mean(ionic_conductivity_final)) |>
      dplyr::distinct(fcomp_formulation, avg_gold)
    
    ba_plot_tab2 <- select_df(df_pick, type = 'PCB') |>
      dplyr::group_by(fcomp_formulation) |>
      dplyr::mutate(avg_pcb = mean(ionic_conductivity_final)) |>
      dplyr::distinct(fcomp_formulation, avg_pcb)
    
    ba_plot_tab <- ba_plot_tab1 |>
      dplyr::full_join(ba_plot_tab2) |>
      dplyr::mutate(avg = mean(avg_gold, avg_pcb),
                    diff = avg_gold - avg_pcb) |>
      dplyr::filter(!is.na(avg_gold) == TRUE)
    
    mean_diff <- mean(ba_plot_tab$diff)
    lower <- mean_diff - 1.96 * stats::sd(ba_plot_tab$diff)
    upper <- mean_diff + 1.96 * stats::sd(ba_plot_tab$diff)
    
    p <-
      ggplot(ba_plot_tab, aes(x = avg, y = diff, key = fcomp_formulation)) +
      geom_point(size = 2) +
      geom_hline(yintercept = mean_diff) +
      geom_hline(yintercept = lower,
                 color = "red",
                 linetype = "dashed") +
      geom_hline(yintercept = upper,
                 color = "red",
                 linetype = "dashed") +
      ggtitle("Bland-Altman Plot") +
      ylab("Difference Between Instruments") +
      xlab("Average") +
      theme_bw()
    
    plotly::ggplotly(p,
                     tooltip = c('fcomp_formulation'))
    }
  
  compare_line_plot(
    final_db_outlier |>
      dplyr::filter(
        !fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.9, 7.1),!fcomp_additive == 'AlO3'
      ),
    c(0, 10, 20, 30)
  )
  
  # bland-altman plot for magnitude
  {
    filter_salt_range <- c(10, 0, 20, 30)
    #dataset pick,
    #df_pick <- final_db
    
    ba_plot_tab1_log <-
      select_df(final_db_outlier, type = 'coin') |>
      # removes the cross formulations
      dplyr::filter(
        fcomp_salt_wt_pct %in% filter_salt_range,!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.9, 7.1)
      ) |>
      dplyr::mutate(
        fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation,
                                                            '4.28',
                                                            '4.29')
      ) |>
      dplyr::group_by(fcomp_formulation) |>
      dplyr::filter(fab_method == 2) |>
      dplyr::mutate(
        avg_gold_log = mean(log10(ionic_conductivity_final)),
        avg_gold = mean(ionic_conductivity_final),
        cv_gold_log = goeveg::cv(log10(ionic_conductivity_final)),
        cv_gold = goeveg::cv(ionic_conductivity_final)
      ) |>
      dplyr::distinct(
        fcomp_formulation,
        avg_gold,
        avg_gold_log,
        fcomp_salt_wt_pct,
        fcomp_additive_wt_pct,
        cv_gold,
        cv_gold_log
      )
    
    ba_plot_tab2_log <-
      select_df(final_db_outlier, type = 'PCB') |>
      # removes the cross formulations
      dplyr::filter(
        fcomp_salt_wt_pct %in% filter_salt_range,!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.86, 7.14)
      ) |>
      dplyr::group_by(fcomp_formulation) |>
      dplyr::mutate(
        avg_pcb_log = mean(log10(ionic_conductivity_final)),
        avg_pcb = mean(ionic_conductivity_final),
        cv_pcb_log = goeveg::cv(log10(ionic_conductivity_final)),
        cv_pcb = goeveg::cv(ionic_conductivity_final)
      ) |>
      dplyr::distinct(
        fcomp_formulation,
        avg_pcb,
        avg_pcb_log,
        fcomp_salt_wt_pct,
        fcomp_additive_wt_pct,
        cv_pcb,
        cv_pcb_log
      )
    
    ba_plot_tab_log <- ba_plot_tab1_log |>
      dplyr::full_join(ba_plot_tab2_log) |>
      dplyr::mutate(
        avg = mean(avg_gold, avg_pcb),
        avg_log = mean(avg_gold_log, avg_pcb_log),
        diff = avg_gold - avg_pcb,
        diff_log = avg_gold_log - avg_pcb_log
      ) |>
      dplyr::filter(!is.na(avg_gold) == TRUE) |>
      #removes the 30% salt wt pct
      dplyr::filter(fcomp_salt_wt_pct %in% filter_salt_range)
    # adds a label to show how many samples per method
    filtered_salt <- ba_plot_tab_log |>
      dplyr::filter(fcomp_salt_wt_pct %in% filter_salt_range)
    
    # calculates the mean of the log of the differences between the two methods
    mean_diff_log <-
      signif(mean(ba_plot_tab_log$diff_log), digits = 2)
    std_diff_log <-
      signif(stats::sd(ba_plot_tab_log$diff_log), digits = 2)
    
    # calculates the mean of the differences between the two methods
    mean_diff <- mean(ba_plot_tab_log$diff)
    
    # calculates the standard error
    
    # calculates the limits of agreements (low)
    lower_log <-
      signif((mean_diff_log - 1.96 * stats::sd(ba_plot_tab_log$diff_log)), digits = 2)
    
    # calculates the limits of agreements (high)
    upper_log <-
      signif((mean_diff_log + 1.96 * stats::sd(ba_plot_tab_log$diff_log)), digits = 2)
    
    
    lower <-
      mean_diff - 1.96 * stats::sd(ba_plot_tab_log$diff)
    upper <-
      mean_diff + 1.96 * stats::sd(ba_plot_tab_log$diff)
    
    # shapiro-wilk test for testing difference normality
    shap_test <- shapiro.test(filtered_salt$diff_log)
    shap_test_p <- shap_test$p.value |> signif(digits = 2)
    shap_test_w <- shap_test$statistic |> signif(digits = 2)
    
    bland_altman_plot_log <-
      ggplot2::ggplot(
        ba_plot_tab_log,
        ggplot2::aes(
          x = avg_log,
          y = diff_log,
          key = fcomp_formulation,
          color = fcomp_salt_wt_pct |> as.factor()
        )
      ) +
      ggplot2::geom_point(size = 2,
                          alpha = 0.8) +
      cowplot::theme_half_open() +
      # cowplot::background_grid() +
      ggplot2::geom_hline(yintercept = mean_diff_log,
                          color = 'blue') +
      ggplot2::geom_hline(yintercept = lower_log,
                          color = "red",
                          linetype = "dashed") +
      ggplot2::geom_hline(yintercept = upper_log,
                          color = "red",
                          linetype = "dashed") +
      ggplot2::annotate(
        'text',
        label = glue::glue('LoA = {upper_log}'),
        y = upper_log - 0.55 * max(range(ba_plot_tab_log$diff_log)),
        x = mean(range(ba_plot_tab_log$avg_log)),
        color = 'red'
      ) +
      ggplot2::annotate(
        'text',
        label = glue::glue('LoA = {lower_log}'),
        y = lower_log + max(range(ba_plot_tab_log$diff_log)) * 0.45,
        x = mean(range(ba_plot_tab_log$avg_log)),
        color = 'red'
      ) +
      ggplot2::annotate(
        'text',
        label = glue::glue('mean = {mean_diff_log}'),
        y = mean_diff_log * 0.70,
        x = mean(range(ba_plot_tab_log$avg_log)),
        color = 'blue'
      ) +
      #ggtitle("Bland-Altman Plot") +
      ggplot2::ylim(c(-1.5, 1)) +
      ggplot2::scale_color_viridis_d(option = 'plasma',
                                     limits = c(10, 30, 20, 0)) +
      ggplot2::guides(color = ggplot2::guide_legend(title = 'LiTFSI (wt%)')) +
      ggplot2::ylab(expression( ~ Delta ~ 'Log'[10] ~ 'Ionic Conductivity'['(Coin Cell - PCB)'])) +
      ggplot2::xlab(expression('Average Log'[10] ~ 'Ionic Conductivity')) +
      ggplot2::labs(title = 'e)',
                    color = 'LiTFSI wt%') +
      ggplot2::theme(
        legend.position = 'right',
        legend.title = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_text(size = 12),
        plot.title.position = 'plot',
        plot.title = ggplot2::element_text(
          size = 16,
          vjust = -6,
          hjust = -0.02
        ),
        plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
      ) +
      ggplot2::scale_color_viridis_d(limits = c('30', '20', '10', '0'))
    #facet_wrap(. ~ fcomp_salt_wt_pct)
    ggplot2::ggsave(
      bland_altman_plot_log,
      filename = glue::glue('{folder_destination}2304-10_pct_blandAltman_plot.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
    #bland_altman_plot <-
    ggplot2::ggplot(ba_plot_tab_log,
                    ggplot2::aes(x = avg,
                                 y = diff,
                                 key = fcomp_formulation)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_hline(yintercept = mean_diff) +
      ggplot2::geom_hline(yintercept = lower,
                          color = "red",
                          linetype = "dashed") +
      ggplot2::geom_hline(yintercept = upper,
                          color = "red",
                          linetype = "dashed") +
      ggplot2::annotate(
        'text',
        label = '+ 1.96 SD',
        y = upper_log - 0.15 * max(range(ba_plot_tab_log$diff)),
        x = mean(range(ba_plot_tab_log$avg)),
        color = 'red'
      ) +
      ggplot2::annotate(
        'text',
        label = '- 1.96 SD',
        y = lower_log + max(range(ba_plot_tab_log$diff)) * 0.55,
        x = mean(range(ba_plot_tab_log$avg)),
        color = 'red'
      ) +
      #ggtitle("Bland-Altman Plot") +
      ggplot2::ylim(c(-0.00004, 0.00004)) +
      ggplot2::scale_x_continuous(trans = 'log10') +
      ggplot2::ylab(expression( ~ Delta ~ 'S/cm')) +
      ggplot2::xlab(expression('Average Log'[10] ~ '(S/cm)')) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'none',
                     legend.text = ggplot2::element_text(size = 3))
    
    correlation_plot <-
      ggplot2::ggplot(data = ba_plot_tab_log,
                      ggplot2::aes(x = avg_gold_log,
                                   y = avg_pcb_log), ) +
      cowplot::theme_half_open() +
      cowplot::background_grid() +
      ggplot2::geom_point(size = 2) +
      ggpmisc::stat_poly_line(se = TRUE,
                              linetype = 'solid',
                              color = 'black',) +
      ggpmisc::stat_poly_eq(formula = y ~ x,
                            ggpmisc::use_label(c('eq', 'R2'))) +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 1,
        color = 'black',
        linetype = 'dashed'
      ) +
      # ggplot2::annotate(
      #   'text',
      #   label = glue::glue('W = {shap_test_w}, p.value = {shap_test_p}'),
      #   color = 'black',
      #   y = -5,
      #   x = -7.85
      # ) +
      ggplot2::ylab(expression('Avg PCB Log'[10] ~ 'IC')) +
      ggplot2::xlab(expression('Avg Coin Cell Log'[10] ~ 'IC')) +
      ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct)
    
    ggplot2::ggsave(
      correlation_plot,
      filename = glue::glue('{folder_destination}2304-DemingRegression_plot.png'),
      dpi = 600,
      width = 8,
      height = 6,
      bg = 'white'
    )
    
    dem_IC <- SimplyAgree::dem_reg(
      x = 'avg_gold_log',
      y = 'avg_pcb_log',
      data = ba_plot_tab_log,
      error.ratio = 4,
      weighted = FALSE
    )
    
    plot(dem_IC)
    dem_IC_check <- SimplyAgree::check(dem_IC) +
      ggplot2::theme(title = ggplot2::element_text(size = 6))
    ggplot2::ggsave(
      dem_IC_check,
      filename = glue::glue('{folder_destination}2304-demIC_check.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
    
    
    qq_log_trans <- ggpubr::ggqqplot(data = ba_plot_tab_log,
                                     x = 'diff_log',
                                     group = '') +
      ggplot2::xlab('Theoretical Quantiles') +
      ggplot2::ylab('Sample Quantiles')
    
    ggplot2::ggsave(
      qq_log_trans,
      filename = glue::glue('{folder_destination}2304-LogTransIC_diffQQ_plot.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
    
    qq_IC <- ggpubr::ggqqplot(data = ba_plot_tab_log,
                              x = 'diff') +
      ggplot2::xlab('Theoretical Quantiles') +
      ggplot2::ylab('Sample Quantiles')
    
    ggplot2::ggsave(
      qq_IC,
      filename = glue::glue('{folder_destination}2304-IC_diffQQ_plot.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
  }
  
  # paired t-test (variables from bland-altman plot) and wilcox test
  {
    # change the values of salt wt% here
    filter_salt_range <- c(10, 0, 30, 20)
    # dataset pick
    
    # apply filter based on range
    filtered_salt <- ba_plot_tab_log |>
      dplyr::filter(fcomp_salt_wt_pct %in% filter_salt_range)
    # test for normality
    shap_test <- shapiro.test(filtered_salt$diff_log)
    # apply paired t-test to compare between two methods
    t_test_result <-
      stats::t.test(filtered_salt$avg_gold, filtered_salt$avg_pcb, paired = TRUE)
    print(t_test_result)
    # apply independent t-test to compare between two methods
    indt_test_result <-
      stats::t.test(filtered_salt$avg_gold_log,
                    filtered_salt$avg_pcb_log,
                    paired = FALSE)
    print(indt_test_result)
    # performs wilcoxon signed-rank test
    wilcox_test_result <-
      stats::wilcox.test(filtered_salt$avg_gold, filtered_salt$avg_pcb, paired = TRUE)
    print(wilcox_test_result)
    # performs mann-whitney test
    wilcox_test_result <-
      stats::wilcox.test(filtered_salt$avg_gold, filtered_salt$avg_pcb, paired = FALSE)
    print(wilcox_test_result)
    #car::leveneTest(c(filtered_salt$avg_gold, filtered_salt$avg_pcb))
    # visualize the avg_gold and avg_pcb against each other
    df_plot <- ggplot2::ggplot(data = filtered_salt,
                               aes(x = avg_gold,
                                   y = avg_pcb,)) +
      #color = fcomp_additive_wt_pct |> as.factor())) +
      cowplot::theme_cowplot() +
      ggplot2::geom_point(size = 1)  +
      ggplot2::geom_smooth(formula = y ~ x,
                           method = lm) +
      ggpmisc::stat_poly_eq(ggpmisc::use_label(c("R2"))) +
      ggplot2::theme(
        legend.position = 'none',
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10)
      ) +
      ggplot2::labs(
        x = expression('Average IC'['Coin'] * '(' ~ 'S' ~ '/cm)'),
        y = expression('Average IC'['PCB'] * '('  ~ 'S' ~ '/cm)')
      )
    
    ggplot2::ggsave(
      df_plot,
      filename = glue::glue('{folder_destination}2304-PCBvsCC_correlation.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
    
    df_plot <-
      ggplot2::ggplot(data = filtered_salt,
                      aes(x = avg_gold,
                          y = avg_pcb)) +
      cowplot::theme_cowplot() +
      geom_point(size = 1)  +
      geom_smooth(formula = y ~ x,
                  method = lm) +
      ggpmisc::stat_poly_eq(ggpmisc::use_label(c("R2"))) +
      theme(
        legend.position = 'none',
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10)
      ) +
      labs(
        x = expression('Average IC'['Coin'] * '(' ~ 'S' ~ '/cm)'),
        y = expression('Average IC'['PCB'] * '('  ~ 'S' ~ '/cm)')
      )
    
    ggplot2::ggsave(
      df_plot,
      filename = glue::glue('{folder_destination}2304-PCBvsCC_correlation.png'),
      dpi = 600,
      width = 4,
      height = 3,
      bg = 'white'
    )
  }
  
  # generates QQ plot
  {
    filter_cond <- c(0, 10, 20, 30)
    pubr_plot_log <- ggpubr::ggqqplot(
      df_pick |>
        dplyr::mutate(IC_log = log10(ionic_conductivity_final)) |>
        dplyr::filter(
          print_type == 'PCB print',!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 2.9, 7.1)
        ),
      x = 'IC_log',
      color = 'fcomp_formulation',
      #group = 'fcomp_salt_wt_pct',
      #labels = 'date_printed'
    ) +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      facet_grid(fcomp_additive_wt_pct |> as.factor() ~ fcomp_salt_wt_pct |> as.factor(),
                 scales = 'free') +
      ggplot2::theme(axis.text =
                       ggplot2::element_text(size = 8),
                     legend.position = 'none')
    
    ggplot2::ggsave(
      pubr_plot_log,
      filename = glue::glue('{folder_destination}2309-QQplot_log.png'),
      dpi = 600,
      width = 8,
      height = 6,
      bg = 'white'
    )
    
    pubr_plot <- ggpubr::ggqqplot(
      df_pick |>
        dplyr::mutate(IC = ionic_conductivity_final) |>
        dplyr::filter(
          print_type == 'PCB print',!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 2.9, 7.1)
        ),
      x = 'IC',
      color = 'fcomp_formulation',
      #group = 'fcomp_salt_wt_pct',
      #labels = 'date_printed'
    ) +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      facet_grid(fcomp_additive_wt_pct |> as.factor() ~ fcomp_salt_wt_pct |> as.factor(),
                 scales = 'free') +
      ggplot2::theme(axis.text =
                       ggplot2::element_text(size = 8),
                     legend.position = 'none')
    
    ggplot2::ggsave(
      pubr_plot,
      filename = glue::glue('{folder_destination}2309-QQplot.png'),
      dpi = 600,
      width = 8,
      height = 6,
      bg = 'white'
    )
    
    #plotly::ggplotly(pubr_plot)
  }
  # generated QQ plot from outlier removed
  # {
  #   filter_cond <- c(10, 20, 30)
  #   pubr_plot <- ggpubr::ggqqplot(
  #     final_db_outlier |>
  #       dplyr::mutate(IC_log = log10(ionic_conductivity_final)) |>
  #       dplyr::filter(fcomp_salt_wt_pct %in% filter_cond) |>
  #       dplyr::filter(outlier_label == 'NO'),
  #     x = 'IC_log',
  #     color = 'print_type',
  #     group = 'fcomp_salt_wt_pct',
  #     labels = 'date_printed'
  #   ) +
  #     facet_wrap(fcomp_salt_wt_pct |> as.factor() ~ .,
  #                scales = 'free')
  #   print(pubr_plot)
  #   #plotly::ggplotly(pubr_plot)
  # }
  
  db_only_PCB10 <- final_db_outlier |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% 10,
      print_type == 'PCB print' |
        print_type == 'Bulk cast',
      date_printed != '2024-01-18'
    ) |>
    dplyr::mutate(print_type = stringr::str_replace(print_type, 'Bulk cast', 'PCB print')) |>
    dplyr::group_by(fcomp_formulation, date_printed) |>
    dplyr::mutate(
      avg_IC = log10(mean(ionic_conductivity_final)),
      avg = mean(ionic_conductivity_final),
      log_ionic = log10(ionic_conductivity_final)
    )
  
  ggplot2::ggplot(
    data = db_only_PCB10,
    ggplot2::aes(
      x = date_printed |> as.factor(),
      y = ionic_conductivity_final,
      group = date_printed |> as.factor(),
      color = date_printed |> as.factor()
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(. ~ fcomp_additive_wt_pct) +
    ggplot2::scale_y_continuous(trans = 'log10') +
    ggplot2::theme(legend.position = 'top')
  
  ggplot2::ggplot(
    data = db_only_PCB10 |>
      dplyr::distinct(avg_IC, fcomp_formulation, print_type, .keep_all = TRUE),
    ggplot2::aes(
      x = fcomp_additive_wt_pct,
      y = avg,
      group = date_printed |> as.factor(),
      color = date_printed |> as.factor()
    )
  ) +
    ggplot2::geom_line(size = 2,
                       alpha = 0.5) +
    ggplot2::scale_y_continuous(trans = 'log10') +
    ggplot2::geom_line(
      inherit.aes = F,
      size = 2,
      alpha = 0.55,
      color = 'black',
      data = select_df(df = final_db, type = 'coin') |>
        dplyr::filter(fcomp_salt_wt_pct %in% 10,
                      fab_method == c(2)) |>
        dplyr::group_by(fcomp_additive_wt_pct) |>
        dplyr::mutate(avg_IC = mean(ionic_conductivity_final)),
      ggplot2::aes(x = fcomp_additive_wt_pct,
                   y = avg_IC)
    ) +
    ggplot2::theme(legend.position = 'top')
  # {
  #   t_test_db10 <- db_only_PCB10 |>
  #     dplyr::select(avg_IC, fcomp_formulation, date_printed) |>
  #     dplyr::filter(!date_printed == '2024-01-18') |>
  #     dplyr::distinct(.keep_all = TRUE)
  #
  #   t_test_db10_1 <- t_test_db10 |>
  #     dplyr::ungroup() |>
  #     dplyr::filter(date_printed == '2024-07-02') |>
  #     dplyr::rename(avg_07022024 = avg_IC) |>
  #     dplyr::select(avg_07022024, fcomp_formulation)
  #
  #   t_test_db10_2 <- t_test_db10 |>
  #     dplyr::ungroup() |>
  #     dplyr::filter(date_printed == '2024-07-11') |>
  #     dplyr::rename(avg_07112024 = avg_IC) |>
  #     dplyr::select(avg_07112024, fcomp_formulation)
  #
  #   t_test_db_all <- t_test_db10_1 |>
  #     dplyr::full_join(t_test_db10_2)
  #
  #   t_test_result <-
  #     stats::t.test(t_test_db_all$avg_07022024,
  #                   t_test_db_all$avg_07112024,
  #                   paired = FALSE)
  #   print(t_test_result)
  #   }
  
  car::leveneTest(
    log_ionic ~ interaction(fcomp_formulation, date_printed),
    data = db_only_PCB10 |>
      dplyr::filter(!date_printed == '2024-01-18') |>
      dplyr::mutate(log_ionic = log10(ionic_conductivity_final))
  )
  
  
  ####
  # need to add normality between all measurements for each print type.
  # need to add ML (BO), i want to add linear regression/or glms to supplement the ML
  # story here is to show that the methods correlate enough to serve as a good proxy for coin cells
  ####
  
  # doing qqplot on all
  
  # no transformation
  
  ggpubr::ggqqplot(
    data = final_db_outlier |>
      dplyr::mutate(
        print_type = stringi::stri_replace_all_regex(print_type,
                                                     'Bulk cast',
                                                     'PCB print')
      ),
    x = 'ionic_conductivity_final',
    group = 'fcomp_formulation',
    color = 'print_type'
  )
  
  ggpubr::ggqqplot(
    data = final_db_outlier |>
      dplyr::mutate(
        print_type = stringi::stri_replace_all_regex(print_type,
                                                     'Bulk cast',
                                                     'PCB print'),
        log_ic = log10(ionic_conductivity_final)
      ) |>
      dplyr::filter(fcomp_salt_wt_pct %in% c(0, 10, 20, 30)),
    x = 'log_ic',
    group = 'fcomp_formulation',
    color = 'fcomp_formulation'
  ) +
    ggplot2::facet_grid(fcomp_salt_wt_pct ~ print_type) +
    ggplot2::theme(legend.position = 'none')
  
  ggpubr::ggqqplot(
    data = final_db_outlier |>
      dplyr::mutate(
        print_type = stringi::stri_replace_all_regex(print_type,
                                                     'Bulk cast',
                                                     'PCB print'),
        log_ic = log10(ionic_conductivity_final)
      ) |>
      dplyr::filter(fcomp_salt_wt_pct %in% c(0, 10, 20, 30)),
    x = 'ionic_conductivity_final',
    group = 'print_type',
    color = 'print_type'
  ) +
    ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct,
                        scales = 'free')
  
  ggpubr::ggqqplot(
    data = final_db_outlier |>
      dplyr::mutate(
        print_type = stringi::stri_replace_all_regex(print_type,
                                                     'Bulk cast',
                                                     'PCB print'),
        log_ic = log10(ionic_conductivity_final)
      ) |>
      dplyr::filter(fcomp_salt_wt_pct %in% c(0, 10, 20, 30)),
    x = 'log_ic',
    group = 'print_type',
    color = 'print_type'
  ) +
    ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct,
                        scales = 'free')
  
  ### process control effects
  
  df_process <- df_PE_combined |>
    dplyr::group_by(date_printed, fcomp_formulation, print_type) |>
    dplyr::mutate(
      log_IC = log10(ionic_conductivity_final),
      mean_logIC = mean(log_IC),
      corr_logIC = log_IC - mean_logIC,
      process_changes =
        dplyr::case_when(
          date_printed >= '2024-03-01' & print_type == 'PCB print' ~ 3,
          date_printed < '2024-03-01' &
            date_printed >= '2023-10-23' &
            print_type == 'PCB print' ~ 2,
          date_printed < '2023-12-01' &
            print_type == 'PCB print' ~ 1
        )
    ) |>
    dplyr::filter(corr_logIC < 5 & corr_logIC > -3,
                  date_printed > '2023-01-01',
                  print_type == 'PCB print')
  
  ggplot2::ggplot(
    data = df_process,
    ggplot2::aes(
      x = date_printed,
      y = corr_logIC,
      group = date_printed,
      color = process_changes |> as.factor()
    )
  ) +
    ggplot2::geom_boxplot(size = 0.2)
  
  ### plotly graph
  
  plotly_3Dplot(
    final_db_outlier,
    x = 'fcomp_salt_wt_pct',
    y = 'fcomp_additive_wt_pct',
    z = 'ionic_conductivity_final',
    color_group = 'print_type',
    x_axisTitle = 'LiTFSI (wt%)',
    y_axisTitle = 'Aerosil (wt%)',
    z_axisTitle = 'Ionic Conductivity (S/cm)',
    graphTitle = ''
  )
  
  #for travis
  
  
  