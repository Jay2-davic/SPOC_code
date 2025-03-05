#sodium data only

#loads OUN to replace user name
require(ggplot2)
require(tidyverse)
require(svDialogs)

user_name <-
  svDialogs::dlg_input("Enter your OUN", Sys.info()["user"])$res

version <-
  svDialogs::dlg_input("Enter version number", Sys.info()['2,27'])$res
# loads the functions
source('~/Git/23-spoc_code/PE_plotTemplates/2401-GridSearch_functions-jimenez45.R')
# loads the plotly plotter
source('~/Git/23-spoc_code/PE_plotTemplates/2310-PlotlyTemplate_PlotlyExport.R')

# output paper figures in the manuscript folder
folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2406-NaION_system\\figs\\'
)
# filters out sodium only formulations
db <-
  readr::read_csv(
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version}_FULL.csv'
    )
  ) |>
  dplyr::relocate(ionic_conductivity_final, .before = print_type) |>
  dplyr::filter(fcomp_salt == 'NaTFSI' |
                  fcomp_salt == 'none',
                date_printed >= '2023-10-23') |>
  dplyr::filter(
    stringr::str_detect(sample_ID, 'KR') == FALSE,
    date_printed != '2024-08-01',
    date_printed != '2024-08-05',
    date_printed != '2024-08-21',
    date_printed != '2024-05-09',
    date_printed != '2024-07-17',
    date_printed != '2024-06-27',
    date_printed != '2024-07-29',
    date_printed != '2024-08-16',
    date_printed != '2024-08-20',
    date_printed != '2024-07-22'
  ) |>
  dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type, 'Bulk cast', 'PCB print'))

final_db <- db |>
  # removes outliers outside 2 sigmas
  z_score_outlier_remove('coin', 3) |>
  dplyr::bind_rows(z_score_outlier_remove(db, 'PCB', 3)) |>
  dplyr::relocate(ionic_conductivity_final, .before = print_type)

readr::write_csv(final_db,
                 glue::glue('{folder_destination}\\2410-PE_NaTFSI_data.csv'))

#######

select_df <- function(df, type = 'coin or PCB') {
  filter_value <- dplyr::case_when(type == 'coin' ~ 'Coin cell',
                                   type == 'PCB' ~ 'PCB print')
  
  df %>%
    #dplyr::filter(fcomp_salt_wt_pct %in% c('0', '10', '20', '30')) %>%
    #dplyr::filter(fcomp_additive_wt_pct %in% c('0', '15', '2.1', '4.3', '6.4', '11', '13', '8.6')) %>%
    dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type,
                                                               'Bulk cast',
                                                               'PCB print')) %>%
    dplyr::filter(print_type == filter_value) %>%
    # assign unique value per formulation based on mpr files
    dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct) %>%
    dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(log_IC = log10(ionic_conductivity_final))
}

ggplot2::ggplot(
  select_df(final_db, 'PCB'),
  aes(
    x = fcomp_salt_wt_pct,
    y = ionic_conductivity_final,
    group = ionic_conductivity_final,
    color = ionic_conductivity_final
  )
) +
  geom_point(size = 4) +
  scale_y_continuous(trans = 'log10') +
  facet_wrap(fcomp_additive_wt_pct ~ .)

# function to select data points for paper
select_df <- function(df, type = 'coin or PCB') {
  filter_value <- dplyr::case_when(type == 'coin' ~ 'Coin cell',
                                   type == 'PCB' ~ 'PCB print')
  
  df |>
    dplyr::filter(fcomp_salt_wt_pct <= 20) |>
    dplyr::filter(fcomp_additive_wt_pct <= 10) |>
    dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type,
                                                               'Bulk cast',
                                                               'PCB print')) |>
    dplyr::mutate(
      fcomp_formulation = stringi::stri_replace_all_regex(
        fcomp_formulation,
        '0.5 PEGMEA, 0.5 PEGDA,',
        '0.9 PEGMEA, 0.1 PEGDA'
      )
    ) |>
    dplyr::filter(print_type == filter_value) |>
    # assign unique value per formulation based on mpr files
    dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct) |>
    dplyr::mutate(group_number = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(log_IC = log10(ionic_conductivity_final))
}

# function to remove outliers from boxplot results in a dataframe
boxplot_outlier_remove <- function(df, type) {
  select_df(df = df, type = type) |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    # log transform the outliers
    dplyr::mutate(log_ic = log10(ionic_conductivity_final)) |>
    # removes the outliers in the coin cell
    # dplyr::summarise(outlier = list(ionic_conductivity_final[ionic_conductivity_final < quantile(ionic_conductivity_final, 0.25) - 1.5 * IQR(ionic_conductivity_final) |
    #                                                            ionic_conductivity_final > quantile(ionic_conductivity_final, 0.75) + 1.5 * IQR(ionic_conductivity_final)])) |>
    dplyr::summarise(outlier = list(log_ic[log_ic < quantile(log_ic, 0.25) - 1.5 * IQR(log_ic) |
                                             log_ic > quantile(log_ic, 0.75) + 1.5 * IQR(log_ic)])) |>
    dplyr::ungroup() |>
    dplyr::left_join(select_df(df = df, type = type)) |>
    dplyr::mutate(outlier_label = ifelse(ionic_conductivity_final %in% unlist(outlier),
                                         'YES',
                                         "NO"))
}

# grid searching
gridSearch_plotter <- function(df, type) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = fcomp_additive_wt_pct,
      color = interaction(fcomp_salt_wt_pct, fcomp_additive_wt_pct),
      group = interaction(fcomp_salt_wt_pct, fcomp_additive_wt_pct)
    )
  ) +
    ggplot2::geom_jitter(
      size = 3,
      width = 0.1,
      height = 0.1,
      alpha = 1
    ) +
    cowplot::theme_cowplot() +
    cowplot::background_grid() +
    ggplot2::labs(x = 'NaTFSI (wt%)',
                  y = 'Aerosil 380 (wt%)', ) +
    # title = glue::glue('Grid Search for {type} Polyelectrolytes')) +
    ggplot2::theme(
      legend.position = 'none',
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_color_viridis_d(option = 'magma')
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue('{folder_destination}2304-Grid_Search_{type}.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

akima_plot <- function(akima_df, df, type) {
  plot <- akima_df |>
    dplyr::filter(!is.na(z)) |>
    tibble::tibble() |>
    ggplot2::ggplot(ggplot2::aes(
      x = x,
      y = y,
      z = z,
      fill = z,
      group = z
    )) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::geom_contour(color = 'blue', alpha = 0.5) +
    # ggplot2::scale_fill_gradientn(colors = c('purple', 'white', 'orange'),
    #                               na.value = 'black',
    #                               trans = 'log',
    #                               labels = custom_labels,
    #                               #limits = c(1e-8, 1e-4),
    #                               #breaks = c(1e-4, 1e-5, 1e-6, 1e-7, 1e-8)
    # ) +
    ggplot2::scale_fill_gradientn(
      colors = shades::saturation(
        paletteer::paletteer_c("scico::roma", n = 100),
        shades::delta(0.90)
      ),
      values = scales::rescale(c(-8.1, -8.0, -7.9, -7.8, -7.7, -7.6, -7.5, -7.4, -7.3, -7.2, -7.1, -7.0, -6.8, -6.6, -6.5, -6.4, -6.2, -6.0, -4.9), to = c(1,0)),  # Rescale midpoints to [0, 1]
      limits = c(-8, -4.9),  # Set limits for the fill scale
      na.value = 'black',
      breaks = c(-8, -7, -6, -5, -4, -3),
      labels = c(expression(10^-8),
                 expression(10^-7), 
                 expression(10^-6),
                 expression(10^-5),
                 expression(10^-4),
                 expression(10^-3)),
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        barwidth = ggplot2::unit(0.5, "cm"),
        # Narrow bar width (can adjust)
        barheight = 6,
        # Full plot height
        title = 'S/cm',
        display = 'gradient',
        title.position = "top",
        ticks = ggplot2::element_line(color = 'black')
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'NaTFSI (wt%)',
                  y = 'Ae380 (wt%)',
                  title = 'c) SPOC grid varying NaTFSI and Ae380 wt%', ) +
    cowplot::theme_half_open() +
    ggplot2::theme(
      legend.position = 'right',
      #legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12, ),
      axis.title = ggplot2::element_text(size = 12, ),
      axis.text = ggplot2::element_text(size = 12, ),
      plot.title = ggplot2::element_text(size = 12),
      plot.title.position = 'plot',
      # legend.key.size = ggplot2::unit(15, 'pt'),
      panel.border = ggplot2::element_rect(size = 2),
      legend.spacing.y = ggplot2::unit(10, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      # legend.spacing.x = ggplot2::unit(50, "pt"),
      legend.box.margin = ggplot2::margin(0, 0,-10,-10)
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0,-0.3),
      limits = c(.1, 20.1),
      labels = c(0, 10, 20),
      breaks = c(0.4, 10, 19.8)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0,-0.3),
      limits = c(-.01, 10.15),
      labels = c(0, 5, 10),
      breaks = c(0.4, 5, 9.8)
    ) + 
    ggforce::geom_circle(ggplot2::aes(
      x0 = 10, y0 = 3, r = 2.0),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red') + 
    ggforce::geom_circle(ggplot2::aes(
      x0 = 20, y0 = 3, r = 2.0),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red')
}
# look at the mean for each grid AKIMA METHOD
heatmap_mean <- function(df, type) {
  # performs linear interpolation on the mean
  akima_mean <-
    akima::interp2xyz(
      akima::interp(
        x = df$fcomp_salt_wt_pct,
        y = df$fcomp_additive_wt_pct,
        #z = df$ionic_conductivity_final,
        z = df$ionic_conductivity_final |> log10(),
        duplicate = 'mean',
        linear = TRUE
      ),
      data.frame = TRUE
    )
  
  plot_akima <- akima_plot(akima_mean, df, type)
  
  plot_points <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = fcomp_salt_wt_pct,
        y = fcomp_additive_wt_pct,
        #fill = 'black'
        )) +
        #ggplot2::geom_density(alpha = 0.5) +
        cowplot::theme_nothing() +
          ggplot2::geom_point(
            size = 1.5,
            alpha = 0.35,
            color = 'black',
            #shape = 1,
            fill = NA
          )

  legend <-
    cowplot::get_legend(plot_akima +
                          ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1)))
  
  plot_combined <-
    cowplot::ggdraw(plot_akima) +
    cowplot::draw_plot(plot) +
    cowplot::draw_plot(
      plot_points,
      x = 0.11,
      y = 0.15,
      width = 0.73,
      height = 0.76
    )
  plot_combined <-
    cowplot::plot_grid(
      plot_combined,
      legend,
      nrow = 1,
      ncol = 1,
      align = 'v'
    )
  
  ggplot2::ggsave(
    plot_combined,
    filename = glue::glue('{folder_destination}2304-Heatmap_{type}_Mean.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}


heatmap_median <- function(df, type) {
  # Calculates the in-between regions using linear interpolation
  median_final_db_akima <-
    akima::interp2xyz(
      akima::interp(
        x = df$fcomp_salt_wt_pct,
        y = df$fcomp_additive_wt_pct,
        #z = df$ionic_conductivity_final,
        z = df$log_IC,
        duplicate = 'median',
        linear = TRUE
      ),
      data.frame = TRUE
    )
  custom_labels <- function(x) {
    paste0("10^", format(x))
  }
  plot_akima <- akima_plot(median_final_db_akima, df)
  
  plot_points <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = fcomp_salt_wt_pct,
        y = fcomp_additive_wt_pct,
        #fill = 'black',
        )) +
        #ggplot2::geom_density(alpha = 0.5) +
        cowplot::theme_nothing() +
          ggplot2::geom_point(
            size = 2,
            alpha = 0.7,
            color = 'black',
            shape = 21,
            fill = NA
          )
  
  legend <-
    cowplot::get_legend(plot_akima +
                          ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1)))
  
  plot_combined <-
    cowplot::ggdraw(plot_akima) +
    #cowplot::draw_plot(plot) +
    cowplot::draw_plot(
      plot_points,
      x = 0.185,
      y = 0.185,
      width = 0.78,
      height = 0.60
    )
  plot_combined <-
    cowplot::plot_grid(
      plot_combined,
      legend,
      nrow = 1,
      ncol = 1,
      align = 'v'
    )
  
  ggplot2::ggsave(
    plot_combined,
    filename = glue::glue('{folder_destination}2304-Heatmap_{type}_median.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

# ggplot template for boxplots
boxplot_plotter_add <- function(df, type) {
  plot <- ggplot2::ggplot(
    # changes the column values to string literals for labeling
    data = df |> dplyr::mutate(fcomp_salt_wt_pct = glue::glue('NaTFSI {fcomp_salt_wt_pct} wt%')),
    ggplot2::aes(
      x = fcomp_additive_wt_pct |> as.factor(),
      y = ionic_conductivity_final,
      group = fcomp_additive_wt_pct,
      color = fcomp_additive_wt_pct
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    #cowplot::background_grid(major = 'y') +
    ggplot2::geom_boxplot(size = 0.3,
                          width = 0.6,
                          outlier.shape = NA) +
    #ggplot2::geom_point(size = 0.3) +
    ggplot2::facet_grid(. ~ fcomp_salt_wt_pct) +
    ggplot2::theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 6, angle = 50),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA)
    ) +
    #ggplot2::guide_legend(colorbar = ggplot2::guide_legend(title.vjust = -1)) +
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x)
        10 ^ x),
      labels = scales::trans_format('log10', scales::math_format(10 ^ .x)),
      limits = c(1e-8, 1e-3)
    ) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(x = 'Aerosil 380 (wt%)',
                  y = expression('Ionic Conductivity (S/cm)'),)
  #title = glue::glue('{type} Comparison'))
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue('{folder_destination}2304-{type}_varyAdd_boxplot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

# ggplot template for boxplots
boxplot_plotter_salt <- function(df, type) {
  plot <- ggplot2::ggplot(
    # changes the column values to string literals for labeling
    data = df |> dplyr::mutate(
      fcomp_additive_wt_pct = glue::glue('Aerosil 380 {fcomp_additive_wt_pct} wt%')
    ),
    aes(
      x = fcomp_salt_wt_pct |> as.factor(),
      y = ionic_conductivity_final,
      group = fcomp_salt_wt_pct,
      color = fcomp_salt_wt_pct
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    #cowplot::background_grid(major = 'y') +
    geom_boxplot(size = 0.3,
                 width = 0.6,
                 outlier.shape = NA) +
    #geom_point(size = 0.3) +
    facet_grid(. ~ fcomp_additive_wt_pct) +
    theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 6, angle = 50),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA)
    ) +
    scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x)
        10 ^ x),
      labels = scales::trans_format('log10', scales::math_format(10 ^ .x)),
      limits = c(1e-8, 1e-3)
    ) +
    scale_color_viridis_c() +
    labs(x = 'NaTFSI (wt%)',
         y = expression('Ionic Conductivity (S/cm)'))
  #title = glue::glue('{type} Comparison')
  
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue('{folder_destination}2304-{type}_varySalt_boxplot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

{
  final_db_outlier <-
    #boxplot_outlier_remove(final_db_weather, 'coin') |>
    dplyr::bind_rows(boxplot_outlier_remove(final_db, 'PCB')) |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    # log transform the outliers
    dplyr::mutate(log_ic = log10(ionic_conductivity_final)) |>
    dplyr::summarise(outlier = list(log_ic[log_ic < quantile(log_ic, 0.25) - 1.5 * IQR(log_ic) |
                                             log_ic > quantile(log_ic, 0.75) + 1.5 * IQR(log_ic)])) |>
    dplyr::ungroup() |>
    dplyr::left_join(final_db) |>
    dplyr::mutate(outlier_label = ifelse(outlier == 'numeric(0)',
                                         'NO',
                                         'YES')) |>
    dplyr::filter(outlier_label == 'NO',
                  date_printed >= '2024-03-04',
                  date_printed != '2024-07-17')
  
  
  readr::write_csv(final_db_outlier,
                   glue::glue('{folder_destination}/2410-PE_database.csv'))
  
  
  write.csv(
    final_db_outlier |>
      dplyr::filter(print_type == 'PCB print') |>
      dplyr::select(
        fcomp_formulation,
        fcomp_additive_wt_pct,
        fcomp_salt_wt_pct,
        ionic_conductivity_final
      ),
    glue::glue('{folder_destination}\\PE_MLtraining_v{version}_NaTFSI_coin.csv')
  )
  
}

select_df(
  df = final_db |>
    # filters only the NaTFSI samples that are 'good'
    dplyr::filter(
      fcomp_salt_wt_pct %in% c(0, 2.9, 5.7, 8.6, 11, 14, 17, 10, 20),
      fcomp_additive_wt_pct %in% c(0, 1.4, 2.9, 4.3, 5, 5.7, 7.1, 8.6, 10, 20)
    ),
  type = 'PCB'
) |>
  gridSearch_plotter('PCB')

# coin cell

# # select_df(df = final_db, type = 'coin') |>
# #   heatmap_mean('Coin Cell')
# select_df(df = final_db_outlier, type = 'coin') |>
#   heatmap_mean('Coin Cell')
# # select_df(df = final_db, type = 'coin') |>
# #   heatmap_median('Coin Cell')
# select_df(df = final_db_outlier, type = 'coin') |>
#   heatmap_median('Coin Cell')

# pcb boards
#select_df(df = final_db, type = 'PCB') |>
#heatmap_mean('PCB Print')
select_df(df = final_db, type = 'PCB') |>
  heatmap_mean('PCB Print')
#select_df(df = final_db, type = 'PCB') |>
#heatmap_median('PCB Print')
select_df(df = final_db, type = 'PCB') |>
  heatmap_median('PCB Print')

# plots the boxplot comparisons across formulations
# select_df(df = final_db, type = 'coin') |>
#   boxplot_plotter('Coin Cell')
# select_df(df = final_db, type = 'PCB') |>
#   boxplot_plotter('PCB Print')

# plots the boxplots after outlier removal
# select_df(df = final_db_outlier, type = 'coin') |>
#   boxplot_plotter('Coin Cell')

# plora the vertical screens
select_df(
  df = final_db |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% c(0, 10, 20),
      fcomp_additive_wt_pct %in% c(0, 1.4, 2.9, 4.3, 5.7, 7.1, 8.6, 10)
    ),
  type = 'PCB'
) |>
  boxplot_plotter_add('PCB Print')

select_df(
  df = final_db |>
    dplyr::filter(
      fcomp_additive_wt_pct %in% c(0, 5, 10),
      fcomp_salt_wt_pct %in% c(0, 2.9, 5.7, 8.6, 11, 14, 17, 20)
    ),
  type = 'PCB'
) |>
  boxplot_plotter_salt('PCB Print')
