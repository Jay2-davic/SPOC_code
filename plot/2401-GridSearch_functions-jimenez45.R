#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Digital Twins (SI)
#Subproject: Polyelectrolyte Screening
# loads the functions

# plot influence of atmospheric pressure
plot_influences <- function(df, stressor) {
  df <- df |>
    dplyr::filter(
      print_type == c('PCB print'),
      fcomp_salt_wt_pct %in% c(0, 10, 20, 30),
      #date_printed != '2024-03-29',
      fcomp_salt_wt_pct != 0,
    )
  
  plot <- ggplot2::ggplot(
    data = df |>
      dplyr::filter(
        print_type == c('PCB print'),
        fcomp_salt_wt_pct %in% c(0, 10, 20, 30),
        #date_printed != '2024-03-29',
        fcomp_salt_wt_pct != 0,
      ),
    ggplot2::aes(
      y = ionic_conductivity_final,
      x = {
        {
          stressor
        }
      } |> as.numeric(),
      #group = print_type |> as.factor(),
      color = 'black'
    )
  ) +
    cowplot::theme_cowplot() +
    ggplot2::geom_point(size = 0.5,
                        aes(color = 'black')) +
    ggplot2::geom_smooth(
      method = 'lm',
      formula = y ~ poly(x, 2),
      color = 'black'
    ) +
    ggpmisc::stat_poly_eq(ggpmisc::use_label(c("R2"))) +
    ggplot2::scale_y_continuous(trans = 'log10') +
    # geom_point(aes(y = ionic_conductivity_final,
    #                x = {{ stressor }} |> as.numeric(),
    #                # group = fcomp_salt_wt_pct |> as.factor(),
    #                color = 'black'
    #                )) +
    ggplot2::theme(
      axis.text = element_text(size = 8),
      #strip.text = element_text(size = 6),
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 6),
      strip.background = element_rect(color = 'black', fill = NA)
    )
}

# function to select data points for paper
select_df <- function(df, type = 'coin or PCB') {
  filter_value <- dplyr::case_when(type == 'coin' ~ 'Coin cell',
                                   type == 'PCB' ~ 'PCB print')
  
  df |>
    # dplyr::filter(fcomp_salt_wt_pct %in% c('0', '10', '20', '30')) |>
    # dplyr::filter(fcomp_additive_wt_pct %in% c('0', '15', '2.1', '4.3', '6.4', '11', '13', '8.6')) |>
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

z_score_outlier_remove <- function(df, type, threshold) {
  selected_df <- select_df(df = df, type = type)
  
  outlier_info <- selected_df |>
    # removes outliers on formulation information, print_type and the day of print
    dplyr::group_by(fcomp_formulation, print_type) |>
    # log transform response for stricter outlier removal
    dplyr::mutate(log_ic = log10(ionic_conductivity_final),
                  z_score = (log_ic - mean(log_ic))/ stats::sd(log_ic)) |>
    dplyr::filter(abs(z_score) <= threshold) |>
    dplyr::select(-z_score)
  
  return(outlier_info)
}
# comparing pcb with coin

compare_line_plot <- function(df, filter) {
  df <- df |>
    dplyr::mutate(fcomp_salt_wt_pct_lab = 
                    paste0(fcomp_salt_wt_pct, ' wt%')
    )
  filter_cond <- function(data) {
    data |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    dplyr::mutate(
      avg_IC = mean(log10(ionic_conductivity_final)),
      sd = sd(log10(ionic_conductivity_final)) |> as.numeric()
    )
  }
  
  filter_cond_alt <- function(data) {
    data |>
      dplyr::group_by(fcomp_formulation, print_type) |>
      dplyr::mutate(
        avg_IC = mean(log10(ionic_conductivity_final_alt)),
        sd = sd(log10(ionic_conductivity_final)) |> as.numeric()
      )
  }
  
  filter_cond_error <- function(data) {
    data |>
      dplyr::group_by(fcomp_formulation, print_type) |>
      dplyr::mutate(
        avg_IC = mean(log10(ionic_conductivity_final)),
        sd = sd(log10(ionic_conductivity_final)) / sqrt(length(log10(ionic_conductivity_final))) |> as.numeric(),
        t = stats::qt(1 - (0.10 / 2), length(log10(ionic_conductivity_final)) -
                        1),
        ci = t * sd
      )
  }
    
  filter_cond_error_alt <- function(data) {
    data |>
      dplyr::group_by(fcomp_formulation, print_type) |>
      dplyr::mutate(
        avg_IC = mean(log10(ionic_conductivity_final_alt)),
        sd = sd(log10(ionic_conductivity_final)) / sqrt(length(log10(ionic_conductivity_final))) |> as.numeric(),
        t = stats::qt(1 - (0.10 / 2), length(log10(ionic_conductivity_final)) -
                        1),
        ci = t * sd
      )
  }
  
  # filters PCB data
  pcb_filter <- select_df(df = df, type = 'PCB') |>
    dplyr::filter(fcomp_salt_wt_pct %in% filter) |>
    filter_cond()
  
  pcb_filter_alt <- select_df(df = df, type = 'PCB') |>
    dplyr::filter(fcomp_salt_wt_pct %in% filter) |>
    filter_cond_alt()
  
  coin_filter <- select_df(df = df, type = 'coin') |>
    dplyr::filter(fcomp_salt_wt_pct %in% filter) |>
    filter_cond()
  
  pcb_filter_error <- 
    select_df(df = df, type = 'PCB') |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% filter,
    ) |>
    filter_cond_error()
  
  pcb_filter_error_alt <- 
    select_df(df = df, type = 'PCB') |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% filter,
    ) |>
    filter_cond_error_alt()
  point_size = .5
  line_size = 0.6
    
  coin_filter_error <- 
    select_df(df = df, type = 'coin') |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% filter,
    ) |>
    dplyr::group_by(fcomp_formulation, print_type) |>
    filter_cond_error()
  
  plot <- ggplot2::ggplot() +
    cowplot::theme_half_open() +
    cowplot::background_grid(major = 'y') +
    ggplot2::geom_point(
      data = pcb_filter,
      size = point_size,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      )
    ) +
    # seongkoos results
    ggplot2::geom_point(
      data = pcb_filter_alt,
      size = point_size,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = 'red'
      )
    ) +
    ggplot2::geom_line(
      data = pcb_filter,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      size = line_size
    ) +
    ggplot2::geom_line(
      data = pcb_filter_alt,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = 'red'
      ),
      size = line_size
    ) +
    ggplot2::geom_errorbar(
      data = pcb_filter_error,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        ymin = avg_IC - ci,
        ymax = avg_IC + ci,
        color = print_type
      ),
      size = 0.25
    ) +
    ggplot2::geom_errorbar(
      data = pcb_filter_error_alt,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        ymin = avg_IC - (ci),
        ymax = avg_IC + (ci),
        color = 'red'
      ),
      size = 0.25
    )+
    ggplot2::geom_point(
      size = point_size,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      data = coin_filter
    ) +
    ggplot2::geom_line(
      data = coin_filter,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      size = line_size
    ) +
    ggplot2::geom_errorbar(
      data = coin_filter_error,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        ymin = avg_IC - ci,
        ymax = avg_IC + ci,
        color = print_type
      ),
      size = 0.25
    ) +
    ggplot2::scale_y_continuous(
      #trans = 'log10',
      limits = c(-9.5, -3.0),
      breaks = c(-2, -3, -4, -5 , -6, -7, -8, -9, -10),
      labels = scales::math_format(10 ^ .x)
    ) +
    ggplot2::scale_x_continuous(
      #trans = 'log10',
      limits = c(-1, 15.9),
      breaks = c(0, 5, 10, 15),
    ) +
    ggplot2::scale_color_manual(labels = c('Coin\ncells', 'SPOC', 'Model'),
                                values = c('#440154FF', '#55C667FF','orange')) +
    ggplot2::labs(x = 'Ae380 (wt%)',
         y = expression('Ionic Conductivity (S/cm)'),
         #title = 'b) Comparison of ionic ionductivities measured\nvia SPOC and coin cells'
         ) +
    ggplot2::theme(legend.position = 'right',
                   color = ggplot2::guide_legend(reverse = TRUE),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 10,
                                                       family = 'Arial'),
                   axis.title = ggplot2::element_text(size = 12,
                                                      family = 'Arial'),
                   axis.text.x = ggplot2::element_text(size = 9,
                                                     family = 'Arial'),
                   axis.text.y = ggplot2::element_text(size = 12,
                                                       family = 'Arial'),
                   legend.key.size = ggplot2::unit(12, 'pt'),
                   legend.spacing.y = ggplot2::unit(10, "pt"),
                   legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.spacing.x = ggplot2::unit(30, "pt"),
                   legend.box.margin = ggplot2::margin(0,0,-8,-12),
                   strip.background = ggplot2::element_rect(fill = NA,
                                                            color = NA,
                                                            size = 2),
                   plot.title.position = 'plot',
                   plot.title = ggplot2::element_text(size = 12),
                   strip.text = ggplot2::element_text(size = 9),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        color = 'black')
                   ) +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::facet_grid(. ~ fcomp_salt_wt_pct_lab)
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue(
      '{folder_destination}2304-comparison10pct_lineplot_error_MicahNewPrint.png'
    ),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

# grid searching
gridSearch_plotter <- function(df, type) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = fcomp_additive_wt_pct,
      color = 'black',
      # group = interaction(fcomp_salt_wt_pct, fcomp_additive_wt_pct)
    )
  ) +
    # ggplot2::geom_jitter(
    #   size = 3,
    #   width = 0.1,
    #   height = 0.1,
    #   alpha = 1
    # ) +
    ggplot2::geom_point(
      size = 3,
      ggplot2::aes(color = 'black')
    ) +
    cowplot::theme_cowplot() +
    #cowplot::background_grid() +
    ggplot2::labs(x = 'LiTFSI (wt%)',
                  y = 'Aerosil 380 (wt%)') +
    # title = glue::glue('Grid Search for {type} Polyelectrolytes')) +
    ggplot2::theme(
      legend.position = 'none',
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10)
    )
  
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue('{folder_destination}2304-Grid_Search_{type}.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  return(plot)
}

gridSearch_plotter_NaTFSI <- function(df, type) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = fcomp_additive_wt_pct,
      color = 'black',
      #group = interaction(fcomp_salt_wt_pct, fcomp_additive_wt_pct)
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
                  y = 'Aerosil 380 (wt%)') +
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

custom_labels <- function(x) {
  paste0(format(x))
}

akima_plot <- function(akima_df, df, type) {
  
  labels <- ifelse(type == 'coin',
                   c(expression(10^-9), 
                     expression(10^-8), 
                     expression(10^-7), 
                     expression(10^-6),
                     expression(10^-5),
                     expression(10^-4)),
                   c(expression(10^-7), 
                     expression(10^-6), 
                     expression(10^-5), 
                     expression(10^-4)))
  
  limits_def <- ifelse(type == 'coin',
                   c(-9.8 |> as.numeric(), -3.3 |> as.numeric()),
                   c(-7.8 |> as.numeric(), -4.3 |> as.numeric()))
  
  breaks <- ifelse(type == 'coin',
                   c(-9.5 |> as.numeric(), -8.5 |> as.numeric(), -7.5 |> as.numeric(), -6.5 |> as.numeric(), -5.5 |> as.numeric(), -4.5 |> as.numeric(), -3.5 |> as.numeric()),
                   c(-7.5 |> as.numeric(), -6.5 |> as.numeric(), -5.5 |> as.numeric(), -4.5 |> as.numeric()))
  
  
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
    # paletteer::scale_fill_paletteer_c(palette = 'viridis::viridis',
    #                               na.value = 'black',
    #                               labels = c(expression(10^-7), 
    #                                          expression(10^-6),
    #                                          expression(10^-5),
    #                                          expression(10^-4),
    #                                          expression(10^-3)),
    #                               limits = c(-7.3, -3.9),
    #                               breaks = c(-7, -6, -5.5, -5, -4.5, -4, -3),
    #                               direction = 1,
    #                               ) +
    ggplot2::scale_fill_gradientn(
      colors = shades::saturation(paletteer::paletteer_c("scico::roma", n = 100), shades::delta(0.90)),
      values = scales::rescale(c(-8, -7.8, -7.5, -7.2, -7, -6.5, -6, -5.8), to = c(1,0)),  # Rescale midpoints to [0, 1]
      limits = c(-8, -3.9),  # Set limits for the fill scale
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
        barwidth = ggplot2::unit(0.5, "cm"),                  # Narrow bar width (can adjust)
        barheight = 6,      # Full plot height
        title = 'S/cm',
        display = 'gradient',
        title.position = "top",
        ticks = ggplot2::element_line(color='black')
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'LiTFSI (wt%)',
                  y = 'Ae380 (wt%)',
                  title = 'a) SPOC grid varying LiTFSI and Ae380 wt%',
    ) +
    #title = glue::glue('{type} Data Mean')) +
    # ggplot2::xlim(0, 30) +
    # ggplot2::ylim(0, 15) +
    cowplot::theme_half_open() +
    ggplot2::theme(
      legend.position = 'right',
      #legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12,
                                          ),
      axis.title = ggplot2::element_text(size = 12,
                                        ),
      axis.text = ggplot2::element_text(size = 12,
                                        ),
      plot.title = ggplot2::element_text(size = 12),
      plot.title.position = 'plot',
      # legend.key.size = ggplot2::unit(15, 'pt'),
      panel.border = ggplot2::element_rect(size = 2),
      legend.spacing.y = ggplot2::unit(10, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      # legend.spacing.x = ggplot2::unit(50, "pt"),
      legend.box.margin = ggplot2::margin(0,0,-10,-10)
      ) +
    ggplot2::scale_x_continuous(expand = c(0,-0.3),
                                limits = c(.1,30.1),
                                labels = c(0, 10, 20, 30),
                                breaks = c(0.4, 10, 20, 29.8)) +
    ggplot2::scale_y_continuous(expand = c(0,-0.3),
                                limits = c(-.02, 15.4),
                                labels = c(0, 5, 10, 15),
                                breaks = c(0.4, 5, 10, 15)) + 
    ggforce::geom_circle(ggplot2::aes(
      x0 = 10, y0 = 11.5, r = 2.2),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red') + 
    ggforce::geom_circle(ggplot2::aes(
      x0 = 10, y0 = 3, r = 2.0),
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
      ggplot2::aes(x = fcomp_salt_wt_pct,
                   y = fcomp_additive_wt_pct,
                   #fill = 'black',
                   )) +
    #ggplot2::geom_density(alpha = 0.5) +
    cowplot::theme_nothing()+
    ggplot2::geom_point(size = 1.5,
                        alpha = 0.35,
                        color = 'black',
                        #shape = 1,
                        fill = NA) +
    ggplot2::expand_limits()
  
  legend <- cowplot::get_legend(plot_akima +
                                  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1)))
  
  plot_combined <- cowplot::ggdraw(plot_akima)+
  cowplot::draw_plot(plot) +
    cowplot::draw_plot(plot_points,
                       x = 0.11,
                       y = 0.146,
                       width = 0.73,
                       height = 0.76)

  plot_combined <- cowplot::plot_grid(
    plot_combined, legend, nrow = 1, ncol = 1, align = 'v'
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
  # Calculates the in-between regions using linear interpolation
  akima_median <-
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

  plot_akima <- akima_plot(akima_median, df, type)
  
  plot_points <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(x = fcomp_salt_wt_pct,
                   y = fcomp_additive_wt_pct,
                   #fill = 'black',
      )) +
    #ggplot2::geom_density(alpha = 0.5) +
    cowplot::theme_nothing()+
    ggplot2::geom_point(size = 2,
                        alpha = 0.7,
                        color = 'black',
                        shape = 21,
                        fill = NA)
  
  legend <- cowplot::get_legend(plot_akima +
                                  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1)))
  
  plot_combined <- cowplot::ggdraw(plot_akima) +
    #cowplot::draw_plot(plot) +
    cowplot::draw_plot(plot_points, 
                       x = 0.145,
                       y = 0.185,
                       width = 0.85,
                       height = 0.60)
  
  plot_combined <- cowplot::plot_grid(
    plot_combined, legend, nrow = 1, ncol = 1, align = 'v'
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

add_label <- function(plot, label) {
  plot +
    annotation_custom(
      grob = grid::textGrob(
        label = label,
        x = grid::unit(-0.1, "npc"),
        y = grid::unit(1.1, "npc"),
        just = c("left", "top"),
        gp = grid::gpar(fontsize = 15)
      )
    )
}

# ggplot template for boxplots
boxplot_plotter <- function(df, type) {
  require(patchwork)
  plot <- ggplot2::ggplot(
    # changes the column values to string literals for labeling
    data = df |>
      dplyr::filter(
        fcomp_salt_wt_pct %in% c(0, 10, 20, 30),
        !fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.9, 7.1)
      ) |>
      dplyr::mutate(fcomp_salt_wt_pct = glue::glue('LiTFSI {fcomp_salt_wt_pct} wt%')),
    ggplot2::aes(
      x = fcomp_additive_wt_pct,
      y = ionic_conductivity_final,
      group = fcomp_additive_wt_pct |> as.factor(),
      color = fcomp_additive_wt_pct |> as.factor()
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    #cowplot::background_grid(major = 'y') +
    ggplot2::geom_boxplot(size = 0.25,
                          width = 2,
                          outlier.shape = NA) +
    ggplot2::facet_grid(. ~ fcomp_salt_wt_pct) +
    ggplot2::theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA),
      axis.title.x = element_text(size = 12)
    ) +
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x)
        10 ^ x),
      labels = scales::trans_format('log10', scales::math_format(10 ^ .x)),
      limits = c(1e-9, 1e-3)
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = 'Aerosil (wt%)',
                  y = expression('Ionic Conductivity (S/cm)'))
  # adds figure label a)
  # plot <- add_label(label = 'a)',
  #                    plot = plot)
  #
  plot2 <- ggplot2::ggplot(
    # changes the column values to string literals for labeling
    data = df |>
      dplyr::filter(
        fcomp_additive_wt_pct %in% c(0, 7.5, 15) &
          !fcomp_salt_wt_pct %in% c(7.5, 10, 20)
      ) |>
      dplyr::mutate(
        fcomp_additive_wt_pct = glue::glue('Aerosil {fcomp_additive_wt_pct} wt%')
      ),
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = ionic_conductivity_final,
      group = fcomp_salt_wt_pct |> as.factor(),
      color = fcomp_salt_wt_pct |> as.factor()
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    #cowplot::background_grid(major = 'y') +
    ggplot2::geom_boxplot(size = 0.25,
                          width = 3,
                          outlier.shape = NA) +
    ggplot2::facet_grid(. ~ fcomp_additive_wt_pct) +
    ggplot2::theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 9),
      strip.text.y = element_text(size = 9),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA),
      axis.title.x = element_text(size = 12)
    ) +
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x)
        10 ^ x),
      labels = scales::trans_format('log10', scales::math_format(10 ^ .x)),
      limits = c(1e-9, 1e-3)
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = 'LiTFSI (wt%)',
                  y = expression('Ionic Conductivity (S/cm)'))
  # adds label b)
  # plot2 <- add_label(label = 'b)',
  #                    plot = plot2)
  
  ggplot2::ggsave(
    plot,
    filename = glue::glue('{folder_destination}2304-{type}_salt_boxplot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  ggplot2::ggsave(
    plot2,
    filename = glue::glue('{folder_destination}2304-{type}_additive_boxplot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  plot_comb <- plot / plot2
  
  ggplot2::ggsave(
    plot_comb,
    filename = glue::glue('{folder_destination}2304-{type}_comb_boxplot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
}

{
  regression_plot <- function(df) {
    ggplot2::ggplot() +
      ggplot2::geom_point(
        data = select_df(df = df, type = 'PCB'),
        ggplot2::aes(
          x = fcomp_salt_wt_pct,
          y = ionic_conductivity_final,
          group = print_type,
          color = print_type
        ),
        size = 4
      ) +
      ggplot2::geom_smooth(
        data = select_df(df = df, type = 'PCB'),
        aes(
          x = fcomp_salt_wt_pct,
          y = ionic_conductivity_final,
          color = print_type,
          fill = print_type
        ),
        method = 'glm',
        #method.args = list('Gamma'),
        formula = y ~ poly(x, 2)
      ) +
      ggplot2::geom_point(
        data = select_df(df = df, type = 'coin'),
        ggplot2::aes(
          x = fcomp_salt_wt_pct,
          y = ionic_conductivity_final,
          group = print_type,
          color = print_type
        ),
        size = 4
      ) +
      ggplot2::geom_smooth(
        data = select_df(df = df, type = 'coin'),
        ggplot2::aes(
          x = fcomp_salt_wt_pct,
          y = ionic_conductivity_final,
          color = print_type,
          fill = print_type
        ),
        method = 'glm',
        #method.args = list('Gamma'),
        formula = y ~ poly(x, 2)
      ) +
      ggplot2::scale_y_continuous(trans = 'log10') +
      ggplot2::facet_wrap(. ~ fcomp_additive_wt_pct,
                          nrow = 2,
                          ncol = 4) +
      ggplot2::labs(x = 'LiTFSI (wt%)',
                    y = expression('Ionic Conductivity (' ~ 'S' ~ '/cm)'))
  }
}

# function to save plots for glm models with low-pressure
glm_moisture_pe_summary <- function(data) {
  # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variables
  data_name <- deparse(substitute(data))
  model_pe <-
    stats::glm(log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct * high_pressure,
    )
  # log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct * high_pressure,
  #log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct)
  data = data |>
    dplyr::mutate(
      Pressure = as.numeric(Pressure),
      DP = as.numeric(DP),
      # print_type = as.factor(print_type),
      # print_type = case_when(print_type == 'Coin cell' ~ 0,
      #                        print_type == 'PCB print' ~ 1),
      # print_type = as.numeric(print_type) |> as.factor()) |>
      filter(print_type == 'PCB print'),
      family = gaussian(link = 'identity')
    )
  #results <- data.table::data.table(anova_pe[[1]])
  residuals_pe <- stats::residuals(model_pe)
  stats <- model_pe %>%
    gtsummary::tbl_regression() %>%
    gtsummary::as_gt() %>%
    gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  print(summary(model_pe))
  print(stats)
  #generates a qq-plot of the residuals (determine model performance with real values)
  qq_plot <- ggpubr::ggqqplot(residuals_pe,
                              xlab = 'Sample Quantile',
                              ylab = 'Theoretical Quantile')
  
  glm_plot <-
    ggiraphExtra::ggPredict(model_pe)
  ggplot2::ggsave(
    qq_plot,
    filename = glue::glue(
      '{folder_destination}2304-{data_name}_moisture_QQresidual_plot.png'
    ),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  return(model_pe)
}

# function to save plots for glm models with just print type, additive% and salt%
glm_pe_summary <- function(data) {
  # applies GLM due to not normally distributed data using print_type and fcomp_formulation as categorical variables
  data_name <- deparse(substitute(data))
  model_pe <-
    stats::glm(
      log_IC ~ fcomp_additive_wt_pct * fcomp_salt_wt_pct,
      #log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct,
      #log_IC ~ print_type * fcomp_additive_wt_pct * fcomp_salt_wt_pct,
      data = data |>
        dplyr::mutate(
          Pressure = as.numeric(Pressure),
          DP = as.numeric(DP),
          print_type = as.factor(print_type)
          # print_type = case_when(print_type == 'Coin cell' ~ 0,
          #                        print_type == 'PCB print' ~ 1),
          # print_type = as.numeric(print_type) |> as.factor()
        ) |>
        dplyr::filter(print_type == 'PCB print'),
      family = gaussian(link = 'identity')
    )
  #results <- data.table::data.table(anova_pe[[1]])
  residuals_pe <- stats::residuals(model_pe)
  #generates a qq-plot of the residuals (determine model performance with real values)
  qq_plot <- ggpubr::ggqqplot(residuals_pe,
                              xlab = 'Sample Quantile',
                              ylab = 'Theoretical Quantile')
  # outputs regression statistics
  stats <- model_pe %>%
    gtsummary::tbl_regression() %>%
    gtsummary::as_gt() %>%
    gt::tab_source_note(gt::md(glue::glue('From {data_name}')))
  glm_plot <-
    ggeffects::ggpredict(model_pe,
                         c('fcomp_salt_wt_pct', 'fcomp_additive_wt_pct')) |> plot(rawdata = TRUE,
                                                                                  facets = TRUE)
  glm_plot2 <-
    ggeffects::ggpredict(model_pe,
                         c('fcomp_additive_wt_pct', 'fcomp_salt_wt_pct')) |> plot(rawdata = TRUE,
                                                                                  facets = TRUE)
  
  print(glm_plot)
  print(glm_plot2)
  
  summary(model_pe) |> print()
  # saves plot
  ggplot2::ggsave(
    qq_plot,
    filename = glue::glue(
      '{folder_destination}2304-{data_name}_QQresidual_plot.png'
    ),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  return(residuals_pe)
}
