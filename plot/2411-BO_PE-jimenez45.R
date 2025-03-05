# replot june's ml results

file_loc <-
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Jay\\2502-ML_Summary-Noh.csv'

user_name <- 'jimenez45'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

file <- readr::read_csv(file_loc)

{
  # reformat data to include upper and lower bounds
  file_Top <- file |>
    dplyr::select(Iteration, Baseline, GP, RF) |>
    # dplyr::rename(
    #   'GP (RQ/EI)' = GP,
    #   'RF (100/PI)' = RF,
    #   'Random Baseline' = Baseline
    # ) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count')
  
  file_lower <- file |>
    dplyr::select(Iteration, GP_lower, RF_lower) |>
    dplyr::rename(GP = GP_lower,
                  RF = RF_lower) |>
    dplyr::mutate(Baseline = NA) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count_lower')
  
  file_upper <- file |>
    dplyr::select(Iteration, GP_upper, RF_upper) |>
    dplyr::rename(GP = GP_upper,
                  RF = RF_upper) |>
    dplyr::mutate(Baseline = NA) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count_upper')
  
  file_Top_all <- dplyr::full_join(file_lower, file_upper) |>
    dplyr::full_join(file_Top) |>
    dplyr::mutate(type = stringr::str_replace_all(type, 'Baseline', 'Random'),
                  type = factor(type, levels = c('RF', 'GP', 'Random')))
}

{
  file_EF <- file |>
    dplyr::select(Iteration, Baseline, GP, RF) |>
    dplyr::mutate(
      GP = GP / Baseline,
      RF = RF / Baseline,
      Baseline = Baseline / Baseline
    ) |>
    # dplyr::rename(
    #   'GP (RQ/EI)' = GP,
    #   'RF (100/PI)' = RF,
    #   'Random Baseline' = Baseline
    # ) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count')
  
  file_EF_lower <- file |>
    dplyr::select(Iteration, Baseline, GP_lower, RF_lower) |>
    dplyr::select(Iteration, Baseline, GP_lower, RF_lower) |>
    dplyr::mutate(
      GP = GP_lower / Baseline,
      RF = RF_lower / Baseline,
      Baseline = Baseline / Baseline
    ) |>
    # dplyr::rename(
    #   'GP (RQ/EI)' = GP_lower,
    #   'RF (100/PI)' = RF_lower,
    #   'Random Baseline' = Baseline_lower
    # ) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count_lower')
  
  file_EF_upper <- file |>
    dplyr::select(Iteration, Baseline, GP_upper, RF_upper) |>
    dplyr::select(Iteration, Baseline, GP_upper, RF_upper) |>
    dplyr::mutate(
      GP = GP_upper / Baseline,
      RF = RF_upper / Baseline,
      Baseline = Baseline / Baseline
    ) |>
    # dplyr::rename(
    #   'GP (RQ/EI)' = GP_upper,
    #   'RF (100/PI)' = RF_upper,
    #   'Random Baseline' = Baseline_upper
    # ) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = 'type',
                        values_to = 'count_upper')
  
  file_EF_all <- dplyr::full_join(file_EF_lower, file_EF_upper) |>
    dplyr::full_join(file_EF) |>
    dplyr::mutate(type = stringr::str_replace_all(type, 'Baseline', 'Random'),
                  type = factor(type, levels = c('RF', 'GP', 'Random'))) |>
    dplyr::filter(!is.na(type))
}

{
  file_AF <- file |>
    dplyr::select(Iteration, Baseline, GP, RF) |>
    dplyr::mutate(
      GP_iter = (GP / 0.0111) / Iteration,
      RF_iter = (RF / 0.0111) / Iteration,
      Baseline_iter = (Baseline / 0.0111) / Iteration
    ) |>
    dplyr::select(-Iteration) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("type", "set"),
      names_sep = "_",
      values_to = 'value'
    ) |>
    tidyr::pivot_wider(
      names_from = set,
      values_from = value,
      names_prefix = "value_"
    ) |>
    tidyr::unnest() |>
    dplyr::rename(top = value_NA,
                  AF = value_iter) |>
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(type, 'GP') == TRUE ~ 'GP',
        stringr::str_detect(type, 'RF') == TRUE ~ 'RF',
        stringr::str_detect(type, 'Baseline') == TRUE ~ 'Random'
      )
    )
  
  # tidyr::pivot_longer(c(GP, RF, Baseline),
  #                     names_to = 'type',
  #                     values_to = 'Top') |>
  # tidyr::pivot_longer(c(GP_iter, RF_iter, Baseline_iter),
  #                     values_to = 'AF')
  
  file_AF_lower <- file |>
    dplyr::select(Iteration, Baseline, GP_lower, RF_lower, GP, RF) |>
    dplyr::rename(Baseline_lower = Baseline) |>
    dplyr::mutate(
      GP_iter = (GP_lower / 0.0111) / Iteration,
      RF_iter = (RF_lower / 0.0111) / Iteration,
      Baseline_iter = (Baseline_lower / 0.0111) / Iteration
    ) |>
    dplyr::select(-c(Iteration, Baseline_lower, GP_lower, RF_lower)) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("type", "set"),
      names_sep = "_",
      values_to = 'value'
    ) |>
    tidyr::pivot_wider(
      names_from = set,
      values_from = value,
      names_prefix = "value_"
    ) |>
    tidyr::unnest() |>
    dplyr::rename(top = value_NA,
                  AF_lower = value_iter) |>
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(type, 'GP') == TRUE ~ 'GP',
        stringr::str_detect(type, 'RF') == TRUE ~ 'RF',
        stringr::str_detect(type, 'Baseline') == TRUE ~ 'Random'
      )
    )
  
  file_AF_upper <- file |>
    dplyr::select(Iteration, Baseline, GP_upper, RF_upper, GP, RF) |>
    dplyr::rename(Baseline_upper = Baseline) |>
    dplyr::mutate(
      GP_iter = (GP_upper / 0.0111) / Iteration,
      RF_iter = (RF_upper / 0.0111) / Iteration,
      Baseline_iter = (Baseline_upper / 0.0111) / Iteration
    ) |>
    dplyr::select(-c(Iteration, Baseline_upper, GP_upper, RF_upper)) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("type", "set"),
      names_sep = "_",
      values_to = 'value'
    ) |>
    tidyr::pivot_wider(
      names_from = set,
      values_from = value,
      names_prefix = "value_"
    ) |>
    tidyr::unnest() |>
    dplyr::rename(top = value_NA,
                  AF_upper = value_iter) |>
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(type, 'GP') == TRUE ~ 'GP',
        stringr::str_detect(type, 'RF') == TRUE ~ 'RF',
        stringr::str_detect(type, 'Baseline') == TRUE ~ 'Random'
      )
    )
  
  file_AF_all <- dplyr::full_join(file_AF_lower, file_AF_upper) |>
    dplyr::full_join(file_AF) |>
    dplyr::mutate(type = stringr::str_replace_all(type, 'Baseline', 'Random'),
                  type = factor(type, levels = c('RF', 'GP', 'Random')))
}

plot_BO <- function(data) {
  ggplot2::ggplot(data = data,
                  ggplot2::aes(
                    x = Iteration,
                    y = count,
                    group = type,
                    color = type
                  )) +
    cowplot::theme_half_open() +
    ggplot2::geom_line(size = .5,
                       ggplot2::aes(linetype = type)) +
    ggplot2::theme(
      legend.position = 'right',
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10,
                                          family = 'Arial'),
      axis.title = ggplot2::element_text(size = 12,
                                         family = 'Arial'),
      axis.text = ggplot2::element_text(size = 12,
                                        family = 'Arial'),
      legend.key.size = ggplot2::unit(15, 'pt'),
      legend.spacing.y = ggplot2::unit(10, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.x = ggplot2::unit(50, "pt"),
      legend.box.margin = ggplot2::margin(0,0,-10,-10)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = count_lower,
        ymax = count_upper,
        group = type,
        color = type,
        fill = type
      ),
      alpha = 0.25,
      linetype = 0,
      show.legend = FALSE
    ) +
    ggplot2::scale_linetype_manual(values = c(
      "Random" = "solid",
      "GP" = "solid",
      "RF" = "solid"
    )) +
    ggplot2::scale_color_manual(
      values = c(
        "Random" = "black",
        "GP" = "#287C8EFF",
        "RF" = "#440154"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Random" = "black",
        "GP" = "#287C8EFF",
        "RF" = "#440154"
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0,100)
    ) 
}

alpha_line = 0.75
alpha_text = 0.6

plot_BO_comparison <- function(data) {
  plot_BO(data) +
    ggplot2::geom_hline(yintercept = 1,
                        linetype = 'dashed',
                        alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0.9,
                        linetype = 'dashed',
                        alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0.8,
                        linetype = 'dashed',
                        alpha = 0.5) +
    ggplot2::annotate(
      'segment',
      x = 90,
      y = 1,
      yend = 1,
      xend = 61,
      color = 'blue',
      alpha = alpha_line,
      arrow = ggplot2::arrow(
        ends = 'both',
        type = 'closed',
        length = ggplot2::unit(0.2, 'cm')
      ),
      lwd = 0.2
    ) +
    ggplot2::annotate(
      'segment',
      x = 79,
      y = .9,
      yend = .9,
      xend = 49,
      color = 'blue',
      alpha = alpha_line,
      arrow = ggplot2::arrow(
        ends = 'both',
        type = 'closed',
        length = ggplot2::unit(0.2, 'cm')
      ),
      lwd = 0.2
    ) +
    ggplot2::annotate(
      'segment',
      x = 71,
      y = .8,
      yend = .8,
      xend = 36,
      color = 'blue',
      alpha = alpha_line,
      arrow = ggplot2::arrow(
        ends = 'both',
        type = 'closed',
        length = ggplot2::unit(0.2, 'cm')
      ),
      lwd = 0.2
    ) +
    ggplot2::annotate(
      'text',
      label = '1.53x',
      x = 50,
      y = 1.05,
      color = 'blue',
      size = 3
    ) +
    ggplot2::annotate(
      'text',
      label = '1.72x',
      x = 40,
      y = 0.945,
      color = 'blue',
      size = 3
    ) +
    ggplot2::annotate(
      'text',
      label = '1.95x',
      x = 29,
      y = 0.845,
      color = 'blue',
      size = 3
    ) +
    ggplot2::labs(x = 'Iterations',
                  y = 'Top %',
                  title = 'd) Analysis of surrogate modeling using SPOC\ndata and rate enhancement') +
    ggplot2::theme(
      plot.title.position = 'plot',
      plot.title = ggplot2::element_text(size = 12),
      legend.position = 'inside',
      legend.position.inside = c(0.75, 0.5)
    ) +
    ggplot2::coord_cartesian(clip = 'off',
                             expand = TRUE,
                             xlim = c(0,100))
}

plot_EF_comparison <- function(data) {
  plot_BO(data) +
    ggplot2::annotate(
      'segment',
      x = 35,
      y = 1,
      yend = 1.98,
      xend = 35,
      color = 'blue',
      alpha = alpha_line,
      arrow = ggplot2::arrow(
        ends = 'both',
        type = 'closed',
        length = ggplot2::unit(0.2, 'cm')
      ),
      lwd = 0.2
    ) +
    ggplot2::annotate(
      'text',
      label = '2.00',
      x = 45,
      y = 1.60,
      color = 'blue',
      size = 4
    ) +
    ggplot2::labs(x = 'Iterations',
                  y = 'Enhancement Factor, EF',
                  title = 'b)') +
    ggplot2::theme(
      plot.title.position = 'plot',
      plot.title = ggplot2::element_text(size = 16,
                                         vjust = -6,
                                         hjust = -0.02),
      plot.margin=grid::unit(c(-5,0,0,3), "mm")) +
    ggplot2::scale_linetype_manual(values = c(
      "Random" = "solid",
      "GP" = "solid",
      "RF" = "solid"
    ),
    limits = c('RF', 'GP', 'Random')) +
    ggplot2::scale_color_manual(
      values = c(
        "Random" = "black",
        "GP" = "#287C8EFF",
        "RF" = "#440154"
      ),
      limits = c('RF', 'GP', 'Random')
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Random" = "black",
        "GP" = "#287C8EFF",
        "RF" = "#440154"
      ),
      limits = c('RF', 'GP', 'Random')
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0,100)
    )
}

plot_Top <- plot_BO_comparison(file_Top_all)

plot_EF <- plot_EF_comparison(file_EF_all)

plot_AF <- plot_BO(file_AF)
#
plot_AF <- 
  ggplot2::ggplot(data = file_AF_all,
                ggplot2::aes(
                  x = top,
                  y = AF,
                  group = type,
                  color = type
                )) +
  cowplot::theme_half_open() +
  ggplot2::geom_line(size = 0.5,
                     ggplot2::aes(linetype = type)) +
  ggplot2::geom_ribbon(ggplot2::aes(
    ymin = AF_lower,
    ymax = AF_upper,
    group = type,
    fill = type
  ),
  alpha = 0.2,
  linetype = 0,
  show.legend = FALSE) +
  ggplot2::scale_linetype_manual(values = c(
    'Random' = "solid",
    "GP" = "solid",
    "RF" = "solid"
  )) +
  ggplot2::scale_color_manual(
    values = c(
      "Random" = "black",
      "GP" = "#287C8EFF",
      "RF" = "#440154"
    ),
    limits = c('RF', 'GP', 'Random')
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Random" = NA,
      "GP" = "#287C8EFF",
      "RF" = "#440154"
    ),
    labels = c(
      "RF" = "RF",
      "GP" = "GP",
      "Random" = "Random"
    ),
    limits = c('RF', 'GP', 'Random')
  ) +
  ggplot2::labs(x = 'Top %',
                y = 'Acceleration Factor, AF',
                title = 'c)') +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10,
                                        family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12,
                                       family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12,
                                      family = 'Arial'),
    legend.key.size = ggplot2::unit(15, 'pt'),
    legend.spacing.y = ggplot2::unit(10, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = ggplot2::unit(50, "pt"),
    legend.box.margin = ggplot2::margin(0,0,-10,-10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16,
                                       vjust = -6,
                                       hjust = -0.02),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")
  ) +
  ggplot2::annotate(
    'segment',
    x = 0.76,
    y = 1,
    yend = 1.98,
    xend = 0.76,
    color = 'blue',
    alpha = alpha_line,
    arrow = ggplot2::arrow(
      ends = 'both',
      type = 'closed',
      length = ggplot2::unit(0.2, 'cm')
    ),
    lwd = 0.2
  ) +
  ggplot2::annotate(
    'text',
    label = '2.00',
    x = 0.67,
    y = 1.65,
    color = 'blue',
    alpha = 0.75,
    size = 4,
    family = 'Arial'
  )

ggplot2::ggsave(
  plot_Top,
  file = glue::glue('{folder_destination}2411-BO_Top.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)


ggplot2::ggsave(
  plot_EF,
  file = glue::glue('{folder_destination}2411-BO_EF.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

ggplot2::ggsave(
  plot_AF,
  file = glue::glue('{folder_destination}2411-BO_AF.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

