# spoc comparison to traidtional

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs_corr\\'
)

data <- tibble::tibble(
  Approach = rep(c('Coin Cell', 'SPOC'), each = 5),
  Value = c(2, 2, 32, 1.5, 10,  1.5, 1, 1.5, 2, 8),
  label = c('no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'yes'),
  Group = c(
    "Formulate",
    "Prepare",
    "Fabricate & Characterize",
    "Clean",
    "Analyze",
    "Clean",
    "Analyze",
    "Formulate",
    "Prepare",
    "Fabricate & Characterize"
  )
)

# Calculate cumulative sums for stacking
data <- data |>
  dplyr::group_by(Approach) |>
  dplyr::mutate(Cumulative = cumsum(Value),
                Group = factor(
                  Group,
                  levels = c(
                    "Clean",
                    "Analyze",
                    "Formulate",
                    "Prepare",
                    "Fabricate & Characterize"
                  )
                ))

# Create the plot
plot <-
  ggplot2::ggplot(data,
                  ggplot2::aes(
                    y = Approach,
                    x = Value,
                    fill = forcats::fct_rev(Group)
                  )) +
  cowplot::theme_half_open() +
  #cowplot::background_grid() +
  ggplot2::geom_bar(
    stat = "identity",
    position = "stack",
    alpha = 0.7,
    color = NA,
    width = 0.9
  ) +
  ggplot2::scale_fill_manual(values = c("darkorange", "darkcyan", "red", "purple", 'green')) +
  ggplot2::labs(x = "Time (hours)", y = '') +
  #ggplot2::theme_minimal(base_family = "Times New Roman", base_size = 12) +
  ggplot2::xlim(0, 50) +
  ggpattern::geom_rect_pattern(
    ggplot2::aes(
      xmin = 6.0,
      xmax = 13.95,
      ymin = 1.56,
      ymax = 2.45
    ),
    color = NA,
    alpha = 0.5,
    pattern_fill = 'red',
    pattern_color = 'red',
    pattern_density = 0.01,
    fill = NA
  ) +
  # ggplot2::geom_text(ggplot2::aes(label = Value),
  #                    position = ggplot2::position_stack(vjust = 0.5),
  #                    angle = 0,
  #                    size = 3) +
  ggplot2::geom_text(label = 'Total Manual: 47.5 hours',
                     x = 30,
                     y = 1) +
  ggplot2::geom_text(label = 'Manual: 6 hours',
                     x = 25,
                     y = 2) +
  ggplot2::geom_text(
    label = 'Automated: 8 hours',
    x = 40,
    y = 2,
    color = 'red'
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 0,
      xmax = 47.4,
      ymin = .55,
      ymax = 1.45
    ),
    color = 'black',
    size = 0.75,
    fill = NA
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 18.5,
      xmax = 31.5,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = 'black',
    size = 0.75,
    fill = NA
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 0,
      xmax = 5.75,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = 'black',
    size = 0.75,
    fill = NA
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 6.15,
      xmax = 14,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = 'red',
    size = 0.75,
    linetype = 'dashed',
    fill = NA
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 32,
      xmax = 47.5,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = 'red',
    size = 0.75,
    linetype = 'dashed',
    fill = NA
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
  ggplot2::theme(legend.position = "top",
                 legend.title = ggplot2::element_blank()) # Adjust legend as needed

print(plot)


ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}2410-SPOC_plot.png'),
  dpi = 600,
  width = 8,
  height = 3,
  bg = 'white'
)

# Data for unique samples
num_uniquesamples <- c(868, 277)
x_unique <- factor(c('SPOC', 'Coin cells'))

# Create a data frame for unique samples
data_unique <- data.frame(Sample = factor(x_unique),
                          Count = num_uniquesamples)

# Plot for unique samples
plot_unique <-
  ggplot2::ggplot(data_unique, ggplot2::aes(y = Sample, x = Count)) +
  cowplot::theme_half_open() +
  #cowplot::background_grid() +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "grey",
    color = "black",
    alpha = 0.6
  ) +
  #ggplot2::xlim(0, 900) +
  ggplot2::scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  ggplot2::labs(x = "# Unique Samples", y = '',
                title = 'b) Number of unique samples measured') +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 16),
    
    plot.title.position = 'plot'
  ) + 
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 277,
      xmax = 868,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = '#55C667FF',
    size = 0.75,
    alpha = 0.5,
    linetype = 'dashed',
    fill = '#55C667FF'
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = num_uniquesamples),
    hjust = 0.5,
    vjust = 1.2,
    position = ggplot2::position_dodge2(width = 1),
    angle = -90,
    size = 5,
    fontface = 'bold'
  )

print(plot_unique)
# Save the plot for unique samples
#ggsave('C:/Users/jimenez45/OneDrive - LLNL/General - High-Throughput Polymer Electrolytes DIW/Manuscripts/2409-PlatformFocus/figs/2024_09_26_SPOC_Figure4_ManualvsSpoc_UniqueSamples_v1.png',
#       plot = plot_unique, width = 2.5, height = 4, dpi = 600)

# Data for number of measurements
num_measurements <- c(2581, 705)
x_measurements <- factor(c('SPOC', 'Coin cells'))

# Create a data frame for measurements
data_measurements <- data.frame(Sample = x_measurements,
                                Count = num_measurements)

# Plot for measurements
plot_measurements <-
  ggplot2::ggplot(data_measurements, ggplot2::aes(y = Sample, x = Count)) +
  cowplot::theme_half_open() +
  #cowplot::background_grid() +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "gray",
    color = "black",
    alpha = 0.6
  ) +
  ggplot2::xlim(0, 2600) +
  ggplot2::labs(x = "# Measurements", y = '',
                title = 'c) Total number of measurements') +
  #theme_minimal(base_family = "Times", base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 16),
    plot.title.position = 'plot'
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 705,
      xmax = 2581,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = '#55C667FF',
    size = 0.75,
    alpha = 0.5,
    linetype = 'dashed',
    fill = '#55C667FF'
  ) +
  ggplot2::scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  ggplot2::geom_text(
    ggplot2::aes(label = num_measurements),
    hjust = 0.5,
    vjust = 1.2,
    position = ggplot2::position_dodge2(width = 1),
    angle = -90,
    size = 4,
    fontface = 'bold'
  )

print(plot_measurements)

# plot for year comparison

num_measurements_year <- c(9984, 1664)
type <- factor(c('SPOC', 'Coin cells'))

year_measurements <- data.frame(Sample = type,
                                Count = num_measurements_year)

plot_measurements_year <-
  ggplot2::ggplot(year_measurements, ggplot2::aes(y = Sample, x = Count)) +
  cowplot::theme_half_open() +
  #cowplot::background_grid() +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "gray",
    color = "black",
    alpha = 0.6
  ) +
  ggplot2::xlim(0, 10000) +
  ggplot2::labs(x = "# Samples", y = '',
                title = 'd) Projected number of samples per year, at full operation') +
  ggplot2::annotate(
    "text",
    label = bquote("6x samples, one-tenth labor"),
    x = 5900,
    y = 1,
    size = 6
  ) +
  ggplot2::geom_segment(
    x = 3400,
    y = .99,
    yend = .99,
    xend = 1800,
    size = .75,
    lwd = 0.25,
    arrow = ggplot2::arrow(
      ends = 'last',
      type = 'closed',
      length = ggplot2::unit(0.3, 'cm')
    )
  ) +
  ggplot2::geom_segment(
    x = 8400,
    y = .99,
    yend = .99,
    xend = 10000,
    size = .75,
    arrow = ggplot2::arrow(
      ends = 'last',
      type = 'closed',
      length = ggplot2::unit(0.3, 'cm')
    ),
    lwd = 0.25
  ) +
  #theme_minimal(base_family = "Times", base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 16),
    plot.title.position = 'plot'
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 1664,
      xmax = 9984,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = '#55C667FF',
    size = 0.75,
    alpha = 0.5,
    linetype = 'dashed',
    fill = '#55C667FF'
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = num_measurements_year),
    hjust = 0.5,
    vjust = 1.2,
    position = ggplot2::position_dodge2(width = 1),
    angle = -90,
    size = 4,
    fontface = 'bold'
  )

print(plot_measurements_year)

library(patchwork)
combined <-
  plot_unique / plot_measurements / plot_measurements_year

ggplot2::ggsave(
  combined,
  file = glue::glue('{folder_destination}2410-SPOC_plot_combined.png'),
  dpi = 600,
  width = 8,
  height = 6,
  bg = 'white'
)


data_week <- readr::read_csv('C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Jay\\SPOC_twoweeks.csv') |>
  dplyr::mutate(day = factor(day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')),
                task = factor(task, levels = c('fab_char', 'prepare', 'formulate', 'clean', 'analyze')),
                week = glue::glue('Week {week}'))

ggplot2::ggplot(data_week, ggplot2::aes(y = system, x = time, fill = task)) +
  cowplot::theme_half_open() +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::facet_grid(week ~ day) + # Create a grid for weeks and systems
  ggplot2::labs(title = "Timeline",
                x = "Operational Time (hrs)",
                y = "",
                fill = "Task") +
  
  ggplot2::scale_fill_manual(values = c("darkorange", "darkcyan", "red", "green", 'purple')) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = 7.5,
      xmax = 15.5,
      ymin = 1.55,
      ymax = 2.45
    ),
    color = 'red',
    size = 0.75,
    linetype = 'dashed',
    fill = NA
  ) +
  ggpattern::geom_rect_pattern(
    ggplot2::aes(
      xmin = 7.5,
      xmax = 15.5,
      ymin = 1.56,
      ymax = 2.45
    ),
    color = NA,
    alpha = 0.5,
    pattern_fill = 'red',
    pattern_color = 'red',
    pattern_density = 0.01,
    fill = NA
  ) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(size = 2))


total_sample <- df_PE_combined |>
  dplyr::mutate(print_type = stringr::str_replace_all(print_type, 'Bulk cast', 'PCB print')) |>
  dplyr::group_by(print_type) |>
  dplyr::mutate(form_count = length(unique(fcomp_formulation))) |>
  dplyr::distinct(print_type, form_count)

total_sample_paper <- df_PE_combined |>
  dplyr::filter(date_printed >= '2024-01-01') |>
  dplyr::mutate(print_type = stringr::str_replace_all(print_type, 'Bulk cast', 'PCB print')) |>
  dplyr::group_by(print_type) |>
  dplyr::mutate(form_count = length(unique(fcomp_formulation))) |>
  dplyr::distinct(print_type, form_count)

baseline_sample <- df_PE_combined |>
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
    
  ) |>
  dplyr::group_by(print_type, fcomp_additive) |>
  dplyr::mutate(form_count = length(unique(fcomp_formulation))) |>
  dplyr::distinct(print_type, form_count, fcomp_additive, fcomp_formulation)
