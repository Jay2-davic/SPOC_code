# fluorescence

user_name <- 'jimenez45'
version_number <- '2,26'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

df_30Li_cal <- tibble::tibble(
  type = c(
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration'
  ),
  salt_wt = c(30, 30, 30, 30, 30, 30,
              30, 30, 30, 30, 30, 30,
              30, 30, 30, 30, 30, 30),
  run = c(1, 1, 1, 1, 1, 1,
          2, 2, 2, 2, 2, 2,
          3, 3, 3, 3, 3, 3),
  nile_red = c(0, 20, 40, 60, 78, 100,
               0, 20, 40, 60, 78, 100,
               0, 20, 40, 60, 78, 100),
  intensity = c(
    136,
    4195,
    10393,
    12149,
    12236,
    14679,
    137,
    4228,
    10443,
    12188,
    13022,
    14725,
    141,
    4106,
    10443,
    12188,
    13022,
    14725
  )
)

df_30Li_purge <- tibble::tibble(
  type = c(
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge'
  ),
  salt_wt = c(
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30
  ),
  run = c(1, 1, 1, 1, 1, 1, 1, 1,
          2, 2, 2, 2, 2, 2, 2, 2,
          3, 3, 3, 3, 3, 3, 3, 3),
  nile_red = c(
    0,
    13,
    26,
    39,
    51,
    64,
    77,
    90,
    0,
    13,
    26,
    39,
    51,
    64,
    77,
    90,
    0,
    13,
    26,
    39,
    51,
    64,
    77,
    90
  ),
  intensity = c(
    0,
    5314,
    5958,
    10016,
    12048,
    10877,
    14850,
    15367,
    0,
    5197,
    5822,
    10074,
    12080,
    10774,
    14900,
    14921,
    0,
    5146,
    6033,
    10144,
    12536,
    11322,
    14770,
    15043
  )
)

df_0Li_cal <- tibble::tibble(
  type = c(
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration',
    'calibration'
  ),
  run = c(1, 1, 1, 1, 1, 1,
          2, 2, 2, 2, 2, 2,
          3, 3, 3, 3, 3, 3),
  salt_wt = c(0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0),
  nile_red = c(0, 20, 41, 59, 79, 100,
               0, 20, 41, 59, 79, 100,
               0, 20, 41, 59, 79, 100),
  intensity = c(
    0,
    7576,
    12410,
    14971,
    14029,
    15779,
    0,
    8190,
    12312,
    14840,
    13867,
    15490,
    0,
    7361,
    12004,
    14722,
    13605,
    15639
  )
)

df_0Li <- tibble::tibble(
  type = c(
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge',
    'purge'
  ),
  run = c(1, 1, 1, 1, 1, 1,
          2, 2, 2, 2, 2, 2,
          3, 3, 3, 3, 3, 3),
  salt_wt = c(0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0),
  nile_red = c(0, 24, 36, 61, 73, 85,
               0, 24, 36, 61, 73, 85,
               0, 24, 36, 61, 73, 85),
  intensity = c(
    0,
    5661,
    9557,
    14330,
    15445,
    15816,
    0,
    5826,
    9493,
    14272,
    15456,
    15771,
    0,
    5621,
    9726,
    14164,
    15414,
    15707
  )
)

df_combined <-
  dplyr::bind_rows(df_30Li_cal, df_30Li_purge, df_0Li_cal, df_0Li)

df_fluorescence <- ggplot2::ggplot(
  data = df_combined |>
    dplyr::mutate(salt_wt = paste0(salt_wt, '% LiTFSI')),
  ggplot2::aes(
    x = nile_red,
    y = intensity,
    color = type,
    group = type
  )
) +
  cowplot::theme_half_open() +
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
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = 'black',
                                         size = 1.2,
                                         fill = NA,),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = ggplot2::unit(50, "pt"),
    legend.box.margin = ggplot2::margin(0,0,-10,-10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16,
                                       vjust = -6,
                                       hjust = -0.02),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")
  ) +
  ggplot2::geom_point(size = 1.5) +
  ggpmisc::stat_poly_eq(
    method = lm,
    formula = y ~ stats::poly(x, 2),
    label.y = c(.5, 0.9),
    label.x = c(0.85, 0.05)
  ) +
  # ggpmisc::stat_poly_line(
  #   method = 'glm',
  #   formula = y ~ stats::poly(x, 2),
  #   se = FALSE,
  #   fullrange = TRUE,
  # ) +
  ggplot2::geom_smooth(
    method = 'lm',
    formula = y ~ stats::poly(x, 2),
    se = FALSE
  ) +
  ggplot2::labs(x = 'Nile Red (wt%)',
                y = 'Intensity (AU)',
                title = 'd)')  +
  ggplot2::facet_grid(salt_wt ~ .) +
  ggplot2::scale_color_manual(#labels = c('SPOC', 'Traditional'),
    values = c('#440154FF', '#55C667FF'))

ggplot2::ggsave(
  df_fluorescence,
  file = glue::glue('{folder_destination}2410-Fluorescence_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)
