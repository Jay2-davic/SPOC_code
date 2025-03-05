#

#DSC

#take the processed values straight from excel

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

df_0 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-66.01,-65.84,-66.95,-65.82,-66.22,-65.96,-65.56,-65.07),
  salt_wt_pct = c(0, 0, 0, 0, 0, 0, 0, 0),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c(
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero'
  )
)

df_30 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-53.35,-51.96,-50.95,-57.77,-57.88,-53.67,-51.61,-54.34),
  salt_wt_pct = c(30, 30, 30, 30, 30, 30, 30, 30),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c(
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero'
  )
)

df_20 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(
    -51.825,
    -51.95,
    -52.39,
    -52.985,
    -53.385,
    -53.03,
    -52.045,
    -52.99
  ),
  salt_wt_pct = c(20, 20, 20, 20, 20, 20, 20, 20),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c(
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero'
  )
)

df_10 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-58.43,-58.38,-59.97,-58.78,-59.58,-62.79,-56.27,-60.51),
  salt_wt_pct = c(10, 10, 10, 10, 10, 10, 10, 10),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c(
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero',
    'varyAero'
  )
)

df_a0 <- tibble::tibble(
  salt_wt_pct = c(0, 4.28, 8.57, 12.86, 17.14, 21.42, 25.71, 30),
  glass_transition = c(-70.81,-73.9,-68.59,-64.23,-62.48,-60.95,-57.05,-61.37),
  additive_wt_pct = c(0, 0, 0, 0, 0, 0, 0, 0),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c(
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI',
    'varyLiTFSI'
  )
)

df_additives <- tibble::tibble(
  salt_wt_pct = c(0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0),
  glass_transition = c(-64.49, -64.6, -64.59, -64.87, -64.67, -65.09, -65.02, -65.25,
                       -66.35666667, -66.47, -67.36333333, -67.61, -67.395, NA, -66.06, -65.18,
                       NA, -61.09, -64.71, -61.94, -61.42, -65.25, -61.94, -64.42,
                       NA, -64.02, -67.01, -67.63, -67.13, -67.08, -67.22, -67.35),
  additive_wt_pct = c(15, 12.86, 10.71, 8.57, 6.42, 4.28, 2.14, 0,
                      15, 12.86, 10.71, 8.57, 6.42, 4.28, 2.14, 0,
                      0, 0.71, 1.43, 2.14, 2.86, 3.57, 4.28, 5,
                      0, 2.14, 4.28, 6.42, 8.57, 10.71, 12.86, 15),
  additive = c('Aero90', 'Aero90', 'Aero90', 'Aero90', 'Aero90', 'Aero90', 'Aero90', 'Aero90',
               'TiO2','TiO2','TiO2','TiO2','TiO2','TiO2','TiO2','TiO2',
               'EH5', 'EH5', 'EH5', 'EH5', 'EH5','EH5', 'EH5', 'EH5',
               'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3'),
  notes = c(
    'noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI',
    'noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI',
    'noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI',
    'noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI','noLITFSI')
  )

df_final <- dplyr::bind_rows(df_0, df_10, df_20, df_30, df_a0, df_additives)

dsc_salt_screen_plot <- ggplot2::ggplot(
  data = df_final |> dplyr::filter(additive == 'Aero380',
                                   notes == 'varyAero'),
  ggplot2::aes(
    x = additive_wt_pct,
    y = glass_transition,
    color = salt_wt_pct |> as.factor(),
    group = salt_wt_pct |> as.factor(),
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(size = 1.5,
                     alpha = 0.4) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_text(size = 10,
                                         family = 'Arial'),
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
  ggplot2::scale_color_discrete(name = 'LiTFSI wt%') +
  ggplot2::scale_y_continuous(limits = c(-70, -50),
                              breaks = c(-70, -65, -60, -55, -50)) +
  ggplot2::labs(x = 'Ae380 (wt%)',
                y = 'Glass Transition (\u00B0C)',
                title = 'c)',
                color = 'LiTFSI wt%') +
  ggplot2::scale_color_viridis_d(limits = c('30', '20', '10', '0'))

ggplot2::ggsave(
  dsc_salt_screen_plot,
  file = glue::glue('{folder_destination}2410-DSC_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)


dsc_additive_screen_plot <-
  ggplot2::ggplot(
  data = df_final |> dplyr::filter(salt_wt_pct == 0),
  ggplot2::aes(
    x = additive_wt_pct,
    y = glass_transition,
    color = additive,
    group = additive,
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(x = 'Additive (wt%)',
                y = 'Glass Transition (\u00B0C)',
                title = 'b)') +
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
                                       hjust = -0.01),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")) +
  ggplot2::scale_y_continuous(limits = c(-70, -60)) +
  ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
  ggplot2::scale_color_viridis_d(
    labels = c(
      "Aero380" = "Ae380",
      "Aero90" = "Ae90",
      "Al2O3" = expression('Al'[2]*'O'[3]),
      "TiO2" = expression('TiO'[2])
    ),
    limits = c('EH5', 'Aero90', 'Aero380', 'Al2O3', 'TiO2')
  )

ggplot2::ggsave(
  dsc_additive_screen_plot,
  file = glue::glue('{folder_destination}2410-DSC_add_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)


