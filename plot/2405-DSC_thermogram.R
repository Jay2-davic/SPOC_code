# dsc processing

file1 <-
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\DSC\\Recovered_11062024\\SCHWARTZ28\\spe\\10-23-23-pegda-pegmea-0a380-0litfsi.txt'
file2 <-
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\DSC\\Recovered_11062024\\SCHWARTZ28\\spe\\10-23-23-pegda-pegmea-0a380-0litfsi-2.txt'

folder_destination <- glue::glue(
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

file_read <- function(file) {
  file |>
  readr::read_csv() |>
  dplyr::filter(!dplyr::row_number() %in% 1:7) |>
  tidyr::separate(sep = '\t',
                  col = 1,
                  into = c('A', 'B', 'C', 'D')) |>
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  dplyr::filter(!dplyr::row_number() %in% 1) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric),
                file = basename(file),
                file = stringr::str_replace_all(file, 'litfsi[.]', 'litfsi-1.'),
                file = stringr::str_replace_all(file, '10-23-23-pegda-pegmea-0a380-0litfsi', ''),
                file = stringr::str_replace_all(file, '-', ''),
                file = stringr::str_replace_all(file, '.txt', ''),
                cycle = as.character(as.numeric(file) + 1),
                cycle = glue::glue('Cycle {cycle}'))
}

file_df <- file_read(file1) |>
  dplyr::bind_rows(file_read(file2))

dsc_baseline <- ggplot2::ggplot( 
  data = file_df 
  #  dplyr::filter(temperature >= -96)
  ,
  ggplot2::aes(x = temperature,
               y = heat_flow_normalized,
               group = cycle |> as.factor(),
               color = cycle |> as.factor())
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1) +
  ggplot2::scale_color_manual(
    values = c("#287C8EFF", "#440154"),) +
  ggplot2::labs(x = expression('Temperature (\u00B0C)'),
                y = bquote("exotherm" %->% ""),
                title = 'b)') +
  ggplot2::annotate(
    'segment',
    x = -60.05,
    y = -0.15,
    yend = -0.1,
    xend = -20,
    color = 'blue',
    alpha = 0.9,
    arrow = ggplot2::arrow(
      ends = 'first',
      type = 'closed',
      length = ggplot2::unit(0.2, 'cm')
    ),
    lwd = 0.2
  ) +
  ggplot2::annotate(
    'text',
    label = expression(T[g] == -62 * "°C"),
    x = 10,
    y = -0.08,
    color = 'blue',
    size = 4
  ) +
  ggplot2::annotate(
    'text',
    label = expression("9 PEGMEA:1 PEGDA"),
    x = 20,
    y = 0.02,
    color = 'black',
    size = 4
  ) +
  #ggplot2::facet_grid(cycle ~ .) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10,
                                        family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12,
                                       family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12,
                                      family = 'Arial'),
    axis.text.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = 'black',
                                         size = 1.2,
                                         fill = NA,),
    strip.background = ggplot2::element_blank(),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16,
                                       vjust = -6,
                                       hjust = -0.02),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")
  )

ggplot2::ggsave(
  dsc_baseline,
  file = glue::glue('{folder_destination}2411-dsc_baseline.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

