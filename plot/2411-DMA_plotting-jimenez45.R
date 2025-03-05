# DMA result

folder_loc <-
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\DMA'

user_name <- 'jimenez45'
version_number <- '2,26'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

files <- tibble::tibble(
  filepath = list.files(folder_loc,
                        full.names = TRUE,
                        recursive = TRUE),
  type = dplyr::case_when(
    stringr::str_detect(filepath, '-2') == TRUE ~ 'data',
    stringr::str_detect(filepath, 'xlsx') == TRUE ~ 'other',
    stringr::str_detect(filepath, '.tri') == TRUE ~ 'raw',
    TRUE ~ 'params'
  )
)

dma_cleaning <- function(data) {
  df <- readr::read_csv(data) |>
    dplyr::filter(!(dplyr::row_number() %in% c(1:11))) |>
    dplyr::rename(data = 1) |>
    tidyr::separate(
      col = 'data',
      sep = '\t',
      into = c('a', 'b', 'c', 'd', 'e', 'f')
    ) |>
    janitor::row_to_names(1) |>
    janitor::clean_names() |>
    dplyr::filter(dplyr::row_number() != 1) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
    dplyr::select(-dplyr::contains('na'))
}

file_only_data <- files |>
  dplyr::filter(type == 'data') |>
  dplyr::mutate(
    # manually add thickness value
    # next time grab thickness data from param file ***
    # then subtract thickness data from the displacement at 0.1 N
    thickness = dplyr::case_when(
      #thickness in mm
      stringr::str_detect(filepath, '_0_') == TRUE ~ 1.157,
      stringr::str_detect(filepath, '_15_') == TRUE ~ 1.712,
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ 1.313
    ),
    # calculate area of square then convert to m^2
    area = dplyr::case_when(
      # area mm^2
      stringr::str_detect(filepath, '_0_') == TRUE ~ 45.312,
      stringr::str_detect(filepath, '_15_') == TRUE ~ 21.939,
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ 22.2194
    ),
    sample = dplyr::case_when(
      # area mm^2
      stringr::str_detect(filepath, '_0_') == TRUE ~ '0%',
      stringr::str_detect(filepath, '_15_') == TRUE ~ '15%',
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ '6.42%'
    ),
    data = furrr::future_map(filepath, dma_cleaning)
  ) |>
  tidyr::unnest(data) |>
  # removes 0.1 N preload step and measurements with no force value
  dplyr::filter(!is.na(force),
                force >= 0.1) |>
  dplyr::group_by(filepath) |>
  dplyr::mutate(
    # subtract displacement at row 1 to zero the baseline
    displacement = displacement / 10^3, # units in um to mm
    corr_displacement = displacement - dplyr::first(displacement),
    compressive_strain = corr_displacement/thickness * 100, # % strain
    # apply clamping factor to stress, times force and divided by area in m^2
    corr_stress = (0.2414 * log(thickness) + 0.4658) * force/(area/10^6),
    # convert compressive stress from p to Mpa
    compressive_stress = corr_stress / 10^6
  )

# plot

plot <- ggplot2::ggplot(data = file_only_data |>
                  dplyr::filter(
                    !stringr::str_detect(filepath, '6,42_Aerosil380_20_LITFSI-b') == TRUE
                  ),
                ggplot2::aes(
                  x = compressive_strain,
                  y = compressive_stress,
                  group = sample |> as.factor(),
                  color = sample |> as.factor()
                )) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1.2) +
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
  ggplot2::scale_y_continuous(limits = c(0, 0.6),
                              breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  ggplot2::scale_x_continuous(limits = c(0, 35),
                              breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  ggplot2::labs(y = 'Compressive Stress (MPa)',
                x = 'Compressive Strain (%)',
                title = 'c)',
                color = "Ae380 wt") +
  ggplot2::scale_color_viridis_d(limits = c('15%', '6.42%', '0%'))

ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}2410-DMA_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

