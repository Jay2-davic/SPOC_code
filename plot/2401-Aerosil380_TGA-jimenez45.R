#TGA

user_name  <- 'jimenez45'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

tga_folder_loc <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\TGA\\'
)

files <- tibble::tibble(
  filepath = list.files(tga_folder_loc,
                        full.names = TRUE),
  file_type = dplyr::case_when(
    stringr::str_detect(filepath, 'txt') == TRUE ~ 'params',
    stringr::str_detect(filepath, '.xl') == TRUE ~ 'data',
    TRUE ~ NA
  ),
  sample_name = stringr::str_remove_all(filepath,
                                        '.*TGA\\\\')
) |>
  dplyr::mutate(
    sample_name = stringr::str_replace_all(sample_name,
                                           '20t',
                                           '20wt'),
    salt_wt = stringr::str_extract_all(sample_name,
                                       '\\d+wtLiTFSI|LiTFSI'),
    salt_wt = dplyr::case_when(
      stringr::str_detect(sample_name, 'PegdaPegmea') == TRUE ~ 0,
      stringr::str_detect(salt_wt, '\\d+') == TRUE ~ stringi::stri_replace_all_regex(salt_wt, '[A-Za-z]+', '') |> as.double(),
      stringr::str_detect(sample_name, 'LiTFSI[.]') == TRUE ~ 100,
    ),
    additive_wt = stringr::str_extract_all(sample_name,
                                           '\\d+wtAE380'),
    additive_wt = dplyr::case_when(
      stringr::str_detect(additive_wt, '\\d+wtAE380') == TRUE ~ stringi::stri_replace_all_regex(additive_wt, 'wtAE380', '') |> as.double(),
      TRUE ~ 0
    )
  )

tga_unpack <- function(file) {
  file |>
    readxl::read_xls(sheet = 2) |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names() |>
    dplyr::filter(!dplyr::row_number() %in% 1) |>
    dplyr::rename(weight = 3,
                  weight_pct = 4) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.double)) |>
    dplyr::filter(temperature >= 150) |>
    dplyr::mutate(corr_weight_pct = weight / max(weight) * 100)
}

files_data <- files |>
  dplyr::filter(file_type == 'data') |>
  dplyr::mutate(data = purrr::map(filepath, tga_unpack)) |>
  tidyr::unnest(cols = data)

plot <- ggplot2::ggplot(
  data = files_data |>
    dplyr::mutate(
      salt_wt = as.factor(salt_wt),
      salt_wt = dplyr::case_when(salt_wt == 100 ~ 'neat',
                                 TRUE ~ salt_wt),
      additive_wt = as.factor(additive_wt),
      name = dplyr::case_when(
        salt_wt == 'neat' ~ glue::glue('LiTFSI neat'),
        TRUE ~ glue::glue(' \nAe380 {additive_wt}%,\nLiTFSI {salt_wt}%\n '))
    ),
  ggplot2::aes(
    x = temperature,
    y = corr_weight_pct,
    color = name
  )
) +
  cowplot::theme_half_open() +
  #cowplot::background_grid() +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::scale_x_continuous(limits = c(150, 400)) +
  ggplot2::scale_y_continuous(limits = c(50, 100)) +
  ggplot2::geom_hline(
    yintercept = 95,
    linetype = 'dashed',
    size = 1,
    alpha = 0.5
  ) +
  ggplot2::annotate('text',
                    label = '5% weight loss',
                    x = 200,
                    y = 92,
                    size = 4,
                    alpha = 0.75) +
  ggplot2::labs(x = 'Temperature (\u00B0C)',
                y = 'Weight (%)',
                title = 'a)') +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_text(size = 10,
                                        family = 'Arial'),
    legend.text = ggplot2::element_text(size = 8,
                                        family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12,
                                       family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12,
                                      family = 'Arial'),
    legend.key.size = ggplot2::unit(15, 'pt'),
    legend.key.spacing.y = ggplot2::unit(2, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.spacing.x = ggplot2::unit(10, "pt"),
    legend.box.margin = ggplot2::margin(0,0,-10,-10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16,
                                       vjust = -6,
                                       hjust = -0.02),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")) +
  ggplot2::guides(
    # linetype = ggplot2::guide_legend(title = 'AE380 wt%',
    #                                  title.position = 'top',
    #                                  ncol = 1),
    color = ggplot2::guide_legend(title = 'Formulation',
                                  keywidth = 0.75,
                                  title.position = 'top',
                                  ncol = 1,
                                  order = 'reverse'),
    
  ) +
  ggplot2::scale_color_viridis_d()


ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}2410-TGA_add_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)
