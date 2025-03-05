# opening trios files

#rheology

folder_loc <-
  r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Rheometer)'

user_name <- 'jimenez45'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

files <- tibble::tibble(filepath = list.files(folder_loc,
                                              full.names = TRUE,
                                              pattern = 'xls'))

clean_xls <- function(data) {
  data |>
    readxl::read_xls(sheet = 2) |>
    janitor::row_to_names(row_number = 1) |>
    dplyr::filter(!dplyr::row_number() %in% c(1:3)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                as.numeric)) |>
    janitor::clean_names() |>
    dplyr::mutate(
      loss_log = log10(loss_modulus),
      storage_log = log10(storage_modulus),
      region_loss = abs(loss_log - dplyr::lag(loss_log)),
      region_storage = abs(storage_log - dplyr::lag(storage_log)),
      #group_name = glue::glue('{additive_wt} wt% Ae380, {salt_wt} wt% LiTFSI')
      )
}

cleaning_files <- files |>
  dplyr::mutate(
    files = stringi::stri_replace_all_regex(
      filepath,
      'C:\\\\Users\\\\jimenez45\\\\OneDrive - LLNL\\\\General - High-Throughput Polymer Electrolytes DIW\\\\Rheometer/',
      ''
    ),
    salt_wt = stringr::str_extract_all(files, '\\d+litfsi') |>
      stringr::str_remove_all('[a-z]+'),
    additive_wt = stringr::str_extract_all(files, '\\d+aero') |>
      stringr::str_remove_all('[a-z]+'),
    data = purrr::map(filepath, clean_xls)
  ) |>
  tidyr::unnest(cols = data) |>
  dplyr::select(-temperature) |> 
  dplyr::group_by(salt_wt, additive_wt) |>
  dplyr::mutate(region_loss_lab = dplyr::case_when(
                  (region_loss <= 0.6 & salt_wt == 0 & additive_wt == 0) ~ 'plateau',
                  (region_loss <= 0.5 & salt_wt == 30 & additive_wt == 0) ~ 'plateau',
                  dplyr::row_number() %in% 1 ~ 'plateau',
                  TRUE ~ 'no_plateau'),
                region_storage_lab = dplyr::case_when(
                  dplyr::row_number() %in% 1 ~ 'plateau',
                  (region_storage <= 0.1 & salt_wt == 0 & additive_wt == 0) ~ 'plateau',
                  (region_storage <= 0.5 & salt_wt == 30 & additive_wt == 0) ~ 'plateau',
                  (oscillation_torque <= 15.8 & salt_wt == 30 & additive_wt == 15) ~ 'plateau',
                  (oscillation_torque <= 0.1 & salt_wt == 0 & additive_wt == 15) ~ 'plateau',
                  TRUE ~ 'no_plateau')) |>
  dplyr::ungroup() |>
  dplyr::group_by(salt_wt, additive_wt) |>
  dplyr::mutate(corr_step_time = step_time - dplyr::first(step_time[region_storage_lab == 'no_plateau']),
                corr_step_time = corr_step_time * 60) |>
  dplyr::ungroup() |>
  dplyr::group_by(salt_wt, additive_wt, region_loss_lab) |>
  stats::na.omit() |>
  dplyr::mutate(cross_over = 
                    abs(storage_log - loss_log),
                min_cross = dplyr::case_when(cross_over == min(cross_over) & region_loss_lab == 'no_plateau' ~ 'YES',
                                             TRUE ~ 'NO'))

plot <- ggplot2::ggplot(data = cleaning_files |> 
                  stats::na.omit(loss_modulus) |> # plots without breaks
                  dplyr::mutate(additive_wt = glue::glue('{additive_wt}% Ae380'),
                                salt_wt = glue::glue('{salt_wt}% LiTFSI'),
                                salt_add = glue::glue('{additive_wt}% Ae380, {salt_wt}% LiTFSI') ),
                ggplot2::aes(x = corr_step_time))+
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1,
                      ggplot2::aes(y = storage_modulus,
                                   color = 'Storage\nModulus'),
                      shape = 2) +
  ggplot2::geom_path(size = 0.5,
                     alpha = 0.5,
                     ggplot2::aes(y = storage_modulus,
                                  color = 'Storage\nModulus')) +
  ggplot2::geom_point(size = 1,
                      ggplot2::aes(y = loss_modulus,
                                   color = 'Loss\nModulus')) +
  ggplot2::geom_path(size = 0.5,
                     alpha = 0.5,
                     ggplot2::aes(y = loss_modulus,
                                  color = 'Loss\nModulus')) +
  ggplot2::scale_y_continuous(trans = 'log10',
                              labels = scales::trans_format('log10', scales::math_format(10^.x))) +
  ggplot2::scale_x_continuous(limits = c(-25, 125)) +
  #ggplot2::scale_color_continuous()
  ggplot2::geom_vline(data = cleaning_files |> dplyr::filter(min_cross == 'YES') |> 
                         stats::na.omit(storage_modulus) |> # plots without breaks
                         dplyr::mutate(additive_wt = glue::glue('{additive_wt}% Ae380'),
                                       salt_wt = glue::glue('{salt_wt}% LiTFSI')),
                      ggplot2::aes(xintercept = corr_step_time),
                      linetype = 'dashed') +
  ggplot2::facet_grid(salt_wt ~ additive_wt,
                      scales = 'fixed',
                      ) +
  ggplot2::labs(x = 'Time (s)',
                y = 'Modulus (MPa)',
                color = '',
                title = 'e)') +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 9,
                                        family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12,
                                       family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12,
                                      family = 'Arial'),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10,
                                       family = 'Arial'),
    panel.border = ggplot2::element_rect(color = 'black',
                                         size = 1.2,
                                         fill = NA,),
    legend.key.size = ggplot2::unit(10, 'pt'),
    legend.spacing.y = ggplot2::unit(10, "pt"),
    legend.key.spacing.y = ggplot2::unit(10, "pt"),
    # legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = ggplot2::unit(0, "pt"),
    legend.box.margin = ggplot2::margin(0,0,-10,-10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16,
                                       vjust = -6,
                                       hjust = -0.02),
    plot.margin=grid::unit(c(-5,0,0,3), "mm")
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE)) +
  ggplot2::scale_color_manual(#labels = c('SPOC', 'Traditional'),
    values = c('#440154FF', '#55C667FF'),
    limits = c('Storage\nModulus', 'Loss\nModulus'))

  plot <- egg::tag_facet(plot, x = 25, y = 10^-6, 
                 vjust = 0.15, hjust = 0,
                 open = "", close = "",
                 fontface = 4,
                 size = 3,
                 family = "arial",
                 tag_pool = c('t = 3s\n0% Ae380\n0% LiTFSI', 
                              't = 19s\n15% Ae380\n0% LiTFSI', 
                              't = 3s\n0% Ae380\n30% LiTFSI',
                              't = 3s\n15% Ae380\n30% LiTFSI'))

ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}2410-Photorheology_plot_combined.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)
