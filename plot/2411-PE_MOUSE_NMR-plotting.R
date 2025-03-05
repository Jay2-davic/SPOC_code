# Mouse NMR

user_name <- 'jimenez45'
version_number <- '2,26'

folder_destination <- glue::glue(
  'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\'
)

# loads the functions
source('~/Git/23-spoc_code/PE_plotTemplates/2401-GridSearch_functions-jimenez45.R')

# load the data
final_db <-
  readr::read_csv(
    glue::glue(
      'C:\\Users\\{user_name}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v{version_number}.csv'
    )
  ) |>
  # dplyr::mutate(fcomp_formulation =
  # stringr::str_replace(fcomp_formulation, '4.28', '4.29')) |>
  dplyr::filter(date_printed != '2024-07-22',
                date_printed > '2024-02-01',
                date_printed != '2024-09-26',
                date_printed != '2024-08-15',
                date_printed != '2024-05-06',
                date_printed != '2024-10-07',
                date_printed != '2024-03-04') |>
  dplyr::mutate(fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation,
                                                                    '4.28',
                                                                    '4.29'),
                humidity = dplyr::case_when(
                  print_type == 'Coin cell' ~ 0.00001,
                  TRUE ~ humidity/100
                ),
                # adds validation labels to misformulated prints
                validation = dplyr::case_when(
                  date_printed == '2024-06-27' ~ 'YES',
                  date_printed == '2024-08-16' ~ 'YES',
                  TRUE ~ 'NO'
                )) |>
  dplyr::filter(
    dplyr::case_when(fcomp_salt_wt_pct != 0 ~ ionic_conductivity_final >= 10^-7,
                     TRUE ~ TRUE),
    fcomp_additive != 'AlO3'
  ) |>
  dplyr::relocate(ionic_conductivity_final, .before = print_type) |>
  dplyr::filter(date_printed == '2024-10-04',
                ionic_conductivity_final <= 10^-3) |>
  dplyr::mutate(
    sample_loc = stringr::str_extract_all(mpr_files_cycle, '_[A-Z]\\d_'), 
    sample_loc = stringr::str_remove_all(sample_loc, '_')
  ) |>
  z_score_outlier_remove('PCB', 1.5)

data_loc <- Ropj::read.opj(
  'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\PCBBoardsamples.opj'
) |>
  purrr::pluck(1) |>
  purrr::pluck(3) |>
  dplyr::rename(
    sample_loc = 1,
    t2a_ms = 2,
    t2a_ms_err = 3,
    ampA = 4,
    ampA_ms_err = 5,
    t2b_ms = 6,
    t2b_ms_err = 7,
    ampB = 8,
    ampB_err = 9,
    relative_fraction_b = 10,
    t1_s = 11,
    t1_s_err = 12,
    del = 13,
    li_t2a_ms = 14,
    li_t2a_ms_err = 15
  ) |>
  dplyr::filter(dplyr::row_number() %in% 1:16) |>
  dplyr::mutate(
    dplyr::across(
      -sample_loc, ~ as.numeric(.)),
    additive_wt_pct = dplyr::case_when(
      stringr::str_detect(sample_loc, '\\dA') == TRUE ~ 0,
      stringr::str_detect(sample_loc, '\\dB') == TRUE ~ 2.14,
      stringr::str_detect(sample_loc, '\\dC') == TRUE ~ 4.28,
      stringr::str_detect(sample_loc, '\\dD') == TRUE ~ 6.42,
      stringr::str_detect(sample_loc, '\\dE') == TRUE ~ 8.57,
      stringr::str_detect(sample_loc, '\\dF') == TRUE ~ 10.71,
      stringr::str_detect(sample_loc, '\\dG') == TRUE ~ 12.86,
      stringr::str_detect(sample_loc, '\\dH') == TRUE ~ 15
      ),
    sample_loc = stringr::str_replace_all(sample_loc, "(\\d)([A-Z])", "\\2\\1")
  )

df_comb <- data_loc |>
  dplyr::full_join(final_db) |>
  dplyr::filter(!is.na(t2a_ms),
                stringr::str_detect(sample_loc, '2') == TRUE
                ) |>
  dplyr::group_by(fcomp_formulation, sample_loc) |>
  dplyr::mutate(
    mean_IC = mean(ionic_conductivity_final)
  ) |>
  dplyr::distinct(fcomp_additive_wt_pct, mean_IC, .keep_all = TRUE)

# ggplot2::ggplot(
#   df_comb,
#   ggplot2::aes(
#     y = log10(mean_IC),
#     x = sample_loc,
#   )) +
#   ggplot2::geom_point(size = 2)
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = log10(mean_IC),
#                   x = t2a_ms,
#                 )) +
#   cowplot::theme_half_open() +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'glm',
#                        formula = y ~ x,
#                        se = FALSE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq()
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = mean_IC |> log10(),
#                   x = t2b_ms,
#                 )) +
#   cowplot::theme_half_open() +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'glm',
#                        formula = y ~ x,
#                        se = FALSE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq()
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = log10(mean_IC),
#                   x = ampB,
#                 )) +
#   cowplot::theme_half_open() +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'lm',
#                        formula = y ~ x,
#                        se = FALSE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq()
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = log10(mean_IC),
#                   x = ampA
#                 )) +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'lm',
#                        formula = y ~ x,
#                        se = TRUE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq()
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = log10(mean_IC),
#                   x = li_t2a_ms,
#                 )) +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'lm',
#                        formula = y ~ x,
#                        se = FALSE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq() +
#   ggplot2::
# 
# ggplot2::ggplot(data = df_comb,
#                 ggplot2::aes(
#                   y = log10(mean_IC),
#                   x = log10(t1_s),
#                 )) +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::stat_smooth(method = 'lm',
#                        formula = y ~ x,
#                        se = TRUE,
#                        geom = 'smooth') +
#   ggpmisc::stat_poly_eq()
# 
# 
Li_plot <- ggplot2::ggplot(data = df_comb,
                ggplot2::aes(
                  y = li_t2a_ms,
                  x = fcomp_additive_wt_pct |> as.factor(),
                  fill = fcomp_additive_wt_pct |> as.factor()
                )) +
  cowplot::theme_half_open() +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = li_t2a_ms - li_t2a_ms_err,
                         ymax = li_t2a_ms + li_t2a_ms_err),
                         width = 0.5) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, 1.6)) +
  ggplot2::labs(x = 'Ae380 (wt%)',
                y = expression(''^7*"Li"~T[2]~"(ms)"),
                title = 'a)'
  ) +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(size = 10),
                 axis.title.x = ggplot2::element_text(size = 12),
                 legend.position = 'none',
                 plot.title.position = 'plot',
                 plot.title = ggplot2::element_text(size = 16,
                                                    vjust = -6,
                                                    hjust = -0.02),
                 plot.margin=grid::unit(c(-5,0,0,3), "mm"))

ggplot2::ggsave(
  Li_plot,
  file = glue::glue('{folder_destination}2410-MOUSE_NMR_LiMobility.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

hT1_T2_plot <- ggplot2::ggplot(data = df_comb |>
                  dplyr::select(fcomp_additive_wt_pct, t2a_ms, t2a_ms_err,
                                t2b_ms, t2b_ms_err),
                ggplot2::aes(
                  x = fcomp_additive_wt_pct |> as.factor()
                )) +
  cowplot::theme_half_open() +
  ggplot2::geom_col(ggplot2::aes(y = t2a_ms,
                                 fill = 'A'),
                    alpha = 0.95,
                    width = 0.4,
                    position = ggplot2::position_nudge(-0.2)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = t2a_ms - t2a_ms_err,
                                      ymax = t2a_ms + t2a_ms_err),
                         position = ggplot2::position_nudge(-0.2),
                         width = 0.4) +
  ggplot2::geom_col(ggplot2::aes(y = t2b_ms,
                                 fill = 'B'),
                    position = ggplot2::position_nudge(0.2),
                    alpha = 0.95,
                    width = 0.4) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = t2b_ms - t2b_ms_err,
                                      ymax = t2b_ms + t2b_ms_err),
                         position = ggplot2::position_nudge(0.2),
                         width = 0.4) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::scale_y_continuous(trans = 'log10',
                              breaks = c(0, 0.1, 1, 10),
                              minor_breaks = scales::breaks_width(0.2)
                              ) +
  ggplot2::theme(legend.position = 'right',
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 12,
                                                     family = 'Arial'),
                 axis.title.x = ggplot2::element_text(size = 12,
                                                    family = 'Arial'),
                 axis.title.y = ggplot2::element_text(size = 12,
                                                      family = 'Arial'),
                 axis.text = ggplot2::element_text(size = 12,
                                                   family = 'Arial'),
                 axis.minor.ticks.length.y = ggplot2::unit(10, 'pt'),
                 legend.key.size = ggplot2::unit(15, 'pt'),
                 legend.spacing.y = ggplot2::unit(10, "pt"),
                 legend.margin = ggplot2::margin(0, 0, 0, 0),
                 legend.spacing.x = ggplot2::rel(0.5),
                 legend.box.margin = ggplot2::margin(0,0,-20,-10),
                 plot.title.position = 'plot',
                 plot.title = ggplot2::element_text(size = 16,
                                                    vjust = -6,
                                                    hjust = -0.02),
                 plot.margin=grid::unit(c(-5,0,0,3), "mm")) +
  ggplot2::labs(x = 'Ae380 (wt%)',
                y = expression(~T[2]~"(ms)"),
                title = 'b)'
  ) +
  ggplot2::scale_fill_viridis_d(labels = c(expression(T[2]^a), expression(T[2]^b)))

ggplot2::ggsave(
  hT1_T2_plot,
  file = glue::glue('{folder_destination}2410-MOUSE_NMR_proton.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)
