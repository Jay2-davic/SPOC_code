# ============================================================================
# Rheology (Photorheology) Processing and Visualization
# 
# Description: Analysis of storage and loss moduli during UV-curing of polymer
#              electrolyte samples with varying Aero380 additive and LiTFSI
#              salt concentrations. Identifies gel point via moduli crossover.
#
# Author: [Your Name]
# Date: [Date]
# Version: 1.0
#
# Required packages:
#   - tibble: for creating tibbles
#   - dplyr: for data manipulation
#   - ggplot2: for plotting
#   - cowplot: for plot themes
#   - glue: for string interpolation
#   - grid: for plot margins
#   - stringr: for string manipulation
#   - stringi: for advanced string operations
#   - readxl: for reading Excel files
#   - janitor: for cleaning column names
#   - purrr: for functional programming
#   - tidyr: for data tidying
#   - scales: for axis formatting
#   - stats: for na.omit
#   - egg: for facet tagging
# ============================================================================

# Load required packages -------------------------------------------------------
library(tibble)
library(dplyr)
library(ggplot2)
library(cowplot)
library(glue)
library(grid)
library(stringr)
library(stringi)
library(readxl)
library(janitor)
library(purrr)
library(tidyr)
library(scales)
library(stats)
library(egg)

# Set output directory ---------------------------------------------------------
folder_destination <- glue::glue('~/Git/SPOC_code/figs')

# TGA data directory
folder_loc <- glue::glue(
  '~/Git/SPOC_code/data/photorheology'
)

# Create output directory if it doesn't exist
if (!dir.exists(folder_destination)) {
  dir.create(folder_destination, recursive = TRUE)
}

# File discovery ---------------------------------------------------------------
files <- tibble::tibble(
  filepath = list.files(
    folder_loc,
    full.names = TRUE,
    pattern = 'xls'
  )
)

# Function to process rheology data files --------------------------------------
#' Clean and process rheology XLS files
#' 
#' @param data Path to the rheology Excel file
#' @return Processed tibble with moduli data and calculated parameters
#' @details Reads sheet 2, calculates log transforms, and identifies plateau regions
clean_xls <- function(data) {
  data |>
    readxl::read_xls(sheet = 2) |>
    janitor::row_to_names(row_number = 1) |>
    dplyr::filter(!dplyr::row_number() %in% c(1:3)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
    janitor::clean_names() |>
    dplyr::mutate(
      # Calculate log transforms for moduli
      loss_log = log10(loss_modulus),
      storage_log = log10(storage_modulus),
      # Calculate region changes for plateau identification
      region_loss = abs(loss_log - dplyr::lag(loss_log)),
      region_storage = abs(storage_log - dplyr::lag(storage_log))
    )
}

# Process all files and extract metadata ---------------------------------------
cleaning_files <- files |>
  dplyr::mutate(
    # Extract filename from full path
    files = stringi::stri_replace_all_regex(
      filepath,
      folder_loc,
      ''
    ),
    files = files |> basename() |> tools::file_path_sans_ext(),
    # Apply cleaning function to each file
    data = purrr::map(filepath, clean_xls)
  ) |>
  tidyr::separate(
    files,
    sep = '_',
    into = c(
      'date', 'poly_rat1', 'poly1', 'poly_rat2', 'poly2', 'additive_wt', 'additive',
      'salt_wt', 'salt'
    )
  ) |>
  tidyr::unnest(cols = data) |>
  dplyr::select(-temperature) |> 
  dplyr::group_by(salt_wt, additive_wt) |>
  dplyr::mutate(
    # Identify plateau regions for loss modulus
    region_loss_lab = dplyr::case_when(
      (region_loss <= 0.6 & salt_wt == 0 & additive_wt == 0) ~ 'plateau',
      (region_loss <= 0.5 & salt_wt == 30 & additive_wt == 0) ~ 'plateau',
      dplyr::row_number() %in% 1 ~ 'plateau',
      TRUE ~ 'no_plateau'
    ),
    # Identify plateau regions for storage modulus
    region_storage_lab = dplyr::case_when(
      dplyr::row_number() %in% 1 ~ 'plateau',
      (region_storage <= 0.1 & salt_wt == 0 & additive_wt == 0) ~ 'plateau',
      (region_storage <= 0.5 & salt_wt == 30 & additive_wt == 0) ~ 'plateau',
      (oscillation_torque <= 15.8 & salt_wt == 30 & additive_wt == 15) ~ 'plateau',
      (oscillation_torque <= 0.1 & salt_wt == 0 & additive_wt == 15) ~ 'plateau',
      TRUE ~ 'no_plateau'
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(salt_wt, additive_wt) |>
  # Correct time to start at first non-plateau point and convert to seconds
  dplyr::mutate(
    corr_step_time = step_time - dplyr::first(step_time[region_storage_lab == 'no_plateau']),
    corr_step_time = corr_step_time * 60
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(salt_wt, additive_wt, region_loss_lab) |>
  stats::na.omit() |>
  # Find moduli crossover point (gel point)
  dplyr::mutate(
    cross_over = abs(storage_log - loss_log),
    min_cross = dplyr::case_when(
      cross_over == min(cross_over) & region_loss_lab == 'no_plateau' ~ 'YES',
      TRUE ~ 'NO'
    )
  )

# Create photorheology plot ----------------------------------------------------
plot <- ggplot2::ggplot(
  data = cleaning_files |> 
    stats::na.omit(loss_modulus) |> # Remove NA values to prevent plot breaks
    dplyr::mutate(
      additive_wt = glue::glue('{additive_wt}% Ae380'),
      salt_wt = glue::glue('{salt_wt}% LiTFSI'),
      salt_add = glue::glue('{additive_wt}% Ae380, {salt_wt}% LiTFSI')
    ),
  ggplot2::aes(x = corr_step_time)
) +
  cowplot::theme_half_open() +
  # Storage modulus points and lines
  ggplot2::geom_point(
    size = 1,
    ggplot2::aes(y = storage_modulus, color = 'Storage\nModulus'),
    shape = 2
  ) +
  ggplot2::geom_path(
    size = 0.5,
    alpha = 0.5,
    ggplot2::aes(y = storage_modulus, color = 'Storage\nModulus')
  ) +
  # Loss modulus points and lines
  ggplot2::geom_point(
    size = 1,
    ggplot2::aes(y = loss_modulus, color = 'Loss\nModulus')
  ) +
  ggplot2::geom_path(
    size = 0.5,
    alpha = 0.5,
    ggplot2::aes(y = loss_modulus, color = 'Loss\nModulus')
  ) +
  # Scale settings
  ggplot2::scale_y_continuous(
    trans = 'log10',
    labels = scales::trans_format('log10', scales::math_format(10^.x))
  ) +
  ggplot2::scale_x_continuous(limits = c(-25, 125)) +
  # Add vertical lines at crossover points
  ggplot2::geom_vline(
    data = cleaning_files |> 
      dplyr::filter(min_cross == 'YES') |> 
      stats::na.omit(storage_modulus) |>
      dplyr::mutate(
        additive_wt = glue::glue('{additive_wt}% Ae380'),
        salt_wt = glue::glue('{salt_wt}% LiTFSI')
      ),
    ggplot2::aes(xintercept = corr_step_time),
    linetype = 'dashed'
  ) +
  # Facet by salt and additive content
  ggplot2::facet_grid(
    salt_wt ~ additive_wt,
    scales = 'fixed'
  ) +
  ggplot2::labs(
    x = 'Time (s)',
    y = 'Modulus (MPa)',
    color = '',
    title = 'e)'
  ) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 9, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10, family = 'Arial'),
    panel.border = ggplot2::element_rect(color = 'black', size = 1.2, fill = NA),
    legend.key.size = ggplot2::unit(10, 'pt'),
    legend.spacing.y = ggplot2::unit(10, "pt"),
    legend.key.spacing.y = ggplot2::unit(10, "pt"),
    legend.spacing.x = ggplot2::unit(0, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, -10, -10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.02),
    plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE)) +
  ggplot2::scale_color_manual(
    values = c('#440154FF', '#55C667FF'),
    limits = c('Storage\nModulus', 'Loss\nModulus')
  )

# Add facet labels with gel times
plot <- egg::tag_facet(
  plot, 
  x = 25, 
  y = 10^-6, 
  vjust = 0.15, 
  hjust = 0,
  open = "", 
  close = "",
  fontface = 4,
  size = 3,
  family = "arial",
  tag_pool = c(
    't = 3s\n0% Ae380\n0% LiTFSI', 
    't = 19s\n15% Ae380\n0% LiTFSI', 
    't = 3s\n0% Ae380\n30% LiTFSI',
    't = 3s\n15% Ae380\n30% LiTFSI'
  )
)

# Save photorheology plot ------------------------------------------------------
ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}/photorheology_facet.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Print completion message
message("Photorheology analysis plot has been successfully generated and saved to: ", folder_destination)