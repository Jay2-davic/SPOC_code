# ============================================================================
# DMA (Dynamic Mechanical Analysis) Processing and Visualization
# 
# Description: Analysis of compressive stress-strain behavior for polymer
#              electrolyte samples with varying Aero380 additive concentrations
#              at 20% LiTFSI salt content
#
# Author: J. Jimenez
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
#   - readr: for reading CSV files
#   - janitor: for cleaning column names
#   - tidyr: for data tidying
#   - furrr: for parallel processing (optional, can use purrr instead)
# ============================================================================

# Load required packages -------------------------------------------------------
library(tibble)
library(dplyr)
library(ggplot2)
library(cowplot)
library(glue)
library(grid)
library(stringr)
library(readr)
library(janitor)
library(tidyr)
library(furrr)  # Can replace with purrr if parallel processing not needed

# DMA data directory
folder_loc <- "~/Git/SPOC_code/data/thermal_properties/DMA/"

# Output directory for figures
folder_destination <- glue::glue('~/Git/SPOC_code/figs')

# Create output directory if it doesn't exist
if (!dir.exists(folder_destination)) {
  dir.create(folder_destination, recursive = TRUE)
}

# File discovery and classification --------------------------------------------
files <- tibble::tibble(
  filepath = list.files(
    folder_loc,
    full.names = TRUE,
    recursive = TRUE
  ),
  # Classify file types based on naming patterns and extensions
  type = dplyr::case_when(
    stringr::str_detect(filepath, '-2') == TRUE ~ 'data',
    stringr::str_detect(filepath, 'xlsx') == TRUE ~ 'other',
    stringr::str_detect(filepath, '.tri') == TRUE ~ 'raw',
    TRUE ~ 'params'
  )
)

# Function to process DMA data files -------------------------------------------
#' Clean and process DMA raw data files
#' 
#' @param data Path to the DMA data file
#' @return Processed tibble with force and displacement data
#' @details Reads CSV, removes header rows, separates tab-delimited columns,
#'          and converts all data to numeric format
dma_cleaning <- function(data) {
  df <- readr::read_delim(data, delim = ' ') |>
    # Remove header rows
    dplyr::filter(!(dplyr::row_number() %in% c(1:11))) |>
    dplyr::rename(data = 1) |>
    # Separate tab-delimited data
    tidyr::separate(
      col = 'data',
      sep = '\t',
      into = c('a', 'b', 'c', 'd', 'e', 'f')
    ) |>
    # Clean column names
    janitor::row_to_names(1) |>
    janitor::clean_names() |>
    # Remove first row after header
    dplyr::filter(dplyr::row_number() != 1) |>
    # Convert to numeric and remove NA columns
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
    dplyr::select(-dplyr::contains('na'))
}

# Process data files and calculate mechanical properties -----------------------
file_only_data <- files |>
  dplyr::filter(type == 'data') |>
  dplyr::mutate(
    # Sample thickness in mm
    # NOTE: Next time, extract thickness data from parameter files
    thickness = dplyr::case_when(
      stringr::str_detect(filepath, '_0_') == TRUE ~ 1.157,
      stringr::str_detect(filepath, '_15_') == TRUE ~ 1.712,
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ 1.313
    ),
    # Sample area in mm²
    area = dplyr::case_when(
      stringr::str_detect(filepath, '_0_') == TRUE ~ 45.312,
      stringr::str_detect(filepath, '_15_') == TRUE ~ 21.939,
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ 22.2194
    ),
    # Sample identification
    sample = dplyr::case_when(
      stringr::str_detect(filepath, '_0_') == TRUE ~ '0%',
      stringr::str_detect(filepath, '_15_') == TRUE ~ '15%',
      stringr::str_detect(filepath, '_6,42_') == TRUE ~ '6.42%'
    ),
    # Apply cleaning function to each file
    data = furrr::future_map(filepath, dma_cleaning)
  ) |>
  tidyr::unnest(data) |>
  # Filter out preload step (< 0.1 N) and missing force values
  dplyr::filter(
    !is.na(force),
    force >= 0.1
  ) |>
  dplyr::group_by(filepath) |>
  dplyr::mutate(
    # Convert displacement from μm to mm
    displacement = displacement / 10^3,
    # Zero baseline by subtracting initial displacement
    corr_displacement = displacement - dplyr::first(displacement),
    # Calculate compressive strain as percentage
    compressive_strain = corr_displacement / thickness * 100,
    # Apply clamping factor correction to stress calculation
    # Clamping factor: 0.2414 * ln(thickness) + 0.4658
    corr_stress = (0.2414 * log(thickness) + 0.4658) * force / (area / 10^6),
    # Convert stress from Pa to MPa
    compressive_stress = corr_stress / 10^6
  )

# Create stress-strain plot ----------------------------------------------------
plot <- ggplot2::ggplot(
  data = file_only_data |>
    # Exclude outlier sample
    dplyr::filter(
      !stringr::str_detect(filepath, '6,42_Aerosil380_20_LITFSI-b') == TRUE
    ),
  ggplot2::aes(
    x = compressive_strain,
    y = compressive_stress,
    group = as.factor(sample),
    color = as.factor(sample)
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1.2) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_text(size = 10, family = 'Arial'),
    legend.text = ggplot2::element_text(size = 10, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    legend.key.size = ggplot2::unit(15, 'pt'),
    legend.spacing.y = ggplot2::unit(10, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = ggplot2::unit(50, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, -10, -10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.02),
    plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 0.6),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 35),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35)
  ) +
  ggplot2::labs(
    y = 'Compressive Stress (MPa)',
    x = 'Compressive Strain (%)',
    title = 'c)',
    color = "Ae380 wt"
  ) +
  ggplot2::scale_color_viridis_d(limits = c('15%', '6.42%', '0%'))

# Save DMA plot ----------------------------------------------------------------
ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}/2410-DMA_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Print completion message
message("DMA analysis plot has been successfully generated and saved to: ", folder_destination)