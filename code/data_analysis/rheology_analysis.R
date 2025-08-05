# ============================================================================
# Rheology Shear Thinning Analysis
# 
# Description: Analysis of shear-thinning behavior (viscosity vs shear rate)
#              for polymer electrolyte formulations with varying additive
#              (Aero380) and salt (LiTFSI/NaTFSI) concentrations. Identifies
#              the printable viscosity range for DIW processing.
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
#   - stringi: for advanced string operations
#   - data.table: for fast file reading
#   - janitor: for cleaning column names
#   - purrr: for functional programming
#   - tidyr: for data tidying
#   - scales: for axis formatting
#   - ggh4x: for geom_pointpath
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
library(data.table)
library(janitor)
library(purrr)
library(tidyr)
library(scales)
library(ggh4x)

# Set parameters and directories ---------------------

# Data directory for rheology files
folder_loc <- glue::glue(
  '~/Git/SPOC_code/data/rheology/'
)

# Output directory for figures
folder_destination <- glue::glue('~/Git/SPOC_code/figs')

# Create output directory if it doesn't exist
if (!dir.exists(folder_destination)) {
  dir.create(folder_destination, recursive = TRUE)
}

# File discovery ---------------------------------------------------------------
files <- tibble::tibble(
  filepath = list.files(
    folder_loc,
    full.names = TRUE,
    pattern = 'txt'
  )
)

# Function to process rheology data files --------------------------------------
#' Clean and process rheology text files
#' 
#' @param data Path to the rheology data file
#' @return Processed tibble with shear rate and viscosity data
#' @details Reads tab-delimited files, removes header rows, and converts to numeric
clean_xls <- function(data) {
  data |>
    data.table::fread(fill = TRUE, sep = '\t') |>
    # Remove header rows
    dplyr::filter(!dplyr::row_number() %in% c(1:10, 12)) |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names() |>
    # Convert all columns to numeric
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.numeric)
    ) |>
    # Remove rows with any NA values
    dplyr::filter(
      dplyr::if_any(!dplyr::everything(), is.na)
    )
}

# Process all files and extract metadata ---------------------------------------
cleaning_files <- files |>
  dplyr::mutate(
    # Extract filename from full path
    sample_name = filepath |> basename() |> tools::file_path_sans_ext(),
    name = dplyr::case_when(
      stringr::str_detect(sample_name, '15_Aerosil380_30_LiTFSI') == TRUE ~ '\n15% Ae380,\n30% LiTFSI\n',
      stringr::str_detect(sample_name, '10_Aerosil380_20_NaTFSI') == TRUE ~ '\n10% Ae380,\n20% NaTFSI\n',
      stringr::str_detect(sample_name, '0_none_30_LiTFSI') == TRUE ~ '\n0% Ae380,\n30% LiTFSI\n',
      stringr::str_detect(sample_name, '0_none_0_none') == TRUE ~ '\n0% Ae380,\n0% LiTFSI\n'),
    # Apply cleaning function to each file
    data = purrr::map(filepath, clean_xls)
  ) |>
  tidyr::separate(
    sample_name,
    sep = '_',
    into = c(
      'date', 'poly_rat1', 'poly1', 'poly_rat2', 'poly2', 'additive_wt', 'additive',
      'salt_wt', 'salt'
    )
  ) |>
  tidyr::unnest(data)

# Create shear thinning plot ---------------------------------------------------
shear_thin_plot <- ggplot2::ggplot(
  data = cleaning_files,
  ggplot2::aes(
    x = shear_rate,
    y = viscosity,
    group = as.factor(name),
    color = as.factor(name))
) +
  cowplot::theme_half_open() +
  # Add printable zone annotation
  ggplot2::annotate(
    'rect',
    xmin = 1, 
    xmax = Inf, 
    ymin = 1/Inf, 
    ymax = 1000,
    fill = ggplot2::alpha('darkgreen', 0.3)
  ) +
  ggplot2::annotate(
    'text',
    label = 'Printable Zone',
    x = 100,
    y = 8,
    size = 5,
    alpha = 0.95
  ) +
  # Add data points
  ggplot2::geom_point(size = 2, alpha = 0.75) +
  # Add upper viscosity limit line
  ggplot2::geom_hline(
    yintercept = 1000,
    linetype = 'dashed',
    alpha = 0.5,
    color = 'red'
  ) +
  # Connect points with paths
  ggh4x::geom_pointpath(size = 0.5, alpha = 0.35) +
  # Set log scales for both axes
  ggplot2::scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(-.7, 13000)
  ) +
  ggplot2::scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(0.08, 5000)
  ) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 8, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, -3, 0, 0)),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    legend.key.spacing = ggplot2::unit(-0.0, 'cm'),
    legend.spacing.y = ggplot2::unit(0, 'cm'),
    legend.spacing.x = ggplot2::unit(0, "pt"),
    legend.box.margin = ggplot2::margin(-5, 0, -10, -11),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.0),
    plot.margin = grid::unit(c(-5, 0, 0, 1), "mm")
  ) +
  ggplot2::labs(
    y = expression('log'~eta~'(Pa.s)'),
    x = expression('Shear Rate (1/s)'),
    title = 'd)'
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE)) +
  ggplot2::scale_color_viridis_d(
    limits = c(
      '\n15% Ae380,\n30% LiTFSI\n',
      '\n10% Ae380,\n20% NaTFSI\n',
      '\n0% Ae380,\n30% LiTFSI\n',
      '\n0% Ae380,\n0% LiTFSI\n'
    )
  )

# Save shear thinning plot -----------------------------------------------------
ggplot2::ggsave(
  shear_thin_plot,
  file = glue::glue('{folder_destination}/shearThin_combined.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Print completion message
message("Rheology shear thinning plot has been successfully generated and saved to: ", folder_destination)