# ============================================================================
# TGA (Thermogravimetric Analysis) Processing and Visualization
# 
# Description: Analysis of thermal stability and weight loss for polymer
#              electrolyte samples with varying LiTFSI salt and Aero380
#              additive concentrations
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
#   - readxl: for reading Excel files
#   - janitor: for cleaning column names
#   - purrr: for functional programming
#   - tidyr: for data tidying
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

# Set output directory ---------------------------------------------------------
folder_destination <- glue::glue('~/Git/SPOC_code/figs')

# TGA data directory
tga_folder_loc <- glue::glue(
  '~/Git/SPOC_code/data/thermal_properties/TGA/'
)

# Create output directory if it doesn't exist
if (!dir.exists(folder_destination)) {
  dir.create(folder_destination, recursive = TRUE)
}

# File discovery and metadata extraction ---------------------------------------
files <- tibble::tibble(
  filepath = list.files(tga_folder_loc, full.names = TRUE),
  # Classify file types based on extension
  file_type = dplyr::case_when(
    stringr::str_detect(filepath, 'txt') == TRUE ~ 'params',
    stringr::str_detect(filepath, '.xl') == TRUE ~ 'data',
    TRUE ~ NA
  ),
  # Extract sample name from filepath
  sample_name = 
    filepath |> 
    basename() |> 
    tools::file_path_sans_ext()
) |>
  tidyr::separate(
    sample_name,
    sep = '_',
    into = c(
      'date', 'poly_rat1', 'poly1', 'poly_rat2', 'poly2', 'additive_wt', 'additive',
      'salt_wt', 'salt'
    )
  )

# Function to process TGA data files -------------------------------------------
#' Read and process TGA Excel files
#' 
#' @param file Path to the TGA Excel file
#' @return Processed tibble with temperature and weight data
#' @details Reads sheet 2, cleans headers, filters for temperatures >= 150°C,
#'          and calculates corrected weight percentage
tga_unpack <- function(file) {
  file |>
    readxl::read_xls(sheet = 2) |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names() |>
    # Remove first row after header
    dplyr::filter(!dplyr::row_number() %in% 1) |>
    # Rename columns for clarity
    dplyr::rename(
      weight = 3,
      weight_pct = 4
    ) |>
    # Convert all columns to numeric
    dplyr::mutate(dplyr::across(dplyr::everything(), as.double)) |>
    # Filter for relevant temperature range
    dplyr::filter(temperature >= 150) |>
    # Calculate corrected weight percentage
    dplyr::mutate(corr_weight_pct = weight / max(weight) * 100)
}

# Process all data files -------------------------------------------------------
files_data <- files |>
  dplyr::filter(file_type == 'data') |>
  dplyr::mutate(data = purrr::map(filepath, tga_unpack)) |>
  tidyr::unnest(cols = data)

# Create TGA plot --------------------------------------------------------------
plot <- ggplot2::ggplot(
  data = files_data |>
    dplyr::mutate(
      # Convert to factors for discrete color scale
      salt_wt = as.factor(salt_wt),
      salt_wt = dplyr::case_when(
        salt_wt == 100 ~ 'neat',
        TRUE ~ salt_wt
      ),
      additive_wt = as.factor(additive_wt),
      # Create legend labels
      name = dplyr::case_when(
        salt_wt == 'neat' ~ glue::glue('LiTFSI neat'),
        TRUE ~ glue::glue(' \nAe380 {additive_wt}%,\nLiTFSI {salt_wt}%\n ')
      )
    ),
  ggplot2::aes(
    x = temperature,
    y = corr_weight_pct,
    color = name
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::scale_x_continuous(limits = c(150, 400)) +
  ggplot2::scale_y_continuous(limits = c(50, 100)) +
  # Add 5% weight loss reference line
  ggplot2::geom_hline(
    yintercept = 95,
    linetype = 'dashed',
    size = 1,
    alpha = 0.5
  ) +
  ggplot2::annotate(
    'text',
    label = '5% weight loss',
    x = 200,
    y = 92,
    size = 4,
    alpha = 0.75
  ) +
  ggplot2::labs(
    x = 'Temperature (°C)',
    y = 'Weight (%)',
    title = 'a)'
  ) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_text(size = 10, family = 'Arial'),
    legend.text = ggplot2::element_text(size = 8, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    legend.key.size = ggplot2::unit(15, 'pt'),
    legend.key.spacing.y = ggplot2::unit(2, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.spacing.x = ggplot2::unit(10, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, -10, -10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.02),
    plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
  ) +
  ggplot2::guides(
    # linetype = ggplot2::guide_legend(title = 'AE380 wt%',
    #                                  title.position = 'top',
    #                                  ncol = 1),
    color = ggplot2::guide_legend(title = 'Formulation',
                                  keywidth = 0.75,
                                  title.position = 'top',
                                  ncol = 1)
  ) +
  ggplot2::scale_color_viridis_d()

# Save TGA plot ----------------------------------------------------------------
ggplot2::ggsave(
  plot,
  file = glue::glue('{folder_destination}/tga_comparison.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Print completion message
message("TGA analysis plot has been successfully generated and saved to: ", folder_destination)