# ============================================================================
# DSC (Differential Scanning Calorimetry) Analysis
# 
# Description: Analysis of glass transition temperatures for various additive
#              and salt concentrations in polymer electrolyte systems
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
# ============================================================================

# Load required packages -------------------------------------------------------
library(tibble)
library(dplyr)
library(ggplot2)
library(cowplot)
library(glue)
library(grid)

# Set output directory ---------------------------------------------------------
folder_destination <- glue::glue('~/Git/SPOC_code/figs')

# Create output directory if it doesn't exist
if (!dir.exists(folder_destination)) {
  dir.create(folder_destination, recursive = TRUE)
}

# Data preparation -------------------------------------------------------------
# Glass transition temperatures for varying Aero380 concentrations
# at different LiTFSI salt concentrations

# 0% LiTFSI samples
df_0 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-66.01, -65.84, -66.95, -65.82, -66.22, -65.96, -65.56, -65.07),
  salt_wt_pct = c(0, 0, 0, 0, 0, 0, 0, 0),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c('varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero')
)

# 10% LiTFSI samples
df_10 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-58.43, -58.38, -59.97, -58.78, -59.58, -62.79, -56.27, -60.51),
  salt_wt_pct = c(10, 10, 10, 10, 10, 10, 10, 10),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c('varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero')
)

# 20% LiTFSI samples
df_20 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-51.825, -51.95, -52.39, -52.985, -53.385, -53.03, -52.045, -52.99),
  salt_wt_pct = c(20, 20, 20, 20, 20, 20, 20, 20),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c('varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero')
)

# 30% LiTFSI samples
df_30 <- tibble::tibble(
  additive_wt_pct = c(0, 2.14, 4.28, 6.43, 8.57, 10.71, 12.85, 15),
  glass_transition = c(-53.35, -51.96, -50.95, -57.77, -57.88, -53.67, -51.61, -54.34),
  salt_wt_pct = c(30, 30, 30, 30, 30, 30, 30, 30),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c('varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero', 'varyAero')
)

# Varying LiTFSI concentration at 0% additive
df_a0 <- tibble::tibble(
  salt_wt_pct = c(0, 4.28, 8.57, 12.86, 17.14, 21.42, 25.71, 30),
  glass_transition = c(-70.81, -73.9, -68.59, -64.23, -62.48, -60.95, -57.05, -61.37),
  additive_wt_pct = c(0, 0, 0, 0, 0, 0, 0, 0),
  additive = c('Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380', 'Aero380'),
  notes = c('varyLiTFSI', 'varyLiTFSI', 'varyLiTFSI', 'varyLiTFSI', 
            'varyLiTFSI', 'varyLiTFSI', 'varyLiTFSI', 'varyLiTFSI')
)

# Different additives at 0% LiTFSI
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
               'TiO2', 'TiO2', 'TiO2', 'TiO2', 'TiO2', 'TiO2', 'TiO2', 'TiO2',
               'EH5', 'EH5', 'EH5', 'EH5', 'EH5', 'EH5', 'EH5', 'EH5',
               'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3', 'Al2O3'),
  notes = c('noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI',
            'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI',
            'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI',
            'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI', 'noLITFSI')
)

# Combine all datasets
df_final <- dplyr::bind_rows(df_0, df_10, df_20, df_30, df_a0, df_additives)

# Plotting ---------------------------------------------------------------------

# Plot 1: Glass transition vs Aero380 concentration at different LiTFSI levels
dsc_salt_screen_plot <- ggplot2::ggplot(
  data = df_final |> 
    dplyr::filter(additive == 'Aero380', notes == 'varyAero'),
  ggplot2::aes(
    x = additive_wt_pct,
    y = glass_transition,
    color = as.factor(salt_wt_pct),
    group = as.factor(salt_wt_pct)
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(size = 1.5, alpha = 0.4) +
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
  ggplot2::scale_color_discrete(name = 'LiTFSI wt%') +
  ggplot2::scale_y_continuous(limits = c(-70, -50), breaks = c(-70, -65, -60, -55, -50)) +
  ggplot2::labs(
    x = 'Ae380 (wt%)',
    y = 'Glass Transition (°C)',
    title = 'c)',
    color = 'LiTFSI wt%'
  ) +
  ggplot2::scale_color_viridis_d(limits = c('30', '20', '10', '0'))

# Save plot 1
ggplot2::ggsave(
  dsc_salt_screen_plot,
  file = glue::glue('{folder_destination}/dsc_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Plot 2: Glass transition vs additive concentration for different additives
dsc_additive_screen_plot <- ggplot2::ggplot(
  data = df_final |> dplyr::filter(salt_wt_pct == 0),
  ggplot2::aes(
    x = additive_wt_pct,
    y = glass_transition,
    color = additive,
    group = additive
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    x = 'Additive (wt%)',
    y = 'Glass Transition (°C)',
    title = 'b)'
  ) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    legend.key.size = ggplot2::unit(15, 'pt'),
    legend.spacing.y = ggplot2::unit(10, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = ggplot2::unit(50, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, -10, -10),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.01),
    plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
  ) +
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

# Save plot 2
ggplot2::ggsave(
  dsc_additive_screen_plot,
  file = glue::glue('{folder_destination}/dsc_add_LiTFSI_plot.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# DSC Raw Data Processing ------------------------------------------------------
# Process raw DSC files to extract glass transition temperature

# Define input files
file1 <- '~/Git/SPOC_code/data/thermal_properties/102323_9_PEGMEA_1_PEGDA_0_none_0_none_1.txt'
file2 <- '~/Git/SPOC_code/data/thermal_properties/102323_9_PEGMEA_1_PEGDA_0_none_0_none_2.txt'


# Function to read and process DSC files
#' Read and process DSC raw data files
#' 
#' @param file Path to the DSC data file
#' @return Processed tibble with temperature and heat flow data
file_read <- function(file) {
  file |>
    readr::read_csv() |>
    # Remove header rows
    dplyr::filter(!dplyr::row_number() %in% 1:7) |>
    # Separate tab-delimited columns
    tidyr::separate(
      sep = '\t',
      col = 1,
      into = c('A', 'B', 'C', 'D')
    ) |>
    # Use first row as column names
    janitor::row_to_names(1) |>
    janitor::clean_names() |>
    # Remove first data row
    dplyr::filter(!dplyr::row_number() %in% 1) |>
    # Convert all columns to numeric and add metadata
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.numeric),
      file = basename(file),
      # Process filename to extract cycle information
      file = stringr::str_replace_all(file, 'litfsi[.]', 'litfsi-1.'),
      file = stringr::str_replace_all(file, '10-23-23-pegda-pegmea-0a380-0litfsi', ''),
      file = stringr::str_replace_all(file, '-', ''),
      file = stringr::str_replace_all(file, '.txt', ''),
      cycle = as.character(as.numeric(file) + 1),
      cycle = glue::glue('Cycle {cycle}')
    )
}

# Read and combine data from both files
file_df <- file_read(file1) |>
  dplyr::bind_rows(file_read(file2))

# Plot 3: DSC baseline plot showing glass transition
dsc_baseline <- ggplot2::ggplot(
  data = file_df,
  ggplot2::aes(
    x = temperature,
    y = heat_flow_normalized,
    group = as.factor(cycle),
    color = as.factor(cycle)
  )
) +
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1) +
  ggplot2::scale_color_manual(
    values = c("#287C8EFF", "#440154")
  ) +
  ggplot2::labs(
    x = expression('Temperature (°C)'),
    y = bquote("exotherm" %->% ""),
    title = 'b)'
  ) +
  # Add arrow pointing to glass transition
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
  # Add Tg annotation
  ggplot2::annotate(
    'text',
    label = expression(T[g] == -62 * "°C"),
    x = 10,
    y = -0.08,
    color = 'blue',
    size = 4
  ) +
  # Add sample composition annotation
  ggplot2::annotate(
    'text',
    label = expression("9 PEGMEA:1 PEGDA"),
    x = 20,
    y = 0.02,
    color = 'black',
    size = 4
  ) +
  ggplot2::theme(
    legend.position = 'right',
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10, family = 'Arial'),
    axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text = ggplot2::element_text(size = 12, family = 'Arial'),
    axis.text.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      color = 'black',
      size = 1.2,
      fill = NA
    ),
    strip.background = ggplot2::element_blank(),
    plot.title.position = 'plot',
    plot.title = ggplot2::element_text(size = 16, vjust = -6, hjust = -0.02),
    plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
  )

# Save plot 3
ggplot2::ggsave(
  dsc_baseline,
  file = glue::glue('{folder_destination}/dsc_baseline.png'),
  dpi = 600,
  width = 4,
  height = 3,
  bg = 'white'
)

# Print final completion message
message("All DSC plots (including baseline) have been successfully generated!")