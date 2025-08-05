# ============================================================================
# Heatmap Visualization for Ionic Conductivity
# 
# Description: Creates interpolated heatmaps showing ionic conductivity as a
#              function of LiTFSI salt and Aero380 additive concentrations.
#              Uses Akima interpolation for smooth gradients and overlays
#              experimental data points. Includes contour lines and highlights
#              regions of interest.
#
# Author: J. Jimenez
# Version: 1.0
#
# Required packages:
#   - ggplot2: for plotting
#   - cowplot: for plot themes and combining plots
#   - akima: for 2D interpolation
#   - scales: for rescaling color values
#   - shades: for color manipulation
#   - paletteer: for color palettes
#   - ggforce: for geom_circle
# ============================================================================

# Load required packages -------------------------------------------------------
library(ggplot2)
library(cowplot)
library(akima)
library(scales)
library(shades)
library(paletteer)
library(ggforce)

# Helper functions -------------------------------------------------------------

#' Custom label formatter
#' 
#' @param x Numeric values to format
#' @return Formatted character strings
custom_labels <- function(x) {
  paste0(format(x))
}

#' Create Akima interpolation plot
#' 
#' @param akima_df Data frame with interpolated values (x, y, z columns)
#' @param df Original data frame (unused in current implementation)
#' @param type Character string indicating plot type ('coin' or other)
#' @return ggplot object with interpolated heatmap
#' @details Creates a raster plot with contours, custom color scale, and
#'          circles highlighting specific regions of interest
akima_plot <- function(akima_df, df, type) {
  # Pre-compute conditional values based on type
  is_coin <- type == 'coin'
  
  # Set labels based on type (currently not used in plot)
  labels <- if(is_coin) {
    expression(10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4)
  } else {
    expression(10^-7, 10^-6, 10^-5, 10^-4)
  }
  
  # Set limits based on type (currently not used in plot)
  limits_def <- if(is_coin) {
    c(-9.8, -3.3)
  } else {
    c(-7.8, -4.3)
  }
  
  # Set breaks based on type (currently not used in plot)
  breaks <- if(is_coin) {
    c(-9.5, -8.5, -7.5, -6.5, -5.5, -4.5, -3.5)
  } else {
    c(-7.5, -6.5, -5.5, -4.5)
  }
  
  # Pre-filter data to remove NA values
  filtered_data <- akima_df[!is.na(akima_df$z), ]
  
  # Create custom color palette with increased saturation
  color_palette <- shades::saturation(
    paletteer::paletteer_c("scico::roma", n = 100), 
    shades::delta(0.90)
  )
  
  # Create main plot
  plot <- ggplot2::ggplot(
    filtered_data, 
    ggplot2::aes(x = x, y = y, z = z, fill = z, group = z)
  ) +
    # Add interpolated raster
    ggplot2::geom_raster(interpolate = TRUE) +
    # Add contour lines
    ggplot2::geom_contour(color = 'blue', alpha = 0.5) +
    # Custom color scale for ionic conductivity
    ggplot2::scale_fill_gradientn(
      colors = color_palette,
      values = scales::rescale(
        c(-8, -7.8, -7.5, -7.2, -7, -6.5, -6, -5.8), 
        to = c(1, 0)
      ),
      limits = c(-8, -3.9),
      na.value = 'black',
      breaks = c(-8, -7, -6, -5, -4, -3),
      labels = expression(10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3)
    ) +
    # Customize color bar
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        barwidth = ggplot2::unit(0.5, "cm"),
        barheight = 6,
        title = 'S/cm',
        display = 'gradient',
        title.position = "top",
        ticks = ggplot2::element_line(color = 'black')
      )
    ) +
    cowplot::theme_half_open() +
    ggplot2::labs(
      x = 'LiTFSI (wt%)',
      y = 'Ae380 (wt%)',
      title = 'a) SPOC grid varying LiTFSI and Ae380 wt%'
    ) +
    ggplot2::theme(
      legend.position = 'right',
      legend.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 12),
      plot.title.position = 'plot',
      panel.border = ggplot2::element_rect(size = 2),
      legend.spacing.y = ggplot2::unit(10, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, -10, -10)
    ) +
    # Set x-axis limits and labels
    ggplot2::scale_x_continuous(
      expand = c(0, -0.3),
      limits = c(0.1, 30.1),
      labels = c(0, 10, 20, 30),
      breaks = c(0.4, 10, 20, 29.8)
    ) +
    # Set y-axis limits and labels
    ggplot2::scale_y_continuous(
      expand = c(0, -0.3),
      limits = c(-0.02, 15.4),
      labels = c(0, 5, 10, 15),
      breaks = c(0.4, 5, 10, 15)
    ) + 
    # Add circle highlighting region 1
    ggforce::geom_circle(
      ggplot2::aes(x0 = 10, y0 = 11.5, r = 2.2),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red'
    ) + 
    # Add circle highlighting region 2
    ggforce::geom_circle(
      ggplot2::aes(x0 = 10, y0 = 3, r = 2.0),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red'
    )
  
  return(plot)
}

#' Create heatmap with mean interpolation
#' 
#' @param df Data frame with columns: fcomp_salt_wt_pct, fcomp_additive_wt_pct, 
#'           ionic_conductivity_final
#' @param type Character string indicating plot type ('coin' or other)
#' @return Combined plot with heatmap and overlaid data points
#' @details Performs Akima interpolation on log10-transformed conductivity values,
#'          creates heatmap, and overlays actual measurement points
heatmap_mean <- function(df, type) {
  # Pre-calculate log10 transformed conductivity values
  z_values <- log10(df$ionic_conductivity_final)
  
  # Perform 2D interpolation using Akima method
  akima_mean <- akima::interp2xyz(
    akima::interp(
      x = df$fcomp_salt_wt_pct,
      y = df$fcomp_additive_wt_pct,
      z = z_values,
      duplicate = 'mean',  # Average duplicate points
      linear = TRUE,       # Use linear interpolation
      extrap = FALSE       # Don't extrapolate beyond data
    ),
    data.frame = TRUE
  )
  
  # Create base interpolated plot
  plot_akima <- akima_plot(akima_mean, df, type)
  
  # Create overlay plot with measurement points
  plot_points <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(x = fcomp_salt_wt_pct, y = fcomp_additive_wt_pct)
  ) +
    cowplot::theme_nothing() +  # Minimal theme for overlay
    ggplot2::geom_point(
      size = 5,
      alpha = 0.35,
      color = 'black',
      fill = NA
    )
  
  # Extract legend for proper positioning
  legend <- cowplot::get_legend(
    plot_akima + 
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1))
  )
  
  # Combine plots: overlay points on heatmap
  plot_combined <- cowplot::ggdraw(plot_akima) +
    cowplot::draw_plot(
      plot_points, 
      x = 0.03,     # X position of overlay
      y = 0.05,    # Y position of overlay
      width = 0.91, # Width of overlay
      height = 0.935 # Height of overlay
    )
  
  # Final combination with legend
  plot_combined <- cowplot::plot_grid(
    plot_combined, 
    legend, 
    nrow = 1, 
    ncol = 1, 
    align = 'v'
  )
  
  return(plot_combined)
}

# Usage example ----------------------------------------------------------------
# df should contain columns:
# - fcomp_salt_wt_pct: Salt weight percentage
# - fcomp_additive_wt_pct: Additive weight percentage  
# - ionic_conductivity_final: Ionic conductivity values
#
# plot <- heatmap_mean(df, type = 'coin')
# ggplot2::ggsave(plot, filename = 'heatmap.png', width = 8, height = 6)