# =============================================================================
# PLOTTING FUNCTIONS - MATCHES ORIGINAL WORKFLOW
# =============================================================================

suppressPackageStartupMessages({
  require(ggplot2)
  require(dplyr)
  require(cowplot)
  require(viridis)
  require(glue)
  require(akima)
  require(ggforce)
  require(patchwork)
  require(scales)
})

# =============================================================================
# DATA SELECTION FOR PLOTTING - ORIGINAL LOGIC
# =============================================================================

#' Select and filter data for plotting - MATCHES select_df() from original
#' @param df Input dataframe
#' @param type Type of data: 'coin' or 'PCB'
#' @return Filtered dataframe with log_IC column
select_df <- function(df, type = 'coin or PCB') {
  
  # Determine filter value
  filter_value <- dplyr::case_when(
    type == 'coin' ~ 'Coin cell',
    type == 'PCB' ~ 'PCB print'
  )
  
  df %>%
    # Standardize print_type (Bulk cast -> PCB print)
    dplyr::mutate(
      print_type = stringi::stri_replace_all_regex(
        print_type,
        'Bulk cast',
        'PCB print'
      )
    ) %>%
    # Standardize formulation names (this was in your original)
    dplyr::mutate(
      fcomp_formulation = stringi::stri_replace_all_regex(
        fcomp_formulation,
        '0.5 PEGMEA, 0.5 PEGDA,',
        '0.9 PEGMEA, 0.1 PEGDA'
      )
    ) %>%
    # Filter by type
    dplyr::filter(print_type == filter_value) %>%
    # Add grouping
    dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct) %>%
    dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    # Add log_IC
    dplyr::mutate(log_IC = log10(ionic_conductivity_final))
}

# =============================================================================
# HEATMAP VISUALIZATION - ORIGINAL LOGIC
# =============================================================================

#' Create base akima plot - ORIGINAL LOGIC
akima_plot <- function(akima_df, df, type) {
  
  # Filter NA values
  filtered_data <- akima_df[!is.na(akima_df$z), ]
  
  # Color palette
  color_palette <- shades::saturation(
    paletteer::paletteer_c("scico::roma", n = 100), 
    shades::delta(0.90)
  )
  
  # Create plot
  plot <- ggplot2::ggplot(
    filtered_data, 
    ggplot2::aes(x = x, y = y, z = z, fill = z, group = z)
  ) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::geom_contour(color = 'blue', alpha = 0.5) +
    ggplot2::scale_fill_gradientn(
      colors = color_palette,
      values = scales::rescale(c(-8, -7.8, -7.5, -7.2, -7, -6.5, -6, -5.8), to = c(1, 0)),
      limits = c(-8, -3.9),
      na.value = 'black',
      breaks = c(-8, -7, -6, -5, -4, -3),
      labels = expression(10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3)
    ) +
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
    ggplot2::scale_x_continuous(
      expand = c(0, -0.3),
      limits = c(0.1, 30.1),
      labels = c(0, 10, 20, 30),
      breaks = c(0.4, 10, 20, 29.8)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, -0.3),
      limits = c(-0.02, 15.4),
      labels = c(0, 5, 10, 15),
      breaks = c(0.4, 5, 10, 15)
    ) + 
    # Add circles highlighting regions of interest (from original)
    ggforce::geom_circle(
      ggplot2::aes(x0 = 10, y0 = 11.5, r = 2.2),
      inherit.aes = FALSE,
      linetype = 'dashed',
      size = 1.5,
      alpha = 0.4,
      color = 'red'
    ) + 
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

#' Create heatmap with akima interpolation - ORIGINAL LOGIC
heatmap_mean <- function(df, type) {
  
  # Pre-calculate log10 values
  z_values <- log10(df$ionic_conductivity_final)
  
  # Akima interpolation
  akima_mean <- akima::interp2xyz(
    akima::interp(
      x = df$fcomp_salt_wt_pct,
      y = df$fcomp_additive_wt_pct,
      z = z_values,
      duplicate = 'mean',
      linear = TRUE,
      extrap = FALSE
    ),
    data.frame = TRUE
  )
  
  # Create base plot
  plot_akima <- akima_plot(akima_mean, df, type)
  
  # Create points overlay
  plot_points <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(x = fcomp_salt_wt_pct, y = fcomp_additive_wt_pct)
  ) +
    cowplot::theme_nothing() +
    ggplot2::geom_point(
      size = 5,
      alpha = 0.35,
      color = 'black',
      fill = NA
    )
  
  # Extract legend
  legend <- cowplot::get_legend(
    plot_akima + ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = 1))
  )
  
  # Combine plots
  plot_combined <- cowplot::ggdraw(plot_akima) +
    cowplot::draw_plot(plot_points, x = 0.03, y = 0.05, width = .91, height = .935)
  
  plot_combined <- cowplot::plot_grid(
    plot_combined, legend, nrow = 1, ncol = 1, align = 'v'
  )
  
  return(plot_combined)
}

# =============================================================================
# BOXPLOT VISUALIZATION - ORIGINAL LOGIC
# =============================================================================

#' Create boxplots for composition analysis - ORIGINAL LOGIC
boxplot_plotter <- function(df, type, salt_grid_1, additive_grid_1, 
                            salt_grid_2, additive_grid_2) {
  require(patchwork)
  
  # Plot 1: Varying additive at fixed salt concentrations (Grid 1)
  plot1 <- ggplot2::ggplot(
    data = df %>%
      dplyr::filter(
        fcomp_salt_wt_pct %in% salt_grid_1,
        fcomp_additive_wt_pct %in% additive_grid_1
      ) %>%
      dplyr::mutate(fcomp_salt_wt_pct = glue::glue('LiTFSI {fcomp_salt_wt_pct} wt%')),
    ggplot2::aes(
      x = fcomp_additive_wt_pct,
      y = ionic_conductivity_final,
      group = fcomp_additive_wt_pct %>% as.factor(),
      color = fcomp_additive_wt_pct %>% as.factor()
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    ggplot2::geom_boxplot(size = 0.25, width = 2, outlier.shape = NA) +
    ggplot2::facet_grid(. ~ fcomp_salt_wt_pct) +
    ggplot2::theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA),
      axis.title.x = element_text(size = 12)
    ) +
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x) 10^x),
      labels = scales::trans_format('log10', scales::math_format(10^.x)),
      limits = c(1e-9, 1e-3)
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(
      x = 'Aerosil (wt%)',
      y = expression('Ionic Conductivity (S/cm)')
    )
  
  # Plot 2: Varying salt at fixed additive concentrations (Grid 2)
  plot2 <- ggplot2::ggplot(
    data = df %>%
      dplyr::filter(
        fcomp_additive_wt_pct %in% additive_grid_2,
        fcomp_salt_wt_pct %in% salt_grid_2
      ) %>%
      dplyr::mutate(
        fcomp_additive_wt_pct = glue::glue('Aerosil {fcomp_additive_wt_pct} wt%')
      ),
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = ionic_conductivity_final,
      group = fcomp_salt_wt_pct %>% as.factor(),
      color = fcomp_salt_wt_pct %>% as.factor()
    )
  ) +
    cowplot::theme_half_open() +
    cowplot::panel_border() +
    ggplot2::geom_boxplot(size = 0.25, width = 3, outlier.shape = NA) +
    ggplot2::facet_grid(. ~ fcomp_additive_wt_pct) +
    ggplot2::theme(
      legend.position = 'none',
      strip.text.x = element_text(size = 9),
      strip.text.y = element_text(size = 9),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      strip.background = element_rect(color = 'black', fill = NA),
      axis.title.x = element_text(size = 12)
    ) +
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks('log10', function(x) 10^x),
      labels = scales::trans_format('log10', scales::math_format(10^.x)),
      limits = c(1e-9, 1e-3)
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(
      x = 'LiTFSI (wt%)',
      y = expression('Ionic Conductivity (S/cm)')
    )
  
  # Combine both plots
  plot_comb <- patchwork::wrap_plots(plot1, plot2, ncol = 1)
  
  return(list(plot1 = plot1, plot2 = plot2, combined = plot_comb))
}

# =============================================================================
# GRID SEARCH VISUALIZATION - ORIGINAL LOGIC
# =============================================================================

#' Plot grid points for LiTFSI
gridSearch_plotter <- function(df, type) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = fcomp_salt_wt_pct,
      y = fcomp_additive_wt_pct,
      color = 'black'
    )
  ) +
    ggplot2::geom_point(size = 3, ggplot2::aes(color = 'black')) +
    cowplot::theme_cowplot() +
    ggplot2::labs(x = 'LiTFSI (wt%)', y = 'Aerosil 380 (wt%)') +
    ggplot2::theme(
      legend.position = 'none',
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10)
    )
  
  return(plot)
}

# =============================================================================
# INTERACTIVE PLOTLY GRID PLOTS FOR TROUBLESHOOTING
# =============================================================================

#' Create interactive plotly grid plot with dates
#' @param df Input dataframe
#' @param type Type: 'coin' or 'PCB'
#' @return plotly object
gridSearch_plotter_interactive <- function(df, type) {
  
  # Prepare hover text
  df <- df %>%
    dplyr::mutate(
      hover_text = paste0(
        "Salt: ", fcomp_salt_wt_pct, " wt%<br>",
        "Additive: ", fcomp_additive_wt_pct, " wt%<br>",
        "Date: ", date_casted, "<br>",
        "IC: ", sprintf("%.2e", ionic_conductivity_final), " S/cm<br>",
        "Sample: ", sample_ID, "<br>",
        "Flagged: ", ifelse(flag_for_removal, "YES", "NO")
      )
    )
  
  # Create plotly plot
  p <- plotly::plot_ly(
    data = df,
    x = ~fcomp_salt_wt_pct,
    y = ~fcomp_additive_wt_pct,
    color = ~as.factor(date_casted),
    colors = "viridis",
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 8,
      opacity = 0.7,
      line = list(width = 1, color = "black")
    ),
    text = ~hover_text,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = paste0(type, " Grid Search - Colored by Date"),
      xaxis = list(title = "LiTFSI (wt%)"),
      yaxis = list(title = "Aerosil 380 (wt%)"),
      hovermode = "closest"
    )
  
  return(p)
}

#' Create side-by-side interactive grid comparison
#' @param df_flagged Dataframe with flag_for_removal column
#' @return List of plotly objects
create_interactive_grids <- function(df_flagged) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("CREATING INTERACTIVE GRID PLOTS FOR TROUBLESHOOTING\n")
  cat(strrep("=", 70), "\n\n")
  
  # Filter for clean data only
  df_clean <- df_flagged %>% 
    dplyr::filter(flag_for_removal == FALSE)
  
  cat(sprintf("Clean coin cells: %d\n", 
              sum(df_clean$print_type == "Coin cell", na.rm = TRUE)))
  cat(sprintf("Clean PCB samples: %d\n", 
              sum(df_clean$print_type == "PCB print", na.rm = TRUE)))
  
  # Coin cell grid
  df_coin <- select_df(df = df_clean, type = 'coin') %>%
    dplyr::filter(!fcomp_additive_wt_pct %in% c(2.9, 5, 5.7, 10, 7.1))
  
  plot_coin <- gridSearch_plotter_interactive(df_coin, "Coin Cell")
  
  # PCB grid
  df_pcb <- select_df(df = df_clean, type = 'PCB') %>%
    dplyr::filter(!fcomp_additive_wt_pct %in% c(1.4, 2.9, 5, 5.7, 10, 7.1))
  
  plot_pcb <- gridSearch_plotter_interactive(df_pcb, "PCB Print")
  
  cat("\n✓ Created interactive plots\n")
  cat("  Use: plot_coin or plot_pcb to view\n")
  cat(strrep("=", 70), "\n\n")
  
  return(list(
    coin = plot_coin,
    pcb = plot_pcb
  ))
}

# =============================================================================
# 3D PLOTLY VISUALIZATION FOR PCB GRID
# =============================================================================

#' Create interactive 3D plotly for PCB grid with dates
#' @param df_flagged Dataframe with flag_for_removal column (use FULL data to see all points)
#' @param show_flagged Whether to show flagged points (default: FALSE for clean view)
#' @return plotly object
create_3d_pcb_grid <- function(df_flagged, show_flagged = FALSE) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("CREATING 3D INTERACTIVE PCB GRID PLOT\n")
  cat(strrep("=", 70), "\n\n")
  
  # Filter for PCB data
  df_pcb <- df_flagged %>%
    dplyr::filter(
      print_type == "PCB print" | print_type == "Bulk cast"
    ) %>%
    dplyr::mutate(
      print_type = stringr::str_replace_all(print_type, "Bulk cast", "PCB print")
    )
  
  # Optionally filter flagged samples
  if (!show_flagged) {
    n_before <- nrow(df_pcb)
    df_pcb <- df_pcb %>% dplyr::filter(flag_for_removal == FALSE)
    cat(sprintf("Showing clean data only: %d samples (removed %d flagged)\n", 
                nrow(df_pcb), n_before - nrow(df_pcb)))
  } else {
    cat(sprintf("Showing all data: %d samples (including flagged)\n", nrow(df_pcb)))
  }
  
  # Prepare data with hover text
  df_pcb <- df_pcb %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Sample:</b> ", sample_ID, "<br>",
        "<b>Date:</b> ", format(date_casted, "%Y-%m-%d"), "<br>",
        "<b>LiTFSI:</b> ", fcomp_salt_wt_pct, " wt%<br>",
        "<b>Aerosil:</b> ", fcomp_additive_wt_pct, " wt%<br>",
        "<b>IC:</b> ", sprintf("%.2e", ionic_conductivity_final), " S/cm<br>",
        "<b>Formulation:</b> ", fcomp_formulation, "<br>",
        "<b>Flagged:</b> ", ifelse(flag_for_removal, "YES", "NO")
      ),
      # Color by date for visualization
      date_numeric = as.numeric(date_casted),
      # Size by IC value
      ic_log = log10(ionic_conductivity_final)
    )
  
  # Create 3D scatter plot
  plot_3d <- plotly::plot_ly(
    data = df_pcb,
    x = ~fcomp_salt_wt_pct,
    y = ~fcomp_additive_wt_pct,
    z = ~ic_log,
    type = "scatter3d",
    mode = "markers",
    color = ~date_casted,
    colors = viridis::viridis(100),
    marker = list(
      size = 5,
      opacity = 0.8,
      line = list(
        color = ~ifelse(flag_for_removal, "red", "black"),
        width = ~ifelse(flag_for_removal, 2, 0.5)
      )
    ),
    text = ~hover_text,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = list(
        text = "3D PCB Grid: LiTFSI vs Aerosil vs IC (colored by date)",
        font = list(size = 14)
      ),
      scene = list(
        xaxis = list(
          title = "LiTFSI (wt%)",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "white"
        ),
        yaxis = list(
          title = "Aerosil 380 (wt%)",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "white"
        ),
        zaxis = list(
          title = "log10(IC) [S/cm]",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "white"
        ),
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.3)
        )
      ),
      showlegend = TRUE,
      legend = list(
        title = list(text = "Date"),
        orientation = "v"
      )
    ) %>%
    plotly::colorbar(
      title = "Date Casted",
      tickformat = "%Y-%m-%d"
    )
  
  cat(sprintf("✓ Created 3D plot with %d points\n", nrow(df_pcb)))
  cat("  - X-axis: LiTFSI (wt%)\n")
  cat("  - Y-axis: Aerosil 380 (wt%)\n")
  cat("  - Z-axis: log10(IC)\n")
  cat("  - Color: Date casted\n")
  cat("  - Hover: Full sample details including date\n")
  if (show_flagged) {
    cat("  - Red outline: Flagged samples\n")
  }
  cat(strrep("=", 70), "\n\n")
  
  return(plot_3d)
}

#' Save interactive 3D plot as HTML
#' @param plot_3d Plotly object from create_3d_pcb_grid()
#' @param output_path Path to save HTML file
save_3d_plot <- function(plot_3d, output_path) {
  htmlwidgets::saveWidget(
    plot_3d, 
    file = output_path,
    selfcontained = TRUE
  )
  cat(sprintf("✓ Saved interactive 3D plot to: %s\n", output_path))
  cat("  Open in browser to interact with the plot\n")
}

# =============================================================================
# MASTER PLOT GENERATION - MATCHES ORIGINAL WORKFLOW
# =============================================================================

#' Generate all plots using original workflow logic
#' @param df_flagged Dataframe with flag_for_removal column
#' @param output_dir Directory to save plots
#' @return List of saved plot paths
generate_all_plots <- function(df_flagged, output_dir) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("GENERATING PLOTS (ORIGINAL WORKFLOW)\n")
  cat(strrep("=", 70), "\n\n")
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  saved_plots <- list()
  
  # CRITICAL: Filter out flagged samples ONCE at the start
  # This matches your original: final_db_outlier (which was already filtered)
  n_before <- nrow(df_flagged)
  df_clean <- df_flagged %>% 
    dplyr::filter(flag_for_removal == FALSE)
  n_removed <- n_before - nrow(df_clean)
  
  cat(sprintf("Starting with: %d samples\n", n_before))
  cat(sprintf("Removed flagged: %d samples\n", n_removed))
  cat(sprintf("Clean data: %d samples\n\n", nrow(df_clean)))
  
  # -------------------------------------------------------------------------
  # HEATMAP (PCB only) - USES CLEAN DATA, LiTFSI + none (0% salt)
  # -------------------------------------------------------------------------
  cat("Creating heatmap...\n")
  tryCatch({
    # Filter for LiTFSI and none (0% salt is still part of the grid)
    heatmap_pcb <- df_clean %>%
      dplyr::filter(fcomp_salt %in% c("LiTFSI", "none")) %>%
      select_df(type = 'PCB') %>%
      heatmap_mean('PCB Print')
    
    filepath <- file.path(output_dir, '2304-Heatmap_PCB_Print_mean.png')
    ggplot2::ggsave(heatmap_pcb, filename = filepath, dpi = 600, width = 8, height = 6, bg = 'white')
    saved_plots$heatmap_pcb <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Heatmap error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # BOXPLOTS (coin cell) - USES CLEAN DATA, LiTFSI + none
  # -------------------------------------------------------------------------
  cat("Creating boxplots...\n")
  tryCatch({
    # Filter for LiTFSI and none (0% salt)
    boxplot_coin <- df_clean %>%
      dplyr::filter(fcomp_salt %in% c("LiTFSI", "none")) %>%
      select_df(type = 'coin') %>%
      boxplot_plotter('Coin Cell')
    
    filepath <- file.path(output_dir, '2304-Coin_Cell_boxplot.png')
    ggplot2::ggsave(boxplot_coin$combined, filename = filepath, dpi = 600, width = 8, height = 6, bg = 'white')
    saved_plots$boxplot_coin <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Boxplot error: %s\n", e$message))
  })
  
  # PCB boxplots - USES CLEAN DATA, LiTFSI + none
  tryCatch({
    # Filter for LiTFSI and none (0% salt)
    boxplot_pcb <- df_clean %>%
      dplyr::filter(fcomp_salt %in% c("LiTFSI", "none")) %>%
      select_df(type = 'PCB') %>%
      boxplot_plotter('PCB Print')
    
    filepath <- file.path(output_dir, '2304-PCB_Print_boxplot.png')
    ggplot2::ggsave(boxplot_pcb$combined, filename = filepath, dpi = 600, width = 8, height = 6, bg = 'white')
    saved_plots$boxplot_pcb <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ PCB boxplot error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # GRID SEARCH PLOTS - LiTFSI + none (0% salt)
  # -------------------------------------------------------------------------
  cat("Creating grid search plots...\n")
  
  # Coin cell grid - LiTFSI + none
  tryCatch({
    grid_coin <- df_clean %>%
      dplyr::filter(fcomp_salt %in% c("LiTFSI", "none")) %>%
      select_df(type = 'coin') %>%
      gridSearch_plotter('Coin')
    
    filepath <- file.path(output_dir, '2304-Grid_Coin_Cell.png')
    ggplot2::ggsave(grid_coin, filename = filepath, dpi = 600, width = 4, height = 3, bg = 'white')
    saved_plots$grid_coin <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Coin grid error: %s\n", e$message))
  })
  
  # PCB grid - LiTFSI + none
  tryCatch({
    grid_pcb <- df_clean %>%
      dplyr::filter(fcomp_salt %in% c("LiTFSI", "none")) %>%
      select_df(type = 'PCB') %>%
      gridSearch_plotter('PCB')
    
    filepath <- file.path(output_dir, '2304-Grid_PCB_Print.png')
    ggplot2::ggsave(grid_pcb, filename = filepath, dpi = 600, width = 4, height = 3, bg = 'white')
    saved_plots$grid_pcb <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ PCB grid error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # METHOD COMPARISON LINE PLOT - LiTFSI only
  # -------------------------------------------------------------------------
  cat("Creating method comparison line plot...\n")
  tryCatch({
    line_plot <- df_clean %>%
      dplyr::filter(fcomp_salt == "LiTFSI") %>%
      compare_line_plot(filter = c(0, 10, 20, 30))
    
    filepath <- file.path(output_dir, '2304-line_plot.png')
    ggplot2::ggsave(line_plot, filename = filepath, dpi = 600, width = 8, height = 3, bg = 'white')
    saved_plots$line_plot <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Line plot error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # BLAND-ALTMAN PLOT - USES CLEAN DATA, LiTFSI only
  # -------------------------------------------------------------------------
  cat("Creating Bland-Altman plot...\n")
  tryCatch({
    # Uses clean data (QC-filtered dataset), LiTFSI only
    ba_plot <- df_clean %>%
      dplyr::filter(fcomp_salt == "LiTFSI") %>%
      bland_altman_plot(salt_filter = c(0, 10, 20, 30))
    
    filepath <- file.path(output_dir, '2304-bland_altman.png')
    ggplot2::ggsave(ba_plot, filename = filepath, dpi = 600, width = 5, height = 4, bg = 'white')
    saved_plots$bland_altman <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Bland-Altman error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # VIOLIN PLOT (uses FULL dataset with special filtering)
  # -------------------------------------------------------------------------
  cat("Creating additive screening violin plot...\n")
  tryCatch({
    # IMPORTANT: Pass df_flagged (full dataset), not df_clean
    # This function applies its own special date filtering
    violin_plot <- additive_violin_plot(df_flagged)
    
    filepath <- file.path(output_dir, '2305-Additive_Screening_violin.png')
    ggplot2::ggsave(violin_plot, filename = filepath, dpi = 600, width = 4, height = 3, bg = 'white')
    saved_plots$violin <- filepath
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath)))
  }, error = function(e) {
    cat(sprintf("  ✗ Violin plot error: %s\n", e$message))
  })
  
  # -------------------------------------------------------------------------
  # SUMMARY STATISTICS BAR PLOTS
  # -------------------------------------------------------------------------
  cat("Creating summary statistics plots...\n")
  tryCatch({
    summary_plots <- create_summary_bar_plots(df_flagged)
    
    # Save individual plots
    filepath_b <- file.path(output_dir, '2304-summary_unique_samples.png')
    ggplot2::ggsave(summary_plots$unique_samples, filename = filepath_b, 
                    dpi = 600, width = 6, height = 2, bg = 'white')
    saved_plots$summary_unique <- filepath_b
    
    filepath_c <- file.path(output_dir, '2304-summary_measurements.png')
    ggplot2::ggsave(summary_plots$total_measurements, filename = filepath_c, 
                    dpi = 600, width = 6, height = 2, bg = 'white')
    saved_plots$summary_measurements <- filepath_c
    
    filepath_d <- file.path(output_dir, '2304-summary_projected.png')
    ggplot2::ggsave(summary_plots$projected, filename = filepath_d, 
                    dpi = 600, width = 6, height = 2, bg = 'white')
    saved_plots$summary_projected <- filepath_d
    
    # Save combined
    filepath_combined <- file.path(output_dir, '2304-summary_all_combined.png')
    ggplot2::ggsave(summary_plots$combined, filename = filepath_combined, 
                    dpi = 600, width = 6, height = 6, bg = 'white')
    saved_plots$summary_combined <- filepath_combined
    
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath_b)))
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath_c)))
    cat(sprintf("  ✓ Saved: %s\n", basename(filepath_d)))
    cat(sprintf("  ✓ Saved: %s (combined)\n", basename(filepath_combined)))
  }, error = function(e) {
    cat(sprintf("  ✗ Summary plots error: %s\n", e$message))
  })
  
  cat(sprintf("\n✓ Generated %d plots in: %s\n", length(saved_plots), output_dir))
  cat(strrep("=", 70), "\n\n")
  
  return(saved_plots)
}

# =============================================================================
# METHOD COMPARISON LINE PLOTS - ORIGINAL LOGIC
# =============================================================================

#' Compare PCB with coin cell line plots
#' @param df Input dataframe (should be clean data)
#' @param filter Vector of salt concentrations to include
#' @return ggplot object
compare_line_plot <- function(df, salt_filter, additive_filter = NULL) {
  
  df <- df %>%
    dplyr::mutate(
      fcomp_salt_wt_pct_lab = paste0(fcomp_salt_wt_pct, ' wt%')
    )
  
  filter_cond <- function(data) {
    data %>%
      dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct, cast_type) %>%
      dplyr::mutate(
        avg_IC = mean(log10(ionic_conductivity_final)),
        sd = sd(log10(ionic_conductivity_final)) %>% as.numeric()
      )
  }
  
  filter_cond_error <- function(data) {
    data %>%
      dplyr::group_by(fcomp_additive_wt_pct, fcomp_salt_wt_pct, cast_type) %>%
      dplyr::mutate(
        avg_IC = mean(log10(ionic_conductivity_final)),
        sd = sd(log10(ionic_conductivity_final)) / 
          sqrt(length(log10(ionic_conductivity_final))) %>% as.numeric(),
        t = stats::qt(1 - (0.10 / 2), length(log10(ionic_conductivity_final)) - 1),
        ci = t * sd
      )
  }
  
  # Filter PCB data
  pcb_filter <- select_df(df = df, type = 'PCB') %>%
    dplyr::filter(fcomp_salt_wt_pct %in% salt_filter)
  
  # Add additive filtering if specified
  if (!is.null(additive_filter)) {
    pcb_filter <- pcb_filter %>%
      dplyr::filter(fcomp_additive_wt_pct %in% additive_filter)
  }
  
  pcb_filter <- pcb_filter %>% filter_cond()
  
  coin_filter <- select_df(df = df, type = 'coin') %>%
    dplyr::filter(fcomp_salt_wt_pct %in% salt_filter)
  
  # Add additive filtering if specified
  if (!is.null(additive_filter)) {
    coin_filter <- coin_filter %>%
      dplyr::filter(fcomp_additive_wt_pct %in% additive_filter)
  }
  
  coin_filter <- coin_filter %>% filter_cond()
  
  pcb_filter_error <- select_df(df = df, type = 'PCB') %>%
    dplyr::filter(fcomp_salt_wt_pct %in% salt_filter)
  
  if (!is.null(additive_filter)) {
    pcb_filter_error <- pcb_filter_error %>%
      dplyr::filter(fcomp_additive_wt_pct %in% additive_filter)
  }
  
  pcb_filter_error <- pcb_filter_error %>% filter_cond_error()
  
  coin_filter_error <- select_df(df = df, type = 'coin') %>%
    dplyr::filter(fcomp_salt_wt_pct %in% salt_filter)
  
  if (!is.null(additive_filter)) {
    coin_filter_error <- coin_filter_error %>%
      dplyr::filter(fcomp_additive_wt_pct %in% additive_filter)
  }
  
  coin_filter_error <- coin_filter_error %>% filter_cond_error()
  
  point_size <- 0.5
  line_size <- 0.6
  
  plot <- ggplot2::ggplot() +
    cowplot::theme_half_open() +
    cowplot::background_grid(major = 'y') +
    ggplot2::geom_point(
      data = pcb_filter,
      size = point_size,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      )
    ) +
    ggplot2::geom_line(
      data = pcb_filter,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      size = line_size
    ) +
    ggplot2::geom_errorbar(
      data = pcb_filter_error,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        ymin = avg_IC - ci,
        ymax = avg_IC + ci,
        color = print_type
      ),
      size = 0.25
    ) +
    ggplot2::geom_point(
      size = point_size,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      data = coin_filter
    ) +
    ggplot2::geom_line(
      data = coin_filter,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        group = print_type,
        color = print_type
      ),
      size = line_size
    ) +
    ggplot2::geom_errorbar(
      data = coin_filter_error,
      ggplot2::aes(
        x = fcomp_additive_wt_pct,
        y = avg_IC,
        ymin = avg_IC - ci,
        ymax = avg_IC + ci,
        color = print_type
      ),
      size = 0.25
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-9.5, -3.0),
      breaks = c(-2, -3, -4, -5, -6, -7, -8, -9, -10),
      labels = scales::math_format(10^.x)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 15.9),
      breaks = c(0, 5, 10, 15)
    ) +
    ggplot2::scale_color_manual(
      labels = c('Coin\ncells', 'SPOC'),
      values = c('#440154FF', '#55C667FF')
    ) +
    ggplot2::labs(
      x = 'Ae380 (wt%)',
      y = expression('Ionic Conductivity (S/cm)')
    ) +
    ggplot2::theme(
      legend.position = 'right',
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10, family = 'Arial'),
      axis.title = ggplot2::element_text(size = 12, family = 'Arial'),
      axis.text.x = ggplot2::element_text(size = 9, family = 'Arial'),
      axis.text.y = ggplot2::element_text(size = 12, family = 'Arial'),
      legend.key.size = ggplot2::unit(12, 'pt'),
      legend.spacing.y = ggplot2::unit(10, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, color = NA, size = 2),
      plot.title.position = 'plot',
      plot.title = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 9),
      panel.border = ggplot2::element_rect(fill = NA, color = 'black')
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::facet_grid(. ~ fcomp_salt_wt_pct_lab)
  
  return(plot)
}

# =============================================================================
# BLAND-ALTMAN ANALYSIS - ORIGINAL LOGIC
# =============================================================================

#' Create Bland-Altman plot for method comparison
#' @param df Input dataframe (clean data)
#' @param salt_filter Vector of salt concentrations to include
#' @return ggplot object
bland_altman_plot <- function(df, salt_filter = c(0, 10, 20, 30)) {
  
  # Prepare data for Bland-Altman
  ba_plot_tab1 <- select_df(df, type = 'coin') %>%
    dplyr::group_by(fcomp_formulation) %>%
    dplyr::mutate(avg_gold_log = mean(log10(ionic_conductivity_final))) %>%
    dplyr::distinct(fcomp_formulation, avg_gold_log, fcomp_salt_wt_pct, fcomp_additive_wt_pct)
  
  ba_plot_tab2 <- select_df(df, type = 'PCB') %>%
    dplyr::group_by(fcomp_formulation) %>%
    dplyr::mutate(avg_pcb_log = mean(log10(ionic_conductivity_final))) %>%
    dplyr::distinct(fcomp_formulation, avg_pcb_log, fcomp_salt_wt_pct, fcomp_additive_wt_pct)
  
  ba_plot_tab <- ba_plot_tab1 %>%
    dplyr::full_join(ba_plot_tab2, by = c("fcomp_formulation", "fcomp_salt_wt_pct", "fcomp_additive_wt_pct")) %>%
    dplyr::mutate(
      avg_log = (avg_gold_log + avg_pcb_log) / 2,
      diff_log = avg_gold_log - avg_pcb_log
    ) %>%
    dplyr::filter(
      !is.na(avg_gold_log),
      fcomp_salt_wt_pct %in% salt_filter
    )
  
  # Calculate statistics
  mean_diff_log <- mean(ba_plot_tab$diff_log, na.rm = TRUE)
  sd_diff_log <- sd(ba_plot_tab$diff_log, na.rm = TRUE)
  lower_log <- mean_diff_log - 1.96 * sd_diff_log
  upper_log <- mean_diff_log + 1.96 * sd_diff_log
  
  # Create plot
  plot <- ggplot2::ggplot(
    ba_plot_tab,
    ggplot2::aes(
      x = avg_log, 
      y = diff_log,
      color = as.factor(fcomp_salt_wt_pct)
    )
  ) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    cowplot::theme_half_open() +
    ggplot2::geom_hline(yintercept = mean_diff_log, color = 'blue') +
    ggplot2::geom_hline(yintercept = lower_log, color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = upper_log, color = "red", linetype = "dashed") +
    ggplot2::annotate(
      'text', 
      label = sprintf('LoA = %.2f', upper_log),
      y = upper_log - 0.2,
      x = mean(range(ba_plot_tab$avg_log, na.rm = TRUE)), 
      color = 'red'
    ) +
    ggplot2::annotate(
      'text', 
      label = sprintf('LoA = %.2f', lower_log),
      y = lower_log + 0.2,
      x = mean(range(ba_plot_tab$avg_log, na.rm = TRUE)), 
      color = 'red'
    ) +
    ggplot2::annotate(
      'text', 
      label = sprintf('mean = %.2f', mean_diff_log),
      y = mean_diff_log * 0.70,
      x = mean(range(ba_plot_tab$avg_log, na.rm = TRUE)), 
      color = 'blue'
    ) +
    ggplot2::ylim(c(-1.5, 1)) +
    ggplot2::scale_color_viridis_d(limits = c('30', '20', '10', '0')) +
    ggplot2::ylab(expression(~Delta~'Log'[10]~'Ionic Conductivity'['(Coin Cell - PCB)'])) +
    ggplot2::xlab(expression('Average Log'[10]~'Ionic Conductivity')) +
    ggplot2::labs(color = 'LiTFSI wt%') +
    ggplot2::theme(
      legend.position = 'right',
      legend.title = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 12)
    )
  
  return(plot)
}

# =============================================================================
# VIOLIN PLOT FOR ADDITIVE SCREENING - ORIGINAL LOGIC
# =============================================================================

#' Create violin plot for additive screening
#' @param df Input dataframe (all data including early dates for this specific plot)
#' @return ggplot object
additive_violin_plot <- function(df) {
  
  # This plot uses DIFFERENT date filtering than other plots
  # From original: date_printed >= '2023-10-23' (solid polymer electrolyte studies)
  # Excludes specific early dates but keeps most data
  plot_data <- df %>%
    dplyr::filter(
      # Date filtering specific to additive screening
      date_casted >= as.Date('2023-10-23') |
        (date_casted != as.Date('2023-06-06') &
           date_casted != as.Date('2023-06-02') &
           date_casted != as.Date('2023-06-15'))
    ) %>%
    # Remove problematic Aerosil 380 samples from 2024 onwards
    dplyr::filter(
      !(fcomp_additive == 'Aerosil 380' & date_casted >= as.Date('2024-01-01'))
    ) %>%
    # Filter for specific conditions
    dplyr::filter(
      print_type != 'Coin cell',
      !(fcomp_additive == 'Al2O3' & ionic_conductivity_final <= 1e6),  # Remove bad Al2O3
      fcomp_salt_wt_pct == 0,
      fcomp_additive != 'EO', 
      fcomp_additive != 'none'
    )
  
  cat(sprintf("Violin plot using %d samples (special date filtering)\n", nrow(plot_data)))
  
  gg_add_violin <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      y = ionic_conductivity_final,
      x = as.factor(fcomp_additive),
      group = as.factor(fcomp_additive),
      color = as.factor(fcomp_additive)
    )
  ) +
    cowplot::theme_half_open() +
    # Highlight optimal conductivity range
    ggplot2::annotate(
      "rect",
      xmin = 0.5, xmax = 1.5,
      ymin = 1e-9, ymax = 5e-6,
      fill = "yellow", alpha = 0.3
    ) +
    ggplot2::geom_violin(
      size = 0.9,
      ggplot2::aes(fill = fcomp_additive),
      scale = 'width',
      alpha = 0.7
    ) +
    ggplot2::geom_boxplot(
      size = .25,
      width = .15,
      color = 'black'
    ) +
    ggplot2::scale_y_continuous(
      trans = 'log10',
      limits = c(1e-9, 1e-5),
      breaks = c(1e-5, 1e-6, 1e-7, 1e-8, 1e-9)
    ) +
    ggplot2::labs(
      y = 'Ionic Conductivity (S/cm)',
      title = 'b) Comparing SPEs with varying filler compositions'
    ) +
    ggplot2::theme(
      legend.position = 'none',
      plot.title.position = 'plot',
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 12, angle = 0, vjust = 0.7),
      strip.text = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11.35)
    ) +
    ggplot2::scale_x_discrete(labels = c(
      expression('Ae380'),
      expression('Ae90'),
      expression(Al[2] * O[3]),
      expression('EH5'),
      expression(TiO[2])
    )) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::scale_color_viridis_d()
  
  return(gg_add_violin)
}

# =============================================================================
# SUMMARY STATISTICS BAR PLOTS - ORIGINAL LOGIC
# =============================================================================

#' Create summary bar plots showing dataset statistics
#' @param df_flagged Dataframe with flag_for_removal column (FULL dataset)
#' @return List of ggplot objects
create_summary_bar_plots <- function(df_flagged) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("CREATING SUMMARY STATISTICS PLOTS\n")
  cat(strrep("=", 70), "\n\n")
  
  # Calculate statistics
  stats <- df_flagged %>%
    dplyr::group_by(print_type) %>%
    dplyr::summarise(
      n_unique_samples = dplyr::n_distinct(sample_ID),
      n_measurements = dplyr::n(),
      n_flagged = sum(flag_for_removal, na.rm = TRUE),
      n_clean = sum(!flag_for_removal, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      print_type = dplyr::case_when(
        print_type == "Coin cell" ~ "Coin cells",
        print_type == "PCB print" ~ "SPOC",
        print_type == "Bulk cast" ~ "SPOC",
        TRUE ~ print_type
      )
    ) %>%
    dplyr::group_by(print_type) %>%
    dplyr::summarise(
      n_unique_samples = sum(n_unique_samples),
      n_measurements = sum(n_measurements),
      n_flagged = sum(n_flagged),
      n_clean = sum(n_clean),
      .groups = 'drop'
    )
  
  cat("Dataset Summary:\n")
  print(stats)
  cat("\n")
  
  # Plot b) Number of unique samples measured
  plot_b <- ggplot2::ggplot(
    stats,
    ggplot2::aes(x = n_unique_samples, y = print_type)
  ) +
    ggplot2::geom_col(fill = "gray80", color = "black") +
    ggplot2::geom_col(
      ggplot2::aes(x = n_clean / dplyr::n_distinct(df_flagged$sample_ID[df_flagged$print_type == print_type]) * n_unique_samples),
      fill = "#55C667FF",
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_unique_samples),
      hjust = -0.1,
      size = 5
    ) +
    cowplot::theme_minimal_hgrid() +
    ggplot2::labs(
      x = "# Unique Samples",
      y = NULL,
      title = "b) Number of unique samples measured"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::xlim(0, max(stats$n_unique_samples) * 1.15)
  
  # Plot c) Total number of measurements
  plot_c <- ggplot2::ggplot(
    stats,
    ggplot2::aes(x = n_measurements, y = print_type)
  ) +
    ggplot2::geom_col(fill = "gray80", color = "black") +
    ggplot2::geom_col(
      ggplot2::aes(x = n_clean),
      fill = "#55C667FF",
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_measurements),
      hjust = -0.1,
      size = 5
    ) +
    cowplot::theme_minimal_hgrid() +
    ggplot2::labs(
      x = "# Measurements",
      y = NULL,
      title = "c) Total number of measurements"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::xlim(0, max(stats$n_measurements) * 1.15)
  
  # Plot d) Projected number of samples per year (example projection)
  # Assume current data represents partial year operation
  date_range <- df_flagged %>%
    dplyr::filter(!is.na(date_casted)) %>%
    dplyr::summarise(
      min_date = min(date_casted),
      max_date = max(date_casted),
      days = as.numeric(difftime(max_date, min_date, units = "days"))
    )
  
  projection_factor <- 365 / max(date_range$days, 1)
  
  stats_projected <- stats %>%
    dplyr::mutate(
      n_projected = round(n_unique_samples * projection_factor)
    )
  
  plot_d <- ggplot2::ggplot(
    stats_projected,
    ggplot2::aes(x = n_projected, y = print_type)
  ) +
    ggplot2::geom_col(fill = "gray80", color = "black") +
    ggplot2::geom_col(
      ggplot2::aes(x = n_projected * (n_clean / n_measurements)),
      fill = "#55C667FF",
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_projected),
      hjust = -0.1,
      size = 5
    ) +
    cowplot::theme_minimal_hgrid() +
    ggplot2::labs(
      x = "# Samples",
      y = NULL,
      title = "d) Projected number of samples per year, at full operation"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::xlim(0, max(stats_projected$n_projected) * 1.15)
  
  # Add annotation for coin cells
  if ("Coin cells" %in% stats_projected$print_type) {
    coin_stat <- stats_projected %>% dplyr::filter(print_type == "Coin cells")
    if (nrow(coin_stat) > 0) {
      plot_d <- plot_d +
        ggplot2::annotate(
          "text",
          x = coin_stat$n_projected / 2,
          y = which(stats_projected$print_type == "Coin cells"),
          label = "6x samples, one-tenth labor",
          hjust = 0.5,
          size = 3.5
        )
    }
  }
  
  cat("✓ Created 3 summary bar plots\n")
  cat(strrep("=", 70), "\n\n")
  
  return(list(
    unique_samples = plot_b,
    total_measurements = plot_c,
    projected = plot_d,
    combined = patchwork::wrap_plots(plot_b, plot_c, plot_d, ncol = 1)
  ))
}