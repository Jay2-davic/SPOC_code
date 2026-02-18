require(jsonlite)

df <- jsonlite::fromJSON("~/Git/23-spoc_code/BO_scripts/results/polymer_v5/polymer_v5_BO.json")
manuscript_directory <- r"(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Manuscripts\2409-PlatformFocus\figs_2\)"

df_melt <- rrapply::rrapply(df, how = 'melt') |>
  dplyr::filter(L3 == 'evaluation') |>
  dplyr::mutate(x = interaction(L4, L5)) |>
  dplyr::select(-c(L1, L5, L3, L4)) |>
  tidyr::pivot_wider(names_from = x, values_from = value) |>
  tidyr::unnest() |>
  dplyr::mutate(L2 = L2 |> as.factor()) |>
  tidyr::separate(
    "L2",
    sep = "_",
    into = c("model_type", "date", "serial_id", "parameter", "acq", "pct")
  ) |>
  # Rename model types
  dplyr::mutate(
    model_type = dplyr::case_when(
      model_type == "GAUSSIAN-PROCESS" ~ "GP",
      model_type == "RANDOM-FOREST" ~ "RF",
      TRUE ~ model_type
    )
  )

# When creating baseline, expand it to all combinations upfront
add_baseline <- function(df) {
  max_index <- max(df$TopPercent.iteration, na.rm = TRUE)
  
  # Get all unique model_type, parameter, acq combinations
  combos <- df |>
    dplyr::distinct(model_type, parameter, acq)
  
  # Create baseline template (without parameter and acq)
  baseline_template <- tibble::tibble(
    TopPercent.iteration = 1:max_index,
    TopPercent.median = (1:max_index) / max_index,
    TopPercent.q05 = (1:max_index) / max_index,
    TopPercent.q95 = (1:max_index) / max_index,
    EF.iteration = 1:max_index,
    EF.median = 1,
    EF.q05 = 1,
    EF.q95 = 1,
    AF.TopPercent = 1:max_index / max_index,
    AF.median = 1,
    model_type = "Random"
  )
  
  # Expand baseline to all combinations
  baseline_expanded <- tidyr::crossing(baseline_template, combos |> dplyr::select(parameter, acq))
  
  dplyr::bind_rows(df, baseline_expanded)
}

# Use it
df_melt <- add_baseline(df_melt)

# Configuration for plot aesthetics
plot_config <- list(
  line_size = 0.5,
  baseline_size = 0.6,
  ribbon_alpha = 0.15,
  arrow_size = 0.6,
  annotation_size = 2.5,
  dashed_line_size = 0.5,
  vertical_line_size = 0.35,
  x_padding = 0.02,
  y_padding = 0.02,
  # Font sizes - increased
  axis_text_size = 9,  # Increased from 8
  axis_title_size = 10,  # Increased from 9
  strip_text_size = 8,
  legend_text_size = 8,  # Increased from 7
  legend_title_size = 9,  # Increased from 8
  # Facet-specific font sizes
  facet_axis_text_size = 8,  # Increased from 7
  facet_axis_title_size = 9,  # Increased from 8
  facet_strip_text_size = 7,
  facet_legend_text_size = 7,  # Increased from 6
  facet_legend_title_size = 8,  # Increased from 7
  # Plot title
  plot_title_size = 10,
  colors = list(
    GP = "#440154FF",
    RF = "#21908CFF",
    Random = "black"
  )
)

plot_BO <- function(df,
                    type = c("TopPercent", "EF", "AF"),
                    facet = c("yes", "no"),
                    manuscript_directory = NULL,
                    config = plot_config) {
  
  # Setup column names
  if (type != "AF") {
    x_main <- glue::glue("{type}.iteration") |> rlang::sym()
  } else {
    x_main <- glue::glue("{type}.TopPercent") |> rlang::sym()
  }
  
  y_main <- glue::glue("{type}.median") |> rlang::sym()
  y_min <- glue::glue("{type}.q05") |> rlang::sym()
  y_max <- glue::glue("{type}.q95") |> rlang::sym()
  
  # Set axis labels
  if (type != "AF") {
    x_label <- "Iterations"  # Changed from expression to simple text
  } else {
    x_label <- "Top %"
  }
  
  y_label <- switch(type,
                    "TopPercent" = "Top %",
                    "EF" = "Enhancement Factor, EF",
                    "AF" = "Acceleration Factor, AF")
  
  # Extract baseline
  baseline_df <- df |> dplyr::filter(model_type == 'Random')
  
  if (facet == "yes") {
    p <- create_facet_plot(df, baseline_df, type, x_main, y_main, y_min, y_max, 
                           x_label, y_label, config)
  } else {
    p <- create_single_plot(df, baseline_df, type, x_main, y_main, y_min, y_max, 
                            x_label, y_label, config)
  }
  
  # Save plot
  filename <- paste0(manuscript_directory, type, "_", facet, ".png")
  ggplot2::ggsave(
    p,
    filename = filename,
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  print(filename)
}

create_facet_plot <- function(df, baseline_df, type, x_main, y_main, y_min, y_max, 
                              x_label, y_label, config) {
  # Get facet combinations
  facet_combos <- df |>
    dplyr::filter(model_type != 'Random') |>
    dplyr::distinct(model_type, acq)
  
  # Replicate baseline for each facet
  baseline_expanded <- facet_combos |>
    dplyr::rowwise() |>
    dplyr::do({
      combo <- .
      baseline_df |>
        dplyr::mutate(model_type = combo$model_type, acq = combo$acq)
    }) |>
    dplyr::ungroup()
  
  # Filter and prepare data
  df_no_baseline <- df |> 
    dplyr::filter(model_type != 'Random') |>
    dplyr::mutate(parameter = factor(parameter, levels = rev(unique(parameter))))
  
  # Calculate axis limits
  limits <- calculate_axis_limits(df_no_baseline, x_main, y_min, y_max, type, config)
  
  # Create base plot
  p <- ggplot2::ggplot(
    df_no_baseline,
    ggplot2::aes(
      x = !!x_main,
      y = !!y_main,
      color = parameter,
      group = parameter,
      ymin = !!y_min,
      ymax = !!y_max,
      fill = parameter
    )
  ) +
    ggplot2::geom_line(size = config$line_size) +
    ggplot2::geom_ribbon(alpha = config$ribbon_alpha, color = NA, show.legend = FALSE) +
    ggplot2::geom_line(
      data = baseline_expanded,
      ggplot2::aes(x = !!x_main, y = !!y_main),
      color = "black",
      linetype = "solid",
      size = config$baseline_size,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_viridis_d(direction = -1) +
    ggplot2::scale_fill_viridis_d(direction = -1) +
    ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
    ggplot2::facet_grid(model_type ~ acq) +
    ggplot2::labs(x = x_label, y = y_label) +  # Removed legend titles
    cowplot::theme_cowplot() +
    cowplot::panel_border() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = config$facet_axis_text_size),
      axis.text.y = ggplot2::element_text(size = config$facet_axis_text_size),
      axis.title.x = ggplot2::element_text(size = config$facet_axis_title_size),
      axis.title.y = ggplot2::element_text(size = config$facet_axis_title_size),
      strip.text = ggplot2::element_text(size = config$facet_strip_text_size),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = config$facet_legend_text_size, margin = ggplot2::margin(r = 2, unit = "pt")),
      legend.title = ggplot2::element_blank(),  # Remove legend title
      legend.spacing.x = ggplot2::unit(2, "pt"),
      legend.spacing.y = ggplot2::unit(0, "pt"),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.margin = ggplot2::margin(0, 0, 2, 0)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    )
  
  # Add horizontal dashed lines for TopPercent
  if (type == "TopPercent") {
    hline_data <- data.frame(y_intercept = c(1.0, 0.9, 0.8))
    hline_expanded <- tidyr::crossing(hline_data, facet_combos)
    
    p <- p +
      ggplot2::geom_hline(
        data = hline_expanded,
        ggplot2::aes(yintercept = y_intercept),
        linetype = "dashed",
        color = "gray50",
        size = config$dashed_line_size
      )
  }
  
  return(p)
}

create_single_plot <- function(df, baseline_df, type, x_main, y_main, y_min, y_max, 
                               x_label, y_label, config) {
  # Prepare data
  df_plot <- df |>
    dplyr::mutate(model_type = factor(model_type, 
                                      levels = c(rev(unique(df$model_type[df$model_type != "Random"])), "Random")))
  
  # Setup colors
  model_types <- levels(df_plot$model_type)
  n_non_random <- sum(model_types != "Random")
  color_values <- c(config$colors$GP, config$colors$RF, config$colors$Random)
  names(color_values) <- model_types
  linetype_values <- rep("solid", length(model_types))
  names(linetype_values) <- model_types
  
  # Calculate axis limits
  limits <- calculate_axis_limits(df_plot, x_main, y_min, y_max, type, config)
  
  # Determine legend position
  if (type == "TopPercent") {
    legend_pos <- c(0.98, 0.02)
    legend_just <- c("right", "bottom")
  } else {
    legend_pos <- "right"
    legend_just <- NULL
  }
  
  # Create base plot
  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x = !!x_main,
      y = !!y_main,
      color = model_type,
      linetype = model_type,
      group = interaction(model_type, parameter, acq),
      ymin = !!y_min,
      ymax = !!y_max,
      fill = model_type
    )
  ) +
    ggplot2::geom_line(size = config$line_size) +
    ggplot2::geom_ribbon(
      data = df_plot |> dplyr::filter(model_type != "Random"),
      alpha = config$ribbon_alpha,
      color = NA,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::scale_fill_manual(values = color_values, guide = "none") +
    ggplot2::scale_linetype_manual(values = linetype_values) +
    ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
    ggplot2::labs(x = x_label, y = y_label) +  # Removed legend titles
    cowplot::theme_cowplot() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = config$axis_text_size),
      axis.text.y = ggplot2::element_text(size = config$axis_text_size),
      axis.title.x = ggplot2::element_text(size = config$axis_title_size),
      axis.title.y = ggplot2::element_text(size = config$axis_title_size),
      legend.text = ggplot2::element_text(size = config$legend_text_size, margin = ggplot2::margin(r = 3, unit = "pt")),
      legend.title = ggplot2::element_blank(),  # Remove legend title
      legend.spacing.x = ggplot2::unit(3, "pt"),
      legend.key.size = ggplot2::unit(0.9, "lines"),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", size = 0.5),
      plot.title = ggplot2::element_text(size = config$plot_title_size, hjust = 0, face = "bold")
    )
  
  # Add plot title for TopPercent only
  if (type == "TopPercent") {
    p <- p + ggplot2::ggtitle("d) Analysis of surrogate modeling using SPOC\ndata and rate enhancement")
  }
  
  # Set legend position
  if (type == "TopPercent") {
    p <- p + ggplot2::theme(
      legend.position = legend_pos,
      legend.justification = legend_just
    )
  } else {
    p <- p + ggplot2::theme(legend.position = "right")
  }
  
  # Add type-specific annotations
  if (type == "TopPercent") {
    p <- add_toppercent_annotations(p, df_plot, x_main, y_main, color_values, config)
  } else if (type %in% c("EF", "AF")) {
    p <- add_ef_af_annotations(p, df_plot, x_main, y_main, color_values, config)
  }
  
  return(p)
}

calculate_axis_limits <- function(df, x_main, y_min, y_max, type, config) {
  x_range <- range(df[[rlang::as_name(x_main)]], na.rm = TRUE)
  x_padding <- diff(x_range) * config$x_padding
  x_limits <- c(x_range[1] - x_padding, x_range[2] + x_padding)
  
  y_range <- range(c(df[[rlang::as_name(y_min)]], df[[rlang::as_name(y_max)]]), na.rm = TRUE)
  y_padding <- diff(y_range) * config$y_padding
  
  if (type %in% c("EF", "AF")) {
    y_limits <- c(0, y_range[2] + y_padding)
  } else {
    y_limits <- c(y_range[1] - y_padding, y_range[2] + y_padding)
  }
  
  list(x = x_limits, y = y_limits)
}

add_toppercent_annotations <- function(p, df_plot, x_main, y_main, color_values, config) {
  y_targets <- c(1.0, 0.9, 0.8)
  x_max <- max(df_plot[[rlang::as_name(x_main)]], na.rm = TRUE)
  
  df_random <- df_plot |> dplyr::filter(model_type == "Random")
  df_non_random <- df_plot |> dplyr::filter(model_type != "Random")
  
  # Add horizontal lines
  for (y_target in y_targets) {
    p <- p + 
      ggplot2::geom_hline(
        yintercept = y_target,
        linetype = "dashed",
        color = "gray50",
        size = config$dashed_line_size
      )
  }
  
  # Collect arrow and annotation data
  arrow_data_list <- list()
  annotation_data_list <- list()
  
  for (y_target in y_targets) {
    random_data <- df_random |> dplyr::arrange(.data[[rlang::as_name(x_main)]])
    random_crossing_idx <- which(random_data[[rlang::as_name(y_main)]] >= y_target)[1]
    
    if (!is.na(random_crossing_idx) && random_crossing_idx > 0) {
      x_random <- random_data[[rlang::as_name(x_main)]][random_crossing_idx]
      
      rf_model_data <- df_non_random |> 
        dplyr::filter(model_type == "RF") |>
        dplyr::arrange(.data[[rlang::as_name(x_main)]])
      
      rf_crossing_idx <- which(rf_model_data[[rlang::as_name(y_main)]] >= y_target)[1]
      
      if (!is.na(rf_crossing_idx) && rf_crossing_idx > 0) {
        x_rf_target <- rf_model_data[[rlang::as_name(x_main)]][rf_crossing_idx]
        arrow_distance <- abs(x_random - x_rf_target)
        
        if (arrow_distance > 10) {
          x_start <- min(x_rf_target, x_random) + 3
          x_end <- max(x_rf_target, x_random) - 3
        } else if (arrow_distance > 2) {
          x_start <- min(x_rf_target, x_random) + 1
          x_end <- max(x_rf_target, x_random) - 1
        } else {
          x_start <- min(x_rf_target, x_random)
          x_end <- max(x_rf_target, x_random)
        }
        
        if (x_start < x_end) {
          arrow_data_list[[length(arrow_data_list) + 1]] <- data.frame(
            x = x_start, xend = x_end, y = y_target, yend = y_target
          )
          
          model_counter <- 0
          for (model in unique(df_non_random$model_type)) {
            model_data <- df_non_random |> 
              dplyr::filter(model_type == model) |>
              dplyr::arrange(.data[[rlang::as_name(x_main)]])
            
            crossing_idx <- which(model_data[[rlang::as_name(y_main)]] >= y_target)[1]
            
            if (!is.na(crossing_idx) && crossing_idx > 0) {
              x_at_target <- model_data[[rlang::as_name(x_main)]][crossing_idx]
              top <- x_max / x_at_target
              vjust_val <- ifelse(model_counter %% 2 == 0, -0.5, 1.5)
              
              annotation_data_list[[length(annotation_data_list) + 1]] <- data.frame(
                x = (x_start + x_end) / 2,
                y = y_target,
                label = sprintf("%.1fx", top),  # Changed to 1 decimal place
                vjust = vjust_val,
                model = model
              )
              
              model_counter <- model_counter + 1
            }
          }
        }
      }
    }
  }
  
  # Add arrows
  if (length(arrow_data_list) > 0) {
    arrow_data <- do.call(rbind, arrow_data_list)
    for (i in 1:nrow(arrow_data)) {
      p <- p +
        ggplot2::geom_segment(
          data = arrow_data[i, ],
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          arrow = ggplot2::arrow(ends = "both", length = ggplot2::unit(0.2, "cm"), type = "closed"),
          color = "black",
          size = config$arrow_size,
          inherit.aes = FALSE
        )
    }
  }
  
  # Add annotations
  if (length(annotation_data_list) > 0) {
    annotation_data <- do.call(rbind, annotation_data_list)
    for (i in 1:nrow(annotation_data)) {
      p <- p +
        ggplot2::annotate(
          "text",
          x = annotation_data$x[i],
          y = annotation_data$y[i],
          label = annotation_data$label[i],
          vjust = annotation_data$vjust[i],
          hjust = 0.5,
          size = config$annotation_size,
          color = color_values[annotation_data$model[i]]
        )
    }
  }
  
  return(p)
}

add_ef_af_annotations <- function(p, df_plot, x_main, y_main, color_values, config) {
  max_vals <- df_plot |>
    dplyr::filter(model_type != "Random") |>
    dplyr::group_by(model_type) |>
    dplyr::slice_max(order_by = .data[[rlang::as_name(y_main)]], n = 1) |>
    dplyr::summarise(
      max_y = max(.data[[rlang::as_name(y_main)]], na.rm = TRUE),
      x_at_max = .data[[rlang::as_name(x_main)]][which.max(.data[[rlang::as_name(y_main)]])],
      .groups = "drop"
    )
  
  for (i in 1:nrow(max_vals)) {
    p <- p +
      ggplot2::geom_segment(
        data = max_vals[i, ],
        ggplot2::aes(x = x_at_max, xend = x_at_max, y = 0, yend = max_y),
        linetype = "dashed",
        color = color_values[max_vals$model_type[i]],
        size = config$vertical_line_size,
        inherit.aes = FALSE
      ) +
      ggplot2::annotate(
        "text",
        x = max_vals$x_at_max[i],
        y = (max_vals$max_y[i] + 1) / 2,
        label = sprintf("%.1f", max_vals$max_y[i]),
        hjust = -0.2,
        vjust = 0.5,
        size = config$annotation_size,
        color = color_values[max_vals$model_type[i]]
      )
  }
  
  return(p)
}

# Run plots
df_final <- df_melt |> dplyr::filter(parameter != "ESS")

plot_BO(df_final, "TopPercent", facet = "yes", manuscript_directory = manuscript_directory)
plot_BO(df_final, "EF", facet = "yes", manuscript_directory = manuscript_directory)
plot_BO(df_final, "AF", facet = "yes", manuscript_directory = manuscript_directory)

best <- df_melt %>%
  dplyr::filter(model_type != "Random") %>%
  dplyr::group_by(model_type, parameter, acq) %>%
  dplyr::summarise(avg_median = mean(TopPercent.median, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(model_type) %>%
  dplyr::slice_max(order_by = avg_median, n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(model_type, parameter, acq)

df_filtered <- df_final %>%
  dplyr::semi_join(best, by = c("model_type", "parameter", "acq"))

baseline_data <- df_final %>%
  dplyr::filter(model_type == "Random")

df_filtered <- dplyr::bind_rows(df_filtered, baseline_data)

plot_BO(df_filtered, "TopPercent", facet = "no", manuscript_directory = manuscript_directory)
plot_BO(df_filtered, "EF", facet = "no", manuscript_directory = manuscript_directory)
plot_BO(df_filtered, "AF", facet = "no", manuscript_directory = manuscript_directory)