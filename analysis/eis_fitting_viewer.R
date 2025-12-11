#!/usr/bin/env Rscript
# eis_fitting_viewer_modular.R

suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(plotly)
  library(stringr)
  library(viridisLite)
  library(DT)
  library(tidyr)
})
json_path <- "~/Git/SPOC_code/data/SPOC_battery.json"
# -----------------------------
# Args and basic helpers
# -----------------------------
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) == 0) stop("Please provide the path to your JSON as a command-line argument.")
# json_path <- args[1]
# if (!file.exists(json_path)) stop(paste("File not found:", json_path))

`%||%` <- function(x, y) if (!is.null(x)) x else y

idx_of <- function(vars, name) {
  vars_chr <- as.character(vars)
  idx <- which(vars_chr == name)
  if (length(idx) == 1) idx else NA_integer_
}

# -----------------------------
# Styling constants
# -----------------------------
raw_color <- "#1f4e79"   # dark blue
fit_color <- "#ff8c00"   # orange
marker_style <- list(color = raw_color, symbol = "circle", size = 8)
line_style   <- list(color = fit_color, width = 2, dash = "dash")

# -----------------------------
# JSON loading and tidying
# -----------------------------
load_json <- function(path) {
  fromJSON(path, simplifyVector = FALSE)
}

get_raw_arrays <- function(cycle) {
  vars <- cycle$data_array_variables %||% cycle$data_array_variable
  vals <- cycle$data_array_values %||% cycle$data_array_value
  list(vars = vars, vals = vals)
}

get_fit_arrays <- function(cycle) {
  # Prefer new schema fit_value_arrays when present
  if (!is.null(cycle$fit_value_arrays)) {
    vars <- cycle$fit_value_arrays$variables %||% cycle$fit_value_arrays[["variables"]]
    vals <- cycle$fit_value_arrays$values %||% cycle$fit_value_arrays[["values"]]
    if (!is.null(vars) && !is.null(vals)) return(list(vars = vars, vals = vals))
  }
  # Fallback to provided example schema
  vars <- cycle$fit_array_variables %||% cycle$all_variables
  vals <- cycle$fit_array_values %||% cycle$all_values
  list(vars = vars, vals = vals)
}

extract_cycle_df <- function(sample_id, sample_meta, pin_key, pin_entry, cycle_obj) {
  raw_arrays <- get_raw_arrays(cycle_obj)
  fit_arrays <- get_fit_arrays(cycle_obj)
  
  pin_num     <- pin_entry$pin %||% NA
  file_name   <- pin_entry$file_name %||% pin_key
  cycle_index <- cycle_obj$cycle %||% NA
  
  comp <- list(
    cast_type = sample_meta$print_quality$cast_type %||% NA,
    date_casted = sample_meta$print_quality$date_casted %||% NA,
    area_cm2 = sample_meta$print_quality$area_cm2 %||% NA,
    fcomp_mat1 = sample_meta$fcomp_mat1 %||% NA,
    fcomp_mat2 = sample_meta$fcomp_mat2 %||% NA,
    fcomp_mat1_ratio = sample_meta$fcomp_mat1_ratio %||% NA,
    fcomp_mat2_ratio = sample_meta$fcomp_mat2_ratio %||% NA,
    fcomp_additive = sample_meta$fcomp_additive %||% NA,
    fcomp_additive_wt_pct = sample_meta$fcomp_additive_wt_pct %||% NA,
    fcomp_salt = sample_meta$fcomp_salt %||% NA,
    fcomp_salt_wt_pct = sample_meta$fcomp_salt_wt_pct %||% NA,
    fcomp_inhibitor = sample_meta$fcomp_inhibitor %||% NA,
    fcomp_inhibitor_wt_pct = sample_meta$fcomp_inhibitor_wt_pct %||% NA,
    fcomp_formulation = sample_meta$fcomp_formulation %||% NA
  )
  
  # Raw
  raw_df <- NULL
  if (!is.null(raw_arrays$vars) && !is.null(raw_arrays$vals)) {
    v <- raw_arrays$vars; a <- raw_arrays$vals
    i_freq <- idx_of(v, "freq")
    i_Re   <- idx_of(v, "Re")
    i_Im   <- idx_of(v, "Im")
    i_Z    <- idx_of(v, "Z")
    i_Ph   <- idx_of(v, "Phase")
    
    valid <- all(!is.na(c(i_freq, i_Re, i_Im)))
    if (valid) {
      n <- length(a[[i_freq]])
      rr <- as.numeric(a[[i_Re]])[seq_len(n)]
      ri <- as.numeric(a[[i_Im]])[seq_len(n)]
      raw_df <- tibble(
        freq    = as.numeric(a[[i_freq]]),
        Raw_Re  = rr,
        Raw_Im  = ri,
        Raw_Z   = if (!is.na(i_Z)) as.numeric(a[[i_Z]])[seq_len(n)] else sqrt(rr^2 + ri^2),
        Raw_Ph  = if (!is.na(i_Ph)) as.numeric(a[[i_Ph]])[seq_len(n)] else atan2(ri, rr) * 180 / pi,
        type    = "Raw",
        sample_id = sample_id,
        pin = pin_num,
        pin_key = pin_key,
        file_name = file_name,
        cycle_index = cycle_index,
        cast_type = comp$cast_type,
        date_casted = comp$date_casted,
        area_cm2 = comp$area_cm2,
        fcomp_mat1 = comp$fcomp_mat1,
        fcomp_mat2 = comp$fcomp_mat2,
        fcomp_mat1_ratio = comp$fcomp_mat1_ratio,
        fcomp_mat2_ratio = comp$fcomp_mat2_ratio,
        fcomp_additive = comp$fcomp_additive,
        fcomp_additive_wt_pct = comp$fcomp_additive_wt_pct,
        fcomp_salt = comp$fcomp_salt,
        fcomp_salt_wt_pct = comp$fcomp_salt_wt_pct,
        fcomp_inhibitor = comp$fcomp_inhibitor,
        fcomp_inhibitor_wt_pct = comp$fcomp_inhibitor_wt_pct,
        fcomp_formulation = comp$fcomp_formulation
      )
    }
  }
  
  # Fit
  fit_df <- NULL
  if (!is.null(fit_arrays$vars) && !is.null(fit_arrays$vals)) {
    v <- fit_arrays$vars; a <- fit_arrays$vals
    i_freq <- idx_of(v, "freq")
    i_FRe <- idx_of(v, "Z_pred_real") %||% idx_of(v, "Re")
    i_FIm <- idx_of(v, "Z_pred_imag") %||% idx_of(v, "Im")
    i_MRe <- idx_of(v, "Z_measured_real")
    i_MIm <- idx_of(v, "Z_measured_imag")
    
    valid <- all(!is.na(c(i_freq, i_FRe, i_FIm)))
    if (valid) {
      n <- length(a[[i_freq]])
      fr <- as.numeric(a[[i_FRe]])[seq_len(n)]
      fi <- as.numeric(a[[i_FIm]])[seq_len(n)]
      mr <- if (!is.na(i_MRe)) as.numeric(a[[i_MRe]])[seq_len(n)] else NA_real_
      mi <- if (!is.na(i_MIm)) as.numeric(a[[i_MIm]])[seq_len(n)] else NA_real_
      fit_df <- tibble(
        freq    = as.numeric(a[[i_freq]]),
        Fit_Re  = fr,
        Fit_Im  = fi,
        Fit_Z   = sqrt(fr^2 + fi^2),
        Fit_Ph  = atan2(fi, fr) * 180 / pi,
        Meas_Re = mr,
        Meas_Im = mi,
        Meas_Z  = if (!all(is.na(mr)) && !all(is.na(mi))) sqrt(mr^2 + mi^2) else NA_real_,
        Meas_Ph = if (!all(is.na(mr)) && !all(is.na(mi))) atan2(mi, mr) * 180 / pi else NA_real_,
        type    = "Fit",
        sample_id = sample_id,
        pin = pin_num,
        pin_key = pin_key,
        file_name = file_name,
        cycle_index = cycle_index,
        cast_type = comp$cast_type,
        date_casted = comp$date_casted,
        area_cm2 = comp$area_cm2,
        fcomp_mat1 = comp$fcomp_mat1,
        fcomp_mat2 = comp$fcomp_mat2,
        fcomp_mat1_ratio = comp$fcomp_mat1_ratio,
        fcomp_mat2_ratio = comp$fcomp_mat2_ratio,
        fcomp_additive = comp$fcomp_additive,
        fcomp_additive_wt_pct = comp$fcomp_additive_wt_pct,
        fcomp_salt = comp$fcomp_salt,
        fcomp_salt_wt_pct = comp$fcomp_salt_wt_pct,
        fcomp_inhibitor = comp$fcomp_inhibitor,
        fcomp_inhibitor_wt_pct = comp$fcomp_inhibitor_wt_pct,
        fcomp_formulation = comp$fcomp_formulation
      )
    }
  }
  
  bind_rows(raw_df, fit_df)
}

tidy_eis_data <- function(json_data) {
  samples <- json_data$samples
  if (is.null(samples) || length(samples) == 0) stop("No samples found in the provided JSON.")
  imap_dfr(samples, function(sample_meta, sample_id) {
    eis <- sample_meta$eis_results
    if (is.null(eis) || length(eis) == 0) return(NULL)
    imap_dfr(eis, function(pin_entry, pin_key) {
      cycles <- pin_entry$cycle_data
      if (is.null(cycles) || length(cycles) == 0) return(NULL)
      map_dfr(cycles, function(cycle_obj) {
        extract_cycle_df(sample_id, sample_meta, pin_key, pin_entry, cycle_obj)
      })
    })
  })
}

# -----------------------------
# Metrics and summary builders
# -----------------------------
extract_fit_metrics <- function(cycles) {
  if (length(cycles) == 0) return(NULL)
  all_keys <- unique(unlist(lapply(cycles, names)))
  exclude_keys <- c("data_array_values","data_array_variables","fit_array_values","fit_array_variables","fit_value_arrays","all_values","all_variables")
  metric_keys <- setdiff(all_keys, exclude_keys)
  main_metrics <- metric_keys[!endsWith(metric_keys, "_stDev")]
  
  metrics <- lapply(main_metrics, function(mk) {
    vals <- lapply(cycles, function(cyc) {
      v <- cyc[[mk]]
      if (is.list(v)) v <- unlist(v)
      if (length(v) > 0) v[1] else NA
    })
    vals_num <- suppressWarnings(as.numeric(vals))
    if (all(is.na(vals_num))) vals_num <- vals
    stdevs <- lapply(cycles, function(cyc) {
      v <- cyc[[paste0(mk, "_stDev")]]
      if (is.list(v)) v <- unlist(v)
      if (length(v) > 0) v[1] else NA
    })
    stdevs_num <- suppressWarnings(as.numeric(stdevs))
    if (all(is.na(stdevs_num))) stdevs_num <- stdevs
    val <- if (length(vals_num) > 1 && is.numeric(vals_num)) mean(vals_num, na.rm = TRUE) else vals_num[[1]]
    stdev <- if (length(stdevs_num) > 1 && is.numeric(stdevs_num)) mean(stdevs_num, na.rm = TRUE) else stdevs_num[[1]]
    list(name = mk, value = val, stdev = stdev)
  })
  Filter(function(x) !is.na(x$value) && x$value != "NA", metrics)
}

build_fit_metrics_table <- function(metrics) {
  if (is.null(metrics) || length(metrics) == 0) {
    return(data.frame(Message = "No fit metrics found for this selection."))
  }
  data.frame(
    Metric = sapply(metrics, function(m) m$name),
    Value = sapply(metrics, function(m) {
      if (!is.na(m$stdev) && !is.na(m$value) && is.numeric(m$value) && is.numeric(m$stdev)) {
        sprintf("%.4g ± %.4g", m$value, m$stdev)
      } else if (!is.na(m$value) && is.numeric(m$value)) {
        sprintf("%.4g", m$value)
      } else if (!is.na(m$value)) {
        as.character(m$value)
      } else {
        "NA"
      }
    }),
    stringsAsFactors = FALSE
  )
}

build_file_summary_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(Message = "No selection or no data."))
  df %>%
    group_by(sample_id, pin, file_name, cycle_index, cast_type, fcomp_formulation, fcomp_additive, fcomp_salt) %>%
    summarise(
      n_raw = sum(type == "Raw"),
      n_fit = sum(type == "Fit"),
      freq_min = suppressWarnings(min(freq, na.rm = TRUE)),
      freq_max = suppressWarnings(max(freq, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(sample_id, pin, file_name, cycle_index)
}

# -----------------------------
# Plot builders
# -----------------------------
label_for <- function(dsub, kind) {
  paste0(
    kind, " | Sample: ", dsub$sample_id[1],
    " | Pin: ", dsub$pin[1],
    " | File: ", dsub$file_name[1],
    " | Cycle: ", dsub$cycle_index[1],
    " | Cast: ", dsub$cast_type[1],
    " | Formulation: ", dsub$fcomp_formulation[1]
  )
}

hover_for_raw <- function(dsub) {
  paste0(
    "Sample: ", dsub$sample_id,
    "<br>Pin: ", dsub$pin,
    "<br>File: ", dsub$file_name,
    "<br>Cycle: ", dsub$cycle_index,
    "<br>freq: ", signif(dsub$freq, 6),
    "<br>Re: ", signif(dsub$Raw_Re, 6),
    "<br>Im: ", signif(dsub$Raw_Im, 6)
  )
}

hover_for_fit <- function(dsub) {
  paste0(
    "Sample: ", dsub$sample_id,
    "<br>Pin: ", dsub$pin,
    "<br>File: ", dsub$file_name,
    "<br>Cycle: ", dsub$cycle_index,
    "<br>freq: ", signif(dsub$freq, 6),
    "<br>Re_fit: ", signif(dsub$Fit_Re, 6),
    "<br>Im_fit: ", signif(dsub$Fit_Im, 6)
  )
}

build_nyquist_plot <- function(df, plot_width, plot_height, overlay_pins, overlay_cycles) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  p <- plot_ly(width = plot_width, height = plot_height)
  
  grouping_vals <- if (isTRUE(overlay_pins)) unique(df$pin) else unique(df$file_name)
  cycles <- if (isTRUE(overlay_cycles)) unique(df$cycle_index) else unique(df$cycle_index)[1]
  
  for (grp in grouping_vals) {
    for (cyc in cycles) {
      dsub_raw <- df %>% filter(type == "Raw",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      dsub_fit <- df %>% filter(type == "Fit",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      if (nrow(dsub_raw) > 0) {
        p <- p %>% add_trace(
          x = dsub_raw$Raw_Re, y = dsub_raw$Raw_Im,
          name = label_for(dsub_raw, "Raw"),
          type = 'scatter', mode = 'markers',
          marker = marker_style, text = hover_for_raw(dsub_raw), hoverinfo = "text"
        )
      }
      if (nrow(dsub_fit) > 0) {
        p <- p %>% add_trace(
          x = dsub_fit$Fit_Re, y = dsub_fit$Fit_Im,
          name = label_for(dsub_fit, "Fit"),
          type = 'scatter', mode = 'lines',
          line = line_style, text = hover_for_fit(dsub_fit), hoverinfo = "text"
        )
      }
    }
  }
  
  p %>% layout(title = "Nyquist Plot", xaxis = list(title = "Z' (Real)"), yaxis = list(title = "Z'' (Imaginary)"))
}

build_bode_mag_plot <- function(df, plot_width, plot_height, overlay_pins, overlay_cycles) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  p <- plot_ly(width = plot_width, height = plot_height)
  
  grouping_vals <- if (isTRUE(overlay_pins)) unique(df$pin) else unique(df$file_name)
  cycles <- if (isTRUE(overlay_cycles)) unique(df$cycle_index) else unique(df$cycle_index)[1]
  
  for (grp in grouping_vals) {
    for (cyc in cycles) {
      dsub_raw <- df %>% filter(type == "Raw",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      dsub_fit <- df %>% filter(type == "Fit",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      if (nrow(dsub_raw) > 0) {
        p <- p %>% add_trace(
          x = dsub_raw$freq, y = sqrt(dsub_raw$Raw_Re^2 + dsub_raw$Raw_Im^2),
          name = paste("Raw |Z|", dsub_raw$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'markers',
          marker = marker_style, text = hover_for_raw(dsub_raw), hoverinfo = "text"
        )
      }
      if (nrow(dsub_fit) > 0) {
        p <- p %>% add_trace(
          x = dsub_fit$freq, y = sqrt(dsub_fit$Fit_Re^2 + dsub_fit$Fit_Im^2),
          name = paste("Fit |Z|", dsub_fit$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'lines',
          line = line_style, text = hover_for_fit(dsub_fit), hoverinfo = "text"
        )
      }
    }
  }
  
  p %>% layout(title = "Bode Magnitude Plot", xaxis = list(title = "Frequency (Hz)", type = "log"), yaxis = list(title = "|Z| (Ohm)", side = "left"))
}

build_bode_phase_plot <- function(df, plot_width, plot_height, overlay_pins, overlay_cycles) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  p <- plot_ly(width = plot_width, height = plot_height)
  
  grouping_vals <- if (isTRUE(overlay_pins)) unique(df$pin) else unique(df$file_name)
  cycles <- if (isTRUE(overlay_cycles)) unique(df$cycle_index) else unique(df$cycle_index)[1]
  
  for (grp in grouping_vals) {
    for (cyc in cycles) {
      dsub_raw <- df %>% filter(type == "Raw",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      dsub_fit <- df %>% filter(type == "Fit",
                                if (isTRUE(overlay_pins)) pin == grp else file_name == grp,
                                cycle_index == cyc)
      if (nrow(dsub_raw) > 0) {
        p <- p %>% add_trace(
          x = dsub_raw$freq, y = atan2(dsub_raw$Raw_Im, dsub_raw$Raw_Re) * 180 / pi,
          name = paste("Raw Phase", dsub_raw$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'markers',
          marker = marker_style, text = hover_for_raw(dsub_raw), hoverinfo = "text"
        )
      }
      if (nrow(dsub_fit) > 0) {
        p <- p %>% add_trace(
          x = dsub_fit$freq, y = atan2(dsub_fit$Fit_Im, dsub_fit$Fit_Re) * 180 / pi,
          name = paste("Fit Phase", dsub_fit$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'lines',
          line = line_style, text = hover_for_fit(dsub_fit), hoverinfo = "text"
        )
      }
    }
  }
  
  p %>% layout(title = "Bode Phase Plot", xaxis = list(title = "Frequency (Hz)", type = "log"), yaxis = list(title = "Phase (deg)", side = "left"))
}

build_linkk_plot <- function(df, plot_width, plot_height, overlay_pins, overlay_cycles) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  fit <- df %>% filter(type == "Fit")
  if (nrow(fit) == 0) return(NULL)
  has_measured <- !all(is.na(fit$Meas_Re)) && !all(is.na(fit$Meas_Im))
  
  p <- plot_ly(width = plot_width, height = plot_height)
  grouping_vals <- if (isTRUE(overlay_pins)) unique(fit$pin) else unique(fit$file_name)
  cycles <- if (isTRUE(overlay_cycles)) unique(fit$cycle_index) else unique(fit$cycle_index)[1]
  
  for (grp in grouping_vals) {
    for (cyc in cycles) {
      dsub_fit <- fit %>% filter(if (isTRUE(overlay_pins)) pin == grp else file_name == grp, cycle_index == cyc)
      if (nrow(dsub_fit) == 0) next
      
      mag_meas <- if (has_measured) dsub_fit$Meas_Z else NA_real_
      ph_meas  <- if (has_measured) dsub_fit$Meas_Ph else NA_real_
      mag_fit  <- dsub_fit$Fit_Z
      ph_fit   <- dsub_fit$Fit_Ph
      mag_res  <- if (has_measured) (mag_meas - mag_fit) / pmax(mag_meas, 1e-12) else NA_real_
      ph_res   <- if (has_measured) (ph_meas - ph_fit) else NA_real_
      
      if (has_measured) {
        p <- p %>% add_trace(
          x = dsub_fit$freq, y = mag_res,
          name = paste("Lin-KK Mag Residual", dsub_fit$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'markers',
          marker = marker_style,
          text = paste0("freq: ", signif(dsub_fit$freq, 6), "<br>mag residual: ", signif(mag_res, 6)),
          hoverinfo = "text"
        )
        p <- p %>% add_trace(
          x = dsub_fit$freq, y = ph_res,
          name = paste("Lin-KK Phase Residual", dsub_fit$file_name[1], "Cycle", cyc),
          type = 'scatter', mode = 'lines',
          line = line_style,
          text = paste0("freq: ", signif(dsub_fit$freq, 6), "<br>phase residual: ", signif(ph_res, 6)),
          hoverinfo = "text"
        )
      }
    }
  }
  
  p %>% layout(title = "Lin-KK Residuals", xaxis = list(title = "Frequency (Hz)", type = "log"),
               yaxis = list(title = "Residuals", side = "left"))
}

build_panel_plot <- function(df, plot_width, plot_height, overlay_pins, overlay_cycles, panel_selection) {
  panels <- list()
  if ("nyq" %in% panel_selection) panels <- append(panels, list(build_nyquist_plot(df, plot_width, plot_height, overlay_pins, overlay_cycles)))
  if ("mag" %in% panel_selection) panels <- append(panels, list(build_bode_mag_plot(df, plot_width, plot_height, overlay_pins, overlay_cycles)))
  if ("ph"  %in% panel_selection) panels <- append(panels, list(build_bode_phase_plot(df, plot_width, plot_height, overlay_pins, overlay_cycles)))
  if ("kk"  %in% panel_selection) panels <- append(panels, list(build_linkk_plot(df, plot_width, plot_height, overlay_pins, overlay_cycles)))
  if (length(panels) == 0) return(plot_ly())
  
  empty_plot <- plot_ly() %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  while (length(panels) < 4) panels <- append(panels, list(empty_plot))
  subplot(panels[[1]], panels[[2]], panels[[3]], panels[[4]], nrows = 2, margin = 0.07, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
}

# -----------------------------
# Filtering utilities
# -----------------------------
apply_filters <- function(df, sample_id, pins, files, cycles, cast_type, fcomp_additive, fcomp_salt, fcomp_formulation) {
  req(sample_id)
  out <- df %>% filter(sample_id == sample_id)
  if (!is.null(pins) && length(pins) > 0)           out <- out %>% filter(pin %in% pins)
  if (!is.null(files) && length(files) > 0)         out <- out %>% filter(file_name %in% files)
  if (!is.null(cycles) && length(cycles) > 0)       out <- out %>% filter(cycle_index %in% cycles)
  if (!is.null(cast_type) && cast_type != "Any")    out <- out %>% filter(cast_type == cast_type)
  if (!is.null(fcomp_additive) && fcomp_additive != "Any") out <- out %>% filter(fcomp_additive == fcomp_additive)
  if (!is.null(fcomp_salt) && fcomp_salt != "Any")  out <- out %>% filter(fcomp_salt == fcomp_salt)
  if (!is.null(fcomp_formulation) && fcomp_formulation != "Any") out <- out %>% filter(fcomp_formulation == fcomp_formulation)
  out
}

# -----------------------------
# UI wrapper
# -----------------------------
build_ui <- function(plot_df) {
  fluidPage(
    titlePanel("EIS Viewer, modular functions with metadata filters"),
    sidebarLayout(
      sidebarPanel(
        selectInput("sample_id", "Sample", choices = unique(plot_df$sample_id)),
        uiOutput("pin_ui"),
        uiOutput("file_ui"),
        uiOutput("cycle_ui"),
        
        h4("Composition filters"),
        uiOutput("cast_type_ui"),
        uiOutput("fcomp_additive_ui"),
        uiOutput("fcomp_salt_ui"),
        uiOutput("fcomp_formulation_ui"),
        
        checkboxInput("overlay_cycles", "Overlay selected cycles", value = FALSE),
        checkboxInput("overlay_pins", "Overlay selected pins", value = FALSE),
        
        hr(),
        h4("Plot Controls"),
        sliderInput("plot_width", "Panel Width (px)", min = 600, max = 1400, value = 1000),
        radioButtons("view_mode", "View mode", choices = c("Individual plots", "2x2 multi-plot"), selected = "2x2 multi-plot"),
        checkboxGroupInput("panel_selection", "Plots to include in 2x2 panel",
                           choices = c("Nyquist" = "nyq", "Bode Magnitude" = "mag", "Bode Phase" = "ph", "Lin-KK Residuals" = "kk"),
                           selected = c("nyq", "mag", "ph", "kk")),
        
        hr(),
        h4("File and metadata summary"),
        DT::dataTableOutput("file_summary"),
        
        hr(),
        h4("Fit Metrics"),
        DT::dataTableOutput("fit_metrics")
      ),
      mainPanel(
        uiOutput("plots_container"),
        uiOutput("nodata_msg")
      )
    )
  )
}

# -----------------------------
# Server wrapper
# -----------------------------
build_server <- function(plot_df, json_data) {
  function(input, output, session) {
    plot_height <- reactive({ input$plot_width * 0.5 })
    
    output$nodata_msg <- renderUI({
      if (is.null(plot_df) || nrow(plot_df) == 0) {
        tags$div(style="color:red;", "No valid EIS data found in the provided JSON.")
      }
    })
    
    # Dynamic filter UIs
    output$pin_ui <- renderUI({
      req(input$sample_id)
      pins <- plot_df %>% filter(sample_id == input$sample_id) %>% pull(pin) %>% unique()
      selectInput("pin", "Pin", choices = sort(na.omit(pins)), multiple = TRUE)
    })
    output$file_ui <- renderUI({
      req(input$sample_id)
      selected_pins <- input$pin %||% unique(plot_df$pin[plot_df$sample_id == input$sample_id])
      files <- plot_df %>% filter(sample_id == input$sample_id, pin %in% selected_pins) %>% pull(file_name) %>% unique()
      selectInput("file_name", "File", choices = sort(files), multiple = TRUE)
    })
    output$cycle_ui <- renderUI({
      req(input$sample_id)
      selected_pins <- input$pin %||% unique(plot_df$pin[plot_df$sample_id == input$sample_id])
      selected_files <- input$file_name %||% unique(plot_df$file_name[plot_df$sample_id == input$sample_id])
      cycles <- plot_df %>%
        filter(sample_id == input$sample_id, pin %in% selected_pins, file_name %in% selected_files) %>%
        pull(cycle_index) %>% unique()
      selectInput("cycle_index", "Cycle", choices = sort(na.omit(cycles)), multiple = TRUE, selected = sort(na.omit(cycles))[1])
    })
    
    output$cast_type_ui <- renderUI({
      types <- plot_df %>% filter(sample_id == input$sample_id) %>% pull(cast_type) %>% unique()
      selectInput("cast_type", "Cast type", choices = c("Any", sort(na.omit(types))), selected = "Any")
    })
    output$fcomp_additive_ui <- renderUI({
      adds <- plot_df %>% filter(sample_id == input$sample_id) %>% pull(fcomp_additive) %>% unique()
      selectInput("fcomp_additive", "Additive", choices = c("Any", sort(na.omit(adds))), selected = "Any")
    })
    output$fcomp_salt_ui <- renderUI({
      salts <- plot_df %>% filter(sample_id == input$sample_id) %>% pull(fcomp_salt) %>% unique()
      selectInput("fcomp_salt", "Salt", choices = c("Any", sort(na.omit(salts))), selected = "Any")
    })
    output$fcomp_formulation_ui <- renderUI({
      forms <- plot_df %>% filter(sample_id == input$sample_id) %>% pull(fcomp_formulation) %>% unique()
      selectInput("fcomp_formulation", "Formulation", choices = c("Any", sort(na.omit(forms))), selected = "Any")
    })
    
    # Filtered data
    filtered_df <- reactive({
      apply_filters(
        df = plot_df,
        sample_id = input$sample_id,
        pins = input$pin,
        files = input$file_name,
        cycles = input$cycle_index,
        cast_type = input$cast_type,
        fcomp_additive = input$fcomp_additive,
        fcomp_salt = input$fcomp_salt,
        fcomp_formulation = input$fcomp_formulation
      )
    })
    
    # Plot container
    output$plots_container <- renderUI({
      if (input$view_mode == "Individual plots") {
        tagList(
          plotlyOutput("nyquist_plot", width = "100%", height = plot_height()),
          plotlyOutput("bode_mag_plot", width = "100%", height = plot_height()),
          plotlyOutput("bode_phase_plot", width = "100%", height = plot_height()),
          plotlyOutput("linkk_plot", width = "100%", height = plot_height())
        )
      } else {
        plotlyOutput("panel_plot", width = input$plot_width, height = plot_height() * 2)
      }
    })
    
    # Individual plots
    output$nyquist_plot   <- renderPlotly({ build_nyquist_plot(filtered_df(), input$plot_width, plot_height(), input$overlay_pins, input$overlay_cycles) })
    output$bode_mag_plot  <- renderPlotly({ build_bode_mag_plot(filtered_df(), input$plot_width, plot_height(), input$overlay_pins, input$overlay_cycles) })
    output$bode_phase_plot<- renderPlotly({ build_bode_phase_plot(filtered_df(), input$plot_width, plot_height(), input$overlay_pins, input$overlay_cycles) })
    output$linkk_plot     <- renderPlotly({ build_linkk_plot(filtered_df(), input$plot_width, plot_height(), input$overlay_pins, input$overlay_cycles) })
    
    # 2x2 panel
    output$panel_plot <- renderPlotly({
      build_panel_plot(filtered_df(), input$plot_width, plot_height(), input$overlay_pins, input$overlay_cycles, input$panel_selection)
    })
    
    # Summary table
    output$file_summary <- DT::renderDataTable({
      build_file_summary_table(filtered_df())
    }, options = list(pageLength = 10, searching = TRUE, lengthChange = FALSE))
    
    # Fit metrics table
    output$fit_metrics <- DT::renderDataTable({
      sample_entry <- json_data$samples[[input$sample_id]]
      cycles <- list()
      if (!is.null(sample_entry) && !is.null(sample_entry$eis_results)) {
        eis <- sample_entry$eis_results
        selected_pins <- if (!is.null(input$pin) && length(input$pin) > 0) input$pin else unique(plot_df$pin[plot_df$sample_id == input$sample_id])
        selected_files <- if (!is.null(input$file_name) && length(input$file_name) > 0) input$file_name else NULL
        selected_cycles <- if (!is.null(input$cycle_index) && length(input$cycle_index) > 0) input$cycle_index else NULL
        
        for (pk in names(eis)) {
          pe <- eis[[pk]]
          if (!is.null(pe$pin) && pe$pin %in% selected_pins) {
            if (!is.null(selected_files) && length(selected_files) > 0 && !(pe$file_name %in% selected_files)) next
            cds <- pe$cycle_data
            if (!is.null(cds) && length(cds) > 0) {
              if (!is.null(selected_cycles) && length(selected_cycles) > 0) {
                cds <- Filter(function(cyc) cyc$cycle %in% selected_cycles, cds)
              }
              cycles <- c(cycles, cds)
            }
          }
        }
      }
      metrics <- extract_fit_metrics(cycles)
      build_fit_metrics_table(metrics)
    }, options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE))
  }
}

# -----------------------------
# Bootstrap app
# -----------------------------

json_data <- load_json(json_path)
plot_df <- tidy_eis_data(json_data)
ui <- build_ui(plot_df)
server <- build_server(plot_df, json_data)
shinyApp(ui, server)