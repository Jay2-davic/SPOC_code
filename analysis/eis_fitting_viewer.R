#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(plotly)
  library(gt)
  library(viridisLite)
  library(jsonlite)
})

# =============================================================================
# UTILITIES
# =============================================================================
`%||%` <- function(x, y) if (!is.null(x)) x else y
null_if_empty <- function(x) if (is.list(x) && length(x) == 0) NULL else x
scalar1 <- function(x) {
  x <- null_if_empty(x)
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.list(x)) x <- x[[1]]
  if (length(x) == 0) return(NA)
  x[1]
}

# =============================================================================
# JSON LOADING - jsonlite only
# =============================================================================
read_file_text <- function(p) {
  con <- if (grepl("\\.gz$", p, ignore.case = TRUE)) gzfile(p, "rt") else file(p, "rt")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

sanitize_json_text <- function(txt, sanitize_non_standard = FALSE) {
  txt <- sub("^\uFEFF", "", txt)
  if (isTRUE(sanitize_non_standard)) {
    txt <- gsub("\\b(NaN|Infinity|-Inf|Inf|NA|Undefined)\\b", "null", txt, ignore.case = TRUE)
  }
  txt
}

load_json <- function(path, sanitize_non_standard = FALSE) {
  if (is.na(path) || !nzchar(path)) return(list())
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))
  txt <- read_file_text(path)
  txt <- sanitize_json_text(txt, sanitize_non_standard)
  jsonlite::fromJSON(
    txt,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE,
    flatten = FALSE
  )
}

# Separate, explicit loaders so you can do slow reads once
load_meta_json <- function(path) {
  load_json(path, sanitize_non_standard = FALSE)
}

load_eis_json <- function(path) {
  # EIS sources often contain non standard literals, sanitize them
  load_json(path, sanitize_non_standard = TRUE)
}

# =============================================================================
# SAMPLE RESOLUTION AND MERGE
# =============================================================================
resolve_sample_map <- function(x) {
  if (is.null(x) || !is.list(x)) return(list())
  
  candidates <- list(
    x$samples,
    x$eis_results,
    x$data$samples,
    if (!is.null(names(x)) && all(vapply(x, is.list, logical(1)))) x else NULL
  )
  
  purrr::detect(candidates, ~ !is.null(.x) && is.list(.x)) %||% list()
}

# =============================================================================
# PIN/CYCLE NORMALIZATION
# =============================================================================
# Heuristic, recognizes common pin key patterns
is_pin_key <- function(nm) {
  grepl("^(pin_?\\d+|P\\d+)$", nm, ignore.case = TRUE)
}

normalize_pin_entry <- function(pin_obj, key = NULL) {
  # Handle atomic vectors gracefully
  if (!is.list(pin_obj)) {
    return(list(
      pin = suppressWarnings(as.integer(gsub("\\D+", "", key))) %||% NA_integer_,
      file_name = key %||% NA_character_,
      cycle_data = list()
    ))
  }
  
  # Safe accessor
  get_field <- function(obj, ...) {
    fields <- list(...)
    for (field in fields) {
      val <- obj[[field]]
      if (!is.null(val)) return(val)
    }
    NULL
  }
  
  pin_val  <- get_field(pin_obj, "pin", "pin_id", "pinNumber") %||%
    suppressWarnings(as.integer(gsub("\\D+", "", key)))
  file_val <- get_field(pin_obj, "file_name", "file", "mpr", "filePath") %||% key
  
  # Known cycle containers
  cycles <- get_field(pin_obj, "cycle_data", "cycles", "eis_cycles")
  
  # Handle cycle_* named lists
  if (is.null(cycles) || length(cycles) == 0) {
    cycle_keys <- names(pin_obj)
    if (!is.null(cycle_keys) && length(cycle_keys)) {
      hits <- grep("^cycle_\\d+$", cycle_keys, value = TRUE)
      if (length(hits) > 0) {
        nums <- suppressWarnings(as.integer(sub("^cycle_", "", hits)))
        ord  <- order(nums)
        cycles <- purrr::map2(pin_obj[hits][ord], nums[ord], function(obj, num) {
          obj <- if (is.list(obj)) obj else list()
          if (is.null(obj$cycle)) obj$cycle <- num
          obj
        })
      }
    }
  }
  
  # Heuristic: if remaining elements look like cycles, treat them as such
  if (is.null(cycles) || length(cycles) == 0) {
    cycles <- list()
  } else {
    nm <- names(cycles)
    cycles <- purrr::imap(cycles, function(cyc, i) {
      cyc <- if (is.list(cyc)) cyc else list()
      # Fill cycle if missing, prefer numeric part of the name, otherwise index
      if (is.null(cyc$cycle)) {
        if (!is.null(nm) && length(nm) >= i && nzchar(nm[[i]])) {
          nn <- suppressWarnings(as.integer(gsub("\\D+", "", nm[[i]])))
          cyc$cycle <- if (!is.na(nn)) nn else as.integer(i)
        } else {
          cyc$cycle <- as.integer(i)
        }
      }
      cyc$eis_flag <- isTRUE(cyc$eis_flag)
      cyc$eis_note <- cyc$eis_note %||% ""
      cyc
    })
  }
  
  list(
    pin = pin_val %||% NA_integer_,
    file_name = file_val %||% key,
    cycle_data = cycles
  )
}

build_pin_map <- function(sample_eis) {
  empty <- list(keys = character(), map = list())
  if (is.null(sample_eis) || !is.list(sample_eis)) return(empty)
  
  ensure_names <- function(xs) {
    nm <- names(xs)
    if (is.null(nm)) nm <- rep("", length(xs))
    missing <- is.na(nm) | nm == ""
    if (any(missing)) nm[missing] <- sprintf("pin_%04d", which(missing))
    names(xs) <- nm
    xs
  }
  
  # Convert a data.frame with pin-like columns into a list-of-pins
  df_to_pin_list <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    nm <- names(df)
    if (is.null(nm) || !length(nm)) return(NULL)
    
    pin_cols <- nm[is_pin_key(nm)]
    if (length(pin_cols)) {
      rn <- rownames(df)
      out <- df[pin_cols] |>
        purrr::imap(~ {
          v <- if (is.list(.x)) .x else as.list(.x)
          cur <- names(v)
          if (!is.null(rn) && length(v) == length(rn) &&
              (is.null(cur) || any(cur == "" | is.na(cur)))) {
            names(v) <- rn
          }
          v
        })
      return(out)
    }
    
    # Single pin shape: rows are cycles with list-columns
    if (any(c("data_array_values","data_array_variables",
              "fit_array_values","fit_array_variables",
              "all_values","all_variables") %in% nm)) {
      rows_as_cycles <- purrr::pmap(as.list(df), ~ list(...))
      return(list(pin_0001 = rows_as_cycles))
    }
    
    NULL
  }
  
  # Scopes to try: itself, one-level unwrap, and direct list/data.frame children
  get_scopes <- function(x) {
    unwrap <- if (is.list(x) && length(x) == 1 && is.list(x[[1]])) list(x[[1]]) else list()
    children <- if (is.list(x)) purrr::keep(x, ~ is.list(.x) || is.data.frame(.x)) else list()
    c(list(x), unwrap, children)
  }
  
  from_df_cols <- function(scope) df_to_pin_list(scope)
  
  from_known <- function(scope) {
    if (!is.list(scope) || is.data.frame(scope)) return(NULL)
    nms <- names(scope)
    if (is.null(nms)) return(NULL)
    keys <- c("eis_results","pins","eis","eisdata","results")
    hits <- which(tolower(nms) %in% keys)
    hits |>
      purrr::map(~ scope[[.x]]) |>
      purrr::map(~ if (is.data.frame(.x)) df_to_pin_list(.x)
                 else if (is.list(.x) && length(.x)) .x
                 else NULL) |>
      purrr::compact() |>
      purrr::pluck(1, .default = NULL)
  }
  
  from_top_level_pins <- function(scope) {
    if (!is.list(scope) || is.data.frame(scope)) return(NULL)
    nms <- names(scope)
    if (is.null(nms)) return(NULL)
    pins <- nms[is_pin_key(nms)]
    if (length(pins)) scope[pins] else NULL
  }
  
  extractors <- list(from_df_cols, from_known, from_top_level_pins)
  
  container <-
    get_scopes(sample_eis) |>
    purrr::map(\(scope)
               purrr::reduce(extractors, .init = NULL, .f = \(acc, ext) if (!is.null(acc)) acc else ext(scope))
    ) |>
    purrr::compact() |>
    purrr::pluck(1, .default = NULL)
  
  if (is.null(container) || !length(container)) return(empty)
  container <- ensure_names(container)
  
  normalized <- purrr::imap(container, ~ {
    pin <- if (is.list(.x)) .x else as.list(.x)
    normalize_pin_entry(pin_obj = pin, key = .y)
  })
  
  list(keys = names(normalized), map = normalized)
}

combine_eis_meta <- function(meta_json, eis_json) {
  meta_samples <- resolve_sample_map(meta_json)
  eis_samples  <- resolve_sample_map(eis_json)
  sample_ids   <- union(names(meta_samples), names(eis_samples))
  
  samples_merged <- purrr::map(purrr::set_names(sample_ids), function(sid) {
    sm <- meta_samples[[sid]] %||% list()
    se <- eis_samples[[sid]] %||% list()
    
    # Prefer explicit EIS structure when present
    if (!is.null(se) && length(se) > 0) {
      pin_map <- build_pin_map(se)
      if (length(pin_map$map) > 0) sm$eis_results <- pin_map$map
    }
    
    # If no eis_results yet, see if the meta side itself embeds EIS that we can normalize
    if (is.null(sm$eis_results) || !length(sm$eis_results)) {
      pin_map <- build_pin_map(sm)
      if (length(pin_map$map) > 0) sm$eis_results <- pin_map$map
    }
    
    sm
  })
  
  list(samples = samples_merged)
}

# =============================================================================
# DATA EXTRACTION
# =============================================================================

to_num_vec <- function(x) {
  if (is.null(x)) return(numeric())
  as.numeric(unlist(x, recursive = TRUE, use.names = FALSE))
}

idx_of_first <- function(vars, name) {
  if (is.null(vars)) return(NA_integer_)
  v <- vars
  if (is.list(v)) v <- unlist(v, recursive = TRUE, use.names = FALSE)
  v <- as.character(v)
  match(name, v, nomatch = NA_integer_)
}

pull_var <- function(values, vars, name, synonyms = NULL) {
  if (is.null(values)) return(numeric())
  candidates <- unique(c(name, synonyms %||% character()))
  
  # named list
  if (is.list(values) && !is.null(names(values))) {
    nm <- names(values)
    hit <- candidates[candidates %in% nm]
    if (length(hit)) return(to_num_vec(values[[hit[1]]]))
  }
  
  # data.frame
  if (is.data.frame(values)) {
    nm <- colnames(values)
    hit <- candidates[candidates %in% nm]
    if (length(hit)) return(to_num_vec(values[[hit[1]]]))
  }
  
  # flat numeric vector that can be reshaped by vars
  if (is.atomic(values) && is.numeric(values) && !is.null(vars)) {
    vv <- vars
    if (is.list(vv)) vv <- unlist(vv, recursive = TRUE, use.names = FALSE)
    vv <- as.character(vv)
    k <- length(vv)
    if (k > 0 && length(values) %% k == 0) {
      mat <- matrix(values, ncol = k, byrow = TRUE)
      colnames(mat) <- vv
      df <- as.data.frame(mat, check.names = FALSE)
      if (name %in% names(df)) return(to_num_vec(df[[name]]))
      hit <- candidates[candidates %in% names(df)]
      if (length(hit)) return(to_num_vec(df[[hit[1]]]))
    }
  }
  
  # positional list-of-arrays
  if (is.list(values)) {
    vv <- vars
    if (is.list(vv)) vv <- unlist(vv, recursive = TRUE, use.names = FALSE)
    vv <- as.character(vv)
    hit_name <- candidates[candidates %in% vv]
    idx <- if (length(hit_name)) match(hit_name[1], vv) else idx_of_first(vv, name)
    if (!is.na(idx) && idx >= 1 && idx <= length(values)) {
      return(to_num_vec(values[[idx]]))
    }
  }
  
  numeric()
}

merge_meta_eis <- function(meta_json, eis_json) {
  meta_samples <- resolve_sample_map(meta_json)
  eis_samples  <- resolve_sample_map(eis_json)
  sample_ids   <- union(names(meta_samples), names(eis_samples))
  
  samples_merged <- purrr::map(setNames(sample_ids, sample_ids), function(sid) {
    sm <- meta_samples[[sid]] %||% list()
    se <- eis_samples[[sid]] %||% list()
    
    # Build pin map from the EIS payload for this sample
    pin_map <- build_pin_map(se)
    if (length(pin_map$map) > 0) {
      sm$eis_results <- pin_map$map
    } else if (is.list(se$eis_results) && length(se$eis_results) > 0) {
      # If already normalized in payload, keep as is
      sm$eis_results <- se$eis_results
    }
    
    sm
  })
  
  list(samples = samples_merged)
}

extract_cycle_rows <- function(sample_id, sample_meta, pin_key, pin_entry, cycle_obj) {
  if (!is.list(cycle_obj)) return(tibble())
  
  # Containers
  v_raw <- cycle_obj$data_array_variables %||% cycle_obj$data_array_variable
  a_raw <- cycle_obj$data_array_values   %||% cycle_obj$data_array_value
  
  v_fit <- cycle_obj$fit_array_variables %||% cycle_obj$all_variables
  a_fit <- cycle_obj$fit_array_values    %||% cycle_obj$all_values
  
  syn <- list(
    freq  = c("freq", "frequency", "f"),
    Re    = c("Re", "Z_real", "real", "realZ"),
    Im    = c("Im", "Z_imag", "imag", "imagZ"),
    Z     = c("Z", "mag", "|Z|", "absZ", "Z_mag", "Magnitude"),
    Phase = c("Phase", "phase", "phi", "angle", "theta", "Phase_deg", "phase_deg"),
    
    Fit_Re  = c("Z_pred_real", "Re_fit", "Re_pred", "Zfit_real"),
    Fit_Im  = c("Z_pred_imag", "Im_fit", "Im_pred", "Zfit_imag"),
    Meas_Re = c("Z_measured_real", "Re_meas", "Re_measured"),
    Meas_Im = c("Z_measured_imag", "Im_meas", "Im_measured")
  )
  
  # Pull raw arrays
  freq   <- pull_var(a_raw, v_raw, "freq",  syn$freq)
  Raw_Re <- pull_var(a_raw, v_raw, "Re",    syn$Re)
  Raw_Im <- pull_var(a_raw, v_raw, "Im",    syn$Im)
  Raw_Z  <- pull_var(a_raw, v_raw, "Z",     syn$Z)
  Raw_Ph <- pull_var(a_raw, v_raw, "Phase", syn$Phase)
  
  if (length(Raw_Z) == 0 && length(Raw_Re) > 0 && length(Raw_Im) > 0) {
    Raw_Z <- sqrt(Raw_Re^2 + Raw_Im^2)
  }
  if (length(Raw_Ph) == 0 && length(Raw_Re) > 0 && length(Raw_Im) > 0) {
    Raw_Ph <- atan2(Raw_Im, Raw_Re) * 180 / pi
  }
  
  raw_lens <- c(freq = length(freq), Re = length(Raw_Re), Im = length(Raw_Im), Z = length(Raw_Z), Ph = length(Raw_Ph))
  raw_present <- raw_lens[raw_lens > 0]
  n_raw <- if (length(raw_present)) min(raw_present) else 0
  
  raw_df <- if (n_raw > 0) {
    tibble(
      freq   = freq[seq_len(n_raw)],
      Raw_Re = if (length(Raw_Re)) Raw_Re[seq_len(n_raw)] else NA_real_,
      Raw_Im = if (length(Raw_Im)) Raw_Im[seq_len(n_raw)] else NA_real_,
      Raw_Z  = if (length(Raw_Z))  Raw_Z[seq_len(n_raw)]  else NA_real_,
      Raw_Ph = if (length(Raw_Ph)) Raw_Ph[seq_len(n_raw)] else NA_real_,
      type   = "Raw"
    )
  } else NULL
  
  # Pull fit arrays
  Fit_Re  <- pull_var(a_fit, v_fit, "Z_pred_real",     syn$Fit_Re)
  Fit_Im  <- pull_var(a_fit, v_fit, "Z_pred_imag",     syn$Fit_Im)
  Meas_Re <- pull_var(a_fit, v_fit, "Z_measured_real", syn$Meas_Re)
  Meas_Im <- pull_var(a_fit, v_fit, "Z_measured_imag", syn$Meas_Im)
  f_freq  <- pull_var(a_fit, v_fit, "freq",            syn$freq)
  
  Fit_Z   <- if (length(Fit_Re)  && length(Fit_Im))  sqrt(Fit_Re^2 + Fit_Im^2) else numeric()
  Fit_Ph  <- if (length(Fit_Re)  && length(Fit_Im))  atan2(Fit_Im, Fit_Re) * 180 / pi else numeric()
  Meas_Z  <- if (length(Meas_Re) && length(Meas_Im)) sqrt(Meas_Re^2 + Meas_Im^2) else numeric()
  Meas_Ph <- if (length(Meas_Re) && length(Meas_Im)) atan2(Meas_Im, Meas_Re) * 180 / pi else numeric()
  
  fit_lens <- c(freq = length(f_freq), Re = length(Fit_Re), Im = length(Fit_Im))
  fit_present <- fit_lens[fit_lens > 0]
  n_fit <- if (length(fit_present)) min(fit_present) else 0
  
  fit_df <- if (n_fit > 0) {
    tibble(
      freq    = f_freq[seq_len(n_fit)],
      Fit_Re  = Fit_Re[seq_len(n_fit)],
      Fit_Im  = Fit_Im[seq_len(n_fit)],
      Fit_Z   = if (length(Fit_Z))  Fit_Z[seq_len(n_fit)]  else NA_real_,
      Fit_Ph  = if (length(Fit_Ph)) Fit_Ph[seq_len(n_fit)] else NA_real_,
      Meas_Re = if (length(Meas_Re) >= n_fit) Meas_Re[seq_len(n_fit)] else NA_real_,
      Meas_Im = if (length(Meas_Im) >= n_fit) Meas_Im[seq_len(n_fit)] else NA_real_,
      Meas_Z  = if (length(Meas_Z)  >= n_fit) Meas_Z[seq_len(n_fit)]  else NA_real_,
      Meas_Ph = if (length(Meas_Ph) >= n_fit) Meas_Ph[seq_len(n_fit)] else NA_real_,
      type    = "Fit"
    )
  } else NULL
  
  # Fallback: synthesize Raw from measured arrays if raw was absent
  if ((is.null(raw_df) || nrow(raw_df) == 0) &&
      n_fit > 0 &&
      length(Meas_Re) >= n_fit &&
      length(Meas_Im) >= n_fit &&
      length(f_freq)  >= n_fit) {
    
    n_use <- min(length(f_freq), length(Meas_Re), length(Meas_Im))
    Raw_Re_f <- Meas_Re[seq_len(n_use)]
    Raw_Im_f <- Meas_Im[seq_len(n_use)]
    Raw_Z_f  <- if (length(Meas_Z)  >= n_use) Meas_Z[seq_len(n_use)]  else sqrt(Raw_Re_f^2 + Raw_Im_f^2)
    Raw_Ph_f <- if (length(Meas_Ph) >= n_use) Meas_Ph[seq_len(n_use)] else atan2(Raw_Im_f, Raw_Re_f) * 180 / pi
    
    raw_df <- tibble(
      freq   = f_freq[seq_len(n_use)],
      Raw_Re = Raw_Re_f,
      Raw_Im = Raw_Im_f,
      Raw_Z  = Raw_Z_f,
      Raw_Ph = Raw_Ph_f,
      type   = "Raw"
    )
  }
  
  # Sample metadata
  get_meta <- function(path) {
    tryCatch({
      if (is.list(sample_meta)) {
        if (length(path) == 1) return(scalar1(sample_meta[[path[1]]]))
        if (length(path) == 2) return(scalar1(sample_meta[[path[1]]][[path[2]]]))
      }
      NA
    }, error = function(e) NA)
  }
  
  comp <- list(
    cast_type = get_meta(c("print_quality", "cast_type")),
    date_casted = get_meta(c("print_quality", "date_casted")),
    area_cm2 = get_meta(c("print_quality", "area_cm2")),
    fcomp_mat1 = get_meta("fcomp_mat1"),
    fcomp_mat2 = get_meta("fcomp_mat2"),
    fcomp_additive = get_meta("fcomp_additive"),
    fcomp_formulation = get_meta("fcomp_formulation")
  )
  
  add_meta <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    nrows <- nrow(df)
    
    # Attach scalar metadata
    df$sample_id   <- sample_id
    df$pin         <- pin_entry$pin %||% NA_integer_
    df$file_name   <- pin_entry$file_name %||% pin_key
    df$cycle_index <- cycle_obj$cycle %||% NA_integer_
    
    # Attach component metadata, repeating to match nrows
    for (nm in names(comp)) {
      df[[nm]] <- rep_len(comp[[nm]], nrows)
    }
    
    df
  }
  
  dplyr::bind_rows(add_meta(raw_df), add_meta(fit_df))
}

tidy_eis_data <- function(json_data) {
  samples <- json_data$samples %||% list()
  if (is.data.frame(samples)) {
    if (nrow(samples) == 0) return(tibble::tibble())
    samples <- split(samples, seq_len(nrow(samples)))
  }
  if (!is.list(samples) || length(samples) == 0) return(tibble::tibble())
  if (is.null(names(samples))) names(samples) <- paste0("sample_", seq_along(samples))
  
  rows <- purrr::imap(samples, function(smp, sid) {
    # Use normalized pins if present, or try to build from the sample itself
    eis <- smp$eis_results %||% build_pin_map(smp)$map %||% list()
    if (!length(eis)) return(tibble::tibble())
    
    purrr::imap_dfr(eis, function(pe, pk) {
      cycles <- pe$cycle_data %||% list()
      if (!length(cycles)) return(tibble::tibble())
      
      purrr::imap_dfr(cycles, function(cyc, ci) {
        extract_cycle_rows(
          sample_id   = sid,
          sample_meta = smp,
          pin_key     = pk,
          pin_entry   = pe,
          cycle_obj   = cyc
        )
      })
    })
  })
  
  dplyr::bind_rows(rows) %>%
    # Stabilize column types for downstream joins
    dplyr::mutate(
      sample_id   = as.character(sample_id),
      pin         = as.integer(pin),
      file_name   = as.character(file_name),
      cycle_index = as.integer(cycle_index),
      type        = as.character(type)
    )
}

# =============================================================================
# PLOTTING
# =============================================================================
label_for <- function(dsub) paste0("C", dsub$cycle_index[1], " | P", dsub$pin[1])

hover_for <- function(dsub, type = "raw") {
  base <- paste0("Sample: ", dsub$sample_id, "<br>Pin: ", dsub$pin,
                 "<br>File: ", dsub$file_name, "<br>Cycle: ", dsub$cycle_index,
                 "<br>freq: ", signif(dsub$freq, 6))
  if (type == "raw") {
    paste0(base, "<br>Re: ", signif(dsub$Raw_Re, 6), "<br>Im: ", signif(dsub$Raw_Im, 6))
  } else {
    paste0(base, "<br>Re_fit: ", signif(dsub$Fit_Re, 6), "<br>Im_fit: ", signif(dsub$Fit_Im, 6))
  }
}

get_palette <- function(palette_mode, n) {
  pal_func <- switch(palette_mode,
                     "Viridis" = viridis, "Plasma" = plasma,
                     "Magma"   = magma,   "Cividis" = cividis, viridis)
  pal_func(n)
}

build_nyquist_plot <- function(df, palette_mode) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  combos <- df %>% distinct(pin, cycle_index) %>% arrange(pin, cycle_index)
  colors <- get_palette(palette_mode, nrow(combos))
  p <- plot_ly()
  
  purrr::iwalk(seq_len(nrow(combos)), function(i, idx) {
    combo <- combos[i, ]
    dsub_raw <- df %>% filter(type == "Raw", pin == combo$pin, cycle_index == combo$cycle_index)
    dsub_fit <- df %>% filter(type == "Fit", pin == combo$pin, cycle_index == combo$cycle_index)
    lbl <- label_for(dsub_raw %||% dsub_fit)
    
    if (nrow(dsub_raw) > 0) {
      p <<- p %>% add_trace(
        x = dsub_raw$Raw_Re, y = dsub_raw$Raw_Im, name = lbl,
        type = 'scatter', mode = 'markers', alpha = 0.5,
        marker = list(color = colors[i], size = 6),
        text = hover_for(dsub_raw, "raw"), hoverinfo = "text",
        legendgroup = lbl, showlegend = TRUE
      )
    }
    if (nrow(dsub_fit) > 0) {
      p <<- p %>% add_trace(
        x = dsub_fit$Fit_Re, y = -dsub_fit$Fit_Im, name = lbl,
        type = 'scatter', mode = 'lines',
        line = list(color = colors[i], width = 2, dash = "dash"),
        text = hover_for(dsub_fit, "fit"), hoverinfo = "text",
        legendgroup = lbl, showlegend = nrow(dsub_raw) == 0
      )
    }
  })
  
  p %>% layout(title = "Nyquist Plot", xaxis = list(title = "Z' (Ω)"),
               yaxis = list(title = "-Z'' (Ω)"))
}

build_bode_mag_plot <- function(df, palette_mode) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  combos <- df %>% distinct(pin, cycle_index) %>% arrange(pin, cycle_index)
  colors <- get_palette(palette_mode, nrow(combos))
  p <- plot_ly()
  
  purrr::iwalk(seq_len(nrow(combos)), function(i, idx) {
    combo <- combos[i, ]
    dsub_raw <- df %>% filter(type == "Raw", pin == combo$pin, cycle_index == combo$cycle_index)
    dsub_fit <- df %>% filter(type == "Fit", pin == combo$pin, cycle_index == combo$cycle_index)
    lbl <- label_for(dsub_raw %||% dsub_fit)
    
    if (nrow(dsub_raw) > 0) {
      p <<- p %>% add_trace(
        x = dsub_raw$freq, y = sqrt(dsub_raw$Raw_Re^2 + dsub_raw$Raw_Im^2),
        name = lbl, type = 'scatter', mode = 'markers', alpha = 0.5,
        marker = list(color = colors[i], size = 6),
        text = hover_for(dsub_raw, "raw"), hoverinfo = "text",
        legendgroup = lbl, showlegend = TRUE
      )
    }
    if (nrow(dsub_fit) > 0) {
      p <<- p %>% add_trace(
        x = dsub_fit$freq, y = sqrt(dsub_fit$Fit_Re^2 + dsub_fit$Fit_Im^2),
        name = lbl, type = 'scatter', mode = 'lines',
        line = list(color = colors[i], width = 2, dash = "dash"),
        text = hover_for(dsub_fit, "fit"), hoverinfo = "text",
        legendgroup = lbl, showlegend = nrow(dsub_raw) == 0
      )
    }
  })
  
  p %>% layout(title = "Bode Magnitude",
               xaxis = list(title = "Frequency (Hz)", type = "log", exponentformat = "e"),
               yaxis = list(title = "|Z| (Ω)"))
}

build_bode_phase_plot <- function(df, palette_mode) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  combos <- df %>% distinct(pin, cycle_index) %>% arrange(pin, cycle_index)
  colors <- get_palette(palette_mode, nrow(combos))
  p <- plot_ly()
  
  purrr::iwalk(seq_len(nrow(combos)), function(i, idx) {
    combo <- combos[i, ]
    dsub_raw <- df %>% filter(type == "Raw", pin == combo$pin, cycle_index == combo$cycle_index)
    dsub_fit <- df %>% filter(type == "Fit", pin == combo$pin, cycle_index == combo$cycle_index)
    lbl <- label_for(dsub_raw %||% dsub_fit)
    
    if (nrow(dsub_raw) > 0) {
      p <<- p %>% add_trace(
        x = dsub_raw$freq, y = atan2(dsub_raw$Raw_Im, dsub_raw$Raw_Re) * 180 / pi,
        name = lbl, type = 'scatter', mode = 'markers', alpha = 0.5,
        marker = list(color = colors[i], size = 6),
        text = hover_for(dsub_raw, "raw"), hoverinfo = "text",
        legendgroup = lbl, showlegend = TRUE
      )
    }
    if (nrow(dsub_fit) > 0) {
      p <<- p %>% add_trace(
        x = dsub_fit$freq, y = -atan2(dsub_fit$Fit_Im, dsub_fit$Fit_Re) * 180 / pi,
        name = lbl, type = 'scatter', mode = 'lines',
        line = list(color = colors[i], width = 2, dash = "dash"),
        text = hover_for(dsub_fit, "fit"), hoverinfo = "text",
        legendgroup = lbl, showlegend = nrow(dsub_raw) == 0
      )
    }
  })
  
  p %>% layout(title = "Bode Phase",
               xaxis = list(title = "Frequency (Hz)", type = "log", exponentformat = "e"),
               yaxis = list(title = "Phase (deg)"))
}

build_linkk_plot <- function(df, palette_mode) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  fit <- df %>% filter(type == "Fit")
  if (nrow(fit) == 0) return(NULL)
  
  has_measured <- !all(is.na(fit$Meas_Re)) && !all(is.na(fit$Meas_Im))
  if (!has_measured) return(NULL)
  
  combos <- fit %>% distinct(pin, cycle_index) %>% arrange(pin, cycle_index)
  colors <- get_palette(palette_mode, nrow(combos))
  p <- plot_ly()
  
  purrr::iwalk(seq_len(nrow(combos)), function(i, idx) {
    combo <- combos[i, ]
    dsub <- fit %>% filter(pin == combo$pin, cycle_index == combo$cycle_index)
    if (nrow(dsub) == 0) return()
    
    lbl <- label_for(dsub)
    re_res <- (dsub$Raw_Re - dsub$Fit_Re)
    im_res  <- dsub$Raw_Im - dsub$Fit_Im
    
    p <<- p %>% add_trace(
      x = dsub$freq, y = re_res, name = lbl,
      type = 'scatter', mode = 'lines',
      marker = list(color = colors[i], size = 6),
      text = paste0("freq: ", signif(dsub$freq, 6), "<br>real residual: ", signif(re_res, 6)),
      hoverinfo = "text", legendgroup = lbl, showlegend = TRUE
    ) %>% add_trace(
      x = dsub$freq, y = im_res, name = lbl,
      type = 'scatter', mode = 'lines',
      line = list(color = colors[i], width = 2, dash = "dash"),
      text = paste0("freq: ", signif(dsub$freq, 6), "<br>imaginary residual: ", signif(im_res, 6)),
      hoverinfo = "text", legendgroup = lbl, showlegend = TRUE
    )
  })
  
  p %>% layout(title = "Lin-KK Residuals",
               xaxis = list(title = "Frequency (Hz)", type = "log", exponentformat = "e"),
               yaxis = list(title = "Residuals"))
}

# =============================================================================
# FIT METRICS
# =============================================================================
extract_fit_metrics <- function(cycles) {
  if (length(cycles) == 0) return(NULL)
  
  all_keys <- unique(unlist(purrr::map(cycles, names)))
  exclude_keys <- c("data_array_values", "data_array_variables",
                    "fit_array_values", "fit_array_variables",
                    "fit_value_arrays", "all_values", "all_variables", "fitting")
  metric_keys <- setdiff(all_keys, exclude_keys)
  main_metrics <- metric_keys[!endsWith(metric_keys, "_stDev")]
  
  purrr::map(main_metrics, function(mk) {
    vals     <- purrr::map(cycles, ~scalar1(.x[[mk]]))
    vals_num <- suppressWarnings(as.numeric(vals))
    
    stdevs     <- purrr::map(cycles, ~scalar1(.x[[paste0(mk, "_stDev")]]))
    stdevs_num <- suppressWarnings(as.numeric(stdevs))
    
    list(
      name  = mk,
      value = if (length(vals_num) > 1 && is.numeric(vals_num)) mean(vals_num, na.rm = TRUE) else vals_num[[1]],
      stdev = if (length(stdevs_num) > 1 && is.numeric(stdevs_num)) mean(stdevs_num, na.rm = TRUE) else stdevs_num[[1]]
    )
  }) %>% purrr::keep(~!is.na(.x$value) && .x$value != "NA")
}

build_fit_metrics_table <- function(metrics_list) {
  if (is.null(metrics_list) || length(metrics_list) == 0) {
    return(gt::gt(data.frame(Message = "No fit metrics found")))
  }
  
  all_metric_names <- unique(unlist(purrr::map(metrics_list, ~purrr::map_chr(.x$metrics, "name"))))
  df <- tibble::tibble(Metric = all_metric_names)
  
  for (item in metrics_list) {
    values <- purrr::map_chr(all_metric_names, function(mn) {
      m <- purrr::detect(item$metrics, ~.x$name == mn)
      if (is.null(m)) return(NA_character_)
      if (!is.na(m$stdev) && is.numeric(m$value) && is.numeric(m$stdev)) {
        sprintf("%.4g ± %.4g", m$value, m$stdev)
      } else if (!is.na(m$value) && is.numeric(m$value)) {
        sprintf("%.4g", m$value)
      } else {
        as.character(m$value)
      }
    })
    df[[item$label]] <- values
  }
  
  df %>% gt() %>%
    tab_style(style = cell_text(size = px(10)), locations = cells_body()) %>%
    tab_style(style = cell_text(size = px(11), weight = "bold"), locations = cells_column_labels()) %>%
    tab_options(table.font.size = px(10))
}

# =============================================================================
# UI
# =============================================================================
build_ui <- function(plot_df, json_data) {
  sample_names <- sort(unique(names(json_data$samples %||% list())))
  if (length(sample_names) == 0) sample_names <- "No samples"
  
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML("
      .plot-col { height: 45vh; }
      .plot-col .html-widget { height: 100% !important; }
      .combo-panel .form-group { margin-bottom: 4px; }
      .combo-panel .shiny-input-container { margin-bottom: 4px !important; }
      .combo-panel .control-label { margin-bottom: 2px; font-weight: 500; font-size: 11px; }
      .combo-panel input[type='text'] { height: 26px; padding: 2px 6px; font-size: 11px; }
      .combo-panel .checkbox { margin-top: 0; margin-bottom: 2px; font-size: 11px; }
      .combo-row {
        display: grid; grid-template-columns: 100px 50px 1fr; gap: 8px;
        align-items: center; margin-bottom: 6px; padding: 4px; border-bottom: 1px solid #eee;
      }
    "))),
    tags$h3("EIS Viewer"),
    
    fluidRow(
      column(3, wellPanel(
        selectInput(
          "sample_id", "Sample",
          choices = sample_names,
          selected = (intersect(sample_names, sort(unique(plot_df$sample_id)))[1] %||% sample_names[1])
        ),
        selectInput("palette_mode", "Color palette",
                    choices = c("Viridis", "Plasma", "Magma", "Cividis"), selected = "Viridis"),
        actionButton("save_progress", "Save Progress", class = "btn-primary", style = "width: 100%;"),
        actionButton("reload_from_env", "Reload from env", style = "width: 100%; margin-top: 6px;")
      )),
      column(5, wellPanel(class = "combo-panel",
                          tags$div(style = "font-weight:600; display:flex; gap:16px; margin-bottom:8px;",
                                   tags$span("Plot"), tags$span("Flag"), tags$span("Notes")),
                          tags$div(style = "max-height: 40vh; overflow-y: auto;", uiOutput("combo_matrix_ui"))
      )),
      column(4, wellPanel(h4("Fit Metrics"), gt_output("metrics_gt")))
    ),
    
    fluidRow(
      column(6, div(class = "plot-col", plotlyOutput("nyquist", height = "100%"))),
      column(6, div(class = "plot-col", plotlyOutput("linkk", height = "100%")))
    ),
    fluidRow(
      column(6, div(class = "plot-col", plotlyOutput("bode_mag", height = "100%"))),
      column(6, div(class = "plot-col", plotlyOutput("bode_phase", height = "100%")))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================
build_server <- function(plot_df, json_data) {
  if (is.null(plot_df) || !"sample_id" %in% colnames(plot_df)) {
    stop("plot_df is invalid or missing the 'sample_id' column")
  }
  
  function(input, output, session) {
    rv_combined <- reactiveVal(json_data)
    rv_fitdf    <- reactiveVal(plot_df)
    rv_flags    <- reactiveVal(list())
    rv_notes    <- reactiveVal(list())
    
    # Defensive combos listing
    combos_df <- reactive({
      req(input$sample_id)
      if (identical(input$sample_id, "No samples")) {
        return(tibble(pin = integer(), file_name = character(), cycle_index = integer(),
                      id = character(), label = character())[0, ])
      }
      df <- rv_fitdf() %>% dplyr::filter(sample_id == input$sample_id)
      
      needed <- c("pin","file_name","cycle_index")
      if (!all(needed %in% names(df))) {
        return(tibble(pin = integer(), file_name = character(), cycle_index = integer(),
                      id = character(), label = character())[0, ])
      }
      
      df %>%
        dplyr::distinct(pin, file_name, cycle_index) %>%
        dplyr::arrange(pin, cycle_index, file_name) %>%
        dplyr::mutate(
          id    = paste(pin, file_name, cycle_index, sep = "||"),
          label = paste0("C", cycle_index, " | P", pin)
        )
    })
    
    # Load flags/notes from JSON when sample changes
    observe({
      req(input$sample_id)
      smp <- rv_combined()$samples[[input$sample_id]]
      
      # Short circuit if no EIS
      if (is.null(smp$eis_results) || !length(smp$eis_results)) {
        rv_flags(list())
        rv_notes(list())
        return()
      }
      
      typed_empty <- tibble(id = character(), flag = logical(), note = character())
      
      flags_notes <- purrr::imap_dfr(smp$eis_results, function(pe, pk) {
        cycles <- pe$cycle_data %||% list()
        if (!length(cycles)) return(typed_empty)
        purrr::imap_dfr(cycles, function(cyc, ci) {
          if (!is.list(cyc)) return(typed_empty)
          tibble(
            id   = paste(pe$pin %||% NA_integer_, pe$file_name %||% "", cyc$cycle %||% NA_integer_, sep = "||"),
            flag = isTRUE(cyc$eis_flag),
            note = cyc$eis_note %||% ""
          )
        })
      })
      
      if (nrow(flags_notes) == 0) {
        rv_flags(list())
        rv_notes(list())
      } else {
        rv_flags(setNames(as.list(flags_notes$flag), flags_notes$id))
        rv_notes(setNames(as.list(flags_notes$note), flags_notes$id))
      }
    })
    
    
    output$combo_matrix_ui <- renderUI({
      df    <- combos_df()
      flags <- rv_flags()
      notes <- rv_notes()
      
      rows <- purrr::imap(df$id, function(combo_id, i) {
        tags$div(class = "combo-row",
                 checkboxInput(paste0("plot_", combo_id), df$label[i], value = i <= 3),
                 checkboxInput(paste0("flag_", combo_id), NULL, value = isTRUE(flags[[combo_id]])),
                 textInput(paste0("note_", combo_id), NULL, value = notes[[combo_id]] %||% "", placeholder = "Notes...")
        )
      })
      do.call(tagList, rows)
    })
    
    # Track flag changes
    observe({
      df <- combos_df()
      flags <- isolate(rv_flags())
      purrr::walk(df$id, function(id) {
        val <- input[[paste0("flag_", id)]]
        if (!is.null(val)) flags[[id]] <- val
      })
      rv_flags(flags)
    })
    
    # Track note changes
    observe({
      df <- combos_df()
      notes <- isolate(rv_notes())
      purrr::walk(df$id, function(id) {
        val <- input[[paste0("note_", id)]]
        if (!is.null(val)) notes[[id]] <- val
      })
      rv_notes(notes)
    })
    
    parsed_combos <- reactive({
      df <- combos_df()
      selected <- purrr::keep(df$id, ~isTRUE(input[[paste0("plot_", .x)]]))
      if (length(selected) == 0) {
        return(tibble(pin = integer(), file_name = character(), cycle_index = integer())[0, ])
      }
      pieces <- strsplit(selected, "\\|\\|")
      tibble(
        pin         = as.integer(purrr::map_chr(pieces, 1)),
        file_name   = purrr::map_chr(pieces, 2),
        cycle_index = as.integer(purrr::map_chr(pieces, 3))
      )
    })
    
    filtered_df <- reactive({
      req(input$sample_id)
      sel <- parsed_combos()
      if (nrow(sel) == 0) return(rv_fitdf()[0, ])
      rv_fitdf() %>%
        dplyr::filter(sample_id == input$sample_id) %>%
        dplyr::inner_join(sel, by = c("pin", "file_name", "cycle_index"))
    })
    
    # Plots
    output$nyquist    <- renderPlotly(build_nyquist_plot(filtered_df(), input$palette_mode))
    output$bode_mag   <- renderPlotly(build_bode_mag_plot(filtered_df(), input$palette_mode))
    output$bode_phase <- renderPlotly(build_bode_phase_plot(filtered_df(), input$palette_mode))
    output$linkk      <- renderPlotly(build_linkk_plot(filtered_df(), input$palette_mode))
    
    # Metrics
    output$metrics_gt <- render_gt({
      req(input$sample_id)
      smp <- rv_combined()$samples[[input$sample_id]]
      if (is.null(smp$eis_results)) return(gt(data.frame(Message = "No EIS")))
      sel <- parsed_combos()
      if (nrow(sel) == 0) return(gt(data.frame(Message = "No selection")))
      metrics_list <- purrr::pmap(sel, function(pin, file_name, cycle_index) {
        pe <- purrr::detect(smp$eis_results, ~(.x$pin %||% NA) == pin && (.x$file_name %||% "") == file_name)
        if (is.null(pe)) return(NULL)
        cycles <- purrr::keep(pe$cycle_data %||% list(), ~.x$cycle == cycle_index)
        if (length(cycles) == 0) return(NULL)
        list(label = sprintf("C%d | P%d", cycle_index, pin), metrics = extract_fit_metrics(cycles))
      }) %>% purrr::compact()
      build_fit_metrics_table(metrics_list)
    })
    
    # Save progress into in-memory object
    observeEvent(input$save_progress, {
      flags    <- rv_flags()
      notes    <- rv_notes()
      combined <- rv_combined()
      
      purrr::iwalk(combined$samples, function(smp, sid) {
        if (is.null(smp$eis_results)) return()
        purrr::iwalk(smp$eis_results, function(pe, pk) {
          purrr::iwalk(pe$cycle_data %||% list(), function(cyc, ci) {
            id <- paste(pe$pin, pe$file_name, cyc$cycle, sep = "||")
            combined$samples[[sid]]$eis_results[[pk]]$cycle_data[[ci]]$eis_flag <<- flags[[id]] %||% FALSE
            combined$samples[[sid]]$eis_results[[pk]]$cycle_data[[ci]]$eis_note <<- notes[[id]] %||% ""
          })
        })
      })
      
      rv_combined(combined)
      assign("combined_eis_data", combined, envir = .GlobalEnv)
      showNotification("Progress saved", type = "message")
    })
    
    observeEvent(input$reload_from_env, {
      if (exists("combined_eis_data", envir = .GlobalEnv)) {
        new_combined <- get("combined_eis_data", envir = .GlobalEnv)
        rv_combined(new_combined)
        rv_fitdf(tidy_eis_data(new_combined))
        updateSelectInput(session, "sample_id", choices = sort(unique(names(new_combined$samples))))
        showNotification("Reloaded", type = "message")
      } else {
        showNotification("combined_eis_data not found", type = "warning")
      }
    })
  }
}

# =============================================================================
# RUNNERS
# =============================================================================
# 1) Preferred: run the app from in-memory objects you loaded earlier
run_eis_app <- function(json_data, plot_df) {
  shinyApp(build_ui(plot_df, json_data), build_server(plot_df, json_data))
}

# 2) Convenience wrapper that loads from disk if you still want a single call
run_eis_viewer <- function(meta_path = NA_character_, eis_path = NA_character_) {
  if (is.na(meta_path)) meta_path <- Sys.getenv("META_JSON", "")
  if (is.na(eis_path))  eis_path  <- Sys.getenv("EIS_JSON", "")
  
  meta_json <- if (nzchar(meta_path)) load_meta_json(meta_path) else list(samples = list())
  eis_json  <- if (nzchar(eis_path))  load_eis_json(eis_path)  else list()
  
  json_data <- combine_eis_meta(meta_json, eis_json)
  plot_df   <- tidy_eis_data(json_data)
  
  shinyApp(build_ui(plot_df, json_data), build_server(plot_df, json_data))
}

# =============================================================================
# Example usage for separated loading
# =============================================================================
meta_json <- load_meta_json("~/Git/SPOC_code/data/SPOC_battery.json")
eis_json  <- load_eis_json("~/Git/SPOC_code/data/eis_autoprocess.json")
json_data <- combine_eis_meta(meta_json, eis_json)
plot_df   <- tidy_eis_data(json_data)
run_eis_app(json_data, plot_df)
