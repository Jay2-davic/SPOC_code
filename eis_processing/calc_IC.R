#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'")
  if (!requireNamespace("dplyr", quietly = TRUE))   stop("Please install 'dplyr'")
  if (!requireNamespace("purrr", quietly = TRUE))   stop("Please install 'purrr'")
  if (!requireNamespace("tibble", quietly = TRUE))  stop("Please install 'tibble'")
  if (!requireNamespace("tidyr", quietly = TRUE))   stop("Please install 'tidyr'")
  if (!requireNamespace("optparse", quietly = TRUE))stop("Please install 'optparse'")
})

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(jsonlite)
library(optparse)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------- JSON sanitization ----------------

read_json_sanitized <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- sub("^\ufeff", "", txt)                         # Strip BOM
  txt <- gsub("\\bNaN\\b", "null", txt, perl = TRUE)     # Normalize special numbers
  txt <- gsub("\\bInfinity\\b", "null", txt, perl = TRUE)
  txt <- gsub("\\b-Infinity\\b", "null", txt, perl = TRUE)
  valid <- jsonlite::validate(txt)
  if (!isTRUE(valid)) stop("Invalid JSON after sanitization: ", valid)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# ---------------- Safe extract helpers ----------------

safe_extract_chr <- function(obj, path) {
  val <- purrr::pluck(obj, !!!path, .default = NA_character_)
  if (is.null(val) || length(val) == 0 || is.na(val)) return(NA_character_)
  if (is.list(val)) val <- unlist(val, use.names = FALSE)
  val <- as.character(val)
  if (length(val) == 0) return(NA_character_)
  val[1]
}

safe_extract_num <- function(obj, path) {
  val <- purrr::pluck(obj, !!!path, .default = NA_real_)
  if (is.list(val)) val <- unlist(val, use.names = FALSE)
  val <- suppressWarnings(as.numeric(val))
  if (length(val) == 0 || all(is.na(val))) return(NA_real_)
  val[1]
}

safe_mean_num_list <- function(obj, path) {
  val <- purrr::pluck(obj, !!!path, .default = NA_real_)
  if (is.null(val)) return(NA_real_)
  if (is.list(val)) val <- unlist(val, use.names = FALSE)
  val <- suppressWarnings(as.numeric(val))
  if (length(val) == 0 || all(is.na(val))) return(NA_real_)
  mean(val, na.rm = TRUE)
}

derive_print_type <- function(sample) {
  ct <- safe_extract_chr(sample, c("print_quality", "cast_type"))
  if (is.na(ct)) return(NA_character_)
  if (grepl("coin", tolower(ct))) "Coin cell" else "PCB print"
}

extract_spe_thickness_avg_cm <- function(sample) {
  avg <- safe_extract_num(sample, c("print_quality", "spe_thickness_avg"))
  if (!is.na(avg)) return(avg)
  safe_mean_num_list(sample, c("print_quality", "spe_thickness"))
}

extract_resistance_list <- function(sample) {
  val <- purrr::pluck(sample, "resistance", .default = NULL)
  if (is.null(val)) return(numeric(0))
  if (is.list(val)) val <- unlist(val, use.names = FALSE)
  v <- suppressWarnings(as.numeric(val))
  v <- v[!is.na(v)]
  if (length(v) == 0) numeric(0) else v
}

find_FittedValue_R1 <- function(x) {
  if (is.null(x)) return(NA_real_)
  candidates <- c("FittedValue.R1", "FittedValue_R1", "R1", "Rb", "R_b")
  for (k in candidates) {
    val <- x[[k]]
    if (!is.null(val)) {
      v <- suppressWarnings(as.numeric(if (is.list(val)) unlist(val, use.names = FALSE) else val))
      if (length(v) > 0 && !all(is.na(v))) return(v[1])
    }
  }
  flat <- tryCatch(unlist(x, recursive = TRUE, use.names = TRUE), error = function(e) NULL)
  if (is.null(flat)) return(NA_real_)
  nm <- names(flat)
  idx <- which(nm %in% candidates | grepl("FittedValue\\.R1$", nm))
  if (length(idx) > 0) {
    v <- suppressWarnings(as.numeric(flat[idx[1]]))
    return(v[1])
  }
  NA_real_
}

# ---------------- Readers ----------------

# Metadata table with minimal fields used for IC computations
read_metadata_json <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  samples <- j$samples
  if (is.null(samples)) stop("No 'samples' key found in metadata JSON: ", path)
  
  tibble(
    sample_id  = names(samples),
    sample_obj = unname(samples)
  ) %>%
    mutate(
      print_type              = purrr::map_chr(sample_obj, derive_print_type),
      area_cm2                = purrr::map_dbl(sample_obj, ~ safe_extract_num(.x, c("print_quality", "area_cm2"))),
      spe_thickness_avg       = purrr::map_dbl(sample_obj, extract_spe_thickness_avg_cm),
      ionic_conductivity_s_cm = purrr::map_dbl(sample_obj, ~ safe_extract_num(.x, c("ionic_conductivity_s_cm"))),
      resistance_list         = purrr::map(sample_obj, extract_resistance_list)
    )
}

# EIS fitted values long form
read_eis_long <- function(path) {
  j <- read_json_sanitized(path)
  eis_results <- j$eis_results
  if (is.null(eis_results) || length(eis_results) == 0) {
    stop("Could not find 'eis_results' entries in EIS JSON: ", path)
  }
  
  # New schema: sample_id -> pin_* -> cycle_*
  new_tbl <- tryCatch({
    purrr::imap_dfr(eis_results, function(sample_entry, sample_id) {
      purrr::imap_dfr(sample_entry, function(pin_entry, pin_key) {
        if (!is.list(pin_entry)) return(NULL)
        file_name <- pin_entry$file_name %||% pin_key
        cycle_names <- names(pin_entry)
        cycle_names <- cycle_names[grepl("^cycle_\\d+$", cycle_names)]
        if (length(cycle_names) == 0) return(NULL)
        purrr::map_dfr(cycle_names, function(cname) {
          cy <- pin_entry[[cname]]
          if (is.null(cy) || !is.list(cy)) return(NULL)
          tibble(
            sample_id        = sample_id,
            pin_id           = as.character(pin_key),
            file_name        = as.character(file_name),
            cycle_idx        = suppressWarnings(as.numeric(safe_extract_num(cy, c("cycle")))),
            fit_status       = safe_extract_chr(cy, c("fit_status")),
            `FittedValue.R1` = find_FittedValue_R1(cy)
          )
        })
      })
    })
  }, error = function(e) NULL)
  
  rows <- NULL
  if (!is.null(new_tbl) && nrow(new_tbl) > 0) {
    rows <- new_tbl %>% filter(is.na(fit_status) | fit_status == "ok")
  } else {
    # Legacy schema: sample_id -> file_name -> cycle_data[]
    rows <- purrr::imap_dfr(eis_results, function(sample, sample_id) {
      purrr::imap_dfr(sample, function(file_entry, file_name) {
        cycles <- file_entry$cycle_data
        if (is.null(cycles) || length(cycles) == 0) return(NULL)
        purrr::imap_dfr(cycles, function(cycle, cycle_idx) {
          tibble(
            sample_id        = sample_id,
            pin_id           = NA_character_,
            file_name        = as.character(file_name),
            cycle_idx        = suppressWarnings(as.numeric(cycle_idx)),
            fit_status       = NA_character_,
            `FittedValue.R1` = suppressWarnings(as.numeric(cycle$FittedValue.R1))
          )
        })
      })
    })
  }
  
  if (is.null(rows) || nrow(rows) == 0) {
    return(tibble(
      sample_id = character(), pin_id = character(), file_name = character(),
      cycle_idx = numeric(), `FittedValue.R1` = numeric()
    ))
  }
  
  rows %>%
    filter(!is.na(`FittedValue.R1`)) %>%
    arrange(sample_id, file_name, cycle_idx)
}

# ---------------- Calculation rows ----------------

compute_rows <- function(meta_df, eis_long) {
  meta_prep <- meta_df %>%
    mutate(
      resistance_list         = purrr::map(resistance_list, function(x) {
        if (is.null(x)) return(numeric(0))
        v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
        v[is.finite(v)]
      }),
      area_cm2                = suppressWarnings(as.numeric(area_cm2)),
      spe_thickness_avg       = suppressWarnings(as.numeric(spe_thickness_avg)/10), # thickness in mm convert to cm
      ionic_conductivity_s_cm = suppressWarnings(as.numeric(ionic_conductivity_s_cm))
    )
  
  # Fit-based rows (0.4/R1 and geom = 0 per your SSna_99 example)
  fits_long <- eis_long %>%
    select(sample_id, pin_id, file_name, cycle_idx, `FittedValue.R1`) %>%
    left_join(meta_prep %>% select(sample_id, print_type, area_cm2, spe_thickness_avg, ionic_conductivity_s_cm), by = "sample_id") %>%
    mutate(
      measurement_source = "fit_R1",
      `FittedValue.R1` = suppressWarnings(as.numeric(`FittedValue.R1`)),
      ionic_conductivity_final = ifelse(is.finite(`FittedValue.R1`), 0.4 / `FittedValue.R1`, NA_real_),
      # Geometry factor applied is zero for fit rows in your SSna_99
      ionic_conductivity_final_geom_factor_applied = ifelse(is.finite(`FittedValue.R1`), 0, NA_real_),
      resistance = NA_real_,
      res_index  = NA_integer_
    ) %>%
    select(
      sample_id, measurement_source, res_index, pin_id, file_name, cycle_idx,
      resistance, `FittedValue.R1`,
      ionic_conductivity_final, ionic_conductivity_final_geom_factor_applied,
      print_type, area_cm2, spe_thickness_avg
    )
  
  # Resistance-based rows: coin cells use L/(R*A), keep same for PCB if you carry resistances
  res_rows <- meta_prep %>%
    filter(lengths(resistance_list) > 0) %>%
    select(sample_id, print_type, area_cm2, spe_thickness_avg, resistance_list) %>%
    unnest_longer(resistance_list, values_to = "resistance") %>%
    group_by(sample_id) %>%
    mutate(res_index = dplyr::row_number()) %>%
    ungroup() %>%
    mutate(
      measurement_source = ifelse(tolower(print_type) == "coin cell", "coin_cell", "resistance"),
      resistance = suppressWarnings(as.numeric(resistance)),
      ionic_conductivity_final = ifelse(
        is.finite(spe_thickness_avg) & spe_thickness_avg > 0 &
          is.finite(area_cm2) & area_cm2 > 0 &
          is.finite(resistance) & resistance > 0,
        spe_thickness_avg / (resistance * area_cm2),
        NA_real_
      ),
      ionic_conductivity_final_geom_factor_applied = ionic_conductivity_final,
      `FittedValue.R1` = NA_real_,
      pin_id    = NA_character_,
      file_name = NA_character_,
      cycle_idx = NA_real_
    ) %>%
    select(
      sample_id, measurement_source, res_index, pin_id, file_name, cycle_idx,
      resistance, `FittedValue.R1`,
      ionic_conductivity_final, ionic_conductivity_final_geom_factor_applied,
      print_type, area_cm2, spe_thickness_avg
    )
  
  bind_rows(fits_long, res_rows) %>%
    mutate(precedence = dplyr::case_when(
      measurement_source == "fit_R1"    ~ 1L,
      measurement_source == "coin_cell" ~ 2L,
      measurement_source == "resistance"~ 2L,
      TRUE ~ 99L
    )) %>%
    arrange(sample_id, precedence, pin_id, file_name, cycle_idx, res_index)
}

# ---------------- Injection ----------------

strip_ic_scalars <- function(meta_raw) {
  if (is.null(meta_raw$samples)) return(meta_raw)
  for (id in names(meta_raw$samples)) {
    if (is.null(meta_raw$samples[[id]])) next
    meta_raw$samples[[id]]$ionic_conductivity_final_scalar <- NULL
    meta_raw$samples[[id]]$ionic_conductivity_final_geom_factor_applied_scalar <- NULL
  }
  meta_raw
}

inject_rows_back <- function(meta_raw, rows_tbl) {
  if (is.null(meta_raw$samples)) stop("Malformed metadata object, 'samples' missing.")
  by_sample <- split(rows_tbl, rows_tbl$sample_id)
  
  for (sid in names(by_sample)) {
    rows <- by_sample[[sid]]
    if (is.null(meta_raw$samples[[sid]])) next
    
    # Build ionic_conductivity_calcs entries with only scalar fields
    calcs <- purrr::pmap(
      rows %>%
        dplyr::select(
          measurement_source, res_index, pin_id, file_name, cycle_idx,
          resistance, `FittedValue.R1`,
          ionic_conductivity_final, ionic_conductivity_final_geom_factor_applied,
          print_type, area_cm2, spe_thickness_avg
        ),
      function(measurement_source, res_index, pin_id, file_name, cycle_idx,
               resistance, `FittedValue.R1`,
               ionic_conductivity_final, ionic_conductivity_final_geom_factor_applied,
               print_type, area_cm2, spe_thickness_avg) {
        
        rec <- list(
          measurement_source = measurement_source %||% NA_character_,
          res_index          = if (is.na(res_index)) NA_real_ else res_index,
          pin_id             = if (is.null(pin_id)) NA_character_ else pin_id,
          file_name          = if (is.null(file_name)) NA_character_ else file_name,
          cycle_idx          = if (is.na(cycle_idx)) NA_real_ else cycle_idx,
          resistance         = if (is.na(resistance)) NA_real_ else resistance,
          FittedValue_R1     = if (is.na(`FittedValue.R1`)) NA_real_ else `FittedValue.R1`,
          ionic_conductivity_final = if (is.na(ionic_conductivity_final)) NA_real_ else ionic_conductivity_final,
          ionic_conductivity_final_geom_factor_applied = if (is.na(ionic_conductivity_final_geom_factor_applied)) NA_real_ else ionic_conductivity_final_geom_factor_applied
        )
        # Include thickness and area only for coin cells
        if (!is.na(print_type) && tolower(print_type) == "coin cell") {
          rec$spe_thickness_avg <- if (is.na(spe_thickness_avg)) NA_real_ else spe_thickness_avg
          rec$area_cm2 <- if (is.na(area_cm2)) NA_real_ else area_cm2
        }
        rec
      }
    )
    
    # Top-level outputs: drop NA, write scalar for one, array for many
    ic_final <- rows$ionic_conductivity_final
    ic_geom  <- rows$ionic_conductivity_final_geom_factor_applied
    ic_final <- ic_final[!is.na(ic_final)]
    ic_geom  <- ic_geom[!is.na(ic_geom)]
    
    meta_raw$samples[[sid]]$ionic_conductivity_calcs <- calcs
    
    if (length(ic_final) == 0) {
      # if nothing computed, do not create an empty array key
      meta_raw$samples[[sid]]$ionic_conductivity_final <- NULL
    } else if (length(ic_final) == 1) {
      meta_raw$samples[[sid]]$ionic_conductivity_final <- ic_final[[1]]
    } else {
      meta_raw$samples[[sid]]$ionic_conductivity_final <- unname(ic_final)
    }
    
    if (length(ic_geom) == 0) {
      meta_raw$samples[[sid]]$ionic_conductivity_final_geom_factor_applied <- NULL
    } else if (length(ic_geom) == 1) {
      meta_raw$samples[[sid]]$ionic_conductivity_final_geom_factor_applied <- ic_geom[[1]]
    } else {
      meta_raw$samples[[sid]]$ionic_conductivity_final_geom_factor_applied <- unname(ic_geom)
    }
  }
  
  strip_ic_scalars(meta_raw)
}

# ---------------- Updater ----------------

update_ionic_conductivity <- function(sample_file, eis_file,
                                      write_out = TRUE) {
  meta_raw <- jsonlite::fromJSON(sample_file, simplifyVector = FALSE)
  meta_df  <- read_metadata_json(sample_file)
  eis_long <- read_eis_long(eis_file)
  
  rows <- compute_rows(meta_df, eis_long)
  
  if (isTRUE(write_out)) {
    updated <- inject_rows_back(meta_raw, rows)
    # auto_unbox = TRUE gives scalars for length-1 and arrays for length>1
    jsonlite::write_json(updated, sample_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  }
  
  invisible(rows)
}

# ---------------- CLI ----------------

option_list <- list(
  optparse::make_option(c("-s", "--sample-file"), type = "character",
                        help = "Path to sample metadata JSON with 'samples' object", metavar = "FILE"),
  optparse::make_option(c("-e", "--eis-file"), type = "character",
                        help = "Path to EIS fitted results JSON", metavar = "FILE"),
  optparse::make_option(c("--dry-run"), action = "store_true", default = FALSE,
                        help = "Do not write JSON, only print a summary")
)

parser <- optparse::OptionParser(
  usage = "Usage: %prog -s SAMPLE_JSON -e EIS_JSON [--dry-run]",
  option_list = option_list
)
opt <- optparse::parse_args(parser, print_help_and_exit = FALSE)

if (is.null(opt$`sample-file`)) {
  optparse::print_help(parser)
  cat("\nError: --sample-file is required\n", file = stderr())
  quit(status = 2)
}
if (is.null(opt$`eis-file`)) {
  optparse::print_help(parser)
  cat("\nError: --eis-file is required\n", file = stderr())
  quit(status = 2)
}

sample_file <- opt$`sample-file`
eis_file    <- opt$`eis-file`

if (!file.exists(sample_file)) {
  cat("Error: sample JSON not found: ", sample_file, "\n", sep = "", file = stderr())
  quit(status = 2)
}
if (!file.exists(eis_file)) {
  cat("Error: EIS JSON not found: ", eis_file, "\n", sep = "", file = stderr())
  quit(status = 2)
}

rows <- tryCatch(
  update_ionic_conductivity(
    sample_file = sample_file,
    eis_file    = eis_file,
    write_out   = !isTRUE(opt$`dry-run`)
  ),
  error = function(e) {
    cat("calc_IC.R error: ", conditionMessage(e), "\n", file = stderr())
    quit(status = 1)
  }
)

if (isTRUE(opt$`dry-run`)) {
  if (is.null(rows) || nrow(rows) == 0) {
    cat("Dry run: no rows computed\n")
  } else {
    cat("Dry run: first 10 rows\n")
    print(utils::head(rows, 10))
  }
} else {
  cat("Ionic conductivity updated. Output written to: ", sample_file, "\n", sep = "")
}

quit(status = 0)