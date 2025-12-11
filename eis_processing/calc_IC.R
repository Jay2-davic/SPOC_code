#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(optparse)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
as_num <- function(x) suppressWarnings(as.numeric(x))
is_valid_num <- function(x) is.finite(x) && !is.na(x)

# ---------------- IO helpers ----------------

read_metadata_raw <- function(path) {
  obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(obj$samples)) stop("No 'samples' key in metadata JSON: ", path)
  obj
}

read_external_eis <- function(path) {
  obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # Expected shape: { "eis_results": { "<sample_id>": { pin_0001: { ... } } }, "eis_summary": {...} }
  if (is.null(obj$eis_results) || !is.list(obj$eis_results)) {
    stop("External EIS JSON missing 'eis_results' object: ", path)
  }
  obj
}

remove_deprecated_fields <- function(meta_raw) {
  # Remove at top level when present
  meta_raw$eis_summary <- NULL
  meta_raw$ic_sources <- NULL
  
  # Remove at sample level when present
  if (!is.null(meta_raw$samples)) {
    for (sid in names(meta_raw$samples)) {
      meta_raw$samples[[sid]]$eis_summary <- NULL
      meta_raw$samples[[sid]]$ic_sources <- NULL
    }
  }
  meta_raw
}

merge_external_eis_into_metadata <- function(meta_raw, external_eis_raw) {
  by_sample <- external_eis_raw$eis_results %||% list()
  for (sid in names(by_sample)) {
    if (!is.null(meta_raw$samples[[sid]])) {
      # Copy external EIS object exactly, do not normalize or change structure
      meta_raw$samples[[sid]]$eis_results <- by_sample[[sid]]
      # Clean deprecated fields at sample level
      meta_raw$samples[[sid]]$eis_summary <- NULL
      meta_raw$samples[[sid]]$ic_sources <- NULL
    }
  }
  # Do not copy external 'eis_summary'; ensure none remain at top level
  meta_raw$eis_summary <- NULL
  meta_raw$ic_sources <- NULL
  meta_raw
}

as_metadata_df <- function(meta_raw) {
  tibble(
    sample_id = names(meta_raw$samples),
    sample_obj = unname(meta_raw$samples)
  ) |>
    mutate(
      area_cm2 = map_dbl(sample_obj, ~ as_num(.x$area_cm2 %||% .x$print_quality$area_cm2 %||% NA_real_)),
      thickness_cm = map_dbl(sample_obj, ~ as_num(.x$print_quality$spe_thickness_avg %||%
                                                    .x$print_quality$spe_thickness %||%
                                                    .x$film_thickness %||% NA_real_)),
      resistance = map_dbl(sample_obj, ~ as_num(.x$resistance %||% NA_real_)),
      existing_R1_len = map_int(sample_obj, ~ length(.x$R1 %||% list())),
      existing_ic_len = map_int(sample_obj, ~ length(.x$ionic_conductivity_final %||% list())),
      existing_ic_geom_len = map_int(sample_obj, ~ length(.x$ionic_conductivity_final_geom_factor_applied %||% list()))
    )
}

# ---------------- EIS extraction (no structure changes) ----------------

extract_cycles_any_shape <- function(pin_entry) {
  # Prefer normalized 'cycle_data' when present
  if (!is.null(pin_entry$cycle_data) && is.list(pin_entry$cycle_data)) {
    return(pin_entry$cycle_data)
  }
  # Else collect 'cycle_N' keys, preserving objects exactly
  nms <- names(pin_entry) %||% character(0)
  cyc_keys <- nms[grepl("^cycle_[0-9]+$", nms)]
  if (length(cyc_keys) == 0) return(list())
  
  cycles <- lapply(cyc_keys, function(ck) {
    cobj <- pin_entry[[ck]]
    if (is.null(cobj$cycle)) {
      # Add cycle number in-memory for ordering only; do not write back
      cobj$cycle <- as_num(sub("^cycle_", "", ck))
    }
    cobj
  })
  # Sort by numeric cycle index if present
  ord <- sapply(cycles, function(x) as_num(x$cycle %||% NA_real_))
  cycles[order(ord, na.last = TRUE)]
}

parse_pin_ord <- function(pin_label, pin_entry) {
  p <- pin_entry$pin
  if (!is.null(p)) return(as_num(p))
  m <- stringr::str_extract(pin_label %||% "", "[0-9]+")
  as_num(m)
}

collect_r1_from_eis <- function(sample_obj) {
  eis <- sample_obj$eis_results
  if (is.null(eis) || length(eis) == 0) return(numeric(0))
  
  rows <- imap(eis, function(pin_entry, pin_label) {
    pin_ord <- parse_pin_ord(pin_label, pin_entry) %||% NA_real_
    cycles <- extract_cycles_any_shape(pin_entry)
    
    map(seq_along(cycles), function(i) {
      cobj <- cycles[[i]]
      fit_status <- tolower(cobj$fit_status %||% "")
      # Include when 'ok' or missing
      if (nzchar(fit_status) && fit_status != "ok") return(NULL)
      
      r1 <- as_num(cobj[["FittedValue.R1"]] %||% NA_real_)
      if (is.na(r1)) return(NULL)
      
      cy_idx <- cobj$cycle %||% i
      list(pin_ord = as_num(pin_ord), cycle_idx = as_num(cy_idx), r1 = r1)
    }) |> compact()
  }) |> flatten()
  
  if (length(rows) == 0) return(numeric(0))
  
  tibble(
    pin_ord = map_dbl(rows, "pin_ord"),
    cycle_idx = map_dbl(rows, "cycle_idx"),
    r1 = map_dbl(rows, "r1")
  ) |>
    arrange(pin_ord, cycle_idx) |>
    pull(r1)
}

# ---------------- IC computation ----------------

# sigma = thickness_cm / (R * area_cm2)
compute_sigma <- function(R_vec, thickness_cm, area_cm2) {
  if (length(R_vec) == 0) return(numeric(0))
  if (!is_valid_num(thickness_cm) || !is_valid_num(area_cm2) || area_cm2 <= 0) {
    return(rep(NA_real_, length(R_vec)))
  }
  (thickness_cm / area_cm2) / R_vec
}

compute_sample_arrays <- function(sample_obj) {
  r1_vec <- collect_r1_from_eis(sample_obj)
  
  area <- as_num(sample_obj$area_cm2 %||% sample_obj$print_quality$area_cm2 %||% NA_real_)
  thickness <- as_num(sample_obj$print_quality$spe_thickness_avg %||%
                        sample_obj$print_quality$spe_thickness %||%
                        sample_obj$film_thickness %||% NA_real_)
  res <- as_num(sample_obj$resistance %||% NA_real_)
  
  if (length(r1_vec) == 0) {
    # Fallback to resistance as a single-element array only when no fitted R1 exists
    if (is_valid_num(res) && res > 0) {
      r1_vec <- c(res)
    } else {
      return(list(
        R1 = numeric(0),
        ionic_conductivity_final = numeric(0),
        ionic_conductivity_final_geom_factor_applied = numeric(0)
      ))
    }
  }
  
  sigma <- compute_sigma(r1_vec, thickness, area)
  sigma_geom <- sigma  # keep aligned and identical unless you specify a different geom factor
  
  list(
    R1 = r1_vec,
    ionic_conductivity_final = sigma,
    ionic_conductivity_final_geom_factor_applied = sigma_geom
  )
}

# ---------------- Injection ----------------

as_json_array <- function(num_vec) {
  if (length(num_vec) == 0) return(list())
  as.list(unname(num_vec))
}

inject_results_back <- function(meta_raw, results_tbl, overwrite_existing = FALSE) {
  samples <- meta_raw$samples
  ids <- results_tbl$sample_id
  
  for (i in seq_along(ids)) {
    id <- ids[i]
    sobj <- samples[[id]]
    if (is.null(sobj)) next
    
    # Remove deprecated keys when present at sample level
    sobj$eis_summary <- NULL
    sobj$ic_sources <- NULL
    
    arrays <- results_tbl$arrays[[i]]
    
    already_has_arrays <- !is.null(sobj$R1) ||
      !is.null(sobj$ionic_conductivity_final) ||
      !is.null(sobj$ionic_conductivity_final_geom_factor_applied)
    
    if (already_has_arrays && !isTRUE(overwrite_existing)) {
      samples[[id]] <- sobj
      next
    }
    
    sobj$R1 <- as_json_array(arrays$R1)
    sobj$ionic_conductivity_final <- as_json_array(arrays$ionic_conductivity_final)
    sobj$ionic_conductivity_final_geom_factor_applied <- as_json_array(arrays$ionic_conductivity_final_geom_factor_applied)
    
    samples[[id]] <- sobj
  }
  
  meta_raw$samples <- samples
  
  # Also clean any deprecated fields at top level
  meta_raw$eis_summary <- NULL
  meta_raw$ic_sources <- NULL
  
  meta_raw
}

# ---------------- Orchestration ----------------

update_ionic_conductivity <- function(metadata_json,
                                      external_eis_json = NULL,
                                      overwrite_existing = FALSE,
                                      write_out = TRUE) {
  meta_raw <- read_metadata_raw(metadata_json)
  
  # Merge external EIS when provided; do not normalize or change cycle structure
  if (!is.null(external_eis_json)) {
    ext_raw <- read_external_eis(external_eis_json)
    meta_raw <- merge_external_eis_into_metadata(meta_raw, ext_raw)
  }
  
  # Always remove deprecated fields if present
  meta_raw <- remove_deprecated_fields(meta_raw)
  
  meta_df <- as_metadata_df(meta_raw)
  
  meta_df <- meta_df |>
    mutate(needs_calc = overwrite_existing |
             (existing_R1_len == 0 | existing_ic_len == 0 | existing_ic_geom_len == 0))
  
  to_calc <- meta_df |> filter(needs_calc)
  
  if (nrow(to_calc) == 0) {
    message("No samples require calculation. Nothing to do.")
    return(invisible(tibble()))
  }
  
  results <- to_calc |>
    mutate(arrays = map(sample_obj, compute_sample_arrays),
           r1_len = map_int(arrays, ~ length(.x$R1)),
           ic_len = map_int(arrays, ~ length(.x$ionic_conductivity_final))) |>
    select(sample_id, area_cm2, thickness_cm, resistance, r1_len, ic_len, arrays)
  
  if (isTRUE(write_out)) {
    meta_updated <- inject_results_back(meta_raw, results, overwrite_existing = overwrite_existing)
    jsonlite::write_json(meta_updated, metadata_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  }
  
  invisible(results)
}

# ---------------- CLI ----------------

option_list <- list(
  optparse::make_option(c("-m", "--meta"), type = "character",
                        help = "Path to primary metadata JSON with 'samples' object", metavar = "FILE"),
  optparse::make_option(c("-e", "--eis"), type = "character", default = NULL,
                        help = "Optional path to external EIS JSON to merge (structure preserved)", metavar = "FILE"),
  optparse::make_option(c("--overwrite"), action = "store_true", default = FALSE,
                        help = "Overwrite existing arrays R1 and ionic_conductivity_*"),
  optparse::make_option(c("--dry-run"), action = "store_true", default = FALSE,
                        help = "Do not write JSON, only print a summary")
)

opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

if (is.null(opt$meta)) {
  cat("Error: --meta is required\n", file = stderr())
  quit(status = 2)
}
if (!file.exists(opt$meta)) {
  cat("Error: metadata JSON not found: ", opt$meta, "\n", sep = "", file = stderr())
  quit(status = 2)
}
if (!is.null(opt$eis) && !file.exists(opt$eis)) {
  cat("Error: external EIS JSON not found: ", opt$eis, "\n", sep = "", file = stderr())
  quit(status = 2)
}

results <- tryCatch(
  update_ionic_conductivity(
    metadata_json     = opt$meta,
    external_eis_json = opt$eis,
    overwrite_existing = isTRUE(opt$overwrite),
    write_out          = !isTRUE(opt$dry_run)
  ),
  error = function(e) {
    cat("calc_IC.R error: ", conditionMessage(e), "\n", file = stderr())
    quit(status = 1)
  }
)

if (isTRUE(opt$dry_run)) {
  if (is.null(results) || nrow(results) == 0) {
    cat("Dry run: no results to display\n")
  } else {
    cat("Dry run summary (first 10 rows):\n")
    print(utils::head(results |> select(sample_id, r1_len, ic_len), 10))
  }
} else {
  cat("Ionic conductivity arrays updated. Output written to: ", opt$meta, "\n", sep = "")
}

quit(status = 0)