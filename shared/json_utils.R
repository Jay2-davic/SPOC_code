#' JSON Database Utilities
#'
#' @description
#' Functions for merging, updating, and querying multi-master JSON databases.

require(tidyverse)
require(jsonlite)

#' Load Master JSON for Analysis
#'
#' @description
#' Loads and flattens master JSON into analysis-ready DataFrame
#' matching the original 85-column CSV structure.
#'
#' @param paths Config paths from get_config().
#' @param apply_flags Logical. Filter by QC flags? (default: TRUE)
#'
#' @return Dataframe with 85 columns matching original CSV.
#'
#' @export
load_master_for_analysis <- function(paths, apply_flags = TRUE) {
  master_path <- file.path(paths$database_dir, "master_database.json")
  
  if (!file.exists(master_path)) {
    stop("Master database not found. Run 'make merge' first.")
  }
  
  master <- jsonlite::read_json(master_path, simplifyVector = TRUE)
  
  df <- as.data.frame(master$measurements)
  
  if (apply_flags) {
    df <- df |>
      dplyr::filter(
        flag_exclude == FALSE | is.na(flag_exclude),
        flag_bad_print == FALSE | is.na(flag_bad_print)
      )
  }
  
  return(df)
}

#' Merge All Master Files
#'
#' @description
#' Combines individual JSON files from each pipeline stage into master database.
#'
#' @param paths Config paths.
#' @param force_rebuild Logical. Force complete rebuild? (default: FALSE)
#'
#' @return Merged database list.
#'
#' @export
merge_to_master <- function(paths, force_rebuild = FALSE) {
  # Load individual masters
  historical <- jsonlite::read_json("historical/outputs/historical_samples.json", simplifyVector = TRUE)
  active <- jsonlite::read_json("data_cleaning/outputs/active_samples.json", simplifyVector = TRUE)
  mpr_files <- jsonlite::read_json("eis_processing/outputs/mpr_files.json", simplifyVector = TRUE)
  cycles <- jsonlite::read_json("eis_processing/outputs/cycles.json", simplifyVector = TRUE)
  
  # Try to load flags (may not exist yet)
  flags <- tryCatch({
    jsonlite::read_json("analysis/outputs/flags.json", simplifyVector = TRUE)
  }, error = function(e) {
    list(sample_flags = list(), cycle_flags = list())
  })
  
  # Combine samples
  all_samples <- c(historical$samples, active$samples)
  
  # Build flat measurements array
  measurements <- list()
  
  for (cycle_id in names(cycles$cycles)) {
    cycle <- cycles$cycles[[cycle_id]]
    sample <- all_samples[[cycle$sample_ID]]
    
    if (is.null(sample)) next
    
    # Flatten into single row matching 85-column structure
    measurements[[length(measurements) + 1]] <- list(
      # Cycle identifiers
      cycle_ID = cycle_id,
      sample_ID = cycle$sample_ID,
      mpr_file_ID = cycle$mpr_file_ID,
      cycle_number = cycle$cycle_number,
      source_file = cycle$source_file,
      
      # Sample metadata
      date_printed = sample$date_printed,
      print_type = sample$print_type,
      fab_method = sample$fab_method,
      purge_time_s = sample$purge_time_s,
      Comp1_flowRate = sample$Comp1_flowRate,
      Comp2_flowRate = sample$Comp2_flowRate,
      notes = sample$notes,
      
      # Formulation
      fcomp_formulation = sample$formulation$fcomp_formulation,
      fcomp_mat1 = sample$formulation$fcomp_mat1,
      fcomp_mat1_ratio = sample$formulation$fcomp_mat1_ratio,
      fcomp_mat2 = sample$formulation$fcomp_mat2,
      fcomp_mat2_ratio = sample$formulation$fcomp_mat2_ratio,
      fcomp_additive = sample$formulation$fcomp_additive,
      fcomp_additive_wt_pct = sample$formulation$fcomp_additive_wt_pct,
      fcomp_additive2 = sample$formulation$fcomp_additive2,
      fcomp_additive2_wt_pct = sample$formulation$fcomp_additive2_wt_pct,
      fcomp_salt = sample$formulation$fcomp_salt,
      fcomp_salt_wt_pct = sample$formulation$fcomp_salt_wt_pct,
      nile_red = sample$formulation$nile_red,
      
      # Geometry
      film_thickness = sample$geometry$film_thickness,
      area_cm2 = sample$geometry$area_cm2,
      film_quality = sample$geometry$film_quality,
      
      # Environmental
      temperature = sample$environmental$temperature,
      humidity = sample$environmental$humidity,
      
      # File paths
      cycle_filepath = cycle$files$cycle_data,
      results_csv = cycle$files$results_csv,
      eis_png = cycle$files$figure,
      
      # Processing info
      head_count = cycle$processing$head_count,
      optimization_success = cycle$processing$optimization_success,
      optimization_target = cycle$processing$optimization_target,
      fit_metric = cycle$processing$fit_metric,
      used_fallback = cycle$processing$used_fallback,
      fallback_type = cycle$processing$fallback_type,
      
      # Initial guesses
      InitialGuess.R0 = cycle$initial_guess$R0,
      InitialGuess.R1 = cycle$initial_guess$R1,
      InitialGuess.C1 = cycle$initial_guess$C1,
      InitialGuess.Wo1_0 = cycle$initial_guess$Wo1_0,
      InitialGuess.Wo1_1 = cycle$initial_guess$Wo1_1,
      
      # Fitted values
      FittedValue.R0 = cycle$fitted_values$R0,
      FittedValue.R1 = cycle$fitted_values$R1,
      FittedValue.C1 = cycle$fitted_values$C1,
      FittedValue.Wo1_0 = cycle$fitted_values$Wo1_0,
      FittedValue.Wo1_1 = cycle$fitted_values$Wo1_1,
      
      # Fitted stdev
      FittedValue.R0_stDev = cycle$fitted_stdev$R0_stDev,
      FittedValue.R1_stDev = cycle$fitted_stdev$R1_stDev,
      FittedValue.C1_stDev = cycle$fitted_stdev$C1_stDev,
      FittedValue.Wo1_0_stDev = cycle$fitted_stdev$Wo1_0_stDev,
      FittedValue.Wo1_1_stDev = cycle$fitted_stdev$Wo1_1_stDev,
      
      # Fit quality
      RMSE_real = cycle$fit_quality$RMSE_real,
      RMSE_imag = cycle$fit_quality$RMSE_imag,
      robust_rmse = cycle$fit_quality$robust_rmse,
      r2_magnitude = cycle$fit_quality$r2_magnitude,
      r2_phase = cycle$fit_quality$r2_phase,
      r2_combined = cycle$fit_quality$r2_combined,
      composite_score = cycle$fit_quality$composite_score,
      linKK_RMSEreal = cycle$fit_quality$linKK_RMSEreal,
      linKK_RMSEimag = cycle$fit_quality$linKK_RMSEimag,
      mu = cycle$fit_quality$mu,
      Circuits = cycle$fit_quality$Circuits,
      `Lin-KK_PassFail` = cycle$fit_quality$LinKK_PassFail,
      
      # Circuit info
      EISmeasurement.Name = cycle$circuit$name,
      EISmeasurement.Circuit = cycle$circuit$circuit,
      EISmeasurement.Fit = cycle$circuit$fit,
      
      # Calculated properties
      resistance = cycle$calculated$resistance,
      ionic_conductivity_final = cycle$calculated$ionic_conductivity_final,
      ionic_conductivity_final_geom_factor_applied = cycle$calculated$ionic_conductivity_geom_corrected,
      
      # Flags (if available)
      flag_outlier = flags$cycle_flags[[cycle_id]]$outlier %||% NA,
      flag_bad_print = flags$sample_flags[[cycle$sample_ID]]$bad_print %||% NA,
      flag_exclude = flags$sample_flags[[cycle$sample_ID]]$exclude_from_analysis %||% NA
    )
  }
  
  # Build summaries
  summaries <- list(
    by_sample = calculate_sample_summaries(measurements, all_samples),
    by_formulation = calculate_formulation_summaries(measurements)
  )
  
  # Create master
  master <- list(
    metadata = list(
      version = active$metadata$version,
      last_merged = as.character(Sys.time()),
      total_samples = length(all_samples),
      total_cycles = length(measurements),
      source_files = c(
        "historical/outputs/historical_samples.json",
        "data_cleaning/outputs/active_samples.json",
        "eis_processing/outputs/mpr_files.json",
        "eis_processing/outputs/cycles.json",
        "analysis/outputs/flags.json"
      )
    ),
    measurements = measurements,
    summaries = summaries
  )
  
  return(master)
}

#' Update Master Database Incrementally
#'
#' @param paths Config paths.
#' @param stage Character. Which stage to update ("clean", "eis", "analysis", "all").
#'
#' @export
update_master_incremental <- function(paths, stage = "all") {
  master_path <- file.path(paths$database_dir, "master_database.json")
  
  if (!file.exists(master_path) || stage == "all") {
    return(merge_to_master(paths, force_rebuild = TRUE))
  }
  
  master <- jsonlite::read_json(master_path, simplifyVector = TRUE)
  
  # Reload only changed sections
  if (stage %in% c("clean", "all")) {
    # Reload sample data and update measurements
    historical <- jsonlite::read_json("historical/outputs/historical_samples.json", simplifyVector = TRUE)
    active <- jsonlite::read_json("data_cleaning/outputs/active_samples.json", simplifyVector = TRUE)
    all_samples <- c(historical$samples, active$samples)
    
    # Update sample-level fields in measurements
    for (i in seq_along(master$measurements)) {
      sample_id <- master$measurements[[i]]$sample_ID
      sample <- all_samples[[sample_id]]
      if (!is.null(sample)) {
        master$measurements[[i]]$date_printed <- sample$date_printed
        master$measurements[[i]]$print_type <- sample$print_type
        # ... update other sample fields
      }
    }
  }
  
  if (stage %in% c("eis", "all")) {
    # Reload cycles and merge new ones
    cycles <- jsonlite::read_json("eis_processing/outputs/cycles.json", simplifyVector = TRUE)
    # Logic to merge new cycles
  }
  
  if (stage %in% c("analysis", "all")) {
    # Reload flags
    flags <- jsonlite::read_json("analysis/outputs/flags.json", simplifyVector = TRUE)
    # Update flag fields
  }
  
  master$metadata$last_merged <- as.character(Sys.time())
  master$metadata$updated_sections <- stage
  
  return(master)
}

#' Calculate Sample-Level Summaries
#'
#' @param measurements List of measurement records.
#' @param all_samples List of all sample records.
#'
#' @return List of summary statistics by sample.
#'
#' @export
calculate_sample_summaries <- function(measurements, all_samples) {
  df <- data.table::rbindlist(measurements, fill = TRUE)
  
  summaries <- df |>
    dplyr::group_by(sample_ID) |>
    dplyr::summarise(
      n_cycles = dplyr::n(),
      avg_ionic_conductivity = mean(ionic_conductivity_final, na.rm = TRUE),
      std_ionic_conductivity = sd(ionic_conductivity_final, na.rm = TRUE),
      cv_percent = 100 * std_ionic_conductivity / avg_ionic_conductivity,
      avg_R1 = mean(FittedValue.R1, na.rm = TRUE),
      avg_RMSE_real = mean(RMSE_real, na.rm = TRUE),
      avg_r2_magnitude = mean(r2_magnitude, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert to named list
  summary_list <- setNames(
    lapply(seq_len(nrow(summaries)), function(i) as.list(summaries[i, -1])),
    summaries$sample_ID
  )
  
  return(summary_list)
}

#' Calculate Formulation-Level Summaries
#'
#' @param measurements List of measurement records.
#'
#' @return List of summary statistics by formulation.
#'
#' @export
calculate_formulation_summaries <- function(measurements) {
  df <- data.table::rbindlist(measurements, fill = TRUE)
  
  summaries <- df |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::summarise(
      n_samples = dplyr::n_distinct(sample_ID),
      n_cycles = dplyr::n(),
      avg_ionic_conductivity = mean(ionic_conductivity_final, na.rm = TRUE),
      std_ionic_conductivity = sd(ionic_conductivity_final, na.rm = TRUE),
      cv_percent = 100 * std_ionic_conductivity / avg_ionic_conductivity,
      .groups = "drop"
    )
  
  summary_list <- setNames(
    lapply(seq_len(nrow(summaries)), function(i) as.list(summaries[i, -1])),
    summaries$fcomp_formulation
  )
  
  return(summary_list)
}

#' Build Cross-Reference Indexes
#'
#' @param merged_data Merged database object.
#'
#' @return List of lookup indexes.
#'
#' @export
build_indexes <- function(merged_data) {
  df <- data.table::rbindlist(merged_data$measurements, fill = TRUE)
  
  list(
    sample_to_cycles = df |>
      dplyr::group_by(sample_ID) |>
      dplyr::summarise(cycle_ids = list(cycle_ID), .groups = "drop") |>
      tibble::deframe(),
    
    mpr_to_cycles = df |>
      dplyr::group_by(mpr_file_ID) |>
      dplyr::summarise(cycle_ids = list(cycle_ID), .groups = "drop") |>
      tibble::deframe(),
    
    sample_to_mpr = df |>
      dplyr::distinct(sample_ID, mpr_file_ID) |>
      dplyr::group_by(sample_ID) |>
      dplyr::summarise(mpr_ids = list(mpr_file_ID), .groups = "drop") |>
      tibble::deframe()
  )
}

#' Save Master Database
#'
#' @param paths Config paths.
#' @param master_data Master database object.
#'
#' @export
save_master_json <- function(paths, master_data) {
  output_path <- file.path(paths$database_dir, "master_database.json")
  
  jsonlite::write_json(master_data, output_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Also save compressed version
  compress_json(output_path)
  
  invisible(NULL)
}

#' Compress JSON File
#'
#' @param json_path Character. Path to JSON file.
#'
#' @export
compress_json <- function(json_path) {
  if (file.exists(json_path)) {
    system(glue::glue("gzip -c {json_path} > {json_path}.gz"), ignore.stdout = TRUE)
  }
  invisible(NULL)
}

#' Decompress JSON File
#'
#' @param json_gz_path Character. Path to .json.gz file.
#'
#' @return Path to decompressed file.
#'
#' @export
decompress_json <- function(json_gz_path) {
  output_path <- stringr::str_remove(json_gz_path, "\\.gz$")
  system(glue::glue("gzip -dc {json_gz_path} > {output_path}"), ignore.stdout = TRUE)
  return(output_path)
}

#' Get Last Merge Timestamp
#'
#' @param paths Config paths.
#'
#' @return POSIXct timestamp or NULL.
#'
#' @export
get_last_merge_timestamp <- function(paths) {
  master_path <- file.path(paths$database_dir, "master_database.json")
  
  if (!file.exists(master_path)) return(NULL)
  
  master <- jsonlite::read_json(master_path, simplifyVector = TRUE)
  
  tryCatch({
    as.POSIXct(master$metadata$last_merged)
  }, error = function(e) NULL)
}