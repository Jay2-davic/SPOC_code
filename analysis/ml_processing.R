# =============================================================================
# CORE DATA LOADING AND QUALITY CONTROL FUNCTIONS
# =============================================================================

suppressPackageStartupMessages({
  require(dplyr)
  require(jsonlite)
  require(tidyr)
  require(lubridate)
  require(stringr)
  require(readr)
  require(tibble)
})

# =============================================================================
# DATA LOADING
# =============================================================================

#' Load JSON and extract data with IC array preservation
#' 
#' @param json_file_path Path to JSON file
#' @return Dataframe with IC arrays expanded to separate rows
load_pe_json <- function(json_file_path) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("LOADING JSON WITH ARRAY PRESERVATION\n")
  cat(strrep("=", 70), "\n\n")
  
  # Read JSON preserving arrays
  json_raw <- jsonlite::fromJSON(
    json_file_path,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    flatten = FALSE
  )
  
  # Determine structure
  if (is.list(json_raw) && length(json_raw) == 1) {
    samples_source <- json_raw[[1]]
  } else {
    samples_source <- json_raw
  }
  
  sample_ids <- names(samples_source)
  cat(sprintf("Found %d samples\n", length(sample_ids)))
  
  # Helper function for type-safe extraction
  safe_extract <- function(obj, field, expected_type = "character") {
    tryCatch({
      val <- obj[[field]]
      
      if (is.null(val) || (is.list(val) && length(val) == 0)) {
        if (expected_type == "character") return("none")
        if (expected_type == "numeric") return(0)
        return(NA)
      }
      
      if (expected_type == "character") {
        if (is.list(val)) return("none")
        return(as.character(val))
      } else if (expected_type == "numeric") {
        if (is.list(val)) return(0)
        return(as.numeric(val))
      } else if (expected_type == "list") {
        return(val)
      }
      
      return(val)
    }, error = function(e) {
      if (expected_type == "character") return("none")
      if (expected_type == "numeric") return(0)
      return(NA)
    })
  }
  
  # Extract data from each sample
  cat("Extracting data...\n")
  
  df_list <- lapply(seq_along(sample_ids), function(idx) {
    id <- sample_ids[idx]
    sample <- samples_source[[id]]
    
    # Extract IC and normalize
    ic_final <- safe_extract(sample, "ionic_conductivity_final", "list")
    
    # Normalize to list format
    if (!is.list(ic_final) && !is.na(ic_final)) {
      ic_final <- list(ic_final)
    } else if (is.list(ic_final)) {
      ic_final <- list(unlist(ic_final))
    } else {
      ic_final <- list(NA_real_)
    }
    
    if (idx %% 100 == 0) {
      cat(sprintf("  %d/%d\r", idx, length(sample_ids)))
    }
    
    tibble::tibble(
      sample_ID = id,
      ionic_conductivity_final = ic_final,
      fcomp_mat1 = safe_extract(sample, "fcomp_mat1", "character"),
      fcomp_mat2 = safe_extract(sample, "fcomp_mat2", "character"),
      fcomp_mat1_ratio = safe_extract(sample, "fcomp_mat1_ratio", "numeric"),
      fcomp_mat2_ratio = safe_extract(sample, "fcomp_mat2_ratio", "numeric"),
      fcomp_additive = safe_extract(sample, "fcomp_additive", "character"),
      fcomp_additive_wt_pct = safe_extract(sample, "fcomp_additive_wt_pct", "numeric"),
      fcomp_salt = safe_extract(sample, "fcomp_salt", "character"),
      fcomp_salt_wt_pct = safe_extract(sample, "fcomp_salt_wt_pct", "numeric"),
      fcomp_formulation = safe_extract(sample, "fcomp_formulation", "character"),
      date_casted = safe_extract(sample$print_quality, "date_casted", "character"),
      cast_type = safe_extract(sample$print_quality, "cast_type", "character"),
      fab_method = safe_extract(sample$print_quality, "fab_method", "numeric"),
      area_cm2 = safe_extract(sample$print_quality, "area_cm2", "numeric"),
      spe_thickness_avg = safe_extract(sample$print_quality, "spe_thickness_avg", "numeric")
    )
  })
  
  cat(sprintf("  %d/%d\n", length(sample_ids), length(sample_ids)))
  
  # Bind rows and standardize
  df <- dplyr::bind_rows(df_list) %>%
    standardize_columns()
  
  # Expand IC arrays
  cat("Expanding IC arrays...\n")
  n_before <- nrow(df)
  
  df <- df %>%
    tidyr::unnest_longer(
      ionic_conductivity_final,
      values_to = "ionic_conductivity_final",
      indices_include = FALSE
    )
  
  cat(sprintf("Expanded from %d samples to %d measurements\n", n_before, nrow(df)))
  cat(strrep("=", 70), "\n\n")
  
  return(df)
}

# =============================================================================
# DATA STANDARDIZATION
# =============================================================================

#' Standardize column values across dataset
#' @param df Input dataframe
#' @return Dataframe with standardized values
standardize_columns <- function(df) {
  df %>%
    dplyr::mutate(
      # Standardize additive names
      fcomp_additive = stringr::str_replace_all(
        fcomp_additive, 
        "aerosil 380", 
        "Aerosil 380"
      ),
      # Standardize cast_type to print_type
      print_type = dplyr::case_when(
        stringr::str_detect(tolower(cast_type), "coin") ~ "Coin cell",
        stringr::str_detect(tolower(cast_type), "pcb|print|bulk|cast") ~ "PCB print",
        TRUE ~ cast_type
      ),
      # Standardize formulation names
      fcomp_formulation = stringr::str_replace_all(
        fcomp_formulation,
        "0.5 PEGMEA, 0.5 PEGDA,",
        "0.9 PEGMEA, 0.1 PEGDA"
      ),
      # Convert date
      date_casted = lubridate::as_date(date_casted)
    )
}

# =============================================================================
# QUALITY CONTROL FLAGGING
# =============================================================================

#' Add all quality control flags to dataset
#' @param df Input dataframe
#' @param zscore_threshold Threshold for z-score outlier detection (default: 4)
#' @return Dataframe with flag_for_removal column added
add_qc_flags <- function(df, zscore_threshold = 3) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("ADDING QUALITY CONTROL FLAGS\n")
  cat(strrep("=", 70), "\n\n")
  
  # Initialize flag column
  df <- df %>%
    dplyr::mutate(flag_for_removal = FALSE)
  
  n_initial <- nrow(df)
  
  # Apply each flagging rule in sequence
  df <- df %>%
    flag_bad_dates() %>%
    flag_fab_method() %>%
    flag_formulation_types() %>%
    flag_additive_types() %>%
    flag_ic_thresholds() %>%
    flag_zscore_outliers(threshold = zscore_threshold)
  
  # Final summary
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  cat(sprintf("\nTotal flagged for removal: %d / %d (%.1f%%)\n", 
              n_flagged, n_initial, 100 * n_flagged / n_initial))
  cat(strrep("=", 70), "\n\n")
  
  return(df)
}
# =============================================================================
# DATA EXPORT FUNCTIONS
# =============================================================================

#' Save plotting dataset (all data with flags, no filtering)
#' @param df Dataframe with flag_for_removal column
#' @param output_path Path to save plotting CSV
#' @return Invisible TRUE if saved successfully
save_plotting_data <- function(df, output_path) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING PLOTTING DATASET (RAW DATA + FLAGS)\n")
  cat(strrep("=", 70), "\n\n")
  
  # Select columns for plotting - NO FILTERING
  selected_cols <- c(
    "sample_ID",
    "ionic_conductivity_final",
    "fcomp_salt",
    "fcomp_additive",
    "fcomp_salt_wt_pct",
    "fcomp_additive_wt_pct",
    "fcomp_formulation",
    "fcomp_mat1",
    "fcomp_mat2",
    "fcomp_mat1_ratio",
    "fcomp_mat2_ratio",
    "print_type",
    "cast_type",
    "date_casted",
    "fab_method",
    "area_cm2",
    "spe_thickness_avg",
    "flag_for_removal"
  )
  
  selected_cols <- selected_cols[selected_cols %in% names(df)]
  
  df_output <- df %>%
    dplyr::select(dplyr::all_of(selected_cols))
  
  # Save to CSV
  readr::write_csv(df_output, output_path)
  
  cat(sprintf("Saved plotting data to: %s\n", output_path))
  cat(sprintf("  Total rows: %d\n", nrow(df_output)))
  cat(sprintf("  Total columns: %d\n", ncol(df_output)))
  cat(sprintf("  Flagged: %d (%.1f%%)\n", 
              sum(df_output$flag_for_removal),
              100 * sum(df_output$flag_for_removal) / nrow(df_output)))
  cat(sprintf("  Clean: %d (%.1f%%)\n", 
              sum(!df_output$flag_for_removal),
              100 * sum(!df_output$flag_for_removal) / nrow(df_output)))
  
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

#' Save ML dataset (aggregated, clean data only)
#' @param df Dataframe with flag_for_removal column
#' @param output_path Path to save ML CSV
#' @param salt_filter Vector of salt values to keep
#' @param additive_filter Vector of additive values to keep
#' @param print_type_filter Print type to keep
#' @return Invisible TRUE if saved successfully
save_ml_data <- function(df, output_path,
                         salt_filter = c("LiTFSI", "none"),
                         additive_filter = c("Aerosil 380", "none"),
                         print_type_filter = "PCB print") {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING ML DATASET\n")
  cat(strrep("=", 70), "\n\n")
  
  # Ensure print_type exists
  if (!"print_type" %in% names(df) && "cast_type" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        print_type = dplyr::case_when(
          stringr::str_detect(tolower(cast_type), "coin") ~ "Coin cell",
          stringr::str_detect(tolower(cast_type), "pcb|print|bulk|cast") ~ "PCB print",
          TRUE ~ cast_type
        )
      )
  }
  
  # Apply composition filters
  df_filtered <- df %>%
    dplyr::filter(
      fcomp_salt %in% salt_filter &
        fcomp_additive %in% additive_filter &
        print_type %in% print_type_filter
    )
  
  cat(sprintf("After composition filters: %d rows\n", nrow(df_filtered)))
  
  # Remove flagged samples
  if ("flag_for_removal" %in% names(df_filtered)) {
    n_before <- nrow(df_filtered)
    df_filtered <- df_filtered %>%
      dplyr::filter(flag_for_removal == FALSE)
    cat(sprintf("After removing flagged: %d rows (removed %d)\n", 
                nrow(df_filtered), n_before - nrow(df_filtered)))
  }
  
  # Aggregate by composition
  cat("\nAggregating by composition groups...\n")
  n_before <- nrow(df_filtered)
  
  df_ml <- df_filtered %>%
    dplyr::group_by(
      fcomp_salt,
      fcomp_additive,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      print_type
    ) %>%
    dplyr::summarize(
      ionic_conductivity_final = mean(ionic_conductivity_final, na.rm = TRUE),
      n_measurements = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # One-hot encode categorical variables
      material_LiTFSI = dplyr::if_else(fcomp_salt == 'LiTFSI', 1.0, 0.0),
      material_none = dplyr::if_else(fcomp_salt == 'none', 1.0, 0.0),
      print_type_PCB = dplyr::if_else(print_type == 'PCB print', 1.0, 0.0)
    ) %>%
    dplyr::select(
      print_type,
      fcomp_salt,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final,
      n_measurements,
      material_LiTFSI,
      material_none,
      print_type_PCB
    )
  
  cat(sprintf("Aggregated to %d groups (from %d measurements)\n", 
              nrow(df_ml), n_before))
  
  # Save to CSV
  readr::write_csv(df_ml, output_path)
  
  cat(sprintf("\nSaved ML dataset to: %s\n", output_path))
  cat(sprintf("  Total groups: %d\n", nrow(df_ml)))
  cat(sprintf("  Features: %d\n", ncol(df_ml) - 1))
  
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

#' Summarize flagging statistics
#' @param df Dataframe with flag_for_removal column
#' @return Summary dataframe
summarize_flags <- function(df) {
  if (!"flag_for_removal" %in% names(df)) {
    cat("Warning: flag_for_removal column not found\n")
    return(NULL)
  }
  
  summary_df <- data.frame(
    total_samples = nrow(df),
    flagged = sum(df$flag_for_removal, na.rm = TRUE),
    clean = sum(!df$flag_for_removal, na.rm = TRUE),
    pct_flagged = 100 * sum(df$flag_for_removal, na.rm = TRUE) / nrow(df)
  )
  
  # Add breakdown by type if available
  if ("print_type" %in% names(df)) {
    by_type <- df %>%
      dplyr::group_by(print_type) %>%
      dplyr::summarise(
        total = dplyr::n(),
        flagged = sum(flag_for_removal, na.rm = TRUE),
        clean = sum(!flag_for_removal, na.rm = TRUE),
        pct_flagged = 100 * sum(flag_for_removal, na.rm = TRUE) / dplyr::n(),
        .groups = 'drop'
      )
    
    return(list(overall = summary_df, by_type = by_type))
  }
  
  return(summary_df)
}
# =============================================================================
# INDIVIDUAL FLAGGING FUNCTIONS
# =============================================================================

#' Flag samples with problematic dates
flag_bad_dates <- function(df) {
  if (!"date_casted" %in% names(df)) {
    cat("ℹ date_casted column not found, skipping date filters\n")
    return(df)
  }
  
  bad_dates <- as.Date(c('2024-07-22', '2024-09-26', '2024-08-15', 
                         '2024-05-06', '2024-10-07', '2024-03-04'))
  early_cutoff <- as.Date('2024-02-02')
  old_cutoff <- as.Date('2024-01-01')
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | 
        date_casted %in% bad_dates |
        date_casted <= early_cutoff |
        date_casted <= old_cutoff
    )
  
  cat(sprintf("✓ Flagged bad/early/old dates: %d (cumulative)\n", 
              sum(df$flag_for_removal, na.rm = TRUE)))
  
  # Flag specific problematic sample
  if ("fcomp_salt_wt_pct" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        flag_for_removal = flag_for_removal | 
          (fcomp_salt_wt_pct == 20 & date_casted == as.Date('2024-09-23'))
      )
    cat(sprintf("✓ Flagged specific sample (20%% salt on 2024-09-23): %d (cumulative)\n", 
                sum(df$flag_for_removal, na.rm = TRUE)))
  }
  
  return(df)
}

#' Flag problematic fabrication methods
flag_fab_method <- function(df) {
  if (!"fab_method" %in% names(df)) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | (fab_method == 3)
    )
  
  cat(sprintf("✓ Flagged fab_method 3: %d (cumulative)\n", 
              sum(df$flag_for_removal, na.rm = TRUE)))
  
  return(df)
}

#' Flag problematic formulation types
flag_formulation_types <- function(df) {
  if (!"fcomp_formulation" %in% names(df)) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | 
        stringr::str_detect(fcomp_formulation, 'Thiolene|PEO')
    )
  
  cat(sprintf("✓ Flagged Thiolene/PEO formulations: %d (cumulative)\n", 
              sum(df$flag_for_removal, na.rm = TRUE)))
  
  return(df)
}

#' Flag problematic additive types
flag_additive_types <- function(df) {
  if (!"fcomp_additive" %in% names(df)) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | (fcomp_additive == 'AlO3')
    )
  
  cat(sprintf("✓ Flagged AlO3 additive: %d (cumulative)\n", 
              sum(df$flag_for_removal, na.rm = TRUE)))
  
  return(df)
}

#' Flag samples outside acceptable IC thresholds
flag_ic_thresholds <- function(df) {
  required_cols <- c("ionic_conductivity_final", "fcomp_salt_wt_pct", "fcomp_salt")
  if (!all(required_cols %in% names(df))) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal |
        # Low IC for samples with salt
        (fcomp_salt_wt_pct != 0 & ionic_conductivity_final < 10^-7) |
        # High IC for all samples
        (ionic_conductivity_final > 10^-3) |
        # High IC for no-salt samples
        (fcomp_salt == "none" & ionic_conductivity_final > 10^-5)
    )
  
  cat(sprintf("✓ Flagged IC threshold violations: %d (cumulative)\n", 
              sum(df$flag_for_removal, na.rm = TRUE)))
  
  return(df)
}

#' Flag statistical outliers using z-score
flag_zscore_outliers <- function(df, 
                                 threshold = 4,
                                 group_vars = c("fcomp_formulation", "print_type"),
                                 value_col = "ionic_conductivity_final") {
  
  # Check required columns
  required_cols <- c(value_col, group_vars[1])
  if (!all(required_cols %in% names(df))) {
    cat("ℹ Required columns not found for z-score flagging\n")
    return(df)
  }
  
  # Ensure print_type exists
  if (!"print_type" %in% names(df)) {
    cat("ℹ print_type column not found for z-score grouping\n")
    return(df)
  }
  
  cat(sprintf("\nApplying z-score outlier detection (%.0fσ)...\n", threshold))
  cat(sprintf("Grouping by: %s\n", paste(group_vars, collapse = ", ")))
  
  n_before <- sum(df$flag_for_removal, na.rm = TRUE)
  
  # Separate already-flagged from clean data
  df_clean <- df %>% dplyr::filter(flag_for_removal == FALSE)
  df_flagged <- df %>% dplyr::filter(flag_for_removal == TRUE)
  
  # Apply z-score to clean data only
  df_clean <- df_clean %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::mutate(
      log_ic = log10(.data[[value_col]]),
      z_score = (log_ic - mean(log_ic, na.rm = TRUE)) / stats::sd(log_ic, na.rm = TRUE),
      flag_for_removal = flag_for_removal | (abs(z_score) >= threshold)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-z_score, -log_ic)
  
  # Recombine
  df <- dplyr::bind_rows(df_clean, df_flagged)
  
  n_after <- sum(df$flag_for_removal, na.rm = TRUE)
  n_new <- n_after - n_before
  
  cat(sprintf("✓ Z-score flagging: %d new flags, %d total (cumulative)\n", n_new, n_after))
  
  return(df)
}

# =============================================================================
# DATA EXPORT FUNCTIONS
# =============================================================================

#' Save plotting dataset (all data with flags, no filtering)
#' @param df Dataframe with flag_for_removal column
#' @param output_path Path to save plotting CSV
#' @return Invisible TRUE if saved successfully
save_plotting_data <- function(df, output_path) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING PLOTTING DATASET (RAW DATA + FLAGS)\n")
  cat(strrep("=", 70), "\n\n")
  
  # Select columns for plotting - NO FILTERING
  selected_cols <- c(
    "sample_ID",
    "ionic_conductivity_final",
    "fcomp_salt",
    "fcomp_additive",
    "fcomp_salt_wt_pct",
    "fcomp_additive_wt_pct",
    "fcomp_formulation",
    "fcomp_mat1",
    "fcomp_mat2",
    "fcomp_mat1_ratio",
    "fcomp_mat2_ratio",
    "print_type",
    "cast_type",
    "date_casted",
    "fab_method",
    "area_cm2",
    "spe_thickness_avg",
    "flag_for_removal"
  )
  
  selected_cols <- selected_cols[selected_cols %in% names(df)]
  
  df_output <- df %>%
    dplyr::select(dplyr::all_of(selected_cols))
  
  # Save to CSV
  readr::write_csv(df_output, output_path)
  
  cat(sprintf("Saved plotting data to: %s\n", output_path))
  cat(sprintf("  Total rows: %d\n", nrow(df_output)))
  cat(sprintf("  Total columns: %d\n", ncol(df_output)))
  cat(sprintf("  Flagged: %d (%.1f%%)\n", 
              sum(df_output$flag_for_removal),
              100 * sum(df_output$flag_for_removal) / nrow(df_output)))
  cat(sprintf("  Clean: %d (%.1f%%)\n", 
              sum(!df_output$flag_for_removal),
              100 * sum(!df_output$flag_for_removal) / nrow(df_output)))
  
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

#' Save ML dataset (aggregated, clean data only)
#' @param df Dataframe with flag_for_removal column
#' @param output_path Path to save ML CSV
#' @param salt_filter Vector of salt values to keep
#' @param additive_filter Vector of additive values to keep
#' @param print_type_filter Print type to keep
#' @return Invisible TRUE if saved successfully
save_ml_data <- function(df, output_path,
                         salt_filter = c("LiTFSI", "none"),
                         additive_filter = c("Aerosil 380", "none"),
                         print_type_filter = "PCB print") {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING ML DATASET\n")
  cat(strrep("=", 70), "\n\n")
  
  # Apply composition filters
  df_filtered <- df %>%
    dplyr::filter(
      fcomp_salt %in% salt_filter &
        fcomp_additive %in% additive_filter &
        print_type %in% print_type_filter
    )
  
  cat(sprintf("After composition filters: %d rows\n", nrow(df_filtered)))
  
  # Remove flagged samples
  if ("flag_for_removal" %in% names(df_filtered)) {
    n_before <- nrow(df_filtered)
    df_filtered <- df_filtered %>%
      dplyr::filter(flag_for_removal == FALSE)
    cat(sprintf("After removing flagged: %d rows (removed %d)\n", 
                nrow(df_filtered), n_before - nrow(df_filtered)))
  }
  
  # Aggregate by composition
  cat("\nAggregating by composition groups...\n")
  n_before <- nrow(df_filtered)
  
  df_ml <- df_filtered %>%
    dplyr::group_by(
      fcomp_salt,
      fcomp_additive,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      print_type
    ) %>%
    dplyr::summarize(
      ionic_conductivity_final = mean(ionic_conductivity_final, na.rm = TRUE),
      n_measurements = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # One-hot encode categorical variables
      material_LiTFSI = dplyr::if_else(fcomp_salt == 'LiTFSI', 1.0, 0.0),
      material_none = dplyr::if_else(fcomp_salt == 'none', 1.0, 0.0),
      print_type_PCB = dplyr::if_else(print_type == 'PCB print', 1.0, 0.0)
    ) %>%
    dplyr::select(
      print_type,
      fcomp_salt,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final,
      n_measurements,
      material_LiTFSI,
      material_none,
      print_type_PCB
    )
  
  cat(sprintf("Aggregated to %d groups (from %d measurements)\n", 
              nrow(df_ml), n_before))
  
  # Save to CSV
  readr::write_csv(df_ml, output_path)
  
  cat(sprintf("\nSaved ML dataset to: %s\n", output_path))
  cat(sprintf("  Total groups: %d\n", nrow(df_ml)))
  cat(sprintf("  Features: %d\n", ncol(df_ml) - 1))
  
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Summarize flagging statistics
#' @param df Dataframe with flag_for_removal column
#' @return Summary dataframe
summarize_flags <- function(df) {
  if (!"flag_for_removal" %in% names(df)) {
    cat("Warning: flag_for_removal column not found\n")
    return(NULL)
  }
  
  summary_df <- data.frame(
    total_samples = nrow(df),
    flagged = sum(df$flag_for_removal, na.rm = TRUE),
    clean = sum(!df$flag_for_removal, na.rm = TRUE),
    pct_flagged = 100 * sum(df$flag_for_removal, na.rm = TRUE) / nrow(df)
  )
  
  # Add breakdown by type if available
  if ("print_type" %in% names(df)) {
    by_type <- df %>%
      dplyr::group_by(print_type) %>%
      dplyr::summarise(
        total = dplyr::n(),
        flagged = sum(flag_for_removal, na.rm = TRUE),
        clean = sum(!flag_for_removal, na.rm = TRUE),
        pct_flagged = 100 * sum(flag_for_removal, na.rm = TRUE) / dplyr::n(),
        .groups = 'drop'
      )
    
    return(list(overall = summary_df, by_type = by_type))
  }
  
  return(summary_df)
}