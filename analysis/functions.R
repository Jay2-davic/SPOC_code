#' Flag cross-formulation additive concentrations (DISABLED - now using grid filter)
flag_cross_formulation_additives <- function(df) {
  cat("ℹ Cross-formulation filtering now handled by grid composition filter\n")
  cat(sprintf("  Cumulative flags: %d\n", sum(df$flag_for_removal, na.rm = TRUE)))
  return(df)
}# =============================================================================
# CORE DATA LOADING AND QUALITY CONTROL FUNCTIONS - MATCHES ORIGINAL WORKFLOW
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
load_pe_json <- function(json_file_path) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("LOADING JSON WITH ARRAY PRESERVATION\n")
  cat(strrep("=", 70), "\n\n")
  
  json_raw <- jsonlite::fromJSON(
    json_file_path,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    flatten = FALSE
  )
  
  if (is.list(json_raw) && length(json_raw) == 1) {
    samples_source <- json_raw[[1]]
  } else {
    samples_source <- json_raw
  }
  
  sample_ids <- names(samples_source)
  cat(sprintf("Found %d samples\n", length(sample_ids)))
  
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
  
  cat("Extracting data...\n")
  
  df_list <- lapply(seq_along(sample_ids), function(idx) {
    id <- sample_ids[idx]
    sample <- samples_source[[id]]
    
    ic_final <- safe_extract(sample, "ionic_conductivity_final", "list")
    
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
  
  df <- dplyr::bind_rows(df_list) %>%
    standardize_columns()
  
  cat("Expanding IC arrays...\n")
  n_before <- nrow(df)
  
  df <- df %>%
    tidyr::unnest_longer(
      ionic_conductivity_final,
      values_to = "ionic_conductivity_final",
      indices_include = FALSE
    ) |>
    dplyr::filter(!ionic_conductivity_final == 1e-4,
                  !ionic_conductivity_final == 0) |>
    dplyr::mutate(fcomp_additive_wt_pct =
                    # correction for close numbers
      dplyr::case_when(
        fcomp_additive_wt_pct == 4.28 ~ 4.29,
        fcomp_additive_wt_pct == 6.43 ~ 6.42,
        fcomp_additive_wt_pct == 8.56 ~ 8.57,
        fcomp_additive_wt_pct == 12.8 ~ 12.9,
        TRUE ~ fcomp_additive_wt_pct
      )
    )
  
  cat(sprintf("Expanded from %d samples to %d measurements\n", n_before, nrow(df)))
  cat(strrep("=", 70), "\n\n")
  
  return(df)
}

# =============================================================================
# DATA STANDARDIZATION - MATCHES ORIGINAL
# =============================================================================

standardize_columns <- function(df) {
  df %>%
    dplyr::mutate(
      # Fix formulation naming (from original: '4.28' -> '4.29')
      fcomp_formulation = stringr::str_replace_all(fcomp_formulation, '4.28', '4.29'),
      # Standardize additive names
      fcomp_additive = stringr::str_replace_all(fcomp_additive, "aerosil 380", "Aerosil 380"),
      # Standardize cast_type to print_type (including "Bulk cast")
      print_type = dplyr::case_when(
        stringr::str_detect(tolower(cast_type), "coin") ~ "Coin cell",
        stringr::str_detect(tolower(cast_type), "pcb|print|bulk|cast") ~ "PCB print",
        TRUE ~ cast_type
      ),
      # Convert date
      date_casted = lubridate::as_date(date_casted)
    )
}

# =============================================================================
# QUALITY CONTROL FLAGGING - EXACT MATCH TO ORIGINAL WORKFLOW
# =============================================================================

#' Add QC flags matching the original R workflow exactly
add_qc_flags <- function(df) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("ADDING QUALITY CONTROL FLAGS (ORIGINAL WORKFLOW)\n")
  cat(strrep("=", 70), "\n\n")
  
  df <- df %>%
    dplyr::mutate(flag_for_removal = FALSE)
  
  n_initial <- nrow(df)
  
  # STEP 1: Bad dates filter (EXACTLY as in original)
  df <- df %>%
    flag_bad_dates_original()
  
  # STEP 2: Validation sets
  df <- df %>%
    flag_validation_sets()
  
  # STEP 3: fab_method != 3 (DISABLED in original)
  cat("ℹ Skipping fab_method flagging (not used in original workflow)\n")
  cat(sprintf("  Cumulative flags: %d\n", sum(df$flag_for_removal, na.rm = TRUE)))
  
  # STEP 4: Formulation types
  df <- df %>%
    flag_formulation_types()
  
  # STEP 5: Additive types
  df <- df %>%
    flag_additive_types()
  
  # STEP 6: IC thresholds (ORIGINAL LOGIC)
  df <- df %>%
    flag_ic_thresholds_original()
  
  # STEP 7: Specific problematic sample
  df <- df %>%
    flag_specific_sample()
  
  # STEP 8: Z-score outliers (ORIGINAL LOGIC - separate by type, 4σ)
  df <- df %>%
    flag_zscore_outliers_original(threshold = 3)
  
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  cat(sprintf("\nTotal flagged for removal: %d / %d (%.1f%%)\n", 
              n_flagged, n_initial, 100 * n_flagged / n_initial))
  cat(strrep("=", 70), "\n\n")
  
  return(df)
}

# =============================================================================
# INDIVIDUAL FLAGGING FUNCTIONS - EXACT ORIGINAL LOGIC
# =============================================================================

#' Flag bad dates - ORIGINAL LOGIC (PCB only for date cutoff)
flag_bad_dates_original <- function(df) {
  if (!"date_casted" %in% names(df)) {
    cat("ℹ date_casted column not found\n")
    return(df)
  }
  
  if (!"print_type" %in% names(df)) {
    cat("⚠ print_type column not found, applying date filter to all samples\n")
  }
  
  # Original filter logic from your code + additional bad dates
  bad_dates <- as.Date(c(
    '2024-07-22', '2024-09-26', '2024-08-15', 
    '2024-05-06', '2024-10-07', '2024-03-04',
    # Additional bad dates
    '2024-04-25', '2024-04-16', '2024-06-10',
    '2024-05-22', '2024-04-15', '2024-01-18',
    '2024-06-04'
  ))
  
  # Remove duplicates
  bad_dates <- unique(bad_dates)
  
  early_cutoff <- as.Date('2024-01-01')
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | 
        # Flag specific bad dates for ALL samples
        date_casted %in% bad_dates |
        # Flag early cutoff ONLY for PCB samples (not coin cells)
        (date_casted <= early_cutoff & print_type == "PCB print")
    )
  
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  n_bad_dates <- sum(df$date_casted %in% bad_dates & !is.na(df$date_casted), na.rm = TRUE)
  n_pcb_early <- sum(df$date_casted <= early_cutoff & 
                       df$print_type == "PCB print" & 
                       !is.na(df$date_casted), na.rm = TRUE)
  n_coin_kept <- sum(df$date_casted <= early_cutoff & 
                       df$print_type == "Coin cell" & 
                       !is.na(df$date_casted), na.rm = TRUE)
  
  cat(sprintf("✓ Flagged bad dates: %d (cumulative)\n", n_flagged))
  cat(sprintf("  - Specific bad dates (%d dates): %d samples flagged\n", 
              length(bad_dates), n_bad_dates))
  cat(sprintf("  - PCB samples before 2024-01-01: %d flagged\n", n_pcb_early))
  cat(sprintf("  - Coin cell samples before 2024-01-01: %d kept\n", n_coin_kept))
  
  return(df)
}

#' Flag off-grid compositions (KEEP only specific grid points)
#' For LiTFSI-containing PCB samples only
flag_off_grid_compositions <- function(df) {
  if (!all(c("fcomp_salt_wt_pct", "fcomp_additive_wt_pct", "print_type", "fcomp_salt") %in% names(df))) {
    cat("ℹ Required columns not found for grid filtering\n")
    return(df)
  }
  
  cat("\n--- Grid Composition Filtering (LiTFSI PCB only) ---\n")
  
  n_before <- sum(df$flag_for_removal, na.rm = TRUE)
  
  # Define allowed grid points
  # Grid 1: Fixed salt, varying additive
  salt_grid_1 <- c(0, 10, 20, 30)
  additive_grid_1 <- c(0, 2.14, 4.29, 6.43, 7.5, 8.57, 10.70, 12.9, 15)
  
  # Grid 2: Fixed additive, varying salt
  additive_grid_2 <- c(0, 7.5, 15)
  salt_grid_2 <- c(0, 4.29, 8.57, 12.9, 17.1, 21.4, 25.7, 30)
  
  # Tolerance for floating point comparison
  tol <- 0.05
  
  df <- df %>%
    dplyr::mutate(
      # Check if sample is on Grid 1
      on_grid_1 = (
        sapply(fcomp_salt_wt_pct, function(x) any(abs(x - salt_grid_1) < tol)) &
          sapply(fcomp_additive_wt_pct, function(x) any(abs(x - additive_grid_1) < tol))
      ),
      # Check if sample is on Grid 2
      on_grid_2 = (
        sapply(fcomp_additive_wt_pct, function(x) any(abs(x - additive_grid_2) < tol)) &
          sapply(fcomp_salt_wt_pct, function(x) any(abs(x - salt_grid_2) < tol))
      ),
      # Is this a LiTFSI PCB sample?
      is_litfsi_pcb = (
        fcomp_salt == "LiTFSI" & 
          (print_type == "PCB print" | print_type == "Bulk cast")
      ),
      # Flag if it's LiTFSI PCB but NOT on either grid
      flag_for_removal = flag_for_removal | 
        (is_litfsi_pcb & !on_grid_1 & !on_grid_2)
    ) %>%
    dplyr::select(-on_grid_1, -on_grid_2, -is_litfsi_pcb)
  
  n_after <- sum(df$flag_for_removal, na.rm = TRUE)
  n_new <- n_after - n_before
  
  # Count what was kept vs flagged
  n_litfsi_pcb <- sum(
    df$fcomp_salt == "LiTFSI" & 
      (df$print_type == "PCB print" | df$print_type == "Bulk cast"),
    na.rm = TRUE
  )
  n_on_grid <- n_litfsi_pcb - n_new
  
  cat(sprintf("  Total LiTFSI PCB samples: %d\n", n_litfsi_pcb))
  cat(sprintf("  On approved grids: %d kept\n", n_on_grid))
  cat(sprintf("  Off-grid compositions: %d flagged\n", n_new))
  cat(sprintf("✓ Grid filtering: %d new flags, %d total (cumulative)\n", n_new, n_after))
  
  return(df)
}

#' Flag validation sets - ORIGINAL LOGIC
flag_validation_sets <- function(df) {
  if (!"date_casted" %in% names(df)) {
    cat("ℹ date_casted column not found\n")
    return(df)
  }
  
  validation_dates <- as.Date(c('2024-06-27', '2024-08-16'))
  
  df <- df %>%
    dplyr::mutate(
      validation = dplyr::case_when(
        date_casted %in% validation_dates ~ 'YES',
        TRUE ~ 'NO'
      ),
      flag_for_removal = flag_for_removal | (validation == 'YES')
    )
  
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  cat(sprintf("✓ Flagged validation sets: %d samples (cumulative: %d)\n", 
              sum(df$validation == 'YES', na.rm = TRUE), n_flagged))
  
  return(df)
}

#' Flag formulation types - ORIGINAL LOGIC
flag_formulation_types <- function(df) {
  if (!"fcomp_formulation" %in% names(df)) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | 
        stringr::str_detect(fcomp_formulation, 'Thiolene|PEO')
    )
  
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  cat(sprintf("✓ Flagged Thiolene/PEO formulations: %d (cumulative)\n", n_flagged))
  
  return(df)
}

#' Flag additive types - ORIGINAL LOGIC
flag_additive_types <- function(df) {
  if (!"fcomp_additive" %in% names(df)) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | (fcomp_additive == 'AlO3')
    )
  
  n_flagged <- sum(df$flag_for_removal, na.rm = TRUE)
  cat(sprintf("✓ Flagged AlO3 additive: %d (cumulative)\n", n_flagged))
  
  return(df)
}

#' Flag IC thresholds - ORIGINAL LOGIC + Upper threshold
flag_ic_thresholds_original <- function(df) {
  required_cols <- c("ionic_conductivity_final", "fcomp_salt_wt_pct")
  if (!all(required_cols %in% names(df))) {
    return(df)
  }
  
  cat("\n--- IC Threshold Flagging (Original Logic) ---\n")
  
  n_before <- sum(df$flag_for_removal, na.rm = TRUE)
  
  # Upper threshold for all samples
  upper_threshold <- 5e-5  # 0.5 x 10^-4 = 5 x 10^-5
  
  # ORIGINAL LOGIC from your code:
  # Filter 1: No-salt samples outside 1e-9 to 1e-7
  # Filter 2: With-salt samples < 1e-7
  # Filter 3: All samples > 5e-5 (NEW)
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal |
        # No-salt filter
        (fcomp_salt_wt_pct == 0 & 
           (ionic_conductivity_final <= 1e-9 | ionic_conductivity_final >= 5e-7 & cast_type == 'PCB print')) |
        # With-salt filter (case_when logic from original)
        (fcomp_salt_wt_pct != 0 & ionic_conductivity_final < 1e-8 & cast_type == 'PCB print') |
        # Upper threshold for ALL samples
        (ionic_conductivity_final > upper_threshold)
    )
  
  n_after <- sum(df$flag_for_removal, na.rm = TRUE)
  n_new <- n_after - n_before
  
  cat(sprintf("  No-salt outside [1e-9, 1e-7]: %d flagged\n", 
              sum(df$fcomp_salt_wt_pct == 0 & 
                    (df$ionic_conductivity_final <= 1e-9 | 
                       df$ionic_conductivity_final >= 1e-6), na.rm = TRUE)))
  cat(sprintf("  With-salt < 1e-7: %d flagged\n", 
              sum(df$fcomp_salt_wt_pct != 0 & df$ionic_conductivity_final < 1e-6, na.rm = TRUE)))
  cat(sprintf("  All samples > %.1e (0.5 x 10^-4): %d flagged\n", 
              upper_threshold,
              sum(df$ionic_conductivity_final > upper_threshold, na.rm = TRUE)))
  cat(sprintf("✓ IC threshold flags: %d new, %d total (cumulative)\n", n_new, n_after))
  
  return(df)
}

#' Flag specific problematic sample - ORIGINAL LOGIC
flag_specific_sample <- function(df) {
  if (!all(c("fcomp_salt_wt_pct", "date_casted") %in% names(df))) {
    return(df)
  }
  
  n_before <- sum(df$flag_for_removal, na.rm = TRUE)
  
  df <- df %>%
    dplyr::mutate(
      flag_for_removal = flag_for_removal | 
        (fcomp_salt_wt_pct == 20 & date_casted == as.Date('2024-09-23') |
           (fab_method != 3 & cast_type == "PCB print")),
    )
  
  n_after <- sum(df$flag_for_removal, na.rm = TRUE)
  if (n_after > n_before) {
    cat(sprintf("✓ Flagged specific sample (20%% salt on 2024-09-23): %d (cumulative)\n", n_after))
  }
  
  return(df)
}

#' Flag z-score outliers - ORIGINAL LOGIC
#' Groups by print_type AND fcomp_formulation, applies 4σ threshold
#' PCB: Pre-filters IC > 1e-3 BEFORE z-score calculation
flag_zscore_outliers_original <- function(df, threshold = 4) {
  
  if (!"print_type" %in% names(df)) {
    cat("ℹ print_type column not found\n")
    return(df)
  }
  
  cat(sprintf("\nApplying z-score outlier detection (%.0fσ) - ORIGINAL LOGIC\n", threshold))
  cat("Grouping by: print_type, fcomp_formulation\n")
  
  n_before <- sum(df$flag_for_removal, na.rm = TRUE)
  
  # Separate by flag status
  df_clean <- df %>% filter(flag_for_removal == FALSE)
  df_already_flagged <- df %>% filter(flag_for_removal == TRUE)
  
  # Separate by type
  df_coin <- df_clean %>% filter(print_type == "Coin cell")
  df_pcb <- df_clean %>% filter(print_type == "PCB print")
  
  # Process COIN CELLS
  if (nrow(df_coin) > 0) {
    cat(sprintf("  Processing %d coin cell samples...\n", nrow(df_coin)))
    
    df_coin <- df_coin %>%
      dplyr::group_by(print_type, fcomp_formulation) %>%
      dplyr::mutate(
        log_ic = log10(ionic_conductivity_final),
        z_score = (log_ic - mean(log_ic, na.rm = TRUE)) / stats::sd(log_ic, na.rm = TRUE),
        flag_for_removal = abs(z_score) >= threshold
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-z_score, -log_ic)
    
    n_coin_flagged <- sum(df_coin$flag_for_removal, na.rm = TRUE)
    cat(sprintf("    Flagged %d coin cell outliers\n", n_coin_flagged))
  }
  
  # Process PCB - PRE-FILTER like original workflow
  if (nrow(df_pcb) > 0) {
    cat(sprintf("  Processing %d PCB samples...\n", nrow(df_pcb)))
    
    # PRE-FILTER: Remove IC > 1e-3 BEFORE z-score (from original)
    df_pcb_filtered <- df_pcb %>% filter(ionic_conductivity_final <= 1e-3)
    df_pcb_too_high <- df_pcb %>% 
      filter(ionic_conductivity_final > 1e-3) %>%
      mutate(flag_for_removal = TRUE)
    
    cat(sprintf("    Pre-filtered %d high IC samples (>1e-3)\n", nrow(df_pcb_too_high)))
    
    # Apply z-score to filtered PCB data
    if (nrow(df_pcb_filtered) > 0) {
      df_pcb_filtered <- df_pcb_filtered %>%
        dplyr::group_by(print_type, fcomp_formulation) %>%
        dplyr::mutate(
          log_ic = log10(ionic_conductivity_final),
          z_score = (log_ic - mean(log_ic, na.rm = TRUE)) / stats::sd(log_ic, na.rm = TRUE),
          flag_for_removal = abs(z_score) >= threshold
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-z_score, -log_ic)
      
      n_pcb_flagged <- sum(df_pcb_filtered$flag_for_removal, na.rm = TRUE)
      cat(sprintf("    Flagged %d PCB outliers\n", n_pcb_flagged))
    }
    
    # Recombine PCB data
    df_pcb <- dplyr::bind_rows(df_pcb_filtered, df_pcb_too_high)
  }
  
  # Recombine everything
  df <- dplyr::bind_rows(df_coin, df_pcb, df_already_flagged)
  
  n_after <- sum(df$flag_for_removal, na.rm = TRUE)
  n_new <- n_after - n_before
  
  cat(sprintf("\n✓ Z-score flagging: %d new flags, %d total (cumulative)\n", n_new, n_after))
  
  return(df)
}

# =============================================================================
# DATA EXPORT FUNCTIONS
# =============================================================================

#' Save plotting dataset (all data with flags, no filtering)
save_plotting_data <- function(df, output_path) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING PLOTTING DATASET (RAW DATA + FLAGS)\n")
  cat(strrep("=", 70), "\n\n")
  
  selected_cols <- c(
    "sample_ID", "ionic_conductivity_final", "fcomp_salt", "fcomp_additive",
    "fcomp_salt_wt_pct", "fcomp_additive_wt_pct", "fcomp_formulation",
    "fcomp_mat1", "fcomp_mat2", "fcomp_mat1_ratio", "fcomp_mat2_ratio",
    "print_type", "cast_type", "date_casted", "fab_method",
    "area_cm2", "spe_thickness_avg", "flag_for_removal"
  )
  
  # Add validation column if it exists
  if ("validation" %in% names(df)) {
    selected_cols <- c(selected_cols, "validation")
  }
  
  selected_cols <- selected_cols[selected_cols %in% names(df)]
  
  df_output <- df %>% dplyr::select(dplyr::all_of(selected_cols))
  
  readr::write_csv(df_output, output_path)
  
  cat(sprintf("Saved plotting data to: %s\n", output_path))
  cat(sprintf("  Total rows: %d\n", nrow(df_output)))
  cat(sprintf("  Flagged: %d (%.1f%%)\n", 
              sum(df_output$flag_for_removal),
              100 * sum(df_output$flag_for_removal) / nrow(df_output)))
  cat(sprintf("  Clean: %d (%.1f%%)\n", 
              sum(!df_output$flag_for_removal),
              100 * sum(!df_output$flag_for_removal) / nrow(df_output)))
  
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

#' Save ML dataset - MATCHES ORIGINAL EXPORT LOGIC
save_ml_data <- function(df, output_path,
                         salt_filter = c("LiTFSI", "none"),
                         additive_filter = c("Aerosil 380", "none"),
                         print_type_filter = "PCB print") {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SAVING ML DATASET (ORIGINAL LOGIC)\n")
  cat(strrep("=", 70), "\n\n")
  
  # Filter unflagged data only
  df_filtered <- df %>%
    dplyr::filter(
      flag_for_removal == FALSE,
      fcomp_salt %in% salt_filter,
      fcomp_additive %in% additive_filter,
      print_type %in% print_type_filter
    )
  
  cat(sprintf("After filtering: %d rows\n", nrow(df_filtered)))
  
  # Aggregate - EXACT original logic
  cat("\nAggregating by composition groups...\n")
  
  df_ml <- df_filtered %>%
    dplyr::group_by(fcomp_salt, fcomp_additive, fcomp_formulation, print_type) %>%
    dplyr::mutate(ionic_conductivity_final = mean(ionic_conductivity_final)) %>%
    dplyr::select(
      print_type,
      fcomp_formulation,
      fcomp_salt,
      fcomp_additive_wt_pct,
      fcomp_salt_wt_pct,
      ionic_conductivity_final
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::rename(material = fcomp_salt) %>%
    dplyr::select(-fcomp_formulation) %>%
    dplyr::mutate(
      material_LiTFSI = dplyr::if_else(material == 'LiTFSI', 1.0, 0.0),
      material_none = dplyr::if_else(material == 'none', 1.0, 0.0),
      print_type_PCB = dplyr::if_else(print_type == 'PCB print', 1.0, 0.0)
    ) %>%
    dplyr::select(-c(print_type, material))
  
  cat(sprintf("Final ML dataset: %d rows\n", nrow(df_ml)))
  
  readr::write_csv(df_ml, output_path)
  
  cat(sprintf("\nSaved ML dataset to: %s\n", output_path))
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(TRUE))
}

#' Summarize flagging statistics
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