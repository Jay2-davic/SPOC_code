suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(stringi)
  library(glue)
  library(jsonlite)
})

# --- Utility: Clean NA and blank strings ---
#' Clean NA-like strings in character columns
#'
#' - Converts "NA", "NaN", "null", "NULL", and blank strings to NA
#'
#' @param df data.frame
#' @return data.frame
clean_na_strings <- function(df) {
  df %>%
    mutate(across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[x %in% c("NA", "NaN", "null", "NULL", "")] <- NA_character_
        x
      }
    ))
}

# --- Main cleaning function for each file ---

# Drop columns with NA or "" names, and placeholder columns like "...65"
drop_bad_named_cols <- function(df, verbose = TRUE) {
  stopifnot(is.data.frame(df))
  nm <- names(df)
  bad <- is.na(nm) | nm == "" | grepl("^\\.\\.\\.\\d+$", nm)
  if (verbose && any(bad)) {
    message("Dropping columns with empty or placeholder names: ",
            paste(nm[bad], collapse = ", "))
  }
  df[, !bad, drop = FALSE]
}

.coalesce_cols_into_first <- function(df, cols) {
  # Extract vectors
  x_list <- lapply(cols, function(cn) df[[cn]])
  # Start with the first
  out <- x_list[[1]]
  
  # Sequentially fill NA from each next column
  if (length(x_list) > 1L) {
    for (j in 2:length(x_list)) {
      # compute a common type and cast
      ptype <- vctrs::vec_ptype_common(out, x_list[[j]])
      out <- vctrs::vec_cast(out, ptype)
      nxt <- vctrs::vec_cast(x_list[[j]], ptype)
      # fill NA in out from nxt
      idx <- is.na(out) & !is.na(nxt)
      if (length(idx) && any(idx)) out[idx] <- nxt[idx]
    }
  }
  df[[cols[1]]] <- out
  # Drop the rest
  if (length(cols) > 1L) {
    for (c in cols[-1]) df[[c]] <- NULL
  }
  df
}

# Coalesce duplicate base names like "sample_ID...2" + "sample_ID...48" -> "sample_ID"
# Keeps the left-most non-missing value row-wise, then drops the suffixed copies.
coalesce_duplicate_names_inplace <- function(
    df,
    pattern = "\\.{3}\\d+$",
    verbose = TRUE,
    custom_order = NULL,
    rename_bases = NULL
) {
  stopifnot(is.data.frame(df))
  nm   <- names(df)
  base <- sub(pattern, "", nm)
  groups <- split(nm, base)
  
  for (b in names(groups)) {
    cols <- groups[[b]]
    if (length(cols) > 1L) {
      # priority: custom > default suffix ordering
      if (!is.null(custom_order) && !is.null(custom_order[[b]])) {
        want <- custom_order[[b]]
        pos <- match(cols, want)
        pos[is.na(pos)] <- max(c(pos, 0), na.rm = TRUE) + seq_len(sum(is.na(pos)))
        cols <- cols[order(pos)]
      } else {
        # default: numbered variants first, base (no suffix) last as fallback
        get_suffix_num <- function(cn) {
          m <- regexpr("(?:\\.{3}|_)\\d+$", cn, perl = TRUE)
          if (m[1] == -1) Inf else as.integer(sub(".*(\\d+)$", "\\1", cn))
        }
        ordnum <- vapply(cols, get_suffix_num, numeric(1))
        cols <- cols[order(ordnum, seq_along(cols))]
      }
      
      if (verbose) {
        message("Coalescing duplicates for '", b, "' using order: ",
                paste(cols, collapse = " > "))
      }
      
      # fill left-to-right, never overwrite non-NA in earlier columns
      out <- df[[cols[1]]]
      if (length(cols) > 1L) {
        for (c in cols[-1]) {
          nxt <- df[[c]]
          idx <- is.na(out) & !is.na(nxt)
          if (any(idx)) out[idx] <- nxt[idx]
        }
      }
      df[[cols[1]]] <- out
      for (c in cols[-1]) df[[c]] <- NULL
      
      # targeted rename to base if requested
      if (!is.null(rename_bases) && b %in% rename_bases) {
        names(df)[names(df) == cols[1]] <- b
      }
    }
  }
  df
}
drop_patterns <- paste(
  "(?i)^\\.?row_?order$",            # .row_order or row_order
  "(?i)^test_?number$",              # test_number or test number
  "(?i)^sample_?number$",            # keep if you still want to remove sample number as well
  "(?i)^electrolyte(?:[_\\. ]?\\d+[a-z]?)$",  # electrolyte1, electrolyte1a, electrolyte_2b, etc.
  "(?i)^wt[_\\. ]?pct(?:[_\\. ]?\\d+[a-z]?)$",# wt_pct1, wt_pct_2a, wt.pct3, etc.
  sep = "|"
)

# Exact token NA normalization, never substring matches like "NaTFSI"
normalize_na_tokens <- function(x) {
  if (!is.character(x)) return(x)
  y <- trimws(x)
  tokens <- c("", "-", "NA", "N/A", "#N/A", "na", "n/a", "NaN", "nan", "NULL", "null")
  y[y %in% tokens] <- NA_character_
  y
}

poly3_cleaning <- function(df,
                           temp_unit = c("C","F"),
                           humidity_scale = c("percent","fraction")) {
  temp_unit <- match.arg(temp_unit)
  humidity_scale <- match.arg(humidity_scale)
  
  df %>%
    tibble::as_tibble() %>%
    drop_bad_named_cols(verbose = TRUE) %>%
    # Pre-clean coalescing of readxl ...<n> dupes
    coalesce_duplicate_names_inplace(
      pattern      = "\\.{3}\\d+$",
      verbose      = TRUE,
      rename_bases = c("sample_ID")
    ) %>%
    janitor::clean_names() %>%  # normalize names: .row_order -> row_order, sample number -> sample_number
    
    # Remove unwanted columns in one go
    dplyr::select(-tidyselect::matches(drop_patterns, perl = TRUE)) %>%
    
    # Optional: second coalescing pass for _<n> style
    coalesce_duplicate_names_inplace(
      pattern      = "_\\d+$",
      verbose      = TRUE,
      custom_order = list(additive = c("additive_1", "additive_2", "additive")),
      rename_bases = c("additive")
    ) %>%
    # Normalize NA-like tokens
    dplyr::mutate(dplyr::across(where(is.character), normalize_na_tokens)) %>%
    # Combine temperature and humidity, drop sources
    { combine_env(., temp_unit = temp_unit, humidity_scale = humidity_scale, drop_sources = TRUE) } %>%
    # Drop columns that are all NA
    dplyr::select(where(~ any(!is.na(.))))
}

combine_env <- function(df,
                        temp_unit = c("C", "F"),
                        humidity_scale = c("percent", "fraction"),
                        drop_sources = TRUE) {
  temp_unit <- match.arg(temp_unit)
  humidity_scale <- match.arg(humidity_scale)
  
  has <- function(nm) nm %in% names(df)
  
  # Parse numerics safely, tolerate strings like "72 F" or "45%"
  parse_num <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x)) return(x)
    if (!requireNamespace("readr", quietly = TRUE)) {
      suppressWarnings(as.numeric(x))
    } else {
      readr::parse_number(x)
    }
  }
  
  # Temperature
  t  <- if (has("temperature"))   parse_num(df$temperature)   else NULL
  tf <- if (has("temperature_f")) parse_num(df$temperature_f) else NULL
  
  # Convert temperature to requested unit
  if (!is.null(tf) && temp_unit == "C") tf <- (tf - 32) * 5/9
  if (!is.null(t)  && temp_unit == "F") t  <- t * 9/5 + 32
  
  # Coalesce temperature
  df$temperature <- if (!is.null(t) && !is.null(tf)) {
    dplyr::coalesce(t, tf)
  } else if (!is.null(t)) {
    t
  } else if (!is.null(tf)) {
    tf
  } else {
    df$temperature
  }
  
  # Humidity
  h  <- if (has("humidity"))          parse_num(df$humidity)          else NULL
  hp <- if (has("humidity_percent"))  parse_num(df$humidity_percent)  else NULL
  
  # Convert humidity_percent to requested scale
  if (!is.null(hp) && humidity_scale == "fraction") hp <- hp / 100
  # If humidity_scale == "percent" we assume humidity is already in percent
  
  # Coalesce humidity
  df$humidity <- if (!is.null(h) && !is.null(hp)) {
    dplyr::coalesce(h, hp)
  } else if (!is.null(h)) {
    h
  } else if (!is.null(hp)) {
    hp
  } else {
    df$humidity
  }
  
  if (drop_sources) {
    df <- dplyr::select(df, -tidyselect::any_of(c("temperature_f", "humidity_percent")))
  }
  
  df
}

# --- Harmonize numeric columns (auto-detected) ---
#' Safely convert selected columns to numeric
#'
#' Prevents coercing known character fields like materials to NA.
#'
#' @param df data.frame
#' @param numeric_cols character vector of candidate names
#' @return data.frame
convert_types <- function(df, numeric_cols) {
  # Do not attempt conversion on non-existent, list, or already numeric columns
  numeric_cols <- intersect(numeric_cols, names(df))
  numeric_cols <- numeric_cols[vapply(df[numeric_cols], function(x) !is.list(x), logical(1))]
  # Exclude clearly character identity columns
  char_blacklist <- c(
    "^notes$", "^print_type$",
    "^fcomp_(mat1|mat2|additive|salt|inhibitor)$",
    "^syringe[12]_(material|additive|salt|inhibitor)$"
  )
  re <- paste(char_blacklist, collapse = "|")
  safe_numeric <- numeric_cols[!grepl(re, numeric_cols)]
  if (length(safe_numeric) > 0) {
    df <- df %>% mutate(across(all_of(safe_numeric), ~ suppressWarnings(as.numeric(.))))
  }
  df
}

# --- Syringe renaming ---
#' Rename syringe-related columns and combine additive fields robustly
#' - Supports new wtpct_add_1 / wtpct_add_2 and legacy forms
#' - Avoids coalesce() length mismatch by supplying same-length NA vectors
#' - Produces syringe1_* and syringe2_* canonical columns
rename_syringe_columns <- function(df) {
  nm <- names(df)
  n  <- nrow(df)

  # Helpers that always return a vector of length n
  col_chr <- function(name) {
    if (name %in% names(df)) as.character(df[[name]]) else rep(NA_character_, n)
  }
  col_num <- function(name) {
    if (name %in% names(df)) suppressWarnings(as.numeric(df[[name]])) else rep(NA_real_, n)
  }

  map <- setNames(nm, nm)
  set_map <- function(src, dst) if (src %in% nm) map[[src]] <<- dst

  # Materials primary and secondary for each syringe
  set_map("material1a",     "syringe1_material")
  set_map("material1a_mw",  "syringe1_material_mw")
  set_map("material1a_rat", "syringe1_ratio")
  set_map("material2a",     "syringe1_material2")
  set_map("material2a_mw",  "syringe1_material2_mw")
  set_map("material2a_rat", "syringe1_ratio2")

  set_map("material1b",     "syringe2_material")
  set_map("material1b_mw",  "syringe2_material_mw")
  set_map("material1b_rat", "syringe2_ratio")
  set_map("material2b",     "syringe2_material2")
  set_map("material2b_mw",  "syringe2_material2_mw")
  set_map("material2b_rat", "syringe2_ratio2")

  # Flow rates
  set_map("comp1_flow_rate","syringe1_flow_rate")
  set_map("comp2_flow_rate","syringe2_flow_rate")

  # Salts
  set_map("salt2a",         "syringe1_salt")
  set_map("wt_pct_salt2a",  "syringe1_salt_wt_pct")
  set_map("salt2b",         "syringe2_salt")
  set_map("wt_pct_salt2b",  "syringe2_salt_wt_pct")

  # Initiator, inhibitor default to syringe1
  set_map("initiator",      "syringe1_initiator")
  set_map("wt_initiator",   "syringe1_wt_initiator")
  set_map("inhibitor",      "syringe1_inhibitor")
  set_map("wt_inhibitor",   "syringe1_inhibitor_wt_pct")

  # Additive name sources (numeric, letter, and fallback)
  set_map("additive_1",     "_add1_name")
  set_map("additive_2",     "_add2_name")
  set_map("additive_a",     "_add1_name_old")
  set_map("additive_b",     "_add2_name_old")
  set_map("additive",       "_add1_name_fallback")  # last resort for syringe1

  # New additive wt pct scheme (wtpct_add_1/2), with tolerant patterns
  # Also accept wt_pct_add_1/2, and legacy A/B and unsuffixed
  # Numeric scheme
  num_wt <- grep("^(wtp?ct?_add_?)(\\d+)$", nm, value = TRUE, perl = TRUE)
  if (length(num_wt)) {
    idx <- as.integer(sub("^(?:wtp?ct?_add_?)(\\d+)$", "\\1", num_wt, perl = TRUE))
    for (i in seq_along(num_wt)) {
      if (idx[i] == 1L) set_map(num_wt[i], "_add1_wt")
      else if (idx[i] == 2L) set_map(num_wt[i], "_add2_wt")
      else warning("Ignoring additive wt pct with unexpected index: ", num_wt[i])
    }
  }
  # Legacy lettered
  set_map("wtpct_add_a", "_add1_wt_old")
  set_map("wt_pct_add_a","_add1_wt_old")
  set_map("wtpct_add_b", "_add2_wt_old")
  set_map("wt_pct_add_b","_add2_wt_old")
  # Legacy unsuffixed (assume syringe1)
  set_map("wt_pct_add",   "_add1_wt_legacy_unsuffixed")

  # Apply renames
  names(df) <- unname(map[nm])

  # Coalesce additive names
  df$syringe1_additive <- dplyr::coalesce(
    col_chr("syringe1_additive"),
    col_chr("_add1_name"),
    col_chr("_add1_name_old"),
    col_chr("_add1_name_fallback")
  )
  df$syringe2_additive <- dplyr::coalesce(
    col_chr("syringe2_additive"),
    col_chr("_add2_name"),
    col_chr("_add2_name_old")
  )

  # Coalesce additive wt pct (numeric)
  df$syringe1_additive_wt_pct <- dplyr::coalesce(
    col_num("syringe1_additive_wt_pct"),
    col_num("_add1_wt"),
    col_num("_add1_wt_old"),
    col_num("_add1_wt_legacy_unsuffixed")
  )
  df$syringe2_additive_wt_pct <- dplyr::coalesce(
    col_num("syringe2_additive_wt_pct"),
    col_num("_add2_wt"),
    col_num("_add2_wt_old")
  )

  # Mirror initiator/inhibitor to syringe2 if syringe2 fields absent
  if ("syringe1_initiator" %in% names(df) && !"syringe2_initiator" %in% names(df))
    df$syringe2_initiator <- df$syringe1_initiator
  if ("syringe1_wt_initiator" %in% names(df) && !"syringe2_wt_initiator" %in% names(df))
    df$syringe2_wt_initiator <- df$syringe1_wt_initiator
  if ("syringe1_inhibitor" %in% names(df) && !"syringe2_inhibitor" %in% names(df))
    df$syringe2_inhibitor <- df$syringe1_inhibitor
  if ("syringe1_inhibitor_wt_pct" %in% names(df) && !"syringe2_inhibitor_wt_pct" %in% names(df))
    df$syringe2_inhibitor_wt_pct <- df$syringe1_inhibitor_wt_pct

  # Drop temporary additive staging columns if present
  tmp <- c("_add1_name","_add2_name","_add1_name_old","_add2_name_old",
           "_add1_name_fallback","_add1_wt","_add2_wt",
           "_add1_wt_old","_add2_wt_old","_add1_wt_legacy_unsuffixed")
  df <- dplyr::select(df, -tidyselect::any_of(tmp))

  # Final safeguard for duplicate names
  if (anyDuplicated(names(df))) {
    warning("Duplicate column names after rename; appending unique suffixes.")
    names(df) <- make.unique(names(df), sep = "_")
  }

  df
}

#' Add explicit material_1 syringe columns
#'
#' Duplicates existing syringe material fields into explicit material_1
#' fields for forward compatibility, without changing existing names.
#'
#' @param df data.frame containing syringe1_* and syringe2_* columns
#' @return data.frame with syringe1_material_1, syringe1_material_1_mw,
#'   syringe1_material_1_ratio and equivalents for syringe2
#' @export
augment_syringe_material_1 <- function(df) {
  df %>%
    mutate(
      syringe1_material_1 = coalesce(syringe1_material, NA_character_),
      syringe1_material_1_mw = suppressWarnings(as.numeric(syringe1_material_mw)),
      syringe1_material_1_ratio = suppressWarnings(as.numeric(syringe1_ratio)),
      syringe2_material_1 = coalesce(syringe2_material, NA_character_),
      syringe2_material_1_mw = suppressWarnings(as.numeric(syringe2_material_mw)),
      syringe2_material_1_ratio = suppressWarnings(as.numeric(syringe2_ratio))
    )
}

# --- Ensure sample_ID column exists and is standardized ---
ensure_sample_id <- function(df) {
  id_col <- names(df)[str_detect(names(df), regex("sample[_]?id", ignore_case = TRUE))]
  if (length(id_col) == 0) stop("No sample_ID column found.")
  df %>% rename(sample_ID = id_col[1])
}

# --- Clean mpr files robustly ---
clean_mpr_files <- function(mpr_files) {
  mpr_files <- unlist(mpr_files)
  mpr_files <- mpr_files[!is.na(mpr_files) & grepl("mpr", mpr_files)]
  mpr_files <- gsub(
    ".*electrolytes[\\\\/]+",
    "",
    mpr_files,
    ignore.case = TRUE
  )
  mpr_files <- gsub("^[/\\s]+", "", mpr_files)
  unique(mpr_files)
}

# --- Summarize samples, combining multiple mpr columns ---
#' Summarize to one row per sample with first non-missing value
#'
#' - For character columns, blank strings are treated as missing
#' - For numeric columns, first non-NA is chosen
#'
#' @param df data.frame
#' @param mpr_cols character vector of mpr columns
#' @return data.frame one row per sample_ID
#' @export
summarize_samples <- function(df, mpr_cols) {
  first_non_missing <- function(x) {
    if (is.list(x)) {
      # Leave list columns alone
      return(x[1])
    }
    if (is.character(x)) {
      y <- x[!is.na(x) & trimws(x) != ""]
      if (length(y)) y[1] else NA_character_
    } else {
      y <- x[!is.na(x)]
      if (length(y)) y[1] else if (is.numeric(x)) NA_real_ else NA
    }
  }
  
  # Build mpr_files first
  mpr_list <- df %>%
    group_by(sample_ID) %>%
    summarise(
      mpr_files = list(clean_mpr_files(unlist(across(all_of(mpr_cols))))),
      .groups = "drop"
    )
  
  # Summarise all other columns using first_non_missing
  # Exclude the raw mpr_cols to avoid confusion, we already built mpr_files
  others <- df %>%
    select(-all_of(mpr_cols)) %>%
    group_by(sample_ID) %>%
    summarise(across(everything(), first_non_missing), .groups = "drop")
  
  # Join back
  dplyr::left_join(others, mpr_list, by = "sample_ID")
}

#' Convert epoch or date-like to ISO-8601 (UTC)
#'
#' @param x numeric or character epoch seconds, or date-like string "YYYY-MM-DD"
#' @param style "datetime" for "YYYY-MM-DDTHH:MM:SSZ" or "date" for "YYYY-MM-DD"
#' @return character ISO-8601 string, or NA_character_ if not parseable
#' @export
to_iso8601 <- function(x, style = c("datetime", "date")) {
  style <- match.arg(style)
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.numeric(x) && !is.na(x)) {
    posix <- as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
    return(if (style == "datetime") format(posix, "%Y-%m-%dT%H:%M:%SZ") else format(as.Date(posix), "%Y-%m-%d"))
  }
  if (is.character(x)) {
    xs <- trimws(x[1])
    if (grepl("^[0-9]+$", xs)) {
      posix <- as.POSIXct(as.numeric(xs), origin = "1970-01-01", tz = "UTC")
      return(if (style == "datetime") format(posix, "%Y-%m-%dT%H:%M:%SZ") else format(as.Date(posix), "%Y-%m-%d"))
    }
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", xs)) return(if (style == "date") substr(xs, 1, 10) else paste0(substr(xs, 1, 10), "T00:00:00Z"))
  }
  NA_character_
}

# --- Clean mpr file paths ---
clean_mpr_paths <- function(df, mpr_cols, prefix) {
  df %>%
    mutate(across(
      intersect(mpr_cols, names(df)),
      ~ gsub(paste0(".*electrolytes[\\\\/]+"), "", .x, ignore.case = TRUE, perl = TRUE)
    ))
}

# --- JSON Export: include all fcomp and syringe fields ---
#' Save samples as JSON with typed fields
#'
#' - Converts date_printed to ISO-8601 date_casted
#' - Ensures fab_method is integer
#' - Normalizes mpr_files to forward slashes
#' - Writes NA as JSON null
#' - Emits syringe_1 and syringe_2 blocks including material_1 keys
#'
#' @param samples_grouped data.frame as returned by summarize_samples
#' @param output_path file path to write JSON
#' @return invisible(NULL)
#' @export
save_samples_json <- function(samples_grouped, output_path) {
  samples_list <- list()
  
  for (i in seq_len(nrow(samples_grouped))) {
    row <- samples_grouped[i, ]
    sample_id <- as.character(row$sample_ID)
    row_list <- as.list(row)
    
    # Build print_quality block
    spe_thickness <- c(row[["spe_thickness1_mm"]], row[["spe_thickness2_mm"]], row[["spe_thickness3_mm"]])
    spe_thickness <- as.numeric(spe_thickness[!is.na(spe_thickness)])
    spe_thickness_avg <- if (length(spe_thickness) > 0) round(mean(spe_thickness), 4) else NA_real_
    
    print_quality <- list(
      date_casted = to_iso8601(row[["date_printed"]], style = "datetime"),
      cast_type   = if (!is.na(row[["print_type"]])) as.character(row[["print_type"]]) else NA_character_,
      fab_method  = suppressWarnings(as.integer(row[["fab_method"]])),
      area_cm2    = suppressWarnings(as.numeric(row[["area_cm2"]])),
      notes       = if (!is.na(row[["notes"]])) as.character(row[["notes"]]) else NA_character_,
      spe_thickness = spe_thickness,
      spe_thickness_avg = spe_thickness_avg
    )
    
    # mpr_files normalized
    mpr_files <- row$mpr_files[[1]]
    mpr_files <- gsub("\\\\", "/", mpr_files)
    
    # Syringe objects (omit for coin cell if needed)
    syringe1_fields <- row_list[grep("^syringe1_", names(row_list))]
    syringe2_fields <- row_list[grep("^syringe2_", names(row_list))]
    
    # Resistance array
    resistance <- c(row[["resistance1_o"]], row[["resistance2_o"]], row[["resistance3_o"]])
    resistance <- as.numeric(resistance[!is.na(resistance)])
    
    # Fallback: reconstruct fcomp_formulation if missing
    if (is.na(row_list[["fcomp_formulation"]])) {
      row_list[["fcomp_formulation"]] <- compose_formulation(
        row_list[["fcomp_mat1_ratio"]], row_list[["fcomp_mat1"]],
        row_list[["fcomp_mat2_ratio"]], row_list[["fcomp_mat2"]],
        row_list[["fcomp_additive_wt_pct"]], row_list[["fcomp_additive"]],
        row_list[["fcomp_salt_wt_pct"]], row_list[["fcomp_salt"]]
      )
    }
    
    # Compose sample object (omit sample_ID inside)
    sample_obj <- c(
      list(
        ionic_conductivity_s_cm = suppressWarnings(as.numeric(row[["ionic_conductivity_s_cm"]])),
        print_quality = print_quality,
        resistance = resistance,
        mpr_files = mpr_files
      ),
      row_list[grep("^fcomp_", names(row_list))],
      list(syringe_1 = syringe1_fields, syringe_2 = syringe2_fields)
    )
    
    samples_list[[sample_id]] <- purrr::compact(sample_obj)
  }
  
  output <- list(samples = samples_list)
  jsonlite::write_json(output, output_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(NULL)
}
guess_numeric <- function(col, dfs, threshold = 0.8) {
  vals <- unlist(lapply(dfs, function(df) if (col %in% names(df)) df[[col]] else NULL), use.names = FALSE)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  suppressWarnings({
    nums <- as.numeric(vals)
  })
  mean(!is.na(nums)) >= threshold
}
# --- Feature engineering: fcomp calculation ---
#' Finalize sample-level formulation fields
#'
#' - Builds fcomp_* fields from syringe fields
#' - Weighted percentages sum to NA when both contributors are NA
#' - fcomp_formulation is omitted (NA) when inputs are missing
#'
#' @param df data.frame of cleaned, renamed syringe columns
#' @return data.frame with fcomp_* fields added
#' @export
finalize_samples <- function(df) {
  # weighted sum of two terms, returns NA if both terms are NA
  wsum2 <- function(a, b) {
    both_na <- is.na(a) & is.na(b)
    out <- rowSums(cbind(a, b), na.rm = TRUE)
    out[both_na] <- NA_real_
    out
  }
  
  # ratios
  s1r <- suppressWarnings(as.numeric(df$syringe1_ratio))
  s2r <- suppressWarnings(as.numeric(df$syringe2_ratio))
  
  # component contributions
  add1   <- s1r * suppressWarnings(as.numeric(df$syringe1_additive_wt_pct))
  add2   <- s2r * suppressWarnings(as.numeric(df$syringe2_additive_wt_pct))
  salt1  <- s1r * suppressWarnings(as.numeric(df$syringe1_salt_wt_pct))
  salt2  <- s2r * suppressWarnings(as.numeric(df$syringe2_salt_wt_pct))
  inhib1 <- s1r * suppressWarnings(as.numeric(df$syringe1_inhibitor_wt_pct))
  inhib2 <- s2r * suppressWarnings(as.numeric(df$syringe2_inhibitor_wt_pct))
  init1  <- s1r * suppressWarnings(as.numeric(df$syringe1_wt_initiator))
  init2  <- s2r * suppressWarnings(as.numeric(df$syringe2_wt_initiator))
  
  df %>%
    mutate(
      fcomp_mat1 = syringe1_material,
      fcomp_mat2 = syringe2_material,
      fcomp_mat1_ratio = s1r,
      fcomp_mat2_ratio = s2r,
      
      fcomp_additive        = coalesce(syringe1_additive, syringe2_additive),
      fcomp_additive_wt_pct = wsum2(add1, add2),
      
      fcomp_salt        = coalesce(syringe1_salt, syringe2_salt),
      fcomp_salt_wt_pct = wsum2(salt1, salt2),
      
      fcomp_inhibitor        = coalesce(syringe1_inhibitor, syringe2_inhibitor),
      fcomp_inhibitor_wt_pct = wsum2(inhib1, inhib2),
      
      initiator    = coalesce(syringe1_initiator, syringe2_initiator),
      wt_initiator = wsum2(init1, init2)
    ) %>%
    mutate(
      # Build the formulation string row by row with scalar-style logic
      fcomp_formulation = purrr::pmap_chr(
        list(
          fcomp_mat1_ratio, fcomp_mat1,
          fcomp_mat2_ratio, fcomp_mat2,
          fcomp_additive_wt_pct, fcomp_additive,
          fcomp_salt_wt_pct, fcomp_salt
        ),
        function(r1, m1, r2, m2, add_wt, add, salt_wt, salt) {
          parts <- character(0)
          if (!is.na(r1) || !is.na(m1))     parts <- c(parts, paste0(r1, " ", m1))
          if (!is.na(r2) || !is.na(m2))     parts <- c(parts, paste0(r2, " ", m2))
          if (!is.na(add_wt) || !is.na(add))   parts <- c(parts, paste0(add_wt, " ", add))
          if (!is.na(salt_wt) || !is.na(salt)) parts <- c(parts, paste0(salt_wt, " ", salt))
          if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
        }
      )
    )
}

ensure_syringe_columns <- function(df) {
  n <- nrow(df)
  required <- list(
    syringe1_inhibitor        = rep(NA_character_, n),
    syringe2_inhibitor        = rep(NA_character_, n),
    syringe1_inhibitor_wt_pct = rep(NA_real_,      n),
    syringe2_inhibitor_wt_pct = rep(NA_real_,      n),
    syringe1_initiator        = rep(NA_character_, n),
    syringe2_initiator        = rep(NA_character_, n),
    syringe1_wt_initiator     = rep(NA_real_,      n),
    syringe2_wt_initiator     = rep(NA_real_,      n)
    # add other required syringe fields as needed
  )
  for (nm in names(required)) {
    if (!nm %in% names(df)) df[[nm]] <- required[[nm]]
  }
  df
}

#' Summarize to one row per sample by choosing the most complete row
#'
#' - Picks the row within each sample_ID that has the highest count of non-NA fields
#' - This makes character selections stable and avoids order-dependent "random" nulls
#' - mpr_files are still aggregated across all rows
#'
#' @param df data.frame with multiple rows per sample_ID
#' @param mpr_cols character vector of mpr columns to aggregate
#' @return data.frame one row per sample_ID plus mpr_files
#' @export
summarize_samples_bestrow <- function(df, mpr_cols) {
  stopifnot(".row_order" %in% names(df))
  
  # Build mpr_files list per sample from all the raw columns
  mpr_list <- df %>%
    dplyr::group_by(sample_ID) %>%
    dplyr::summarise(
      mpr_files = list(clean_mpr_files(unlist(dplyr::across(all_of(mpr_cols))))),
      .groups = "drop"
    )
  
  # Score each row by number of non-missing fields, excluding mpr columns and tech columns
  score_exclude <- c(mpr_cols, "sample_ID", ".row_order")
  score_cols <- setdiff(names(df), score_exclude)
  df_scored <- df %>%
    dplyr::mutate(.row_score = rowSums(!is.na(dplyr::across(all_of(score_cols)))))
  
  # Pick the single best row per sample_ID, breaking ties by original row order
  repr <- df_scored %>%
    dplyr::arrange(sample_ID, dplyr::desc(.row_score), .row_order) %>%
    dplyr::group_by(sample_ID) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.row_score)
  
  dplyr::left_join(repr, mpr_list, by = "sample_ID")
}

#' Compose a formulation string, allowing partially missing pieces
#' Produces strings like "0.9 PEGMEA, 0.9 PEGMEA, 9 NA, 18 NaTFSI"
#' If nothing is available, returns NA_character_.
compose_formulation <- function(r1, m1, r2, m2, add_wt, add, salt_wt, salt) {
  # Keep behavior consistent with earlier JSONs that included "NA" literals
  part1 <- if (!is.na(r1) || !is.na(m1)) glue::glue("{r1} {m1}") else NA_character_
  part2 <- if (!is.na(r2) || !is.na(m2)) glue::glue("{r2} {m2}") else NA_character_
  part3 <- if (!is.na(add_wt) || !is.na(add)) glue::glue("{add_wt} {add}") else NA_character_
  part4 <- if (!is.na(salt_wt) || !is.na(salt)) glue::glue("{salt_wt} {salt}") else NA_character_
  parts <- c(part1, part2, part3, part4)
  parts <- parts[!is.na(parts)]
  if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
}

#' Normalize a date-like vector to Date
#' @param x vector that may be Date, POSIXt, numeric (epoch seconds or Excel serial), or character
#' @param tz timezone for epoch conversion
#' @param excel_origin Excel origin, typical is "1899-12-30" for Windows
#' @return Date vector
to_Date <- function(x, tz = "UTC", excel_origin = "1899-12-30") {
  # Already Date
  if (inherits(x, "Date")) return(x)
  # POSIXt -> Date
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x, tz = tz))
  # Numeric: detect epoch seconds vs Excel serial days
  if (is.numeric(x)) {
    if (all(is.na(x))) return(as.Date(x))
    mx <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.finite(mx) && mx > 1e9) {
      # Unix epoch seconds
      return(as.Date(as.POSIXct(x, origin = "1970-01-01", tz = tz)))
    }
    if (is.finite(mx) && mx > 20000 && mx < 60000) {
      # Excel serial date (Windows)
      return(as.Date(x, origin = excel_origin))
    }
    # Fallback, treat as days since 1970
    return(as.Date(x, origin = "1970-01-01"))
  }
  # Character: try a few common formats
  if (is.character(x)) {
    x2 <- trimws(x)
    x2[!nzchar(x2)] <- NA_character_
    # Try ISO first
    out <- suppressWarnings(as.Date(x2))
    if (all(is.na(out))) {
      # Try common orders via strptime
      try_orders <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y",
                      "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S")
      for (fmt in try_orders) {
        out <- suppressWarnings(as.Date(strptime(x2, format = fmt, tz = tz)))
        if (any(!is.na(out))) break
      }
    }
    return(out)
  }
  # Fallback
  suppressWarnings(as.Date(x))
}

append_or_write_samples_json_atomic <- function(new_samples_df,
                                                output_path,
                                                merge_strategy = c("append_only", "prefer_existing", "prefer_new")) {
  merge_strategy <- match.arg(merge_strategy)
  
  # Ensure destination directory exists
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # If file does not exist, write fresh JSON and return
  if (!file.exists(output_path)) {
    message("Output JSON not found. Writing a new file: ", output_path)
    save_samples_json(new_samples_df, output_path)
    return(invisible(list(
      written = TRUE,
      merged = FALSE,
      added = nrow(new_samples_df),
      overwritten = 0,
      skipped = 0
    )))
  }
  
  # Create a temporary JSON for the new batch using your existing writer
  tmp_new <- file.path(tempdir(), paste0("new_", basename(output_path)))
  save_samples_json(new_samples_df, tmp_new)
  
  # Read both JSONs as list objects
  existing_obj <- tryCatch(jsonlite::fromJSON(output_path, simplifyVector = FALSE),
                           error = function(e) {
                             warning("Failed to read existing JSON. Will overwrite with new JSON. Error: ", conditionMessage(e))
                             NULL
                           })
  new_obj <- tryCatch(jsonlite::fromJSON(tmp_new, simplifyVector = FALSE),
                      error = function(e) {
                        unlink(tmp_new, force = TRUE)
                        stop("Failed to read newly created temporary JSON: ", conditionMessage(e))
                      })
  
  # If existing failed to read, just write the new object to output_path
  if (is.null(existing_obj)) {
    # Atomic write
    tmp_out <- paste0(output_path, ".tmp")
    jsonlite::write_json(new_obj, tmp_out, pretty = TRUE, auto_unbox = TRUE, na = "null")
    ok <- file.rename(tmp_out, output_path)
    if (!ok) {
      file.copy(tmp_out, output_path, overwrite = TRUE)
      unlink(tmp_out)
    }
    unlink(tmp_new, force = TRUE)
    return(invisible(list(
      written = TRUE,
      merged = FALSE,
      added = length(new_obj$samples %||% list()),
      overwritten = 0,
      skipped = 0
    )))
  }
  
  # Pull 'samples' maps, make sure they are lists
  existing_samples <- existing_obj$samples
  if (is.null(existing_samples)) existing_samples <- list()
  new_samples <- new_obj$samples
  if (is.null(new_samples)) new_samples <- list()
  
  # Merge in-memory
  merged <- merge_samples_maps(existing_samples, new_samples, merge_strategy = merge_strategy)
  
  # Put back into the existing root object, preserving any other root fields
  existing_obj$samples <- merged$map
  
  # Atomic write to avoid partial files
  tmp_out <- paste0(output_path, ".tmp")
  jsonlite::write_json(existing_obj, tmp_out, pretty = TRUE, auto_unbox = TRUE, na = "null")
  ok <- file.rename(tmp_out, output_path)
  if (!ok) {
    file.copy(tmp_out, output_path, overwrite = TRUE)
    unlink(tmp_out)
  }
  
  # Cleanup temporary
  unlink(tmp_new, force = TRUE)
  
  message(sprintf("Append complete. Added: %d, Overwritten: %d, Skipped: %d",
                  merged$added, merged$overwritten, merged$skipped))
  
  invisible(list(
    written = TRUE,
    merged = TRUE,
    added = merged$added,
    overwritten = merged$overwritten,
    skipped = merged$skipped
  ))
}

merge_samples_maps <- function(existing_map,
                               new_map,
                               merge_strategy = c("append_only", "prefer_existing", "prefer_new")) {
  merge_strategy <- match.arg(merge_strategy)
  added <- 0L
  overwritten <- 0L
  skipped <- 0L
  
  # Ensure names are present
  if (is.null(names(existing_map))) names(existing_map) <- character(length(existing_map))
  if (is.null(names(new_map))) names(new_map) <- character(length(new_map))
  
  for (id in names(new_map)) {
    if (!nzchar(id)) {
      skipped <- skipped + 1L
      next
    }
    if (is.null(existing_map[[id]])) {
      # not present, add it
      existing_map[[id]] <- new_map[[id]]
      added <- added + 1L
    } else {
      # already exists
      if (merge_strategy == "prefer_new") {
        existing_map[[id]] <- new_map[[id]]
        overwritten <- overwritten + 1L
      } else if (merge_strategy == "prefer_existing") {
        skipped <- skipped + 1L
      } else { # append_only
        skipped <- skipped + 1L
      }
    }
  }
  
  list(map = existing_map, added = added, overwritten = overwritten, skipped = skipped)
}

# Convenience infix for null-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x