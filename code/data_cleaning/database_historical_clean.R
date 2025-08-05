# =============================================================================
# POLYELECTROLYTE DATABASE CLEANING AND INTEGRATION
# =============================================================================
# DESCRIPTION:
# This script integrates and cleans polyelectrolyte experimental data from
# multiple Excel sources into a unified database. The pipeline includes:
# 1. Loading data from multiple Excel files with different formats
# 2. Standardizing column names and data types across sources
# 3. Parsing complex material formulation strings
# 4. Calculating final composition percentages based on flow rates
# 5. Applying corrections for experimental conditions and file paths
# 6. Generating standardized formulation strings for analysis
#
# DATA SOURCES:
# - Screening data from preliminary experiments (Michell Updates)
# - Rivadeneira database (main experimental dataset)
# - Solid state studies database
# - NaTFSI-specific experiments database
#
# OUTPUTS:
# - df_PE_combined: Unified dataframe with all cleaned and processed data
# - Standardized formulation strings and composition calculations
# - File path corrections for downstream analysis
#
# KEY FEATURES:
# - Handles multiple Excel sheet formats automatically
# - Parses complex material naming conventions
# - Calculates weighted compositions from flow rate data
# - Applies temporal corrections for protocol changes
# - Standardizes environmental condition reporting
#
# DEPENDENCIES:
# - R packages: xlsx, tidyverse, janitor, lubridate, stringi, glue
# - Excel files must be accessible at specified paths
# - User credentials for file access
#
# USAGE:
# 1. Update configuration paths for your environment
# 2. Ensure all Excel files are accessible
# 3. Run script to generate df_PE_combined dataset
# 4. Optionally uncomment save commands to export results
#
# =============================================================================
# CONFIGURATION - Update paths here for different environments
# =============================================================================

# DEBUG PATHS (uncomment for local debugging)
# ONEDRIVE_BASE <- glue::glue('C:\\Users\\{user_name}\\OneDrive - LLNL')

# Define data file paths for systematic access
# Each path points to a specific experimental dataset with unique characteristics
DATA_PATHS <- list(
  # Preliminary screening experiments with complex material naming
  data_1 = glue::glue('~/Git/SPOC_code/data/database/Screening data Preliminary.xls'),
  # Main experimental database with comprehensive measurements
  data_2 = glue::glue('~/Git/SPOC_code/data/database/PEdatabase_1.xlsx'),
  # Specialized solid-state electrolyte studies
  data_3 = glue::glue('~/Git/SPOC_code/data/database/PEdatabase_2.xlsx'),
  # Sodium-ion electrolyte experiments (NaTFSI salt studies)
  data_na = glue::glue('~/Git/SPOC_code/data/database/PEdatabase_NaTFSI.xlsx')
)

# Output path for processed data
OUTPUT_PATH <- glue::glue('~/Git/SPOC_code/data/database/')

# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================

# Core data manipulation and file I/O libraries
require(xlsx)      # Excel file reading and writing
require(tidyverse) # Comprehensive data manipulation toolkit (includes dplyr, tidyr, stringr, etc.)

# Additional libraries used implicitly by functions:
# janitor (for clean_names()), lubridate (for date handling), 
# stringi (for advanced string operations), glue (for string interpolation)

# =============================================================================
# DATA LOADING FROM MULTIPLE EXCEL SOURCES
# =============================================================================

# Load and preprocess Excel files from different experimental campaigns
# Each file has different format and requires specific handling

# 1. SCREENING DATA - Preliminary experimental results
# Sheet 3 contains main dataset, requires sample ID generation
excel_file <- xlsx::read.xlsx(DATA_PATHS$data_1, sheetIndex = 3) |>
  # Remove header row that contains metadata rather than data
  dplyr::filter(dplyr::row_number() != 1) |>
  # Generate standardized sample IDs using MM prefix
  dplyr::mutate(sample_ID = paste('MM', row_number(), sep = "_"))

# 2. RIVADENEIRA DATABASE - Primary experimental dataset
# Contains multiple measurements per sample in wide format, requires restructuring
excel_file2 <- xlsx::read.xlsx(DATA_PATHS$data_2, sheetIndex = 1) |>
  # Handle missing thickness measurements by using average when individual measurement is missing
  dplyr::mutate(
    SPE_thickness1_mm = dplyr::case_when(
      is.na(SPE_thickness1_mm) == TRUE ~ SPE_thicknessAVG_mm,
      TRUE ~ SPE_thickness1_mm
    )
  ) |>
  # Restructure EIS measurements from wide to long format
  # Each sample may have multiple EIS files, stored in separate columns
  tidyr::pivot_longer(
    cols = dplyr::contains('EIS_mpr'),
    names_to = 'delete',
    values_to = 'EIS_mpr'
  ) |>
  # Remove redundant columns created during pivoting
  dplyr::select(-c(delete, resistanceAVG_o, SPE_thicknessAVG_mm, SPE_thicknessAVG_cm)) |>
  # Filter out empty measurements but preserve first row as template
  dplyr::filter(!is.na(EIS_mpr) | dplyr::row_number() == 1) |>
  # Restructure resistance measurements from wide to long format
  tidyr::pivot_longer(
    cols = dplyr::contains('resistance'),
    names_to = 'delete',
    values_to = 'resistance'
  ) |>
  # Calculate average film thickness from multiple SPE thickness measurements
  dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
  # Remove samples with missing critical measurements (both EIS file and thickness required)
  dplyr::filter(is.na(EIS_mpr) == FALSE & is.na(film_thickness) == FALSE) |>
  # Convert thickness units from mm to cm for consistency across datasets
  dplyr::mutate(film_thickness = film_thickness / 10) |>
  # Remove duplicate rows that may have been created during restructuring
  dplyr::distinct()

# 3. SOLID STATE STUDIES - Specialized experimental conditions
# Similar structure to rivadeneira_db but with different filtering criteria
excel_file3 <- xlsx::read.xlsx(DATA_PATHS$data_3, sheetIndex = 1) |>
  # Apply similar preprocessing as rivadeneira database
  tidyr::pivot_longer(
    cols = dplyr::contains('EIS_mpr'),
    names_to = 'delete',
    values_to = 'EIS_mpr'
  ) |>
  dplyr::select(-c(delete, resistanceAVG_o)) |>
  tidyr::pivot_longer(
    cols = dplyr::contains('resistance'),
    names_to = 'delete',
    values_to = 'resistance'
  ) |>
  dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
  # Allow coin cell samples even without thickness measurements (different measurement geometry)
  dplyr::filter(is.na(film_thickness) == FALSE | print_type == 'Coin cell') |>
  dplyr::mutate(film_thickness = film_thickness / 10) |>
  dplyr::distinct()

# 4. NaTFSI DATABASE - Sodium-ion electrolyte experiments
# Contains specialized columns for temperature and humidity with non-standard naming
excel_file4 <- xlsx::read.xlsx(DATA_PATHS$data_na, sheetIndex = 1) |>
  # Apply similar preprocessing pipeline
  tidyr::pivot_longer(
    cols = dplyr::contains('EIS_mpr'),
    names_to = 'delete',
    values_to = 'EIS_mpr'
  ) |>
  dplyr::select(-c(delete, resistanceAVG_o)) |>
  tidyr::pivot_longer(
    cols = dplyr::contains('resistance'),
    names_to = 'delete',
    values_to = 'resistance'
  ) |>
  dplyr::mutate(film_thickness = rowMeans(dplyr::across(dplyr::contains('SPE')), na.rm = TRUE)) |>
  dplyr::filter(is.na(film_thickness) == FALSE | print_type == 'Coin cell') |>
  dplyr::mutate(film_thickness = film_thickness / 10) |>
  # Handle special column names with periods and special characters
  # These were created by Excel export and need standardization
  dplyr::rename(temperature = 'temperature..F.', humidity = 'humidity....') |>
  dplyr::distinct()

# =============================================================================
# DATA CLEANING FUNCTIONS - COMPREHENSIVE PROCESSING PIPELINE
# =============================================================================

# MAIN CLEANING FUNCTION - Handles complex formulation string parsing
# This function processes the original screening data with complex material naming conventions
# The original data uses free-form text that needs to be parsed into structured components

polyelectrolyte_DataCleaning <- function(excel_file) {
  require(tidyverse)
  suppressWarnings({
    # STEP 1: Basic data cleaning and standardization
    bulkClean_excel_file <- excel_file |>
      # Standardize column names (removes spaces, special characters, converts to snake_case)
      janitor::clean_names() |>
      # Remove empty rows based on key identifier columns
      dplyr::filter(sample_number != is.na(.data)) |>
      dplyr::filter(date_printed != is.na(.data))
    
    # STEP 2: Complex string parsing and material naming standardization
    # The original data uses complex naming conventions that need systematic parsing
    # Target pattern: MaterialA_MW_Ratio_MaterialB_MW_Ratio_Additive_wt%_Salt_wt%
    bulkClean_excel_file1 <- bulkClean_excel_file |>
      # Standardize column names for consistency
      dplyr::rename('material_a' = 'material_a_linear_actuator_1') |>
      dplyr::rename('material_b' = 'material_b_linear_actuator_2') |>
      # Remove parentheses that interfere with parsing
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., '\\(|\\)', ''))) |>
      
      # Apply series of regex replacements to standardize material naming
      # These replacements convert free-form text to structured naming convention
      # Each replacement handles a specific material combination pattern
      
      # PEGMEA/PEGDA polymer blend (common base polymer system)
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., '0.9 PEGMEA 550, ', 'PEGMEA_550_0.9_'))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., '0.1 PEGDA 250', 'PEGDA_250_0.1___'))) |>
      
      # PAL/PEGDME thiol-ene system (50/50 weight ratio assumed)
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., 'PAL, Pentaerythrytol tetrakis3mercaptopropionate, PEGDME', 'PAL__0.5_PEGDME__0.5_'))) |>
      
      # Ethylene oxide/LiTFSI system (electrolyte blend)
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., ', \\[EO\\]\\/LiTFSI ', 'EO_1_LiTFSI_'))) |>
      
      # Diallyl carbonate/ETTMP thiol systems (crosslinking chemistry)
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., 'Diallyl carbonate, ETTMP thiol ', 'diallylCarbonate__0.5_ETTMPthiol__0.5_'))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., 'Allyl Carbonate, ETTMP', 'allylCarbonate__0.5_ETTMP__0.5_'))) |>
      
      # Convert weight percentage notation from '## wt% name' to 'name_##' format
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_replace(., ',(\\d+) wt% (\\w+)', '\\2_\\1'))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_replace(., ', (\\d+) wt% (\\w+)', '\\2_\\1'))) |>
      
      # Clean up chamber composition notation (remove equipment-specific labels)
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(., 'Comp1 =|Comp2 =|Comp1=|Comp2='))) |>
      
      # Standardize programmed composition format for flow rate parsing
      dplyr::mutate(
        programmed_composition = stringi::stri_replace_all_regex(programmed_composition, ',', '_'),
        programmed_composition = stringi::stri_replace_all_regex(programmed_composition, ' ', ''),
        programmed_composition = stringi::stri_replace_all_regex(programmed_composition, '0.968', '0.968_'),
        # Standardize print type terminology across datasets
        print_type = stringi::stri_replace_all_regex(print_type, 'Coin Cell |Coin cell |coin cell', 'Coin cell'),
        print_type = stringi::stri_replace_all_regex(print_type, 'PCB print ', 'PCB print')
      )
    
    # STEP 3: Parse structured naming convention into separate columns
    # Split the standardized strings into individual components for quantitative analysis
    bulkClean_excel_file2 <- bulkClean_excel_file1 |>
      # Parse material A into structured components
      # Format: material1_MW_ratio_material2_MW_ratio_additive_wt%_salt_wt%
      tidyr::separate(
        col = material_a,
        into = c('material1a', 'material1aMW', 'material1aRat', 'material2a', 'material2aMW', 'material2aRat', 'additive_a', 'wt_pctAdd_a', 'salt2a', 'wt_pctSalt2a'),
        sep = '_'
      ) |>
      # Parse material B with same structure
      tidyr::separate(
        col = material_b,
        into = c('material1b', 'material1bMW', 'material1bRat', 'material2b', 'material2bMW', 'material2bRat', 'additive_b', 'wt_pctAdd_b', 'salt2b', 'wt_pctSalt2b'),
        sep = '_'
      ) |>
      # Parse flow rate composition (dual-syringe system parameters)
      tidyr::separate(
        col = programmed_composition,
        into = c('Comp1_flowRate', 'Comp2_flowRate'),
        sep = '_'
      ) |>
      
      # STEP 4: Clean up missing values and standardize data types
      # Replace various representations of missing data with proper NA values
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., " "))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., ""))) |>
      
      # Convert measurement columns to numeric data types
      dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
      # Automatically detect and convert numeric columns (flexible approach)
      dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(c("^[0-9]+$|[.]"), .))), ~ as.double(as.character(.)))) |>
      
      # Calculate sample area in cm² from mm² measurements
      dplyr::mutate(area_cm2 = area_mm2 * 0.01)
    
    # STEP 5: Final formatting and date handling
    bulkClean_excel_file2 <- bulkClean_excel_file2 |>
      # Convert date strings to proper R date objects
      dplyr::mutate(date_printed = lubridate::ymd(date_printed)) |>
      # Rename columns for consistency with other datasets
      dplyr::rename(SPE_thickness_mm = thickness_mm, sample_ID = sample_id) |>
      # Ensure numeric conversion for ratio calculations
      dplyr::mutate(ratio_eo_li = as.double(ratio_eo_li))
  })
  
  return(bulkClean_excel_file2)
}

# SECONDARY CLEANING FUNCTIONS - Handle simpler data formats
# These functions process datasets with more standardized structure but require EIS file handling

poly2_cleaning <- function(excel_file) {
  require(tidyverse)
  # Separate EIS file information to handle as character data (prevents type conflicts)
  df_eis <- excel_file |>
    dplyr::transmute(EIS_mpr = as.character(EIS_mpr))
  
  # Process main dataset without EIS column
  df <- excel_file |>
    tibble::as_tibble() |>
    # Remove index column and EIS column (will rejoin EIS data later)
    dplyr::select(-c(1, EIS_mpr)) |>
    # Convert string "NA" representations to proper R NA values
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., "NA", NA))) |>
    # Convert key measurement columns to numeric
    dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(c("^[0-9]+$|[.]"), .))), ~ as.double(as.character(.)))) |>
    # Rename columns for consistency across datasets
    dplyr::rename(additive_a = Additive, additive_b = Additive., wt_pctAdd_a = wt_pctAdd, wt_pctAdd_b = wt_pctAdd.1) |>
    # Handle date conversion
    dplyr::mutate(date_printed = as.Date(date_printed)) |>
    # Rejoin with EIS file data
    dplyr::bind_cols(df_eis) |>
    # Filter out entries with missing critical data
    dplyr::filter(date_printed != is.na(.data)) |>
    # Standardize print type terminology
    dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type, 'coin cell', 'Coin cell')) |>
    # Ensure numeric conversion for analysis columns
    dplyr::mutate(ratio_eo_li = as.double(ratio_eo_li))
}

poly3_cleaning <- function(excel_file_) {
  require(tidyverse)
  # Similar approach to poly2_cleaning but handles additional data type conversions
  df_eis <- excel_file_ |>
    dplyr::transmute(EIS_mpr = as.character(EIS_mpr))
  
  df <- excel_file_ |>
    tibble::as_tibble() |>
    dplyr::select(-c(1, EIS_mpr)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringi::stri_replace_all_regex(., "NA", NA))) |>
    dplyr::mutate(ionic_conductivity_s_cm = as.double(ionic_conductivity_s_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::where(~ any(grepl(c("^[0-9]+$|[.]"), .))), ~ as.double(as.character(.)))) |>
    dplyr::mutate(date_printed = as.Date(date_printed)) |>
    dplyr::bind_cols(df_eis) |>
    dplyr::filter(date_printed != is.na(.data)) |>
    dplyr::mutate(print_type = stringi::stri_replace_all_regex(print_type, 'coin cell', 'Coin cell')) |>
    # Additional conversions specific to this dataset
    dplyr::mutate(wt_pctSalt2b = as.double(wt_pctSalt2b), ratio_eo_li = as.double(ratio_eo_li))
}

# UTILITY FUNCTIONS - Handle data type conversions and corrections
# These functions ensure data consistency after combining multiple sources

PE_addArea_changeVarType <- function(data) {
  require(tidyverse)
  data |>
    tibble::as_tibble() |>
    # Remove entries with missing date information
    dplyr::filter(date_printed != is.na(.data)) |>
    # Ensure numeric conversion for key measurement columns
    # This step is necessary because rbind operations can revert data types
    dplyr::mutate(
      wt_pct_add_a = as.double(wt_pctAdd_a),
      wt_pct_add_b = as.double(wt_pctAdd_b),
      temperature = as.double(temperature),
      humidity = as.double(humidity)
    )
}

PE_collectDF_changeVarType <- function(data) {
  require(tidyverse)
  data |>
    # Ensure data type consistency after combining datasets
    # Different sources may have different data type representations
    dplyr::mutate(EIS_mpr = as.character(EIS_mpr)) |>
    dplyr::mutate(
      wt_pct_add_a = as.double(wt_pct_add_a),
      wt_pct_add_b = as.double(wt_pct_add_b),
      temperature = as.double(temperature),
      humidity = as.double(humidity)
    )
}

PE_column_cleaning <- function(data) {
  require(tidyverse)
  data |>
    # Apply temporal corrections for experimental protocol changes
    # These corrections account for changes in experimental procedures over time
    dplyr::mutate(
      # Before 2023-10-22: coin cell and bulk cast samples used single-component systems
      Comp1_flowRate = dplyr::case_when(
        (print_type == 'Coin cell' & date_printed < as.Date('2023-10-22')) ~ as.numeric('1'),
        (print_type == 'Bulk cast' & date_printed < as.Date('2023-10-22') & date_printed > as.Date('2024-01-01')) ~ as.numeric('1'),
        TRUE ~ Comp1_flowRate
      ),
      Comp2_flowRate = dplyr::case_when(
        (print_type == 'Coin cell' & date_printed < as.Date('2023-10-22')) ~ as.numeric('0'),
        (print_type == 'Bulk cast' & date_printed < as.Date('2023-10-22') & date_printed > as.Date('2024-01-01')) ~ as.numeric('0'),
        TRUE ~ Comp2_flowRate
      )
    ) |>
    # Replace NA values with 'none' for character columns (except sample_ID which should preserve NAs)
    dplyr::mutate(dplyr::across(dplyr::where(is.character) & !names('sample_ID'), ~ dplyr::if_else(is.na(.), 'none', .)))
}

# COMPOSITION CALCULATION FUNCTION - Core of the formulation analysis
# This function calculates final compositions based on dual-syringe mixing ratios
PE_fcomp_adding <- function(data) {
  require(tidyverse)
  data |>
    # STEP 1: Determine final additive and salt composition from dual-component system
    # Logic: Use component A if present, otherwise use component B, otherwise 'none'
    dplyr::mutate(
      fcomp_additive = dplyr::if_else(
        additive_a != 'none',
        additive_a,
        dplyr::if_else(additive_b != 'none', additive_b, 'none')
      ),
      fcomp_salt = dplyr::if_else(
        salt2a != 'none',
        salt2a,
        dplyr::if_else(salt2b != 'none', salt2b, 'none')
      )
    ) |>
    # STEP 2: Handle missing numerical values by setting to zero
    # This prevents calculation errors in subsequent steps
    dplyr::mutate(dplyr::across(c(wt_pctAdd_a, wt_pctSalt2a, wt_pctAdd_b, wt_pctSalt2b), ~ dplyr::if_else(is.na(.), 0, .))) |>
    
    # STEP 3: Calculate final compositions based on flow rates
    # Formula: Final_wt% = (ComponentA_wt% × FlowRateA) + (ComponentB_wt% × FlowRateB)
    # This accounts for mixing ratios in dual-syringe printing system
    dplyr::mutate(
      fcomp_additive_wt_pct = (wt_pctAdd_a * Comp1_flowRate + wt_pctAdd_b * Comp2_flowRate),
      digit = 3,  # Precision control
      fcomp_additive_wt_pct = as.double(fcomp_additive_wt_pct),
      fcomp_salt_wt_pct = wt_pctSalt2a * Comp1_flowRate + wt_pctSalt2b * Comp2_flowRate,
      fcomp_salt_wt_pct = as.double(fcomp_salt_wt_pct),
      # Calculate effective sample area (set to 0 if missing to prevent calculation errors)
      area_cm2 = signif(dplyr::if_else(is.na(area_cm2), 0, area_cm2)),
      # Apply standard purge time for samples after SOP implementation (April 2023)
      purge_time_s = dplyr::if_else(date_printed > '2023-04-01', 240, purge_time_s),
      fcomp_mat1 = material1a
    ) |>
    
    # STEP 4: Calculate material ratios accounting for print type and flow rates
    # Different calculation methods for different fabrication approaches
    dplyr::mutate(
      fcomp_mat1_ratio = signif(dplyr::if_else(
        fcomp_mat1 == 'none',
        0,
        dplyr::if_else(
          print_type == 'PCB print' | print_type == 'Bulk cast',
          # For printed samples: weighted average based on flow rates
          material1aRat * Comp1_flowRate + material1bRat * Comp2_flowRate,
          # For coin cells: use component A ratio directly (single component system)
          dplyr::if_else(print_type == 'Coin cell', material1aRat, 0.5)
        )
      ), digits = 2),
      fcomp_mat2 = material2a,
      fcomp_mat2_ratio = signif(dplyr::if_else(
        fcomp_mat2 == 'none',
        0,
        dplyr::if_else(
          print_type == 'PCB print' | print_type == 'Bulk cast',
          material2aRat * Comp1_flowRate + material2bRat * Comp2_flowRate,
          dplyr::if_else(print_type == 'Coin cell', material2aRat, 0.5)
        )
      ), digits = 2),
      # Recalculate salt percentage with proper flow rate weighting
      fcomp_salt_wt_pct = signif(dplyr::if_else(
        is.na(fcomp_salt) == T,
        0,
        dplyr::if_else(
          print_type == 'PCB print' | print_type == 'Bulk cast',
          (wt_pctSalt2a * Comp1_flowRate + wt_pctSalt2b * Comp2_flowRate),
          dplyr::if_else(print_type == 'Coin cell', wt_pctSalt2a, wt_pctSalt2a + wt_pctSalt2b)
        )
      ), digits = 2)
    ) |>
    
    # STEP 5: Reorder columns and determine film quality
    dplyr::relocate(ionic_conductivity_s_cm, .after = fcomp_salt_wt_pct) |>
    dplyr::mutate(
      # Determine film quality from experimental notes
      film_quality = dplyr::if_else(
        notes == 'none',
        'transparent',
        dplyr::if_else(str_detect(notes, 'white|opaque') == T, 'opaque', 'transparent')
      ),
      # Set component names to 'none' if weight percentage is zero
      fcomp_salt = dplyr::if_else(fcomp_salt_wt_pct == 0, 'none', fcomp_salt),
      fcomp_additive = dplyr::if_else(fcomp_additive_wt_pct == 0, 'none', fcomp_additive)
    ) |>
    
    # STEP 6: Apply corrections for known measurement inconsistencies
    # These corrections address systematic errors identified through data analysis
    dplyr::mutate(
      fcomp_additive_wt_pct = dplyr::case_when(
        fcomp_additive_wt_pct == 1.43000 ~ 1.4860,   # Correct for rounding error in original data
        fcomp_additive_wt_pct == 2.14000 ~ 2.14290,
        fcomp_additive_wt_pct == 2.86000 ~ 2.85710,
        fcomp_additive_wt_pct == 4.28000 ~ 4.28570,
        fcomp_additive_wt_pct == 6.42000 ~ 6.42860,
        fcomp_additive_wt_pct == 8.56000 | fcomp_additive_wt_pct == 8.57000 ~ 8.57140,
        fcomp_additive_wt_pct == 10.71000 ~ 10.7140,
        fcomp_additive_wt_pct == 12.85000 | fcomp_additive_wt_pct == 8.56000 ~ 12.85700,
        TRUE ~ fcomp_additive_wt_pct
      )
    ) |>
    
    # STEP 7: Apply significant figure rounding for consistency
    dplyr::mutate(
      fcomp_additive_wt_pct = signif(fcomp_additive_wt_pct, digits = 3),
      fcomp_salt_wt_pct = signif(fcomp_salt_wt_pct, digits = 3)
    ) |>
    
    # STEP 8: Generate standardized formulation string for analysis and reporting
    # Format: "ratio1 material1, ratio2 material2, wt% additive, wt% salt"
    dplyr::mutate(
      fcomp_formulation = glue::glue('{fcomp_mat1_ratio} {fcomp_mat1}, {fcomp_mat2_ratio} {fcomp_mat2}, {fcomp_additive_wt_pct} {fcomp_additive}, {fcomp_salt_wt_pct} {fcomp_salt}')
    ) |>
    
    # STEP 9: Select and organize final columns for output
    dplyr::select(
      c(
        # Temporal and experimental metadata
        date_printed, print_type, purge_time_s, Comp1_flowRate, Comp2_flowRate,
        fab_method, notes,
        # Physical measurements
        film_thickness, resistance, resistanceAVG_o,
        # Material composition (structured)
        fcomp_mat1, fcomp_mat1_ratio, fcomp_mat2, fcomp_mat2_ratio,
        fcomp_additive, fcomp_additive_wt_pct, fcomp_salt, fcomp_salt_wt_pct,
        # Performance measurements
        ionic_conductivity_s_cm, film_quality,
        # Analysis identifiers
        fcomp_formulation, EIS_mpr, area_cm2, sample_ID,
        # Environmental conditions
        temperature, humidity
      )
    ) |>
    # Reorder key columns for better visibility in analysis
    dplyr::relocate(c(fcomp_formulation, ionic_conductivity_s_cm, print_type), .after = date_printed)
}

# AMENDMENT FUNCTIONS - Handle specific corrections and historical updates
# These functions apply corrections for known issues and temporal changes

PE_AMEND_ethanolAddition <- function(data) {
  require(tidyverse)
  # Track ethanol addition during a specific time period
  # Between 2023-05-22 and 2023-10-23, removing samples containing ethanol
  data |>
    dplyr::mutate(
      # Flag samples that may contain nile red dye (used as tracer)
      nile_red = dplyr::case_when(
        date_printed > as.Date('2023-05-22') &
          date_printed < as.Date('2023-10-23') &
          fcomp_additive_wt_pct != '0' ~ TRUE,
        TRUE ~ FALSE
      ),
      # Track ethanol as secondary additive during contamination period
      fcomp_additive2 = dplyr::case_when(
        date_printed > as.Date('2023-05-22') &
          date_printed < as.Date('2023-10-23') &
          fcomp_additive_wt_pct != '0' ~ 'Ethanol',
        TRUE ~ 'none'
      ),
      # Estimate ethanol concentration based on experimental conditions
      # 20% is estimated based on cleaning solvent residue analysis
      fcomp_additive2_wt_pct = dplyr::case_when(
        fcomp_additive2 == 'Ethanol' ~ '20',
        TRUE ~ '0'
      )
    )
}

PE_AMEND_eis_filepathFill <- function(data) {
  require(tidyverse)
  # Correct file path inconsistencies from specific experimental dates
  data |>
    dplyr::mutate(
      # Fix file extension issues for 2023-10-26 files
      # These files had non-standard naming that needs correction
      EIS_mpr = dplyr::case_when(
        date_printed == '2023-10-26' & stringr::str_detect(EIS_mpr, 'pin') == TRUE ~ 
          glue::glue(r'({stringi::stri_replace_all_regex(EIS_mpr, '\\_C01[.]mpr', '')})'),
        TRUE ~ EIS_mpr
      ),
      # Ensure numeric data types for environmental conditions
      temperature = as.double(temperature),
      humidity = as.numeric(humidity)
    ) |>
    # Fix specific file path pattern issues
    dplyr::mutate(EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '_C01\\\\pin', '\\\\pin'))
}

PE_AMEND_eis_correctedFilepath <- function(data) {
  require(dplyr)
  require(stringi)
  # Update file paths to reflect new directory structure
  # Files were moved from temporary locations to permanent database structure
  data |>
    dplyr::mutate(
      # Update paths for files moved from Katherine Updates to Database directory
      EIS_mpr = stringi::stri_replace_all_regex(
        EIS_mpr,
        'Katherine Updates\\\\08222023',
        'Database of polymer electrolytes/mpr files/08222023'
      ),
      EIS_mpr = stringi::stri_replace_all_regex(
        EIS_mpr,
        'Katherine Updates\\\\091223',
        'Database of polymer electrolytes/mpr files/091223'
      )
    )
}

# =============================================================================
# DATA PROCESSING PIPELINE - SYSTEMATIC INTEGRATION
# =============================================================================

# Apply cleaning functions to each dataset with dataset-specific modifications
# Each dataset requires slightly different processing due to format differences

# DATASET 1: Screening data (complex parsing required)
df_PE1 <- polyelectrolyte_DataCleaning(excel_file) |>
  # Add missing columns for consistency with other datasets
  dplyr::mutate(
    # Convert thickness units for consistency
    SPE_thicknessAVG_cm = SPE_thickness_mm * 0.1,
    # Add placeholder columns for environmental conditions (not measured in this dataset)
    temperature = NA,
    humidity = NA
  ) |>
  # Rename for consistency
  dplyr::rename(resistanceAVG_o = resistance_o) |>
  # Remove columns not needed for analysis or not comparable across datasets
  dplyr::select(-c(area_mm2, SPE_thickness_mm, ratio_eo_li, x_li_tfsi, additive, additive_wt, temperature, humidity))

# DATASET 2: Rivadeneira database (main dataset)
df_PE2 <- poly2_cleaning(excel_file2) |>
  # Ensure proper data types for environmental conditions
  dplyr::mutate(temperature = as.double(temperature), humidity = as.numeric(humidity)) |>
  # Remove columns that are not standardized across datasets
  dplyr::select(-c(electrolyte1a, wt_pct1a, electrolyte1b, wt_pct1b, ratio_eo_li, x_li_tfsi, additive, additive_wt, temperature, humidity))

# DATASET 3: Solid state studies
df_PE3 <- poly3_cleaning(excel_file3) |>
  dplyr::mutate(temperature = as.double(temperature), humidity = as.numeric(humidity)) |>
  dplyr::select(-c(electrolyte1a, wt_pct1a, electrolyte1b, wt_pct1b, ratio_eo_li, x_li_tfsi, additive, additive_wt, temperature, humidity))

# DATASET 4: NaTFSI studies (sodium-ion electrolytes)
df_PE4 <- poly3_cleaning(excel_file4) |>
  # Remove dataset-specific columns that don't apply to integrated analysis
  dplyr::select(-c(electrolyte1a, wt_pct1a, electrolyte1b, wt_pct1b, ratio_eo_li, x_li_tfsi, additive, additive_wt)) |>
  # Apply dataset-specific corrections
  dplyr::mutate(
    resistance = as.numeric(resistance),
    EIS_mpr = as.character(EIS_mpr),
    temperature = as.double(temperature),
    # Convert humidity from percentage to fraction and correct for measurement scale
    humidity = as.numeric(humidity) * 100,
    notes = as.character(notes)
  )

# =============================================================================
# DATASET INTEGRATION AND FINAL PROCESSING
# =============================================================================

# Combine all datasets and apply comprehensive transformations
# Order of operations is critical for proper data integration
df <- df_PE4 |>
  # Combine datasets using row binding (creates union of all samples)
  dplyr::bind_rows(df_PE2, df_PE3, df_PE1) |>
  
  # Apply processing pipeline in correct order
  PE_addArea_changeVarType() |>           # Standardize data types
  PE_collectDF_changeVarType() |>         # Ensure consistency after binding
  PE_column_cleaning() |>                 # Apply temporal corrections
  PE_fcomp_adding() |>                    # Calculate final compositions
  PE_AMEND_ethanolAddition() |>           # Track contamination periods
  PE_AMEND_eis_correctedFilepath() |>     # Update file paths
  
  # Apply final file path standardizations
  dplyr::mutate(
    # Ensure all EIS files have proper .mpr extension
    EIS_mpr = dplyr::if_else(
      !str_detect(EIS_mpr, '[.]mpr'),
      glue::glue('{EIS_mpr}.mpr'),
      EIS_mpr
    ),
    # Standardize path separators for cross-platform compatibility
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '\\\\', '/'),
    # Normalize whitespace in file paths
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '\\s', ' '),
    # Remove institutional prefixes that are not part of relative path
    EIS_mpr = stringi::stri_replace_all_regex(EIS_mpr, '.* LLNL', '')
  )

readr::write_csv(df, glue::glue(OUTPUT_PATH, 'historical_files.csv'))
