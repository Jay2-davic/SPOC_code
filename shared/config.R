# Configuration Management
# No @export tags needed - functions are loaded directly via source()

suppressPackageStartupMessages(require(glue))

build_paths <- function(base_dir) {
  user_name <- Sys.info()["user"]
  
  list(
    base_dir = base_dir,
    user_name = user_name,
    excel_dir = file.path(base_dir, "data/excel"),
    output_dir = file.path(base_dir, "data_cleaning/outputs"),
    git_directory = base_dir
  )
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  for (i in seq_along(args)) {
    if (args[i] %in% c("--base-dir", "--base_dir", "-b")) {
      if (i < length(args)) {
        return(normalizePath(args[i + 1], mustWork = FALSE))
      }
    }
  }
  
  current_dir <- getwd()
  
  if (basename(current_dir) %in% c("data_cleaning", "eis_processing", "analysis")) {
    return(dirname(current_dir))
  }
  
  return(current_dir)
}

get_config <- function() {
  base_dir <- parse_args()
  
  if (!dir.exists(base_dir)) {
    stop(glue("Base directory does not exist: {base_dir}"))
  }
  
  paths <- build_paths(base_dir)
  
  if (!dir.exists(paths$excel_dir)) {
    stop(glue(
      "Excel directory not found: {paths$excel_dir}\n",
      "Expected structure: {base_dir}/data/excel/\n",
      "Please check your directory structure."
    ))
  }
  
  message(glue("Configuration loaded:"))
  message(glue("  Base: {paths$base_dir}"))
  message(glue("  Excel: {paths$excel_dir}"))
  message(glue("  User: {paths$user_name}"))
  
  return(paths)
}

get_excel_files <- function(paths) {
  xlsx_files <- list.files(paths$excel_dir, pattern = "\\.xlsx$", full.names = TRUE)
  xls_files <- list.files(paths$excel_dir, pattern = "\\.xls$", full.names = TRUE)
  
  excel_files <- c(xlsx_files, xls_files)
  
  if (length(excel_files) == 0) {
    stop(glue("No Excel files found in {paths$excel_dir}"))
  }
  
  names(excel_files) <- basename(excel_files)
  
  message(glue("Found {length(excel_files)} Excel files:"))
  for (name in names(excel_files)) {
    ext <- tools::file_ext(name)
    message(glue("  - {name} ({ext})"))
  }
  
  if (length(xls_files) > 0) {
    message(glue("\nNote: Found {length(xls_files)} .xls file(s)."))
    message("These will be skipped. Convert to .xlsx format if needed.")
  }
  
  return(excel_files)
}