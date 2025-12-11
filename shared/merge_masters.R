#!/usr/bin/env Rscript

#' Merge Master Databases CLI
#'
#' @description
#' Merges individual JSON files into master database.

suppressPackageStartupMessages({
  require(tidyverse)
  require(jsonlite)
  require(glue)
})

source("shared/config.R")
source("shared/json_utils.R")

paths <- get_config()

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
stage <- "all"

if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] %in% c("--stage", "-s")) {
      stage <- args[i + 1]
    }
  }
}

message(glue("Merging Master Database v{paths$version_number}"))

tryCatch({
  if (stage == "all") {
    master <- merge_to_master(paths, force_rebuild = TRUE)
  } else {
    master <- update_master_incremental(paths, stage = stage)
  }
  
  save_master_json(paths, master)
  
  message(glue("✓ Complete: {length(master$measurements)} measurements merged\n"))
  quit(status = 0)
  
}, error = function(e) {
  message(glue("Error: {e$message}"))
  quit(status = 1)
})