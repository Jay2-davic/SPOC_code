#!/usr/bin/env Rscript
# =============================================================================
# POLYMER ELECTROLYTE ANALYSIS PIPELINE - STEP BY STEP
# =============================================================================
options(expressions = 500000)
suppressPackageStartupMessages({
  require(dplyr)
  require(glue)
  require(ggplot2)
})

# Source functions
source("~/Git/SPOC_code/analysis/functions.R")
source("~/Git/SPOC_code/analysis/plot_functions.R")

# =============================================================================
# CONFIGURATION
# =============================================================================
json_input <- "data/pe_database.json"
data_dir <- "data"
output_dir <- "output"
plot_dir <- file.path(output_dir, "plots")
step_dir <- file.path(output_dir, "steps")

# Create directories
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(step_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Save step summary
save_step_summary <- function(df, step_number, step_name) {
  summary <- data.frame(
    step = step_number,
    name = step_name,
    total_samples = nrow(df),
    flagged = sum(df$flag_for_removal, na.rm = TRUE),
    clean = sum(!df$flag_for_removal, na.rm = TRUE),
    pct_flagged = 100 * sum(df$flag_for_removal, na.rm = TRUE) / nrow(df),
    timestamp = Sys.time()
  )
  
  cat(sprintf("\n%s. %s:\n", step_number, step_name))
  cat(sprintf("  Total: %d | Flagged: %d (%.1f%%) | Clean: %d (%.1f%%)\n",
              summary$total_samples, summary$flagged, summary$pct_flagged,
              summary$clean, 100 - summary$pct_flagged))
  
  return(summary)
}

#' Quick heatmap check for PCB data
#' Quick heatmap check for PCB and Coin Cell data
quick_heatmap <- function(df, step_name, output_path_pcb, output_path_coin = NULL) {
  
  # PCB Heatmap
  tryCatch({
    df_pcb <- df %>%
      filter(flag_for_removal == FALSE, 
             fcomp_salt == "LiTFSI",
             print_type == "PCB print" | print_type == "Bulk cast")
    
    if (nrow(df_pcb) == 0) {
      cat(sprintf("  ⚠ No PCB data available for heatmap at step: %s\n", step_name))
    } else {
      p_pcb <- ggplot(df_pcb, aes(x = fcomp_salt_wt_pct, y = fcomp_additive_wt_pct)) +
        geom_point(aes(color = log10(ionic_conductivity_final)), size = 3) +
        scale_color_viridis_c() +
        labs(title = paste("PCB Grid -", step_name),
             x = "LiTFSI (wt%)", y = "Aerosil (wt%)",
             color = "log10(IC)") +
        theme_minimal()
      
      ggsave(output_path_pcb, p_pcb, width = 6, height = 4, dpi = 300)
      cat(sprintf("  ✓ Saved PCB heatmap: %s\n", basename(output_path_pcb)))
    }
  }, error = function(e) {
    cat(sprintf("  ✗ Error creating PCB heatmap: %s\n", e$message))
  })
  
  # Coin Cell Heatmap
  if (!is.null(output_path_coin)) {
    tryCatch({
      df_coin <- df %>%
        filter(flag_for_removal == FALSE, 
               fcomp_salt == "LiTFSI",
               print_type == "Coin cell")
      
      if (nrow(df_coin) == 0) {
        cat(sprintf("  ⚠ No Coin cell data available for heatmap at step: %s\n", step_name))
      } else {
        p_coin <- ggplot(df_coin, aes(x = fcomp_salt_wt_pct, y = fcomp_additive_wt_pct)) +
          geom_point(aes(color = log10(ionic_conductivity_final)), size = 3) +
          scale_color_viridis_c() +
          labs(title = paste("Coin Cell Grid -", step_name),
               x = "LiTFSI (wt%)", y = "Aerosil (wt%)",
               color = "log10(IC)") +
          theme_minimal()
        
        ggsave(output_path_coin, p_coin, width = 6, height = 4, dpi = 300)
        cat(sprintf("  ✓ Saved Coin cell heatmap: %s\n", basename(output_path_coin)))
      }
    }, error = function(e) {
      cat(sprintf("  ✗ Error creating Coin cell heatmap: %s\n", e$message))
    })
  }
}

# =============================================================================
# MAIN PIPELINE - STEP BY STEP
# =============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("STEP-BY-STEP POLYMER ELECTROLYTE ANALYSIS PIPELINE\n")
cat(strrep("=", 70), "\n\n")

# Initialize progress tracking
progress_log <- list()

# =============================================================================
# STEP 0: Load Raw Data
# =============================================================================
cat(strrep("=", 70), "\n")
cat("STEP 0: LOADING RAW DATA\n")
cat(strrep("=", 70), "\n")

df_raw <- load_pe_json(json_input)
df <- df_raw %>% mutate(flag_for_removal = FALSE)

progress_log[[1]] <- save_step_summary(df, 0, "Initial Load")
quick_heatmap(df, "Step 0 - Raw Data", 
              file.path(step_dir, "step0_raw_data_pcb.png"),
              file.path(step_dir, "step0_raw_data_coin.png"))

# =============================================================================
# STEP 1: Flag Bad Dates
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 1: FLAGGING BAD DATES\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_bad_dates_original()

progress_log[[2]] <- save_step_summary(df, 1, "Bad Dates")
quick_heatmap(df, "Step 1 - After Bad Dates", 
              file.path(step_dir, "step1_bad_dates_pcb.png"),
              file.path(step_dir, "step1_bad_dates_coin.png"))

# =============================================================================
# STEP 2: Flag Validation Sets
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 2: FLAGGING VALIDATION SETS\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_validation_sets()

progress_log[[3]] <- save_step_summary(df, 2, "Validation Sets")
quick_heatmap(df, "Step 2 - After Validation", 
              file.path(step_dir, "step2_validation_pcb.png"),
              file.path(step_dir, "step2_validation_coin.png"))

# =============================================================================
# STEP 3: Flag Off-Grid Compositions
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 3: FLAGGING OFF-GRID COMPOSITIONS\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_off_grid_compositions()

progress_log[[4]] <- save_step_summary(df, 3, "Off-Grid Compositions")
quick_heatmap(df, "Step 3 - After Grid Filter", 
              file.path(step_dir, "step3_grid_filter_pcb.png"),
              file.path(step_dir, "step3_grid_filter_coin.png"))

# =============================================================================
# STEP 4: Flag Formulation Types
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 4: FLAGGING FORMULATION TYPES\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_formulation_types()

progress_log[[5]] <- save_step_summary(df, 4, "Formulation Types")
quick_heatmap(df, "Step 4 - After Formulation", 
              file.path(step_dir, "step4_formulation_pcb.png"),
              file.path(step_dir, "step4_formulation_coin.png"))

# =============================================================================
# STEP 5: Flag Additive Types
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 5: FLAGGING ADDITIVE TYPES\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_additive_types()

progress_log[[6]] <- save_step_summary(df, 5, "Additive Types")
quick_heatmap(df, "Step 5 - After Additive", 
              file.path(step_dir, "step5_additive_pcb.png"),
              file.path(step_dir, "step5_additive_coin.png"))

# =============================================================================
# STEP 6: Flag IC Thresholds
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 6: FLAGGING IC THRESHOLDS\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_ic_thresholds_original()

progress_log[[7]] <- save_step_summary(df, 6, "IC Thresholds")
quick_heatmap(df, "Step 6 - After IC Thresholds", 
              file.path(step_dir, "step6_ic_thresholds_pcb.png"),
              file.path(step_dir, "step6_ic_thresholds_coin.png"))

# =============================================================================
# STEP 7: Flag Specific Samples
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 7: FLAGGING SPECIFIC SAMPLES\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_specific_sample()

progress_log[[8]] <- save_step_summary(df, 7, "Specific Samples")
quick_heatmap(df, "Step 7 - After Specific", 
              file.path(step_dir, "step7_specific_pcb.png"),
              file.path(step_dir, "step7_specific_coin.png"))

# =============================================================================
# STEP 8: Flag Z-Score Outliers
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 8: FLAGGING Z-SCORE OUTLIERS\n")
cat(strrep("=", 70), "\n")

df <- df %>% flag_zscore_outliers_original(threshold = 4)

progress_log[[9]] <- save_step_summary(df, 8, "Z-Score Outliers")
quick_heatmap(df, "Step 8 - After Z-Score", 
              file.path(step_dir, "step8_zscore_pcb.png"),
              file.path(step_dir, "step8_zscore_coin.png"))

cat("\n", strrep("=", 70), "\n")
cat("SAVING CLEANED DATA TO CSV\n")
cat(strrep("=", 70), "\n")

# Create clean dataset (remove flagged samples)
df_clean <- df %>% 
  filter(flag_for_removal == FALSE)

# Save full dataset with flags
full_csv_path <- file.path(output_dir, "pe_data_with_flags.csv")
write.csv(df, full_csv_path, row.names = FALSE)
cat(sprintf("✓ Saved full dataset (with flags): %s\n", basename(full_csv_path)))
cat(sprintf("  - Total samples: %d\n", nrow(df)))
cat(sprintf("  - Flagged: %d\n", sum(df$flag_for_removal, na.rm = TRUE)))
cat(sprintf("  - Clean: %d\n", sum(!df$flag_for_removal, na.rm = TRUE)))

# Save clean dataset only
clean_csv_path <- file.path(output_dir, "pe_data_clean.csv")
write.csv(df_clean, clean_csv_path, row.names = FALSE)
cat(sprintf("\n✓ Saved clean dataset: %s\n", basename(clean_csv_path)))
cat(sprintf("  - Total samples: %d\n", nrow(df_clean)))

# Save summary by print type
summary_by_type <- df_clean %>%
  group_by(print_type) %>%
  summarise(
    n_samples = n(),
    n_unique_samples = n_distinct(sample_ID),
    mean_IC = mean(ionic_conductivity_final, na.rm = TRUE),
    median_IC = median(ionic_conductivity_final, na.rm = TRUE),
    sd_IC = sd(ionic_conductivity_final, na.rm = TRUE),
    .groups = 'drop'
  )

summary_csv_path <- file.path(output_dir, "summary_by_print_type.csv")
write.csv(summary_by_type, summary_csv_path, row.names = FALSE)
cat(sprintf("\n✓ Saved summary by print type: %s\n", basename(summary_csv_path)))
print(summary_by_type)

cat(strrep("=", 70), "\n\n")

# =============================================================================
# STEP 9: Generate All Final Plots
# =============================================================================
cat("9.1 Creating heatmap...\n")
tryCatch({
  df_pcb_for_heatmap <- df |>
    dplyr::filter(flag_for_removal == FALSE,
                  fcomp_salt == "LiTFSI" | fcomp_salt == "none") |> 
    select_df(type = 'PCB')
  
  # DIAGNOSTIC: Show what compositions we're plotting
  cat("\n  Compositions being plotted:\n")
  compositions <- df_pcb_for_heatmap %>%
    distinct(fcomp_salt_wt_pct, fcomp_additive_wt_pct) %>%
    arrange(fcomp_salt_wt_pct, fcomp_additive_wt_pct)
  print(compositions, n = 100)
  cat(sprintf("\n  Total unique compositions: %d\n", nrow(compositions)))
  
  heatmap_pcb <- df_pcb_for_heatmap %>% heatmap_mean('PCB Print')
  ggsave(file.path(plot_dir, 'heatmap_pcb.png'), heatmap_pcb, 
         dpi = 600, width = 8, height = 6, bg = 'white')
  cat("  ✓ Saved: heatmap_pcb.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.2 Boxplots (Coin)
cat("\n9.2 Creating boxplots (coin cell)...\n")
tryCatch({
  salt_grid_1 <- c(0, 10, 20, 30)
  additive_grid_1 <- c(0, 2.14, 4.29, 6.43, 8.57, 10.70, 12.9, 15)
  
  additive_grid_2 <- c(0, 7.5, 15)
  salt_grid_2 <- c(0, 4.29, 8.57, 12.9, 17.1, 21.4, 25.7, 30)
  
  # Create boxplots
  boxplot_coin <- df |>
    dplyr::filter(fcomp_salt == "LiTFSI" | fcomp_salt == "none") |> 
    select_df(type = 'coin') |>
    boxplot_plotter('Coin Cell', salt_grid_1, additive_grid_1, 
                    salt_grid_2, additive_grid_2)
  
  # Save both plots
  ggsave(file.path(plot_dir, 'boxplot_coin_grid1.png'), boxplot_coin$plot1, 
         dpi = 600, width = 8, height = 6, bg = 'white')
  ggsave(file.path(plot_dir, 'boxplot_coin_grid2.png'), boxplot_coin$plot2, 
         dpi = 600, width = 8, height = 6, bg = 'white')
  ggsave(file.path(plot_dir, 'boxplot_coin_combined.png'), boxplot_coin$combined, 
         dpi = 600, width = 8, height = 12, bg = 'white')
})

# 9.3 Boxplots (PCB)
cat("\n9.3 Creating boxplots (PCB)...\n")
tryCatch({
  salt_grid_1 <- c(0, 10, 20, 30)
  additive_grid_1 <- c(0, 2.14, 4.29, 6.43, 8.57, 10.70, 12.9, 15)
  
  additive_grid_2 <- c(0, 7.5, 15)
  salt_grid_2 <- c(0, 4.29, 8.57, 12.9, 17.1, 21.4, 25.7, 30)
  
  # Create boxplots
  boxplot_coin <- df |>
    dplyr::filter(fcomp_salt == "LiTFSI" | fcomp_salt == "none") |> 
    select_df(type = 'PCB') |>
    boxplot_plotter('PCB print', salt_grid_1, additive_grid_1, 
                    salt_grid_2, additive_grid_2)
  
  # Save both plots
  ggsave(file.path(plot_dir, 'boxplot_pcb_grid1.png'), boxplot_coin$plot1, 
         dpi = 600, width = 8, height = 6, bg = 'white')
  ggsave(file.path(plot_dir, 'boxplot_pcb_grid2.png'), boxplot_coin$plot2, 
         dpi = 600, width = 8, height = 6, bg = 'white')
  ggsave(file.path(plot_dir, 'boxplot_pcb_combined.png'), boxplot_coin$combined, 
         dpi = 600, width = 8, height = 12, bg = 'white')
})

# 9.4 Grid plots
cat("\n9.4 Creating grid search plots...\n")
tryCatch({
  grid_coin <- select_df(df = df, type = 'coin') %>%
    gridSearch_plotter('Coin')
  ggsave(file.path(plot_dir, 'grid_coin.png'), grid_coin, 
         dpi = 600, width = 4, height = 3, bg = 'white')
  cat("  ✓ Saved: grid_coin.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

tryCatch({
  grid_pcb <- select_df(df = df, type = 'PCB') %>%
    gridSearch_plotter('PCB')
  ggsave(file.path(plot_dir, 'grid_pcb.png'), grid_pcb, 
         dpi = 600, width = 4, height = 3, bg = 'white')
  cat("  ✓ Saved: grid_pcb.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.5 Line plot
cat("\n9.5 Creating method comparison line plot...\n")
tryCatch({
  # Define your grids at the top of your script
  salt_grid_1 <- c(0, 10, 20, 30)
  additive_grid_1 <- c(0, 2.14, 4.29, 6.43, 7.5, 8.57, 10.70, 12.9, 15)
  
  line_plot <- df |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% salt_grid_1,
      fcomp_additive_wt_pct %in% additive_grid_1
    ) |>
    compare_line_plot(filter = salt_grid_1)
  
  ggsave(file.path(plot_dir, 'line_plot.png'), line_plot, 
         dpi = 600, width = 12, height = 6, bg = 'white')
  cat("  ✓ Saved: line_plot.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.6 Bland-Altman
cat("\n9.6 Creating Bland-Altman plot...\n")
tryCatch({
  ba_plot <- bland_altman_plot(df, salt_filter = c(0, 10, 20, 30))
  ggsave(file.path(plot_dir, 'bland_altman.png'), ba_plot, 
         dpi = 600, width = 4, height = 3, bg = 'white')
  cat("  ✓ Saved: bland_altman.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.7 Violin plot (uses full data)
cat("\n9.7 Creating violin plot...\n")
tryCatch({
  violin_plot <- additive_violin_plot(df_raw)
  ggsave(file.path(plot_dir, 'violin_additive.png'), violin_plot, 
         dpi = 600, width = 4, height = 3, bg = 'white')
  cat("  ✓ Saved: violin_additive.png\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.8 Summary bar plots
cat("\n9.8 Creating summary statistics plots...\n")
tryCatch({
  summary_plots <- create_summary_bar_plots(df)
  ggsave(file.path(plot_dir, 'summary_unique_samples.png'), summary_plots$unique_samples, 
         dpi = 600, width = 6, height = 2, bg = 'white')
  ggsave(file.path(plot_dir, 'summary_measurements.png'), summary_plots$total_measurements, 
         dpi = 600, width = 6, height = 2, bg = 'white')
  ggsave(file.path(plot_dir, 'summary_projected.png'), summary_plots$projected, 
         dpi = 600, width = 6, height = 2, bg = 'white')
  ggsave(file.path(plot_dir, 'summary_combined.png'), summary_plots$combined, 
         dpi = 600, width = 6, height = 6, bg = 'white')
  cat("  ✓ Saved: summary_*.png (4 files)\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# 9.9 Interactive 3D plot
cat("\n9.9 Creating interactive 3D PCB grid...\n")
tryCatch({
  plot_3d <- create_3d_pcb_grid(df, show_flagged = TRUE)
  save_3d_plot(plot_3d, file.path(plot_dir, 'pcb_grid_3d_interactive.html'))
  cat("  ✓ Saved: pcb_grid_3d_interactive.html\n")
}, error = function(e) cat(sprintf("  ✗ Error: %s\n", e$message)))

# =============================================================================
# STEP 10: Save Progress Log
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("STEP 10: SAVING PROGRESS LOG\n")
cat(strrep("=", 70), "\n")

progress_df <- do.call(rbind, progress_log)
write.csv(progress_df, file.path(output_dir, "filtering_progress.csv"), row.names = FALSE)

cat("\n")
cat(strrep("=", 70), "\n")
cat("PROGRESS SUMMARY\n")
cat(strrep("=", 70), "\n")
print(progress_df, row.names = FALSE)

# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PIPELINE COMPLETE\n")
cat(strrep("=", 70), "\n")
cat(sprintf("  Started with: %d samples\n", progress_df$total_samples[1]))
cat(sprintf("  Ended with: %d clean samples (%.1f%%)\n", 
            progress_df$clean[nrow(progress_df)],
            100 * progress_df$clean[nrow(progress_df)] / progress_df$total_samples[1]))
cat(sprintf("  Flagged: %d samples (%.1f%%)\n\n",
            progress_df$flagged[nrow(progress_df)],
            100 * progress_df$flagged[nrow(progress_df)] / progress_df$total_samples[1]))
cat(sprintf("  Step-by-step data: %s\n", step_dir))
cat(sprintf("  Final plots: %s\n", plot_dir))
cat(sprintf("  Progress log: %s\n", file.path(output_dir, "filtering_progress.csv")))
cat(strrep("=", 70), "\n\n")