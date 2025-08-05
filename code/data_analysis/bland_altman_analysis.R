# ============================================================================
# Bland-Altman Analysis for Method Comparison
#
# Description: Functions for performing Bland-Altman analysis to compare
#              measurement methods (Coin Cell vs PCB) in polymer electrolyte
#              conductivity measurements. Includes both standard and
#              log-transformed analyses.
#
# Author: [Your Name]
# Date: [Date]
# Version: 1.0
#
# Required packages:
#   - dplyr: for data manipulation
#   - ggplot2: for plotting
#   - cowplot: for plot themes
#   - plotly: for interactive plots
#   - stats: for statistical functions
#   - goeveg: for coefficient of variation
#   - ggpmisc: for regression equations
#   - SimplyAgree: for Deming regression
#   - ggpubr: for Q-Q plots
#   - glue: for string formatting
#   - grid: for plot margins
#   - stringi: for string manipulation
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cowplot)
library(plotly)
library(stats)
library(goeveg)
library(ggpmisc)
library(SimplyAgree)
library(ggpubr)
library(glue)
library(grid)
library(stringi)

# load function from other files

source('~/Git/SPOC_code/code/data_cleaning/database_outlier_remove.R')

# Basic Bland-Altman Analysis Functions ----------------------------------------

# Perform basic Bland-Altman analysis
basic_bland_altman <- function(df_pick) {
  # Prepare data for Bland-Altman analysis
  ba_plot_tab1 <- select_df(df_pick, type = 'coin') |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::mutate(avg_gold = mean(ionic_conductivity_final)) |>
    dplyr::distinct(fcomp_formulation, avg_gold)
  
  ba_plot_tab2 <- select_df(df_pick, type = 'PCB') |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::mutate(avg_pcb = mean(ionic_conductivity_final)) |>
    dplyr::distinct(fcomp_formulation, avg_pcb)
  
  ba_plot_tab <- ba_plot_tab1 |>
    dplyr::full_join(ba_plot_tab2) |>
    dplyr::mutate(avg = mean(avg_gold, avg_pcb), diff = avg_gold - avg_pcb) |>
    dplyr::filter(!is.na(avg_gold) == TRUE)
  
  # Calculate Bland-Altman statistics
  mean_diff <- mean(ba_plot_tab$diff)
  lower <- mean_diff - 1.96 * stats::sd(ba_plot_tab$diff)
  upper <- mean_diff + 1.96 * stats::sd(ba_plot_tab$diff)
  
  # Create Bland-Altman plot
  p <- ggplot(ba_plot_tab, aes(x = avg, y = diff, key = fcomp_formulation)) +
    geom_point(size = 2) +
    geom_hline(yintercept = mean_diff) +
    geom_hline(yintercept = lower,
               color = "red",
               linetype = "dashed") +
    geom_hline(yintercept = upper,
               color = "red",
               linetype = "dashed") +
    ggtitle("Bland-Altman Plot") +
    ylab("Difference Between Instruments") +
    xlab("Average") +
    theme_bw()
  
  # Return both plot and data
  return(list(
    plot = p,
    data = ba_plot_tab,
    stats = list(
      mean_diff = mean_diff,
      lower = lower,
      upper = upper
    )
  ))
}

# Advanced Bland-Altman Analysis Functions -------------------------------------

# Prepare coin cell data with log transformation
prepare_coin_data_log <- function(final_db_outlier, filter_salt_range) {
  select_df(final_db_outlier, type = 'coin') |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% filter_salt_range,!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.9, 7.1)
    ) |>
    dplyr::mutate(fcomp_formulation = stringi::stri_replace_all_regex(fcomp_formulation, '4.28', '4.29')) |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::filter(fab_method == 2) |>
    dplyr::mutate(
      avg_gold_log = mean(log10(ionic_conductivity_final)),
      avg_gold = mean(ionic_conductivity_final),
      cv_gold_log = goeveg::cv(log10(ionic_conductivity_final)),
      cv_gold = goeveg::cv(ionic_conductivity_final)
    ) |>
    dplyr::distinct(
      fcomp_formulation,
      avg_gold,
      avg_gold_log,
      fcomp_salt_wt_pct,
      fcomp_additive_wt_pct,
      cv_gold,
      cv_gold_log
    )
}

# Prepare PCB data with log transformation
prepare_pcb_data_log <- function(final_db_outlier, filter_salt_range) {
  select_df(final_db_outlier, type = 'PCB') |>
    dplyr::filter(
      fcomp_salt_wt_pct %in% filter_salt_range,!fcomp_additive_wt_pct %in% c(1.4, 5, 5.7, 10, 7.5, 2.86, 7.14)
    ) |>
    dplyr::group_by(fcomp_formulation) |>
    dplyr::mutate(
      avg_pcb_log = mean(log10(ionic_conductivity_final)),
      avg_pcb = mean(ionic_conductivity_final),
      cv_pcb_log = goeveg::cv(log10(ionic_conductivity_final)),
      cv_pcb = goeveg::cv(ionic_conductivity_final)
    ) |>
    dplyr::distinct(
      fcomp_formulation,
      avg_pcb,
      avg_pcb_log,
      fcomp_salt_wt_pct,
      fcomp_additive_wt_pct,
      cv_pcb,
      cv_pcb_log
    )
}

# Combine and process Bland-Altman data
process_bland_altman_data <- function(ba_plot_tab1_log,
                                      ba_plot_tab2_log,
                                      filter_salt_range) {
  ba_plot_tab_log <- ba_plot_tab1_log |>
    dplyr::full_join(ba_plot_tab2_log) |>
    dplyr::mutate(
      avg = mean(avg_gold, avg_pcb),
      avg_log = mean(avg_gold_log, avg_pcb_log),
      diff = avg_gold - avg_pcb,
      diff_log = avg_gold_log - avg_pcb_log
    ) |>
    dplyr::filter(!is.na(avg_gold) == TRUE,
                  fcomp_salt_wt_pct %in% filter_salt_range)
  
  return(ba_plot_tab_log)
}

# Calculate Bland-Altman statistics
calculate_ba_statistics <- function(ba_plot_tab_log) {
  mean_diff_log <- signif(mean(ba_plot_tab_log$diff_log), digits = 2)
  std_diff_log <- signif(stats::sd(ba_plot_tab_log$diff_log), digits = 2)
  mean_diff <- mean(ba_plot_tab_log$diff)
  
  # Calculate limits of agreement
  lower_log <- signif((mean_diff_log - 1.96 * stats::sd(ba_plot_tab_log$diff_log)), digits = 2)
  upper_log <- signif((mean_diff_log + 1.96 * stats::sd(ba_plot_tab_log$diff_log)), digits = 2)
  
  lower <- mean_diff - 1.96 * stats::sd(ba_plot_tab_log$diff)
  upper <- mean_diff + 1.96 * stats::sd(ba_plot_tab_log$diff)
  
  return(
    list(
      mean_diff_log = mean_diff_log,
      std_diff_log = std_diff_log,
      mean_diff = mean_diff,
      lower_log = lower_log,
      upper_log = upper_log,
      lower = lower,
      upper = upper
    )
  )
}

# Create enhanced Bland-Altman plot with log transformation
create_ba_plot_log <- function(ba_plot_tab_log, stats) {
  bland_altman_plot_log <- ggplot2::ggplot(
    ba_plot_tab_log,
    ggplot2::aes(
      x = avg_log,
      y = diff_log,
      key = fcomp_formulation,
      color = fcomp_salt_wt_pct |> as.factor()
    )
  ) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    cowplot::theme_half_open() +
    ggplot2::geom_hline(yintercept = stats$mean_diff_log,
                        color = 'blue') +
    ggplot2::geom_hline(
      yintercept = stats$lower_log,
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = stats$upper_log,
      color = "red",
      linetype = "dashed"
    ) +
    # Add statistical annotations
    ggplot2::annotate(
      'text',
      label = glue::glue('LoA = {stats$upper_log}'),
      y = stats$upper_log - 0.55 * max(range(ba_plot_tab_log$diff_log)),
      x = mean(range(ba_plot_tab_log$avg_log)),
      color = 'red'
    ) +
    ggplot2::annotate(
      'text',
      label = glue::glue('LoA = {stats$lower_log}'),
      y = stats$lower_log + max(range(ba_plot_tab_log$diff_log)) * 0.45,
      x = mean(range(ba_plot_tab_log$avg_log)),
      color = 'red'
    ) +
    ggplot2::annotate(
      'text',
      label = glue::glue('mean = {stats$mean_diff_log}'),
      y = stats$mean_diff_log * 0.70,
      x = mean(range(ba_plot_tab_log$avg_log)),
      color = 'blue'
    ) +
    ggplot2::ylim(c(-1.5, 1)) +
    ggplot2::scale_color_viridis_d(option = 'plasma', limits = c(10, 30, 20, 0)) +
    ggplot2::guides(color = ggplot2::guide_legend(title = 'LiTFSI (wt%)')) +
    ggplot2::ylab(expression( ~ Delta ~ 'Log'[10] ~ 'Ionic Conductivity'['(Coin Cell - PCB)'])) +
    ggplot2::xlab(expression('Average Log'[10] ~ 'Ionic Conductivity')) +
    ggplot2::labs(title = 'e)', color = 'LiTFSI wt%') +
    ggplot2::theme(
      legend.position = 'right',
      legend.title = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 12),
      plot.title.position = 'plot',
      plot.title = ggplot2::element_text(
        size = 16,
        vjust = -6,
        hjust = -0.02
      ),
      plot.margin = grid::unit(c(-5, 0, 0, 3), "mm")
    ) +
    ggplot2::scale_color_viridis_d(limits = c('30', '20', '10', '0'))
  
  return(bland_altman_plot_log)
}

# Create correlation plot between methods
create_correlation_plot <- function(ba_plot_tab_log) {
  correlation_plot <- ggplot2::ggplot(data = ba_plot_tab_log, ggplot2::aes(x = avg_gold_log, y = avg_pcb_log)) +
    cowplot::theme_half_open() +
    cowplot::background_grid() +
    ggplot2::geom_point(size = 2) +
    ggpmisc::stat_poly_line(se = TRUE,
                            linetype = 'solid',
                            color = 'black') +
    ggpmisc::stat_poly_eq(formula = y ~ x, ggpmisc::use_label(c('eq', 'R2'))) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = 'black',
      linetype = 'dashed'
    ) +
    ggplot2::ylab(expression('Avg PCB Log'[10] ~ 'IC')) +
    ggplot2::xlab(expression('Avg Coin Cell Log'[10] ~ 'IC')) +
    ggplot2::facet_wrap(. ~ fcomp_salt_wt_pct)
  
  return(correlation_plot)
}

# Create Q-Q plots for normality assessment
create_qq_plots <- function(ba_plot_tab_log) {
  qq_log_trans <- ggpubr::ggqqplot(data = ba_plot_tab_log, x = 'diff_log', group = '') +
    ggplot2::xlab('Theoretical Quantiles') +
    ggplot2::ylab('Sample Quantiles')
  
  qq_IC <- ggpubr::ggqqplot(data = ba_plot_tab_log, x = 'diff') +
    ggplot2::xlab('Theoretical Quantiles') +
    ggplot2::ylab('Sample Quantiles')
  
  return(list(qq_log = qq_log_trans, qq_raw = qq_IC))
}

# Statistical Testing Functions ------------------------------------------------

# Perform F-test for variance comparison between methods
perform_f_test <- function(coin_data, pcb_data) {
  # Perform F-test
  f_test_result <- var.test(coin_data, pcb_data)
  
  return(
    list(
      f_statistic = f_test_result$statistic,
      p_value = f_test_result$p.value,
      df1 = f_test_result$parameter[1],
      df2 = f_test_result$parameter[2],
      conf_int = f_test_result$conf.int,
      ratio = f_test_result$estimate
    )
  )
}

# Compare methods by groups using F-test
compare_methods_by_group <- function(final_db_outlier,
                                     group_by_vars = c("fcomp_additive_wt_pct", "fcomp_salt_wt_pct")) {
  # Prepare data for both methods
  coin_data <- select_df(final_db_outlier, type = 'coin') |>
    dplyr::select(all_of(c(
      group_by_vars, "ionic_conductivity_final"
    )))
  
  pcb_data <- select_df(final_db_outlier, type = 'PCB') |>
    dplyr::select(all_of(c(
      group_by_vars, "ionic_conductivity_final"
    )))
  
  # Group and perform F-tests
  results <- coin_data |>
    dplyr::inner_join(pcb_data, by = group_by_vars, suffix = c("_coin", "_pcb")) |>
    dplyr::group_by(across(all_of(group_by_vars))) |>
    dplyr::group_modify( ~ {
      # Get conductivity values for each method
      coin_values <- .x$ionic_conductivity_final_coin
      pcb_values <- .x$ionic_conductivity_final_pcb
      
      # Check if we have enough data
      if (length(coin_values) < 2 || length(pcb_values) < 2) {
        return(
          tibble::tibble(
            n_coin = length(coin_values),
            n_pcb = length(pcb_values),
            mean_coin = mean(coin_values, na.rm = TRUE),
            mean_pcb = mean(pcb_values, na.rm = TRUE),
            var_coin = var(coin_values, na.rm = TRUE),
            var_pcb = var(pcb_values, na.rm = TRUE),
            f_statistic = NA_real_,
            p_value = NA_real_,
            significant = NA
          )
        )
      }
      
      # Perform F-test
      f_result <- perform_f_test(coin_values, pcb_values)
      
      # Return results
      tibble::tibble(
        n_coin = length(coin_values),
        n_pcb = length(pcb_values),
        mean_coin = mean(coin_values, na.rm = TRUE),
        mean_pcb = mean(pcb_values, na.rm = TRUE),
        var_coin = var(coin_values, na.rm = TRUE),
        var_pcb = var(pcb_values, na.rm = TRUE),
        f_statistic = f_result$f_statistic,
        p_value = f_result$p_value,
        significant = f_result$p_value < 0.05
      )
    }) |>
    dplyr::ungroup()
  
  return(results)
}

# Perform comprehensive method comparison with multiple tests
comprehensive_method_comparison <- function(final_db_outlier, filter_salt_range = NULL) {
  # Apply salt range filter if provided
  if (!is.null(filter_salt_range)) {
    final_db_outlier <- final_db_outlier |>
      dplyr::filter(fcomp_salt_wt_pct %in% filter_salt_range)
  }
  
  # Get data for each method
  coin_data <- select_df(final_db_outlier, type = 'coin')
  pcb_data <- select_df(final_db_outlier, type = 'PCB')
  
  # 1. F-test by groups
  f_test_results <- compare_methods_by_group(final_db_outlier)
  
  # 2. Overall F-test
  overall_f_test <- perform_f_test(coin_data$ionic_conductivity_final,
                                   pcb_data$ionic_conductivity_final)
  
  # 3. T-test (paired where possible)
  # Match formulations
  matched_data <- coin_data |>
    dplyr::inner_join(pcb_data, by = "fcomp_formulation", suffix = c("_coin", "_pcb"))
  
  if (nrow(matched_data) > 0) {
    paired_t_test <- t.test(
      matched_data$ionic_conductivity_final_coin,
      matched_data$ionic_conductivity_final_pcb,
      paired = TRUE
    )
  } else {
    paired_t_test <- NULL
  }
  
  # 4. Wilcoxon test (non-parametric alternative)
  if (nrow(matched_data) > 0) {
    wilcox_test <- wilcox.test(
      matched_data$ionic_conductivity_final_coin,
      matched_data$ionic_conductivity_final_pcb,
      paired = TRUE
    )
  } else {
    wilcox_test <- NULL
  }
  
  # 5. Log-transformed tests
  coin_log <- log10(coin_data$ionic_conductivity_final)
  pcb_log <- log10(pcb_data$ionic_conductivity_final)
  
  f_test_log <- perform_f_test(coin_log, pcb_log)
  
  # Return comprehensive results
  return(
    list(
      f_test_by_group = f_test_results,
      overall_f_test = overall_f_test,
      paired_t_test = paired_t_test,
      wilcoxon_test = wilcox_test,
      f_test_log_transformed = f_test_log,
      summary_stats = list(
        coin = summary(coin_data$ionic_conductivity_final),
        pcb = summary(pcb_data$ionic_conductivity_final)
      )
    )
  )
}

# Create visualization for F-test results
plot_f_test_results <- function(f_test_results) {
  # Create variance ratio plot
  variance_plot <- ggplot2::ggplot(
    f_test_results,
    ggplot2::aes(
      x = interaction(fcomp_additive_wt_pct, fcomp_salt_wt_pct),
      y = var_coin / var_pcb,
      fill = significant
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 1,
                        linetype = "dashed",
                        color = "red") +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "gray50"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")
    ) +
    ggplot2::labs(
      x = "Additive wt% . Salt wt%",
      y = "Variance Ratio (Coin/PCB)",
      title = "F-test: Variance Comparison Between Methods",
      fill = "Significant"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Create p-value plot
  p_value_plot <- ggplot2::ggplot(
    f_test_results,
    ggplot2::aes(
      x = fcomp_additive_wt_pct,
      y = fcomp_salt_wt_pct,
      fill = -log10(p_value)
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = -log10(0.05),
      name = "-log10(p-value)"
    ) +
    ggplot2::labs(x = "Additive wt%", y = "Salt wt%", title = "F-test p-values Heatmap") +
    ggplot2::theme_bw()
  
  return(list(variance_plot = variance_plot, p_value_plot = p_value_plot))
}

# Main Bland-Altman Analysis Function ------------------------------------------

# Perform complete Bland-Altman analysis
perform_bland_altman_analysis <- function(final_db_outlier,
                                          filter_salt_range,
                                          manuscript_directory) {
  # Prepare data
  ba_plot_tab1_log <- prepare_coin_data_log(final_db_outlier, filter_salt_range)
  ba_plot_tab2_log <- prepare_pcb_data_log(final_db_outlier, filter_salt_range)
  ba_plot_tab_log <- process_bland_altman_data(ba_plot_tab1_log, ba_plot_tab2_log, filter_salt_range)
  
  # Calculate statistics
  ba_stats <- calculate_ba_statistics(ba_plot_tab_log)
  
  # Perform normality test
  filtered_salt <- ba_plot_tab_log |> dplyr::filter(fcomp_salt_wt_pct %in% filter_salt_range)
  shap_test <- shapiro.test(filtered_salt$diff_log)
  shap_test_p <- shap_test$p.value |> signif(digits = 2)
  shap_test_w <- shap_test$statistic |> signif(digits = 2)
  
  # Create plots
  bland_altman_plot_log <- create_ba_plot_log(ba_plot_tab_log, ba_stats)
  correlation_plot <- create_correlation_plot(ba_plot_tab_log)
  qq_plots <- create_qq_plots(ba_plot_tab_log)
  
  # Perform method comparison tests
  method_comparison <- comprehensive_method_comparison(final_db_outlier, filter_salt_range)
  
  # Create F-test visualizations
  f_test_plots <- plot_f_test_results(method_comparison$f_test_by_group)
  
  # Save plots
  ggplot2::ggsave(
    bland_altman_plot_log,
    filename = glue::glue('{manuscript_directory}2304-10_pct_blandAltman_plot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  ggplot2::ggsave(
    correlation_plot,
    filename = glue::glue('{manuscript_directory}2304-DemingRegression_plot.png'),
    dpi = 600,
    width = 8,
    height = 6,
    bg = 'white'
  )
  
  ggplot2::ggsave(
    qq_plots$qq_log,
    filename = glue::glue('{manuscript_directory}2304-LogTransIC_diffQQ_plot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  ggplot2::ggsave(
    qq_plots$qq_raw,
    filename = glue::glue('{manuscript_directory}2304-IC_diffQQ_plot.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  # Save F-test plots
  ggplot2::ggsave(
    f_test_plots$variance_plot,
    filename = glue::glue('{manuscript_directory}2304-F_test_variance_plot.png'),
    dpi = 600,
    width = 8,
    height = 6,
    bg = 'white'
  )
  
  ggplot2::ggsave(
    f_test_plots$p_value_plot,
    filename = glue::glue('{manuscript_directory}2304-F_test_pvalue_heatmap.png'),
    dpi = 600,
    width = 6,
    height = 5,
    bg = 'white'
  )
  
  # Perform Deming regression
  dem_IC <- SimplyAgree::dem_reg(
    x = 'avg_gold_log',
    y = 'avg_pcb_log',
    data = ba_plot_tab_log,
    error.ratio = 4,
    weighted = FALSE
  )
  
  plot(dem_IC)
  dem_IC_check <- SimplyAgree::check(dem_IC) +
    ggplot2::theme(title = ggplot2::element_text(size = 6))
  
  ggplot2::ggsave(
    dem_IC_check,
    filename = glue::glue('{manuscript_directory}2304-demIC_check.png'),
    dpi = 600,
    width = 4,
    height = 3,
    bg = 'white'
  )
  
  # Return results
  return(
    list(
      data = ba_plot_tab_log,
      stats = ba_stats,
      shapiro_test = list(p_value = shap_test_p, statistic = shap_test_w),
      deming_regression = dem_IC,
      method_comparison = method_comparison,
      plots = list(
        bland_altman = bland_altman_plot_log,
        correlation = correlation_plot,
        qq_log = qq_plots$qq_log,
        qq_raw = qq_plots$qq_raw,
        deming_check = dem_IC_check,
        f_test_variance = f_test_plots$variance_plot,
        f_test_pvalue = f_test_plots$p_value_plot
      )
    )
  )
}

# Usage Example ----------------------------------------------------------------
filter_salt_range <- c(10, 0, 20, 30)
final_db_outlier <- readr::read_csv('~/Git/SPOC_code/data/database/pe_database.csv') |>
  apply_outlier_removal()
  
  results <- perform_bland_altman_analysis(final_db_outlier, filter_salt_range, manuscript_directory)
#
# # View F-test results by group
# print(results$method_comparison$f_test_by_group)
#
# # For basic Bland-Altman analysis:
# basic_results <- basic_bland_altman(final_db_outlier)
# plotly::ggplotly(basic_results$plot, tooltip = c('fcomp_formulation'))
#
# # For standalone method comparison:
# comparison_results <- comprehensive_method_comparison(final_db_outlier)
# print(comparison_results$f_test_by_group)