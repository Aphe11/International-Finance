# ENHANCED QUANTILE REGRESSION PLOTS
library(ggplot2)
library(patchwork)
library(ggthemes)

# 1. MAIN UNCERTAINTY COEFFICIENT PLOT ----------------------------------------

plot_uncertainty_coefficients <- function(qr_results, title = "Quantile Regression: US Uncertainty Effects") {
  
  # Filter for uncertainty variables only
  uncertainty_data <- qr_results %>%
    filter(variable %in% c("macro_uncertainty_lag", "real_uncertainty_lag", "financial_uncertainty_lag")) %>%
    mutate(
      variable_clean = case_when(
        variable == "macro_uncertainty_lag" ~ "Macro Uncertainty",
        variable == "real_uncertainty_lag" ~ "Real Uncertainty", 
        variable == "financial_uncertainty_lag" ~ "Financial Uncertainty"
      ),
      dependent_clean = case_when(
        dependent_var == "gross_inflows" ~ "Gross Inflows",
        dependent_var == "gross_outflows" ~ "Gross Outflows",
        dependent_var == "net_inflows" ~ "Net Inflows"
      ),
      significant = p_value < 0.10
    )
  
  p <- ggplot(uncertainty_data, aes(x = tau, y = coefficient, color = variable_clean)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_line(size = 1.2) +
    geom_point(aes(shape = significant), size = 2.5) +
    geom_ribbon(aes(ymin = coefficient - 1.96*std_error, 
                    ymax = coefficient + 1.96*std_error, 
                    fill = variable_clean), 
                alpha = 0.2, color = NA) +
    facet_wrap(~dependent_clean, scales = "free_y", ncol = 1) +
    labs(
      title = title,
      subtitle = "Coefficient estimates across quantiles with 95% confidence intervals",
      x = "Quantile (τ)",
      y = "Coefficient Estimate (Billions USD)",
      color = "Uncertainty Type",
      fill = "Uncertainty Type",
      shape = "Significant (p < 0.10)"
    ) +
    scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_shape_manual(values = c(1, 16)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(p)
}

# 2. SINGLE VARIABLE ACROSS DEPENDENT VARIABLES --------------------------------

plot_single_variable <- function(qr_results, target_var, var_name) {
  
  plot_data <- qr_results %>%
    filter(variable == target_var) %>%
    mutate(
      dependent_clean = case_when(
        dependent_var == "gross_inflows" ~ "Gross Inflows",
        dependent_var == "gross_outflows" ~ "Gross Outflows", 
        dependent_var == "net_inflows" ~ "Net Inflows"
      ),
      significant = p_value < 0.10
    )
  
  p <- ggplot(plot_data, aes(x = tau, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_ribbon(aes(ymin = coefficient - 1.96*std_error, 
                    ymax = coefficient + 1.96*std_error), 
                alpha = 0.3, fill = "steelblue") +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(aes(shape = significant), color = "steelblue", size = 2) +
    facet_wrap(~dependent_clean, scales = "free_y") +
    labs(
      title = paste("Quantile Regression:", var_name),
      x = "Quantile (τ)", 
      y = "Coefficient Estimate (Billions USD)",
      shape = "Significant (p < 0.10)"
    ) +
    scale_shape_manual(values = c(1, 16)) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# 3. HEATMAP OF COEFFICIENTS --------------------------------------------------

plot_coefficient_heatmap <- function(qr_results) {
  
  # Prepare data for heatmap
  heatmap_data <- qr_results %>%
    filter(variable %in% c("macro_uncertainty_lag", "real_uncertainty_lag", "financial_uncertainty_lag",
                           "kopen", "gdpg_lag", "lnoil_lag")) %>%
    mutate(
      variable_clean = case_when(
        variable == "macro_uncertainty_lag" ~ "Macro\nUncertainty",
        variable == "real_uncertainty_lag" ~ "Real\nUncertainty",
        variable == "financial_uncertainty_lag" ~ "Financial\nUncertainty",
        variable == "kopen" ~ "Financial\nOpenness",
        variable == "gdpg_lag" ~ "GDP\nGrowth",
        variable == "lnoil_lag" ~ "Oil Prices\n(Log)"
      ),
      dependent_clean = case_when(
        dependent_var == "gross_inflows" ~ "Gross\nInflows",
        dependent_var == "gross_outflows" ~ "Gross\nOutflows",
        dependent_var == "net_inflows" ~ "Net\nInflows"
      ),
      significance_star = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**", 
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
  
  p <- ggplot(heatmap_data, aes(x = factor(tau), y = variable_clean, fill = coefficient)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(coefficient, 1), significance_star)), 
              size = 3, color = "black") +
    facet_grid(~dependent_clean) +
    scale_fill_gradient2(
      low = "red", 
      mid = "white", 
      high = "blue",
      midpoint = 0,
      name = "Coefficient\nValue"
    ) +
    labs(
      title = "Quantile Regression Coefficients Heatmap",
      subtitle = "Color intensity shows coefficient magnitude; ***p<0.01, **p<0.05, *p<0.10",
      x = "Quantile (τ)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

# 4. COMPARISON PLOT: QUANTILE VS OLS -----------------------------------------

plot_quantile_ols_comparison <- function(qr_results, ols_results, target_vars) {
  
  # Prepare quantile data (median)
  quantile_median <- qr_results %>%
    filter(tau == 0.5, variable %in% target_vars) %>%
    mutate(
      variable_clean = case_when(
        variable == "macro_uncertainty_lag" ~ "Macro Uncertainty",
        variable == "real_uncertainty_lag" ~ "Real Uncertainty",
        variable == "financial_uncertainty_lag" ~ "Financial Uncertainty",
        variable == "kopen" ~ "Financial Openness",
        TRUE ~ variable
      ),
      method = "Quantile (Median)"
    )
  
  # Prepare OLS data
  ols_clean <- ols_results %>%
    filter(term %in% target_vars) %>%
    rename(
      variable = term,
      coefficient = estimate,
      std_error = std.error,
      p_value = p.value
    ) %>%
    mutate(
      variable_clean = case_when(
        variable == "macro_uncertainty_lag" ~ "Macro Uncertainty",
        variable == "real_uncertainty_lag" ~ "Real Uncertainty", 
        variable == "financial_uncertainty_lag" ~ "Financial Uncertainty",
        variable == "kopen" ~ "Financial Openness",
        TRUE ~ variable
      ),
      method = "OLS",
      dependent_var = case_when(
        Model == "Gross_Inflows" ~ "gross_inflows",
        Model == "Gross_Outflows" ~ "gross_outflows",
        Model == "Net_Inflows" ~ "net_inflows"
      )
    )
  
  # Combine data
  comparison_data <- bind_rows(quantile_median, ols_clean) %>%
    mutate(
      dependent_clean = case_when(
        dependent_var == "gross_inflows" ~ "Gross Inflows",
        dependent_var == "gross_outflows" ~ "Gross Outflows",
        dependent_var == "net_inflows" ~ "Net Inflows"
      )
    )
  
  p <- ggplot(comparison_data, aes(x = variable_clean, y = coefficient, fill = method)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = coefficient - 1.96*std_error, 
                      ymax = coefficient + 1.96*std_error),
                  position = position_dodge(width = 0.8), 
                  width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~dependent_clean, scales = "free_y") +
    labs(
      title = "Comparison: Quantile Regression (Median) vs OLS Estimates",
      subtitle = "Bars show coefficient estimates; error bars show 95% confidence intervals",
      x = NULL,
      y = "Coefficient Estimate (Billions USD)",
      fill = "Method"
    ) +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

# 5. EXECUTE ALL PLOTS --------------------------------------------------------

# Create directory for plots
if (!dir.exists("quantile_plots2")) dir.create("quantile_plots2")

# Plot 1: Main uncertainty coefficients
p1 <- plot_uncertainty_coefficients(qr_results)
ggsave("quantile_plots/uncertainty_coefficients.png", p1, width = 10, height = 8, dpi = 300)

# Plot 2: Key control variables
p2 <- plot_single_variable(qr_results, "kopen", "Financial Openness")
p3 <- plot_single_variable(qr_results, "gdpg_lag", "GDP Growth")
p4 <- plot_single_variable(qr_results, "lnoil_lag", "Oil Prices (Log)")

combined_controls <- p2 / p3 / p4
ggsave("quantile_plots/control_variables.png", combined_controls, width = 10, height = 10, dpi = 300)

# Plot 3: Coefficient heatmap
p5 <- plot_coefficient_heatmap(qr_results)
ggsave("quantile_plots/coefficient_heatmap.png", p5, width = 12, height = 6, dpi = 300)

# Plot 4: Quantile vs OLS comparison
target_vars_compare <- c("macro_uncertainty_lag", "real_uncertainty_lag", "financial_uncertainty_lag", "kopen")
p6 <- plot_quantile_ols_comparison(qr_results, ols_lagged_results, target_vars_compare)
ggsave("quantile_plots/quantile_vs_ols.png", p6, width = 12, height = 6, dpi = 300)

cat("All plots saved to 'quantile_plots' directory!\n")
print(p1)  # Display the main plot
