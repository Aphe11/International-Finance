# Load required libraries
library(dplyr)
library(quantreg)
library(ggplot2)
library(plm)
library(stargazer)
library(openxlsx)
library(lubridate)
library(tidyr)
library(tidyverse)
library(purrr)
library(zoo)
library(broom)

# Load datasets
panel_data <- read.csv("capital_flows_panel.csv", stringsAsFactors = FALSE) %>%
  mutate(time = as.Date(time), country = as.character(country))

ts_data <- read.csv("timeseries_controls.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))

uncertainty_data <- read.csv("us_uncertainty.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))

# Convert uncertainty data to quarterly
convert_to_quarterly <- function(monthly_data, date_col, value_col) {
  monthly_data %>%
    mutate(qtr_date = as.yearqtr(!!sym(date_col))) %>%
    group_by(qtr_date) %>%
    summarise(quarterly_value = (3 * last(!!sym(value_col)) + 
                                   2 * nth(!!sym(value_col), 2) + 
                                   1 * first(!!sym(value_col))) / 6) %>%
    rename(!!paste0(value_col, "_qtr") := quarterly_value)
}

macro_unc_qtr <- convert_to_quarterly(uncertainty_data, "date", "macro_usuncertainty")
real_unc_qtr <- convert_to_quarterly(uncertainty_data, "date", "real_usuncertainty")
financial_unc_qtr <- convert_to_quarterly(uncertainty_data, "date", "finan_usuncertainty")

uncertainty_qtr <- macro_unc_qtr %>%
  left_join(real_unc_qtr, by = "qtr_date") %>%
  left_join(financial_unc_qtr, by = "qtr_date") %>%
  mutate(year = year(qtr_date), quarter = quarter(qtr_date))

# Merge all datasets
panel_data_clean <- panel_data %>%
  mutate(year = year(time), quarter = quarter(time))

final_panel <- panel_data_clean %>%
  left_join(uncertainty_qtr %>% select(-qtr_date), by = c("year", "quarter")) %>%
  left_join(ts_data %>% 
              mutate(year = year(date), quarter = quarter(date)) %>% 
              select(-date), 
            by = c("year", "quarter"))

# Data preparation with lagged variables
final_panel_clean <- final_panel %>%
  mutate(across(c(gross_inflows, gross_outflows, net_inflows, kopen, gdp_pc, gdpg, cpi, 
                  exchange_rate, reserves, ir_change, lnoil, commodity,
                  macro_usuncertainty_qtr, real_usuncertainty_qtr, finan_usuncertainty_qtr),
                as.numeric)) %>%
  # Convert capital flows and reserves to billions of USD
  mutate(
    gross_inflows = gross_inflows / 1e9,
    gross_outflows = gross_outflows / 1e9,
    net_inflows = net_inflows / 1e9,
    reserves = reserves / 1e9
  ) %>%
  group_by(country) %>%
  arrange(time) %>%
  mutate(
    macro_uncertainty_lag = lag(macro_usuncertainty_qtr, 1),
    real_uncertainty_lag = lag(real_usuncertainty_qtr, 1),
    financial_uncertainty_lag = lag(finan_usuncertainty_qtr, 1),
    kopen = kopen,
    gdp_pc_lag = lag(gdp_pc, 1),
    gdpg_lag = lag(gdpg, 1),
    cpi_lag = lag(cpi, 1),
    exchange_rate_lag = lag(exchange_rate, 1),
    reserves_lag = lag(reserves, 1),
    ir_change_lag = lag(ir_change, 1),
    lnoil_lag = lag(lnoil, 1),
    commodity_lag = lag(commodity, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(gross_inflows) | !is.na(gross_outflows) | !is.na(net_inflows))

final_panel_clean <- pdata.frame(final_panel_clean, index = c("country", "time"))

#------------------------------------------------------Pre-estimation procedures

# Descriptive statistics
key_vars <- c("gross_inflows", "gross_outflows", "net_inflows",
              "macro_uncertainty_lag", "real_uncertainty_lag", "financial_uncertainty_lag",
              "kopen", "gdp_pc_lag", "gdpg_lag", "cpi_lag", "exchange_rate_lag",
              "reserves_lag", "ir_change_lag", "lnoil_lag", "commodity_lag")

desc_stats <- data.frame()
for(var in key_vars) {
  if(var %in% names(final_panel_clean)) {
    x <- final_panel_clean[[var]]
    stats <- data.frame(
      Variable = var,
      N = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE)
    )
    desc_stats <- rbind(desc_stats, stats)
  }
}
desc_stats <- desc_stats %>% mutate(across(-Variable, ~round(., 4)))

# Correlation matrix
cor_vars <- c("gross_inflows", "gross_outflows", "net_inflows",
              "macro_uncertainty_lag", "real_uncertainty_lag", "financial_uncertainty_lag",
              "kopen", "gdp_pc_lag", "gdpg_lag", "cpi_lag", "exchange_rate_lag",
              "reserves_lag", "ir_change_lag", "lnoil_lag", "commodity_lag")

cor_data <- final_panel_clean[cor_vars] %>% na.omit()
correlation_matrix <- cor(cor_data)
#---------------------------------------------------------------Model Estimation
# OLS baseline models
ols_data <- as.data.frame(final_panel_clean) %>%
  select(country, time, gross_inflows, gross_outflows, net_inflows,
         macro_uncertainty_lag, real_uncertainty_lag, financial_uncertainty_lag,
         kopen, gdp_pc_lag, gdpg_lag, cpi_lag, exchange_rate_lag, 
         reserves_lag, ir_change_lag, lnoil_lag, commodity_lag) %>%
  na.omit()

ols_formula_lagged <- as.formula("~ macro_uncertainty_lag + real_uncertainty_lag + 
                                 financial_uncertainty_lag + kopen + gdp_pc_lag + 
                                 gdpg_lag + cpi_lag + exchange_rate_lag + reserves_lag + 
                                 ir_change_lag + lnoil_lag + commodity_lag")

ols_gross_inflows_lagged <- lm(update.formula(ols_formula_lagged, gross_inflows ~ .), data = ols_data)
ols_gross_outflows_lagged <- lm(update.formula(ols_formula_lagged, gross_outflows ~ .), data = ols_data)
ols_net_inflows_lagged <- lm(update.formula(ols_formula_lagged, net_inflows ~ .), data = ols_data)

ols_lagged_results <- bind_rows(
  tidy(ols_gross_inflows_lagged) %>% mutate(Model = "Gross_Inflows"),
  tidy(ols_gross_outflows_lagged) %>% mutate(Model = "Gross_Outflows"),
  tidy(ols_net_inflows_lagged) %>% mutate(Model = "Net_Inflows")
)

ols_model_fit <- bind_rows(
  glance(ols_gross_inflows_lagged) %>% mutate(Model = "Gross_Inflows"),
  glance(ols_gross_outflows_lagged) %>% mutate(Model = "Gross_Outflows"),
  glance(ols_net_inflows_lagged) %>% mutate(Model = "Net_Inflows")
) %>%
  select(Model, r.squared, adj.r.squared, sigma, statistic, p.value, df, logLik, AIC, BIC)
print(ols_model_fit)

# Quantile regression analysis
quantile_data <- as.data.frame(final_panel_clean) %>%
  select(country, time, all_of(key_vars)) %>%
  na.omit()

run_quantile_regression <- function(dependent_var, data, tau_values = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  formula <- as.formula(paste(dependent_var, "~ macro_uncertainty_lag + real_uncertainty_lag + 
                              financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                              cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                              lnoil_lag + commodity_lag"))
  
  results_list <- list()
  for(tau in tau_values) {
    qr_model <- rq(formula, data = data, tau = tau)
    model_summary <- summary(qr_model, se = "boot", R = 200)
    
    results_df <- data.frame(
      dependent_var = dependent_var,
      tau = tau,
      variable = names(coef(qr_model)),
      coefficient = coef(qr_model),
      std_error = model_summary$coefficients[, 2],
      t_value = model_summary$coefficients[, 3],
      p_value = model_summary$coefficients[, 4],
      stringsAsFactors = FALSE
    )
    results_list[[paste(dependent_var, tau, sep="_")]] <- results_df
  }
  return(bind_rows(results_list))
}

qr_results <- bind_rows(
  run_quantile_regression("gross_inflows", quantile_data),
  run_quantile_regression("gross_outflows", quantile_data),
  run_quantile_regression("net_inflows", quantile_data)
)
#----------------------------------------------------------------POST ESTIMATION
#-OLS Diagnostics
#VIF
vif_test <- function(data, formula) {
  ols_model <- lm(formula, data = data)
  vif_values <- car::vif(ols_model)
  return(vif_values)
}

formula_base_inflows <- as.formula("gross_inflows ~ macro_uncertainty_lag + real_uncertainty_lag + 
                           financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                           cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                           lnoil_lag + commodity_lag")
formula_base_outflows <- as.formula("gross_outflows ~ macro_uncertainty_lag + real_uncertainty_lag + 
                           financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                           cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                           lnoil_lag + commodity_lag")
formula_base_net <- as.formula("net_inflows ~ macro_uncertainty_lag + real_uncertainty_lag + 
                           financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                           cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                           lnoil_lag + commodity_lag")

vif_results_inf <- vif_test(as.data.frame(final_panel_clean), formula_base_inflows)
vif_results_outfl <- vif_test(as.data.frame(final_panel_clean), formula_base_outflows)
vif_results_net <- vif_test(as.data.frame(final_panel_clean), formula_base_net)

#CS
cs_test <- pcdtest(plm(gross_inflows ~ macro_uncertainty_lag + real_uncertainty_lag + 
                         financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                         cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                         lnoil_lag + commodity_lag, 
                       data = final_panel_clean, model = "pooling"))
#-----------------------------------------------------------Quantile Diagnostics
# Residual Analysis for Quantile Regression
cat("\n=== QUANTILE RESIDUAL ANALYSIS ===\n")

analyze_quantile_residuals <- function(dependent_var, data, tau = 0.5) {
  formula <- as.formula(paste(dependent_var, "~ macro_uncertainty_lag + real_uncertainty_lag + 
                              financial_uncertainty_lag + kopen + gdp_pc_lag + gdpg_lag + 
                              cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                              lnoil_lag + commodity_lag"))
  
  qr_model <- rq(formula, data = data, tau = tau)
  residuals <- resid(qr_model)
  fitted <- fitted(qr_model)
  
  # Basic residual statistics
  residual_stats <- data.frame(
    Dependent_Var = dependent_var,
    Tau = tau,
    Mean_Residual = mean(residuals),
    SD_Residual = sd(residuals),
    Skewness = moments::skewness(residuals),
    Kurtosis = moments::kurtosis(residuals)
  )
  
  # Residual plot
  plot_data <- data.frame(Fitted = fitted, Residuals = residuals)
  p <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residual Plot for", dependent_var, "at tau =", tau),
         x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  ggsave(paste0("analysis_plots/residuals_", dependent_var, "_tau", tau, ".png"), p, width = 8, height = 6)
  
  return(residual_stats)
}

# Analyze residuals 
residual_diagnostics <- map_dfr(
  c("gross_inflows", "gross_outflows", "net_inflows"),
  function(variable) {
    map_dfr(
      c(0.1, 0.25, 0.5, 0.75, 0.9),  # Multiple quantiles across distribution
      function(tau) {
        analyze_quantile_residuals(variable, quantile_data, tau)
      }
    )
  }
)

print(residual_diagnostics)


# Robustness tests
convert_to_quarterly_arithmetic <- function(monthly_data, date_col, value_col) {
  monthly_data %>%
    mutate(qtr_date = as.yearqtr(!!sym(date_col))) %>%
    group_by(qtr_date) %>%
    summarise(quarterly_value = mean(!!sym(value_col), na.rm = TRUE)) %>%
    rename(!!paste0(value_col, "_qtr_arithmetic") := quarterly_value)
}

macro_unc_qtr_arith <- convert_to_quarterly_arithmetic(uncertainty_data, "date", "macro_usuncertainty")
real_unc_qtr_arith <- convert_to_quarterly_arithmetic(uncertainty_data, "date", "real_usuncertainty")
financial_unc_qtr_arith <- convert_to_quarterly_arithmetic(uncertainty_data, "date", "finan_usuncertainty")

uncertainty_qtr_arith <- macro_unc_qtr_arith %>%
  left_join(real_unc_qtr_arith, by = "qtr_date") %>%
  left_join(financial_unc_qtr_arith, by = "qtr_date") %>%
  mutate(year = year(qtr_date), quarter = quarter(qtr_date))

final_panel_arith <- panel_data_clean %>%
  left_join(uncertainty_qtr_arith %>% select(-qtr_date), by = c("year", "quarter")) %>%
  left_join(ts_data %>% 
              mutate(year = year(date), quarter = quarter(date)) %>% 
              select(-date), 
            by = c("year", "quarter"))

final_panel_arith_clean <- final_panel_arith %>%
  mutate(across(c(gross_inflows, gross_outflows, net_inflows, kopen, gdp_pc, gdpg, cpi, 
                  exchange_rate, reserves, ir_change, lnoil, commodity,
                  macro_usuncertainty_qtr_arithmetic, real_usuncertainty_qtr_arithmetic, 
                  finan_usuncertainty_qtr_arithmetic), as.numeric)) %>%
  # Convert capital flows to billions of USD
  mutate(
    gross_inflows = gross_inflows / 1e9,
    gross_outflows = gross_outflows / 1e9,
    net_inflows = net_inflows / 1e9,
    reserves = reserves / 1e9
  ) %>%
  group_by(country) %>%
  arrange(time) %>%
  mutate(
    macro_uncertainty_lag_arith = lag(macro_usuncertainty_qtr_arithmetic, 1),
    real_uncertainty_lag_arith = lag(real_usuncertainty_qtr_arithmetic, 1),
    financial_uncertainty_lag_arith = lag(finan_usuncertainty_qtr_arithmetic, 1),
    kopen = kopen,
    gdp_pc_lag = lag(gdp_pc, 1),
    gdpg_lag = lag(gdpg, 1),
    cpi_lag = lag(cpi, 1),
    exchange_rate_lag = lag(exchange_rate, 1),
    reserves_lag = lag(reserves, 1),
    ir_change_lag = lag(ir_change, 1),
    lnoil_lag = lag(lnoil, 1),
    commodity_lag = lag(commodity, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(gross_inflows) | !is.na(gross_outflows) | !is.na(net_inflows))

quantile_data_arith <- as.data.frame(final_panel_arith_clean) %>%
  select(country, time, gross_inflows, gross_outflows, net_inflows,
         macro_uncertainty_lag_arith, real_uncertainty_lag_arith, financial_uncertainty_lag_arith,
         kopen, gdp_pc_lag, gdpg_lag, cpi_lag, exchange_rate_lag, 
         reserves_lag, ir_change_lag, lnoil_lag, commodity_lag) %>%
  na.omit()

run_quantile_regression_robust <- function(dependent_var, data, tau_values = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  formula <- as.formula(paste(dependent_var, "~ macro_uncertainty_lag_arith + real_uncertainty_lag_arith + 
                              financial_uncertainty_lag_arith + kopen + gdp_pc_lag + gdpg_lag + 
                              cpi_lag + exchange_rate_lag + reserves_lag + ir_change_lag + 
                              lnoil_lag + commodity_lag"))
  
  results_list <- list()
  for(tau in tau_values) {
    qr_model <- rq(formula, data = data, tau = tau)
    model_summary <- summary(qr_model, se = "boot", R = 200)
    
    results_df <- data.frame(
      dependent_var = dependent_var,
      tau = tau,
      variable = names(coef(qr_model)),
      coefficient = coef(qr_model),
      std_error = model_summary$coefficients[, 2],
      t_value = model_summary$coefficients[, 3],
      p_value = model_summary$coefficients[, 4],
      stringsAsFactors = FALSE
    )
    results_list[[paste(dependent_var, tau, sep="_")]] <- results_df
  }
  return(bind_rows(results_list))
}

qr_results_robust <- bind_rows(
  run_quantile_regression_robust("gross_inflows", quantile_data_arith),
  run_quantile_regression_robust("gross_outflows", quantile_data_arith),
  run_quantile_regression_robust("net_inflows", quantile_data_arith)
)

# Alternative quantiles robustness
qr_results_alt_quantiles <- bind_rows(
  run_quantile_regression("gross_inflows", quantile_data, c(0.05, 0.95)),
  run_quantile_regression("gross_outflows", quantile_data, c(0.05, 0.95)),
  run_quantile_regression("net_inflows", quantile_data, c(0.05, 0.95))
)

# Export results
results_workbook <- list(
  Descriptive_Statistics = desc_stats,
  Correlation_Matrix = correlation_matrix,
  OLS_Results = ols_lagged_results,
  OLS_Model_Fit = ols_model_fit,
  Quantile_Regression_Results = qr_results,
  vif_results_inf = as.data.frame(vif_results_inf) %>% mutate(Variable = rownames(.)),
  vif_results_outfl = as.data.frame(vif_results_outfl) %>% mutate(Variable = rownames(.)),
  VIF_Results_net = as.data.frame(vif_results_net) %>% mutate(Variable = rownames(.)),
  CrossSectional_Dependence = data.frame(Test_Statistic = cs_test$statistic, P_Value = cs_test$p.value),
  Quantile_Residual_Analysis = residual_diagnostics,
  Robustness_Arithmetic_Mean = qr_results_robust,
  Robust_Alt_Quantiles = qr_results_alt_quantiles,
  Dataset_Overview = data.frame(
    Metric = c("Countries", "Time_Periods", "Total_Observations", "Start_Date", "End_Date"),
    Value = c(length(unique(final_panel_clean$country)), length(unique(final_panel_clean$time)),
              nrow(final_panel_clean), as.character(min(as.Date(final_panel_clean$time))),
              as.character(max(as.Date(final_panel_clean$time))))
  )
)

write.xlsx(results_workbook, "final_results.xlsx")
write.csv(final_panel_clean, "final_analysis_dataset.csv", row.names = FALSE)
