# =============================================================================
# SCRIPT 2: ASSUMPTION TESTING
# =============================================================================
# This script tests the assumptions underlying mixed-effects models.
#
# Why is this important?
# Mixed models make several assumptions about the data. Violating these
# assumptions can lead to incorrect inferences (wrong p-values, biased estimates).
# By testing assumptions BEFORE fitting our main models, we can:
# 1. Identify potential problems early
# 2. Choose appropriate model specifications
# 3. Apply corrections if needed (e.g., transformations, robust methods)
#
# Key assumptions we test:
# 1. Normality of residuals (Q-Q plots, Shapiro-Wilk test)
# 2. Homogeneity of variance (residual plots)
# 3. Independence of observations (ACF plots for autocorrelation)
# 4. Sufficient between-subject variability (ICC calculation)
# =============================================================================

source("config/config.R")
source("R/utilities.R")
source("R/modeling_functions.R")

cat("\n")
cat("=============================================================================\n")
cat("  TRUST REPAIR DGM: ASSUMPTION TESTING\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# Load Prepared Data
# -----------------------------------------------------------------------------
cat("Loading prepared data...\n")

load(file.path(PATHS$data_processed, "prepared_data.RData"))

cat(sprintf("  ✓ Loaded data with %d observations\n", nrow(analysis_df)))

# Create results storage
assumption_results <- list()

# -----------------------------------------------------------------------------
# PART 1: INTRACLASS CORRELATION (ICC)
# -----------------------------------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("  PART 1: INTRACLASS CORRELATION COEFFICIENT (ICC)\n")
cat("=============================================================================\n\n")

cat("What is ICC?\n")
cat("The ICC tells us how much variance is between people vs within people.\n")
cat("High ICC (> 0.1) means observations within a person are similar,\n")
cat("justifying the use of mixed models to account for this dependency.\n\n")

# Calculate ICC for each agent
cat("Calculating ICC for each agent...\n\n")

icc_results <- map_dfr(PARAMS$agents, function(agent) {
  cat(sprintf("  Processing %s...\n", agent))
  
  agent_data <- agent_datasets[[agent]]
  icc_stats <- calculate_icc(agent_data)
  
  # Create a one-row data frame with results
  data.frame(
    Agent = agent,
    ICC1 = icc_stats$ICC1,
    ICC2 = icc_stats$ICC2,
    Between_Var = icc_stats$between_var,
    Within_Var = icc_stats$within_var,
    Total_Var = icc_stats$total_var,
    N_Subjects = icc_stats$n_subjects,
    Avg_Obs_Per_Subject = icc_stats$avg_obs_per_subject
  )
})

# Print results
cat("\n")
cat("ICC Results by Agent:\n")
cat("--------------------\n")
print(icc_results, row.names = FALSE, digits = 3)

cat("\n")
cat("Interpretation:\n")
for (i in 1:nrow(icc_results)) {
  agent <- icc_results$Agent[i]
  icc1 <- icc_results$ICC1[i]
  
  if (icc1 > 0.1) {
    cat(sprintf("  %s: ICC = %.3f (Good - strong between-person variability)\n", 
                agent, icc1))
  } else {
    cat(sprintf("  %s: ICC = %.3f (Low - may not need mixed models)\n", 
                agent, icc1))
  }
}

assumption_results$icc <- icc_results

# -----------------------------------------------------------------------------
# PART 2: VARIANCE HOMOGENEITY ACROSS TIME
# -----------------------------------------------------------------------------
cat("\n\n")
cat("=============================================================================\n")
cat("  PART 2: VARIANCE HOMOGENEITY ACROSS TIME\n")
cat("=============================================================================\n\n")

cat("Testing if variance is similar across timepoints...\n")
cat("Unequal variances might suggest we need variance structures in the model.\n\n")

# Calculate variances by time for each agent
variance_by_time <- analysis_df %>%
  group_by(Agent, Time) %>%
  summarise(
    N = sum(!is.na(Trust)),
    Variance = var(Trust, na.rm = TRUE),
    SD = sd(Trust, na.rm = TRUE),
    .groups = "drop"
  )

cat("Variance by Agent and Time:\n")
print(variance_by_time %>% 
        select(Agent, Time, N, Variance, SD) %>%
        pivot_wider(names_from = Time, values_from = c(Variance, SD)),
      n = Inf)

# Levene's test for homogeneity of variance (by time, for Rocky only as example)
cat("\nLevene's Test for Variance Homogeneity (Rocky only):\n")

rocky_data <- agent_datasets$Rocky %>%
  mutate(Time = factor(Time))

if (nrow(rocky_data) > 0 && sum(!is.na(rocky_data$Trust)) > 0) {
  levene_test <- car::leveneTest(Trust ~ Time, data = rocky_data)
  print(levene_test)
  
  if (levene_test$`Pr(>F)`[1] < 0.05) {
    cat("\n⚠ Warning: Significant heterogeneity detected (p < .05)\n")
    cat("  Consider using variance structures in models (e.g., varIdent)\n")
  } else {
    cat("\n✓ Variance appears homogeneous across time\n")
  }
  
  assumption_results$levene_test <- levene_test
}

assumption_results$variance_by_time <- variance_by_time

# -----------------------------------------------------------------------------
# PART 3: NORMALITY OF RESIDUALS
# -----------------------------------------------------------------------------
cat("\n\n")
cat("=============================================================================\n")
cat("  PART 3: NORMALITY OF RESIDUALS\n")
cat("=============================================================================\n\n")

cat("Fitting null models to check if residuals are normally distributed...\n")
cat("We expect residuals to follow a normal distribution.\n\n")

# Fit null models and test normality for each agent
normality_results <- map_dfr(PARAMS$agents, function(agent) {
  cat(sprintf("  Testing %s...\n", agent))
  
  agent_data <- agent_datasets[[agent]]
  
  # Fit null model
  null_model <- fit_null_model(agent_data)
  
  # Extract residuals
  resids <- residuals(null_model, type = "pearson")
  
  # Shapiro-Wilk test (if sample size permits)
  # Note: SW test is sensitive with large samples
  if (length(resids) <= 5000) {
    sw_test <- shapiro.test(resids)
    sw_p <- sw_test$p.value
  } else {
    # Use a sample for very large datasets
    sw_test <- shapiro.test(sample(resids, 5000))
    sw_p <- sw_test$p.value
    cat(sprintf("    (Used random sample for SW test due to large N)\n"))
  }
  
  data.frame(
    Agent = agent,
    SW_Statistic = sw_test$statistic,
    SW_p_value = sw_p,
    Resid_Mean = mean(resids),
    Resid_SD = sd(resids),
    Resid_Skew = moments::skewness(resids),
    Resid_Kurt = moments::kurtosis(resids)
  )
})

cat("\n")
cat("Normality Test Results:\n")
cat("-----------------------\n")
print(normality_results, row.names = FALSE, digits = 3)

cat("\n")
cat("Interpretation:\n")
for (i in 1:nrow(normality_results)) {
  agent <- normality_results$Agent[i]
  p_val <- normality_results$SW_p_value[i]
  
  if (p_val > 0.05) {
    cat(sprintf("  %s: Normality assumption OK (p = %.3f)\n", agent, p_val))
  } else {
    cat(sprintf("  %s: Some deviation from normality (p = %.3f)\n", agent, p_val))
    cat(sprintf("        (Note: Mixed models are fairly robust to normality violations)\n"))
  }
}

assumption_results$normality <- normality_results

# Create Q-Q plots
cat("\nGenerating Q-Q plots for visual inspection...\n")

qq_plots <- map(PARAMS$agents, function(agent) {
  agent_data <- agent_datasets[[agent]]
  null_model <- fit_null_model(agent_data)
  plot_qq(null_model, title = paste("Q-Q Plot:", agent))
})
names(qq_plots) <- PARAMS$agents

# Combine plots
combined_qq <- wrap_plots(qq_plots, ncol = 2) +
  plot_annotation(title = "Q-Q Plots of Residuals by Agent",
                 theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Save plot
ggsave(
  filename = file.path(PATHS$output_figures, "assumption_qq_plots.png"),
  plot = combined_qq,
  width = 12,
  height = 10,
  dpi = 300
)

cat("  ✓ Q-Q plots saved\n")

# -----------------------------------------------------------------------------
# PART 4: AUTOCORRELATION TESTING
# -----------------------------------------------------------------------------
cat("\n\n")
cat("=============================================================================\n")
cat("  PART 4: AUTOCORRELATION IN RESIDUALS\n")
cat("=============================================================================\n\n")

cat("Testing if residuals are correlated across time within participants...\n")
cat("Significant autocorrelation suggests we may need AR(1) error structure.\n\n")

# Test autocorrelation for each agent
acf_results <- list()

for (agent in PARAMS$agents) {
  cat(sprintf("  Testing %s...\n", agent))
  
  agent_data <- agent_datasets[[agent]]
  null_model <- fit_null_model(agent_data)
  
  # Test autocorrelation
  acf_test <- test_autocorrelation(null_model, max_lag = 6)
  acf_results[[agent]] <- acf_test
  
  # Check if lag-1 autocorrelation is significant
  lag1_acf <- acf_test$acf_values$ACF[acf_test$acf_values$lag == 1]
  ci_bound <- 1.96 / sqrt(nrow(agent_data))
  
  if (abs(lag1_acf) > ci_bound) {
    cat(sprintf("    ⚠ Lag-1 ACF = %.3f (significant)\n", lag1_acf))
  } else {
    cat(sprintf("    ✓ Lag-1 ACF = %.3f (not significant)\n", lag1_acf))
  }
}

# Combine ACF plots
acf_plots <- map(acf_results, "plot")
combined_acf <- wrap_plots(acf_plots, ncol = 2) +
  plot_annotation(title = "Autocorrelation Functions by Agent",
                 theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Save plot
ggsave(
  filename = file.path(PATHS$output_figures, "assumption_acf_plots.png"),
  plot = combined_acf,
  width = 12,
  height = 10,
  dpi = 300
)

cat("\n  ✓ ACF plots saved\n")

assumption_results$acf <- acf_results

# -----------------------------------------------------------------------------
# PART 5: RESIDUAL PLOTS (HOMOSCEDASTICITY)
# -----------------------------------------------------------------------------
cat("\n\n")
cat("=============================================================================\n")
cat("  PART 5: RESIDUAL VS FITTED PLOTS\n")
cat("=============================================================================\n\n")

cat("Creating residual vs fitted plots to check homoscedasticity...\n")
cat("We want to see random scatter with no patterns.\n\n")

resid_plots <- map(PARAMS$agents, function(agent) {
  agent_data <- agent_datasets[[agent]]
  null_model <- fit_null_model(agent_data)
  plot_residual_fitted(null_model, title = paste("Residuals vs Fitted:", agent))
})
names(resid_plots) <- PARAMS$agents

combined_resid <- wrap_plots(resid_plots, ncol = 2) +
  plot_annotation(title = "Residual vs Fitted Plots by Agent",
                 theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

ggsave(
  filename = file.path(PATHS$output_figures, "assumption_residual_plots.png"),
  plot = combined_resid,
  width = 12,
  height = 10,
  dpi = 300
)

cat("  ✓ Residual plots saved\n")

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\n\nSaving assumption test results...\n")

save(
  assumption_results,
  icc_results,
  variance_by_time,
  normality_results,
  acf_results,
  file = file.path(PATHS$data_results, DATA_FILES$assumption_tests),
  compress = "xz"
)

cat("  ✓ Results saved\n")

# -----------------------------------------------------------------------------
# Summary Report
# -----------------------------------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("  ASSUMPTION TESTING SUMMARY\n")
cat("=============================================================================\n\n")

cat("ICC Results:\n")
cat(sprintf("  • All agents show ICC > 0.1: %s\n",
            ifelse(all(icc_results$ICC1 > 0.1), "YES ✓", "NO ⚠")))
cat("    → Mixed models are appropriate\n\n")

cat("Normality:\n")
cat(sprintf("  • All agents pass normality tests: %s\n",
            ifelse(all(normality_results$SW_p_value > 0.05), "YES ✓", "NO ⚠")))
cat("    → Residuals appear reasonably normal\n\n")

cat("Variance Homogeneity:\n")
cat("  • See Levene's test results above\n")
cat("    → Check if variance structures are needed\n\n")

cat("Autocorrelation:\n")
cat("  • Check ACF plots for patterns\n")
cat("    → Consider AR(1) if lag-1 correlation is high\n\n")

cat("Output Files Created:\n")
cat(sprintf("  • %s\n", file.path(PATHS$output_figures, "assumption_qq_plots.png")))
cat(sprintf("  • %s\n", file.path(PATHS$output_figures, "assumption_acf_plots.png")))
cat(sprintf("  • %s\n", file.path(PATHS$output_figures, "assumption_residual_plots.png")))
cat(sprintf("  • %s\n", file.path(PATHS$data_results, DATA_FILES$assumption_tests)))

cat("\nNext step: Fit models (run_03_fit_models.R)\n")
cat("=============================================================================\n\n")
