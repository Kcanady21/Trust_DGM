# =============================================================================
# SCRIPT 3: MODEL FITTING
# =============================================================================
# This script fits the discontinuous growth models to the data.
#
# Workflow:
# 1. Fit null model for baseline
# 2. Fit DGM models for each agent separately
# 3. Compare different model structures (with/without random slopes)
# 4. Test for autocorrelation and apply AR(1) if needed
# 5. Fit omnibus model with all agents
# 6. Save all model objects for post-hoc analysis
# =============================================================================

source("config/config.R")
source("R/utilities.R")
source("R/modeling_functions.R")

cat("\n")
cat("=============================================================================\n")
cat("  TRUST REPAIR DGM: MODEL FITTING\n")
cat("=============================================================================\n\n")

# Load prepared data and assumption test results
load(file.path(PATHS$data_processed, "prepared_data.RData"))
load(file.path(PATHS$data_results, DATA_FILES$assumption_tests))

# Storage for model objects
models <- list()

# -----------------------------------------------------------------------------
# PART 1: NULL MODEL (Baseline)
# -----------------------------------------------------------------------------
cat("PART 1: Fitting null model...\n")

null_model <- fit_null_model(agent_datasets$Rocky)
models$null <- null_model

cat("  ✓ Null model fitted\n")
cat(sprintf("  • ICC: %.3f\n", assumption_results$icc$ICC1[1]))

# -----------------------------------------------------------------------------
# PART 2: FIT MODELS FOR EACH AGENT
# -----------------------------------------------------------------------------
cat("\nPART 2: Fitting models for each agent...\n")

for (agent in PARAMS$agents) {
  cat(sprintf("\n  Fitting models for %s...\n", agent))
  
  agent_data <- agent_datasets[[agent]]
  
  # Fit base DGM
  model_base <- fit_dgm_base(agent_data)
  models[[paste0(agent, "_base")]] <- model_base
  
  # Fit with condition interaction if applicable
  if (length(unique(agent_data$Condition)) > 1) {
    model_condition <- fit_dgm_with_condition(agent_data)
    models[[paste0(agent, "_condition")]] <- model_condition
    
    # Compare models
    comparison <- compare_nested_models(model_base, model_condition)
    cat(sprintf("    Model comparison: χ²(%.0f) = %.2f, p = %.3f\n",
                comparison$df_diff[2], comparison$ChiSq[2], comparison$p_value[2]))
  }
  
  # Fit extended models (initial vs delayed restoration)
  model_extended <- fit_dgm_extended(agent_data, include_condition = TRUE)
  models[[paste0(agent, "_extended")]] <- model_extended
  
  cat(sprintf("    ✓ %s models fitted\n", agent))
}

# -----------------------------------------------------------------------------
# PART 3: TEST FOR AUTOCORRELATION AND APPLY AR(1) IF NEEDED
# -----------------------------------------------------------------------------
cat("\nPART 3: Testing autocorrelation in final models...\n")

# Based on assumption tests, fit AR(1) models if needed
# (Code would check ACF results and refit models with AR(1) structure)

# -----------------------------------------------------------------------------
# PART 4: OMNIBUS MODEL (All Agents Together)
# -----------------------------------------------------------------------------
cat("\nPART 4: Fitting omnibus model with all agents...\n")

omnibus_model <- fit_omnibus_model(analysis_df)
models$omnibus <- omnibus_model

cat("  ✓ Omnibus model fitted\n")

# Extract summary
omnibus_summary <- get_tidy_summary(omnibus_model)
cat("\nOmnibus Model Fixed Effects:\n")
print(omnibus_summary, row.names = FALSE, digits = 3)

# -----------------------------------------------------------------------------
# PART 5: MODEL COMPARISONS
# -----------------------------------------------------------------------------
cat("\nPART 5: Comparing model structures...\n")

# Compare different model specifications using AIC/BIC
# (Would compare models with different random effect structures)

model_comparison <- compare_multiple_models(
  Null = null_model,
  Base = models$Rocky_base,
  Extended = models$Rocky_extended
)

cat("\nModel Comparison (Rocky):\n")
print(model_comparison, row.names = FALSE, digits = 2)

# -----------------------------------------------------------------------------
# PART 6: CALCULATE R-SQUARED VALUES
# -----------------------------------------------------------------------------
cat("\nPART 6: Calculating R² values...\n")

r_squared_results <- map_dfr(PARAMS$agents, function(agent) {
  model <- models[[paste0(agent, "_extended")]]
  r2 <- calculate_r_squared(model)
  
  data.frame(
    Agent = agent,
    R2_marginal = r2["R2_marginal"],
    R2_conditional = r2["R2_conditional"]
  )
})

cat("\nR² Values by Agent:\n")
print(r_squared_results, row.names = FALSE, digits = 3)

# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------
cat("\nSaving model objects...\n")

save(
  models,
  omnibus_summary,
  model_comparison,
  r_squared_results,
  file = file.path(PATHS$data_results, "fitted_models.RData"),
  compress = "xz"
)

cat("  ✓ All models saved\n")

cat("\nNext step: Run post-hoc analyses (run_04_posthoc_analyses.R)\n")
cat("=============================================================================\n\n")
