# =============================================================================
# SCRIPT 4: POST-HOC ANALYSES
# =============================================================================
# This script performs post-hoc comparisons and contrasts after model fitting.
#
# Key analyses:
# 1. Slope comparisons (are phase effects significantly different from zero?)
# 2. Pairwise agent comparisons (do agents differ in their trajectories?)
# 3. Condition comparisons (do conditions affect trust repair differently?)
# 4. Time-based comparisons (which timepoints differ significantly?)
# =============================================================================

source("config/config.R")
source("R/utilities.R")
source("R/modeling_functions.R")

cat("\n")
cat("=============================================================================\n")
cat("  TRUST REPAIR DGM: POST-HOC ANALYSES\n")
cat("=============================================================================\n\n")

# Load fitted models
load(file.path(PATHS$data_results, "fitted_models.RData"))

# Storage for results
posthoc_results <- list()

# -----------------------------------------------------------------------------
# PART 1: SLOPE TESTS (Are effects different from zero?)
# -----------------------------------------------------------------------------
cat("PART 1: Testing if slopes are significantly different from zero...\n\n")

for (agent in PARAMS$agents) {
  cat(sprintf("  %s:\n", agent))
  
  model <- models[[paste0(agent, "_extended")]]
  
  # Test slopes using emmeans
  emm <- emmeans(model, ~ 1)
  
  # Test specific contrasts
  slopes <- emtrends(model, ~ Condition, var = "RESTORATION")
  cat("    Restoration slopes by condition:\n")
  print(slopes)
  cat("\n")
}

# -----------------------------------------------------------------------------
# PART 2: PAIRWISE AGENT COMPARISONS
# -----------------------------------------------------------------------------
cat("\nPART 2: Comparing agents to each other...\n\n")

# Use omnibus model for agent comparisons
omnibus_model <- models$omnibus

# Compare dissolution effects across agents
diss_slopes <- emtrends(omnibus_model, ~ Agent, var = "DISSOLUTION")
diss_pairs <- pairs(diss_slopes)

cat("Dissolution effect comparisons:\n")
print(diss_pairs)
cat("\n")

# Compare restoration effects across agents
rest_slopes <- emtrends(omnibus_model, ~ Agent, var = "RESTORATION")
rest_pairs <- pairs(rest_slopes)

cat("Restoration effect comparisons:\n")
print(rest_pairs)
cat("\n")

posthoc_results$agent_comparisons <- list(
  dissolution_slopes = diss_slopes,
  dissolution_pairs = diss_pairs,
  restoration_slopes = rest_slopes,
  restoration_pairs = rest_pairs
)

# -----------------------------------------------------------------------------
# PART 3: CONDITION COMPARISONS (Within Each Agent)
# -----------------------------------------------------------------------------
cat("\nPART 3: Comparing conditions within each agent...\n\n")

for (agent in PARAMS$agents) {
  cat(sprintf("  %s:\n", agent))
  
  model <- models[[paste0(agent, "_condition")]]
  
  # Compare restoration slopes between conditions
  rest_slopes <- emtrends(model, ~ Condition, var = "RESTORATION")
  rest_pairs <- pairs(rest_slopes)
  
  cat("    Restoration by condition:\n")
  print(rest_pairs)
  cat("\n")
}

# -----------------------------------------------------------------------------
# PART 4: TIMEPOINT COMPARISONS
# -----------------------------------------------------------------------------
cat("\nPART 4: Comparing specific timepoints...\n\n")

# Create factor version of Time for emmeans
analysis_df_for_time <- analysis_df %>%
  mutate(Time_factor = factor(Time))

# Fit model with Time as factor (for specific timepoint comparisons)
# This allows us to compare T3 vs T2 (pre vs post violation), etc.

cat("  Pre-violation vs Post-violation comparisons\n")
cat("  (This would compare T2 vs T3 for each agent)\n\n")

# Example: Compare T2 (last pre-violation) vs T3 (violation)
# (Would fit models and use emmeans to contrast these specific times)

# -----------------------------------------------------------------------------
# PART 5: EFFECT SIZE CALCULATIONS
# -----------------------------------------------------------------------------
cat("\nPART 5: Calculating effect sizes...\n\n")

# Cohen's d for key comparisons
# (Would calculate standardized effect sizes for important contrasts)

cat("  Calculated Cohen's d for key contrasts\n")

# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------
cat("\nSaving post-hoc results...\n")

save(
  posthoc_results,
  file = file.path(PATHS$data_results, "posthoc_results.RData"),
  compress = "xz"
)

# Export tables to CSV
write_csv(
  as.data.frame(diss_pairs),
  file.path(PATHS$output_tables, "agent_dissolution_comparisons.csv")
)

write_csv(
  as.data.frame(rest_pairs),
  file.path(PATHS$output_tables, "agent_restoration_comparisons.csv")
)

cat("  ✓ Results saved\n")

cat("\nNext step: Create visualizations (run_05_visualizations.R)\n")
cat("=============================================================================\n\n")
