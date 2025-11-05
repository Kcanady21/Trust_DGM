# =============================================================================
# SCRIPT 1: DATA PREPARATION
# =============================================================================
# This script loads raw data, transforms it to analysis-ready format, and
# performs validation checks.
#
# Key improvements over original code:
# 1. Uses functions instead of repetitive code blocks
# 2. Includes validation checks to catch data issues early
# 3. Clearly documents each step
# 4. Separates data prep from analysis
# 5. Saves intermediate outputs for reproducibility
# =============================================================================

# Load configuration and functions
source("config/config.R")
source("R/utilities.R")

cat("\n")
cat("=============================================================================\n")
cat("  TRUST REPAIR DGM: DATA PREPARATION\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# Step 1: Load Raw Data
# -----------------------------------------------------------------------------
cat("Step 1: Loading raw data...\n")

# Construct file path using our centralized paths
raw_data_path <- file.path(PATHS$data_raw, DATA_FILES$raw_wide)

# Check if file exists before trying to read it
if (!file.exists(raw_data_path)) {
  stop(sprintf("Data file not found: %s\nPlease ensure data is in the correct location.",
               raw_data_path))
}

# Load the data
wide_df <- read_csv(
  raw_data_path,
  show_col_types = FALSE  # Suppress column type messages
)

cat(sprintf("  ✓ Loaded data with %d participants and %d columns\n", 
            nrow(wide_df), ncol(wide_df)))

# -----------------------------------------------------------------------------
# Step 2: Validate Raw Data Structure
# -----------------------------------------------------------------------------
cat("\nStep 2: Validating data structure...\n")

# Check that required columns exist
required_cols <- c("P_ID", "Condition")

validation_results <- validate_data_structure(
  data = wide_df,
  required_cols = required_cols,
  trust_range = c(VALIDATION$trust_min, VALIDATION$trust_max)
)

# Print validation results
print_validation_results(validation_results)

# Stop if validation failed
if (!validation_results$valid) {
  stop("Data validation failed. Please fix issues before proceeding.")
}

# -----------------------------------------------------------------------------
# Step 3: Transform to Long Format
# -----------------------------------------------------------------------------
cat("\nStep 3: Transforming data to analysis-ready format...\n\n")

# This single function call replaces ~100 lines of repetitive code!
# It handles:
# - Transforming each agent separately
# - Combining agents together
# - Adding phase indicators
# - Ensuring correct data types
# - Creating time indices

analysis_df <- prepare_analysis_data(wide_df)

cat(sprintf("  ✓ Final dataset: %d observations from %d participants\n",
            nrow(analysis_df),
            length(unique(analysis_df$P_ID))))

# -----------------------------------------------------------------------------
# Step 4: Data Quality Checks
# -----------------------------------------------------------------------------
cat("\nStep 4: Performing data quality checks...\n")

# Check missing data patterns
missing_summary <- analysis_df %>%
  group_by(Agent) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(Trust)),
    pct_missing = 100 * mean(is.na(Trust)),
    .groups = "drop"
  )

cat("\nMissing data by agent:\n")
print(missing_summary, n = Inf)

# Check observations per participant
obs_per_participant <- analysis_df %>%
  filter(!is.na(Trust)) %>%
  group_by(P_ID, Agent) %>%
  summarise(n_obs = n(), .groups = "drop")

cat("\nObservations per participant summary:\n")
cat(sprintf("  Mean: %.1f\n", mean(obs_per_participant$n_obs)))
cat(sprintf("  SD: %.1f\n", sd(obs_per_participant$n_obs)))
cat(sprintf("  Min: %d\n", min(obs_per_participant$n_obs)))
cat(sprintf("  Max: %d\n", max(obs_per_participant$n_obs)))

# Identify participants with very few observations
few_obs <- obs_per_participant %>%
  filter(n_obs < VALIDATION$min_obs_per_participant)

if (nrow(few_obs) > 0) {
  cat(sprintf("\n  ⚠ Warning: %d participant-agent combinations have < %d observations\n",
              nrow(few_obs), VALIDATION$min_obs_per_participant))
}

# Check balance across conditions
condition_balance <- analysis_df %>%
  distinct(P_ID, Condition) %>%
  count(Condition)

cat("\nParticipants per condition:\n")
print(condition_balance)

# -----------------------------------------------------------------------------
# Step 5: Calculate Descriptive Statistics
# -----------------------------------------------------------------------------
cat("\nStep 5: Calculating descriptive statistics...\n")

# Overall descriptive statistics by time and agent
descriptives_by_agent_time <- analysis_df %>%
  group_by(Agent, Time) %>%
  summarise(
    N = sum(!is.na(Trust)),
    Mean = mean(Trust, na.rm = TRUE),
    SD = sd(Trust, na.rm = TRUE),
    Min = min(Trust, na.rm = TRUE),
    Max = max(Trust, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nDescriptive statistics calculated for each Agent × Time combination\n")

# Descriptives by condition, agent, and time
descriptives_full <- calculate_descriptives(
  data = analysis_df,
  value_col = "Trust",
  group_cols = c("Condition", "Agent", "Time")
)

cat("  ✓ Full descriptive statistics table created\n")

# -----------------------------------------------------------------------------
# Step 6: Create Agent-Specific Subsets
# -----------------------------------------------------------------------------
cat("\nStep 6: Creating agent-specific datasets...\n")

# It's often useful to have separate datasets for each agent
# This makes agent-specific analyses cleaner

agent_datasets <- list()

for (agent in PARAMS$agents) {
  agent_datasets[[agent]] <- analysis_df %>%
    filter(Agent == agent) %>%
    droplevels()  # Remove unused factor levels
  
  cat(sprintf("  ✓ Created %s dataset: %d observations\n",
              agent, nrow(agent_datasets[[agent]])))
}

# -----------------------------------------------------------------------------
# Step 7: Save Processed Data
# -----------------------------------------------------------------------------
cat("\nStep 7: Saving processed data...\n")

# Save long-format data
long_path <- file.path(PATHS$data_processed, DATA_FILES$long_format)
write_csv(analysis_df, long_path)
cat(sprintf("  ✓ Saved long-format data: %s\n", long_path))

# Save analysis-ready data (same as long but emphasizes it's ready for modeling)
analysis_path <- file.path(PATHS$data_processed, DATA_FILES$analysis_ready)
write_csv(analysis_df, analysis_path)
cat(sprintf("  ✓ Saved analysis-ready data: %s\n", analysis_path))

# Save descriptive statistics
descriptives_path <- file.path(PATHS$data_results, "descriptive_statistics.csv")
write_csv(descriptives_full, descriptives_path)
cat(sprintf("  ✓ Saved descriptive statistics: %s\n", descriptives_path))

# Save workspace with key objects for next steps
# This allows subsequent scripts to load prepared data without re-running prep
save(
  analysis_df,
  agent_datasets,
  wide_df,
  descriptives_by_agent_time,
  descriptives_full,
  missing_summary,
  file = file.path(PATHS$data_processed, "prepared_data.RData"),
  compress = "xz"
)

cat("  ✓ Saved R workspace with all prepared objects\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("  DATA PREPARATION COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("Participants: %d\n", length(unique(analysis_df$P_ID))))
cat(sprintf("Agents: %d\n", length(PARAMS$agents)))
cat(sprintf("Timepoints: %d\n", length(PARAMS$timepoints)))
cat(sprintf("Total observations: %d\n", nrow(analysis_df)))
cat(sprintf("Complete cases: %d (%.1f%%)\n",
            sum(complete.cases(analysis_df)),
            100 * mean(complete.cases(analysis_df))))
cat("\nNext step: Run assumption testing (run_02_assumption_tests.R)\n")
cat("=============================================================================\n\n")
