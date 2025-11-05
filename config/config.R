# =============================================================================
# CONFIGURATION FILE
# =============================================================================
# This file centralizes all project settings, paths, and parameters.
# By keeping everything in one place, we make the project easier to maintain
# and adapt to different computing environments.
#
# To use in any script: source("config/config.R")
# =============================================================================

# -----------------------------------------------------------------------------
# Package Management
# -----------------------------------------------------------------------------
# We use pacman for streamlined package management. It automatically installs
# missing packages and loads all required libraries in one step.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  # Data manipulation
  tidyverse,        # Core tidyverse packages (dplyr, tidyr, ggplot2, etc.)
  readxl,          # Reading Excel files
  writexl,         # Writing Excel files
  haven,           # Reading SPSS/Stata/SAS files
  here,            # Project-relative file paths
  
  # Mixed effects modeling
  nlme,            # Linear and nonlinear mixed effects models
  lme4,            # Linear mixed effects models (alternative to nlme)
  lmerTest,        # Testing in linear mixed effects models
  
  # Statistical analysis
  emmeans,         # Estimated marginal means and contrasts
  car,             # Companion to Applied Regression
  effsize,         # Effect size calculations
  MuMIn,           # Model selection and multi-model inference
  broom,           # Tidy model outputs
  
  # Model diagnostics
  performance,     # Model quality and performance metrics
  see,             # Visualization for model diagnostics
  
  # Visualization
  lattice,         # Trellis graphics
  effects,         # Effect displays for models
  patchwork,       # Combining plots
  ggpattern,       # Pattern fills for ggplot2
  
  # Additional utilities
  multilevel,      # Multilevel modeling utilities
  progress,        # Progress bars for long operations
  doParallel,      # Parallel computing
  foreach          # Parallel loops
)

# -----------------------------------------------------------------------------
# Project Paths
# -----------------------------------------------------------------------------
# We define all paths relative to the project root for portability.
# This way, the code works regardless of where the project folder is located.

# Get project root directory (assumes config folder is one level down)
PROJECT_ROOT <- here::here()

# Define subdirectories
PATHS <- list(
  config    = file.path(PROJECT_ROOT, "config"),
  r_scripts = file.path(PROJECT_ROOT, "R"),
  scripts   = file.path(PROJECT_ROOT, "scripts"),
  
  # Data directories
  data_raw       = file.path(PROJECT_ROOT, "data", "raw"),
  data_processed = file.path(PROJECT_ROOT, "data", "processed"),
  data_results   = file.path(PROJECT_ROOT, "data", "results"),
  
  # Output directories
  output_figures = file.path(PROJECT_ROOT, "output", "figures"),
  output_tables  = file.path(PROJECT_ROOT, "output", "tables"),
  output_reports = file.path(PROJECT_ROOT, "output", "reports")
)

# Create directories if they don't exist
for (path in PATHS) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# -----------------------------------------------------------------------------
# Data File Names
# -----------------------------------------------------------------------------
# Centralize all file names so they're easy to update

DATA_FILES <- list(
  # Input files
  raw_wide = "Repair_Just_Trust_Dataset.csv",
  
  # Processed files
  long_format = "Trust_Repair_Long_Format.csv",
  analysis_ready = "Trust_Repair_Analysis_Ready.csv",
  
  # Results files
  model_results = "trust_DGM_results.RData",
  assumption_tests = "assumption_test_results.RData"
)

# -----------------------------------------------------------------------------
# Analysis Parameters
# -----------------------------------------------------------------------------
# Define key parameters for the analysis

PARAMS <- list(
  # Agent names and order (Rocky is reference group)
  agents = c("Rocky", "Boulder", "Falcon", "Eagle", "Team"),
  
  # Timepoints in the study
  timepoints = 0:6,
  
  # Phase definitions
  phases = list(
    development = 0:2,      # Trust development phase
    dissolution = 3,        # Trust violation occurs
    restoration = 4:6       # Trust repair phase
  ),
  
  # Modeling parameters
  alpha_level = 0.05,       # Significance threshold
  reml = TRUE,              # Use REML for estimation
  
  # emmeans options
  emmeans_df_method = "satterthwaite",
  emmeans_limit = 1e6,
  
  # Parallel processing
  n_cores = parallel::detectCores() - 1  # Leave one core free
)

# Set emmeans options globally
emm_options(
  lmer.df = PARAMS$emmeans_df_method,
  lmerTest.limit = PARAMS$emmeans_limit
)

# -----------------------------------------------------------------------------
# Phase Indicator Variable Definitions
# -----------------------------------------------------------------------------
# These define how the discontinuous growth model phases are coded

PHASE_CODING <- list(
  # DEVELOPMENT: Codes the development phase
  # 0 at T0, 1 at T1, 2 at T2-T6
  DEVELOPMENT = function(time) {
    case_when(
      time == 0 ~ 0,
      time == 1 ~ 1,
      time >= 2 ~ 2,
      TRUE ~ NA_real_
    )
  },
  
  # DISSOLUTION: Indicator for post-violation period
  # 0 at T0-T2, 1 at T3-T6
  DISSOLUTION = function(time) {
    case_when(
      time <= 2 ~ 0,
      time >= 3 ~ 1,
      TRUE ~ NA_real_
    )
  },
  
  # RESTORATION: Linear time since violation
  # 0 at T0-T3, 1 at T4, 2 at T5, 3 at T6
  RESTORATION = function(time) {
    case_when(
      time <= 3 ~ 0,
      time == 4 ~ 1,
      time == 5 ~ 2,
      time == 6 ~ 3,
      TRUE ~ NA_real_
    )
  },
  
  # INITIAL_RESTORATION: Indicator for immediate repair period
  # 0 at T0-T3, 1 at T4-T6
  INITIAL_RESTORATION = function(time) {
    case_when(
      time <= 3 ~ 0,
      time >= 4 ~ 1,
      TRUE ~ NA_real_
    )
  },
  
  # INITIAL_RESTORATION_SQ: Quadratic restoration term
  # 0 at T0-T3, 1 at T4, 4 at T5, 9 at T6
  INITIAL_RESTORATION_SQ = function(time) {
    case_when(
      time <= 3 ~ 0,
      time == 4 ~ 1,
      time == 5 ~ 4,
      time == 6 ~ 9,
      TRUE ~ NA_real_
    )
  },
  
  # DELAYED_RESTORATION: Indicator for delayed repair
  # 0 at T0-T4, 1 at T5-T6
  DELAYED_RESTORATION = function(time) {
    case_when(
      time <= 4 ~ 0,
      time >= 5 ~ 1,
      TRUE ~ NA_real_
    )
  },
  
  # RETROSPECTIVE: Time since delayed restoration begins
  # 0 at T0-T4, 1 at T5, 2 at T6
  RETROSPECTIVE = function(time) {
    case_when(
      time <= 4 ~ 0,
      time == 5 ~ 1,
      time == 6 ~ 2,
      TRUE ~ NA_real_
    )
  }
)

# -----------------------------------------------------------------------------
# Plotting Themes and Settings
# -----------------------------------------------------------------------------
# Standardize plot appearance across all visualizations

PLOT_THEME <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )

# Color palette for agents (colorblind-friendly)
AGENT_COLORS <- c(
  "Rocky"   = "#D55E00",  # Vermillion
  "Boulder" = "#0072B2",  # Blue
  "Falcon"  = "#009E73",  # Bluish green
  "Eagle"   = "#CC79A7",  # Reddish purple
  "Team"    = "#F0E442"   # Yellow
)

# -----------------------------------------------------------------------------
# Validation Rules
# -----------------------------------------------------------------------------
# Define expected data structure for validation checks

VALIDATION <- list(
  # Required columns in raw data
  required_cols = c("P_ID", "Condition"),
  
  # Trust scale bounds
  trust_min = 1,
  trust_max = 5,
  
  # Minimum observations per participant
  min_obs_per_participant = 3
)

# -----------------------------------------------------------------------------
# Session Info
# -----------------------------------------------------------------------------
# Print configuration summary when this file is sourced

cat("\n")
cat("=============================================================\n")
cat("  Trust Repair DGM Analysis - Configuration Loaded\n")
cat("=============================================================\n")
cat("Project Root:", PROJECT_ROOT, "\n")
cat("Data Path:   ", PATHS$data_raw, "\n")
cat("Output Path: ", PATHS$output_figures, "\n")
cat("R Version:   ", R.version.string, "\n")
cat("=============================================================\n")
cat("\n")
