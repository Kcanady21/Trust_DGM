# Trust Repair Discontinuous Growth Model Analysis

## Project Overview
This project analyzes trust repair trajectories across multiple agents using discontinuous growth models (DGM). The analysis examines trust development, dissolution, and restoration phases over 7 timepoints.

## Project Structure

```
project/
├── README.md                          # This file
├── config/
│   └── config.R                       # Configuration and global parameters
├── R/
│   ├── 01_data_loading.R             # Data import and initial validation
│   ├── 02_data_transformation.R      # Wide to long format transformation
│   ├── 03_utilities.R                # Reusable utility functions
│   └── 04_modeling_functions.R       # DGM modeling functions
├── scripts/
│   ├── run_01_prepare_data.R         # Step 1: Prepare data
│   ├── run_02_assumption_tests.R     # Step 2: Test model assumptions
│   ├── run_03_fit_models.R           # Step 3: Fit DGM models
│   ├── run_04_posthoc_analyses.R     # Step 4: Post-hoc comparisons
│   └── run_05_visualizations.R       # Step 5: Create plots
├── data/
│   ├── raw/                          # Original data files (read-only)
│   ├── processed/                    # Cleaned and transformed data
│   └── results/                      # Model outputs and statistics
└── output/
    ├── figures/                      # Plots and visualizations
    ├── tables/                       # Summary tables
    └── reports/                      # Generated reports
```

## Workflow

The analysis follows a sequential pipeline:

1. **Data Preparation** (`run_01_prepare_data.R`)
   - Load raw data
   - Transform from wide to long format
   - Create phase indicators (DEVELOPMENT, DISSOLUTION, etc.)
   - Validate data structure

2. **Assumption Testing** (`run_02_assumption_tests.R`)
   - Check normality of residuals
   - Test homogeneity of variance
   - Assess autocorrelation
   - Calculate ICCs
   - Generate diagnostic plots

3. **Model Fitting** (`run_03_fit_models.R`)
   - Fit null model (random intercepts only)
   - Fit DGM models per agent
   - Fit omnibus model with agent interactions
   - Compare model structures
   - Save model objects

4. **Post-hoc Analyses** (`run_04_posthoc_analyses.R`)
   - Slope comparisons across agents
   - Pairwise contrasts by condition
   - Time-based comparisons
   - Effect size calculations

5. **Visualizations** (`run_05_visualizations.R`)
   - Trust trajectories by agent
   - Phase timeline diagrams
   - Model diagnostic plots
   - Publication-ready figures

## Key Improvements Over Original Code

- **Modularity**: Separate scripts for different purposes
- **Reusability**: Functions for repeated operations
- **Documentation**: Clear comments and structure
- **Reproducibility**: Explicit workflow and dependencies
- **Validation**: Data quality checks at each stage
- **Maintainability**: Easier to update and debug

## Running the Analysis

Execute scripts in order:
```r
source("scripts/run_01_prepare_data.R")
source("scripts/run_02_assumption_tests.R")
source("scripts/run_03_fit_models.R")
source("scripts/run_04_posthoc_analyses.R")
source("scripts/run_05_visualizations.R")
```

Or run all at once:
```r
source("scripts/run_all.R")
```

## Dependencies

See `config/config.R` for required packages.
