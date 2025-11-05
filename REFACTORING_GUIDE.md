# =============================================================================
# COMPREHENSIVE GUIDE: Understanding the Refactored Analysis Pipeline
# =============================================================================
# This guide explains why and how the code was restructured, teaching you
# data science best practices that will benefit all your future projects.
# =============================================================================

## TABLE OF CONTENTS
## 1. The Problem: What Was Wrong With the Original Code?
## 2. The Solution: Key Principles of Good Data Pipeline Design
## 3. Detailed Walkthrough of Improvements
## 4. How to Use the New Structure
## 5. Best Practices for Future Projects
## 6. Common Questions and Troubleshooting

# =============================================================================
# 1. THE PROBLEM: WHAT WAS WRONG WITH THE ORIGINAL CODE?
# =============================================================================

# Your original code worked and produced results, but it had several issues that
# made it hard to maintain, debug, and extend. Let's understand these issues:

## Issue 1: Code Repetition (Violation of DRY Principle)
# --------------------------------------------------------------
# In your original transformation script, you wrote nearly identical code
# five times - once for each agent:
#
# rocky_data_long <- rocky_data %>%
#   pivot_longer(...) %>%
#   mutate(...)
# 
# boulder_data_long <- boulder_data %>%
#   pivot_longer(...) %>%
#   mutate(...)
# 
# [... repeated 3 more times]
#
# Why is this bad?
# - If you find a bug, you have to fix it 5 times
# - If you want to add a 6th agent, you copy-paste again
# - More code = more places for bugs to hide
# - It's harder to read and understand
#
# The "DRY Principle" (Don't Repeat Yourself) says: if you're copying code,
# you should write a function instead. The variations between copies should
# become function parameters.

## Issue 2: Everything in One Giant Script
# --------------------------------------------------------------
# Your main analysis script (V4_Full_DGM.R) was over 1300 lines long and did:
# - Data loading
# - Data transformation
# - Assumption testing
# - Model fitting
# - Post-hoc analyses
# - Plotting
# - Saving results
#
# All mixed together!
#
# Why is this bad?
# - Hard to find specific parts of the analysis
# - Can't easily re-run just one piece
# - Difficult to debug when something breaks
# - Overwhelming to read and understand
# - Makes collaboration harder
#
# Think of it like writing a book: you don't put all chapters in one file
# with no structure. Each chapter should be separate, with clear purpose.

## Issue 3: No Separation of Concerns
# --------------------------------------------------------------
# Your code mixed three fundamentally different types of operations:
# 1. Data transformation (changing data structure)
# 2. Statistical analysis (testing, modeling)
# 3. Presentation (plotting, tables)
#
# These should be separate because:
# - They serve different purposes
# - They might be done by different people
# - They happen at different stages of the workflow
# - Changes to one shouldn't affect the others

## Issue 4: Assumption Testing Mixed With Analysis
# --------------------------------------------------------------
# The original code did some assumption testing, but it was scattered
# throughout the analysis code. This is problematic because:
# - You might fit models before knowing if assumptions are met
# - Hard to find and review assumption test results
# - Diagnostic plots weren't systematically created
# - No clear "go/no-go" decision point before proceeding

## Issue 5: Hard-Coded Values and Lack of Configuration
# --------------------------------------------------------------
# File paths, parameter values, and settings were scattered throughout the code:
# cd <- getwd()
# data_dir <- file.path(cd, "Data")
# [later in code] alpha_level <- 0.05
# [much later] agents <- c("Rocky", "Boulder", ...)
#
# Why is this bad?
# - To change a setting, you hunt through all the code
# - Easy to have inconsistencies (using 0.05 in one place, 0.01 in another)
# - Hard to share code (paths work on your computer but not others')
# - No single source of truth for project parameters

## Issue 6: No Validation or Error Checking
# --------------------------------------------------------------
# The original code assumed everything would work perfectly:
# - No checks that files exist before loading
# - No validation that data structure is correct
# - No warnings about missing data patterns
# - Hard-to-interpret errors when something goes wrong

# =============================================================================
# 2. THE SOLUTION: KEY PRINCIPLES OF GOOD DATA PIPELINE DESIGN
# =============================================================================

# Now let's understand the principles that guided the refactoring. These
# principles apply to ANY data analysis project, not just yours!

## Principle 1: Modularity - Separate Concerns Into Distinct Components
# --------------------------------------------------------------
# Each script should have ONE clear purpose. Think of your analysis as a
# multi-stage pipeline where data flows from one stage to the next:
#
# Raw Data → [Preparation] → Clean Data → [Testing] → Validated Data → 
# [Modeling] → Fitted Models → [Analysis] → Results → [Visualization] → Figures
#
# Each arrow represents a separate script. Each box represents saved output.
#
# Benefits:
# - Easy to re-run just one part
# - Clear where to look when debugging
# - Can work on different parts independently
# - Natural checkpoints to verify everything is correct

## Principle 2: DRY (Don't Repeat Yourself) - Use Functions
# --------------------------------------------------------------
# When you notice patterns in your code, extract them into functions.
# A function is like a recipe: you write it once, then use it many times.
#
# Before (repetitive):
#   rocky_long <- rocky_data %>% pivot_longer(...) %>% mutate(...)
#   boulder_long <- boulder_data %>% pivot_longer(...) %>% mutate(...)
#   [... repeat 3 more times]
#
# After (functional):
#   transform_agent <- function(data, agent_name) {
#     data %>% pivot_longer(...) %>% mutate(...)
#   }
#   rocky_long <- transform_agent(rocky_data, "Rocky")
#   boulder_long <- transform_agent(boulder_data, "Boulder")
#   [... no repetition!]
#
# Benefits:
# - Fix bugs once, applies everywhere
# - Easier to test (test the function once)
# - More readable (function name explains what it does)
# - Easier to extend (just call function with new inputs)

## Principle 3: Configuration Management - Centralize Settings
# --------------------------------------------------------------
# All project settings should live in ONE place (config file):
# - File paths
# - Analysis parameters
# - Package requirements
# - Constants and thresholds
#
# Think of it like a car dashboard: all the controls in one place, clearly
# labeled, easy to find and adjust.
#
# Benefits:
# - Change a path once, works everywhere
# - Easy to see all project settings at a glance
# - Reduces errors from inconsistent values
# - Makes code more portable (just update config for new environment)

## Principle 4: Documentation - Make Intent Clear
# --------------------------------------------------------------
# Good code documents WHAT and WHY, not just HOW:
# - Comments explain the reasoning behind decisions
# - Function names and variable names are descriptive
# - Each script starts with a clear statement of purpose
# - Complex logic includes explanatory comments
#
# Think of it like leaving a trail of breadcrumbs for your future self
# (or collaborators). When you return to the code in 6 months, you should
# be able to quickly understand what's happening and why.

## Principle 5: Validation and Error Handling - Fail Fast and Clearly
# --------------------------------------------------------------
# Don't assume everything will work perfectly. Check assumptions explicitly:
# - Does the data file exist?
# - Does it have the expected columns?
# - Are values in expected ranges?
# - Are assumptions met before fitting models?
#
# When something goes wrong, fail EARLY with a CLEAR error message.
# This is like having smoke detectors in a building: detect problems early
# before they cause bigger issues downstream.

## Principle 6: Reproducibility - Make Results Replicable
# --------------------------------------------------------------
# Anyone (including future you) should be able to:
# 1. Clone your project
# 2. Run your scripts in order
# 3. Get identical results
#
# This requires:
# - Clear workflow documentation (README)
# - No manual steps (everything scripted)
# - Saved intermediate outputs
# - Version control friendly formats
# - Explicit package versions and dependencies

# =============================================================================
# 3. DETAILED WALKTHROUGH OF IMPROVEMENTS
# =============================================================================

# Now let's walk through how these principles were applied in the refactoring.

## Improvement 1: Project Structure
# --------------------------------------------------------------
# Old structure:
#   project/
#   ├── Data/
#   │   └── [various CSV files]
#   ├── Change_dataset_to_Janky_long.r
#   └── V4_Full_DGM.R
#
# New structure:
#   project/
#   ├── README.md                    # Documentation!
#   ├── config/
#   │   └── config.R                 # All settings in one place
#   ├── R/
#   │   ├── utilities.R              # Reusable functions
#   │   └── modeling_functions.R     # Modeling-specific functions
#   ├── scripts/
#   │   ├── run_01_prepare_data.R    # Stage 1: Prepare
#   │   ├── run_02_assumption_tests.R # Stage 2: Validate
#   │   ├── run_03_fit_models.R      # Stage 3: Model
#   │   ├── run_04_posthoc_analyses.R # Stage 4: Compare
#   │   └── run_05_visualizations.R  # Stage 5: Visualize
#   ├── data/
#   │   ├── raw/                     # Original data (read-only!)
#   │   ├── processed/               # Cleaned data
#   │   └── results/                 # Analysis outputs
#   └── output/
#       ├── figures/                 # Plots
#       └── tables/                  # Summary tables
#
# What changed and why:
# - README explains the project at a high level
# - config/ separates settings from logic
# - R/ contains reusable functions (like a library)
# - scripts/ contains the actual analysis workflow (numbered for order!)
# - data/ is organized by stage (raw → processed → results)
# - output/ separates final deliverables

## Improvement 2: Configuration File (config/config.R)
# --------------------------------------------------------------
# This file replaced scattered settings throughout your code.
# 
# Key features:
# - Package management in one place
# - All paths defined using PROJECT_ROOT (portable!)
# - Analysis parameters (agents, phases, alpha level)
# - Phase coding functions (the complex case_when logic)
# - Plot themes and color schemes
# - Validation rules
#
# Why this matters:
# To change alpha from 0.05 to 0.01, you change ONE line in the config,
# not hunt through 1300 lines of code. To add a new agent, you add it to
# PARAMS$agents and everything else adjusts automatically.

## Improvement 3: Utility Functions (R/utilities.R)
# --------------------------------------------------------------
# This file contains general-purpose functions used throughout the analysis.
#
# Key functions created:
#
# validate_data_structure()
#   - Checks that data has expected columns
#   - Validates value ranges
#   - Provides clear error messages
#   - WHY: Catch problems early before they cause cryptic errors later
#
# transform_agent_to_long()
#   - Replaced 5 nearly identical code blocks with 1 function
#   - Takes agent name as parameter
#   - WHY: DRY principle - write once, use many times
#
# prepare_analysis_data()
#   - Orchestrates the entire transformation pipeline
#   - Calls other functions in sequence
#   - Provides progress updates
#   - WHY: Complex operations broken into manageable, testable pieces
#
# calculate_icc()
#   - Computes ICC statistics
#   - Returns structured results
#   - WHY: Isolates statistical calculations for testing and reuse
#
# plot_qq(), plot_residual_fitted()
#   - Create standardized diagnostic plots
#   - Consistent appearance across all plots
#   - WHY: Easier to compare diagnostics across models

## Improvement 4: Modeling Functions (R/modeling_functions.R)
# --------------------------------------------------------------
# This file contains model-fitting and comparison functions.
#
# Key functions created:
#
# fit_null_model(), fit_dgm_base(), fit_dgm_extended()
#   - Encapsulate different model specifications
#   - Consistent syntax and defaults
#   - WHY: Easy to fit same model structure to different data subsets
#
# compare_nested_models(), compare_multiple_models()
#   - Standardized model comparison procedures
#   - Clear, formatted output
#   - WHY: Consistent evaluation criteria across all comparisons
#
# test_autocorrelation()
#   - Tests ACF and creates diagnostic plots
#   - Returns both statistics and visualizations
#   - WHY: Combines testing and visualization in one call
#
# get_tidy_summary(), calculate_r_squared()
#   - Extract model results in clean format
#   - Ready for tables and reporting
#   - WHY: Separates analysis from presentation

## Improvement 5: Analysis Scripts (scripts/)
# --------------------------------------------------------------
# The workflow is now split into 5 sequential scripts, each with clear purpose:
#
# Script 1: run_01_prepare_data.R
#   Purpose: Transform raw data to analysis-ready format
#   Inputs: Raw CSV file
#   Outputs: Long-format data, validation results
#   Key activities:
#     - Load and validate raw data
#     - Transform wide → long format
#     - Create phase indicators
#     - Calculate descriptive statistics
#     - Save processed data
#   WHY: Separate data prep from analysis (different skills, different stage)
#
# Script 2: run_02_assumption_tests.R
#   Purpose: Test model assumptions before fitting
#   Inputs: Prepared data from Script 1
#   Outputs: Assumption test results, diagnostic plots
#   Key activities:
#     - Calculate ICC (justify mixed models)
#     - Test normality (Q-Q plots, Shapiro-Wilk)
#     - Test homogeneity of variance (Levene's test)
#     - Test autocorrelation (ACF plots)
#     - Create comprehensive diagnostic plots
#   WHY: Verify assumptions BEFORE proceeding to modeling
#
# Script 3: run_03_fit_models.R
#   Purpose: Fit discontinuous growth models
#   Inputs: Validated data from Script 2
#   Outputs: Fitted model objects
#   Key activities:
#     - Fit null model (baseline)
#     - Fit DGM for each agent
#     - Compare model structures
#     - Test for autocorrelation in final models
#     - Fit omnibus model (all agents)
#     - Calculate R² values
#   WHY: Separate model fitting from model interpretation
#
# Script 4: run_04_posthoc_analyses.R
#   Purpose: Perform post-hoc comparisons and contrasts
#   Inputs: Fitted models from Script 3
#   Outputs: Comparison tables, contrast results
#   Key activities:
#     - Test if slopes differ from zero
#     - Compare agents (pairwise contrasts)
#     - Compare conditions within agents
#     - Compare specific timepoints
#     - Calculate effect sizes
#   WHY: Separate hypothesis testing from model fitting
#
# Script 5: run_05_visualizations.R
#   Purpose: Create publication-ready figures
#   Inputs: Data and results from all previous scripts
#   Outputs: High-quality plots
#   Key activities:
#     - Plot trust trajectories by agent
#     - Plot condition comparisons
#     - Create phase timeline diagrams
#     - Plot model predictions with CIs
#     - Create effect size visualizations
#   WHY: Separate visualization from analysis logic

## Improvement 6: Progressive Enhancement
# --------------------------------------------------------------
# The new structure makes it easy to enhance the analysis:
#
# Want to add a new agent?
#   → Add to PARAMS$agents in config.R
#   → Everything else adjusts automatically!
#
# Want to test a different model structure?
#   → Write a new fit_dgm_xxx() function
#   → Call it in run_03_fit_models.R
#
# Want different plots?
#   → Modify run_05_visualizations.R
#   → Doesn't affect the statistical analysis!
#
# Want to share your analysis?
#   → Everything is documented and organized
#   → Collaborators can understand and extend your work

# =============================================================================
# 4. HOW TO USE THE NEW STRUCTURE
# =============================================================================

## Getting Started
# --------------------------------------------------------------
# 1. Place your raw data in data/raw/
# 2. Update config.R if needed (file names, parameters)
# 3. Run scripts in order:

source("scripts/run_01_prepare_data.R")
source("scripts/run_02_assumption_tests.R")  
source("scripts/run_03_fit_models.R")
source("scripts/run_04_posthoc_analyses.R")
source("scripts/run_05_visualizations.R")

# Or create a "run all" script:
# source("scripts/run_all.R")  # Would source all scripts in sequence

## Understanding the Workflow
# --------------------------------------------------------------
# Each script:
# 1. Sources config.R and necessary function files
# 2. Loads inputs (either raw data or results from previous scripts)
# 3. Performs its specific task
# 4. Saves outputs for the next script
# 5. Prints a summary of what was accomplished
#
# This creates a clear audit trail: you can always see what happened
# and trace results back to their origins.

## Debugging Tips
# --------------------------------------------------------------
# When something goes wrong:
#
# 1. Read the error message carefully
#    - New code provides clear, informative errors
#    - Error tells you WHERE problem occurred (which script, which step)
#
# 2. Check the most recent script
#    - Problem is usually in the last script you ran
#    - Look at the console output to see where it stopped
#
# 3. Check intermediate outputs
#    - Each script saves its results
#    - Load and inspect these to verify correctness
#
# 4. Run code interactively
#    - Source the script line-by-line in RStudio
#    - Inspect objects as you go
#    - Identify exactly where things go wrong
#
# 5. Use validation results
#    - run_01 creates validation reports
#    - Check these for data quality issues

## Modifying the Analysis
# --------------------------------------------------------------
# Common modifications and where to make them:
#
# Change significance threshold:
#   → config.R: PARAMS$alpha_level
#
# Add/remove agents:
#   → config.R: PARAMS$agents
#
# Change phase definitions:
#   → config.R: PHASE_CODING list
#
# Add new transformation:
#   → R/utilities.R: Write new function
#   → scripts/run_01_prepare_data.R: Call your function
#
# Try different model:
#   → R/modeling_functions.R: Write fit_dgm_xxx() function
#   → scripts/run_03_fit_models.R: Call your function
#
# Add new plot:
#   → scripts/run_05_visualizations.R: Add plotting code

# =============================================================================
# 5. BEST PRACTICES FOR FUTURE PROJECTS
# =============================================================================

# The principles applied here work for ANY data analysis project.
# Here's how to apply them to your next project:

## Start With Structure
# --------------------------------------------------------------
# Before writing ANY code, set up your project structure:
# 1. Create a clear directory hierarchy
# 2. Create a README explaining the project
# 3. Create a config file for settings
# 4. Plan your workflow (what are the stages?)
#
# Time spent on structure early saves MUCH more time later!

## Write Functions Early
# --------------------------------------------------------------
# As soon as you copy-paste code, stop and ask:
# "Should this be a function?"
#
# If you're doing something more than once, probably yes!
# 
# Good function characteristics:
# - Does ONE thing well
# - Has a clear, descriptive name
# - Takes inputs as parameters (not global variables)
# - Returns results (doesn't rely on side effects)
# - Has a docstring explaining purpose and usage

## Document As You Go
# --------------------------------------------------------------
# Don't wait until the end to document. Write documentation
# AS you write code:
# - Start each script with a header explaining its purpose
# - Add comments explaining WHY you made decisions
# - Document functions with purpose, parameters, and return values
# - Keep a running log of decisions and their rationale
#
# Future you will thank present you!

## Test Assumptions Explicitly
# --------------------------------------------------------------
# Don't assume data is perfect or models are appropriate.
# Build validation and testing into your workflow:
# - Validate data structure early
# - Test model assumptions before fitting
# - Check results for sanity (do they make sense?)
# - Create diagnostic plots systematically
#
# It's much easier to catch problems early than debug them later!

## Version Control
# --------------------------------------------------------------
# Use Git (or another version control system) to:
# - Track changes over time
# - Try experiments without fear
# - Collaborate effectively
# - Maintain a record of your work
#
# Even if you work alone, version control is invaluable.

## Reproducibility Checklist
# --------------------------------------------------------------
# Before sharing your analysis, verify:
# □ Raw data is in data/raw/
# □ Scripts run in order without errors
# □ All paths are relative (not absolute)
# □ Package versions are documented
# □ README explains how to run the analysis
# □ Intermediate outputs are saved
# □ No manual steps required
# □ Results match what you report

# =============================================================================
# 6. COMMON QUESTIONS AND TROUBLESHOOTING
# =============================================================================

## Q: This seems like more code. Why is that better?
# --------------------------------------------------------------
# A: More code, but BETTER code. The original code had repetition hidden
#    throughout (the same pivot_longer code 5 times, for example). The new
#    code makes this explicit with functions, and adds validation and
#    documentation that didn't exist before. Total code is similar, but
#    it's much more maintainable.
#
#    Think of it like a messy room vs. an organized room. The organized
#    room might have the same amount of stuff, but it's much easier to
#    find what you need and keep it clean!

## Q: Do I need all these separate scripts? Can't I combine them?
# --------------------------------------------------------------
# A: You CAN combine them, but you SHOULDN'T. Separate scripts provide:
#    - Clear checkpoints to verify correctness
#    - Ability to re-run just one stage
#    - Better organization (easier to find things)
#    - Natural places for collaboration (different people can work on
#      different scripts without conflicts)
#
#    If you really want one script, create a run_all.R that sources all
#    the others in sequence. Best of both worlds!

## Q: What if I need to change how phases are coded?
# --------------------------------------------------------------
# A: Update the PHASE_CODING functions in config.R. Because the coding
#    logic is centralized, changing it in one place updates it everywhere.
#    This is MUCH better than hunting through 1300 lines of code to find
#    all the case_when statements!

## Q: How do I know if assumptions are met?
# --------------------------------------------------------------
# A: Run run_02_assumption_tests.R and examine:
#    - ICC values (should be > 0.1 for mixed models)
#    - Q-Q plots (points should follow the diagonal line)
#    - Residual plots (should show random scatter, no patterns)
#    - ACF plots (bars should be within confidence bounds)
#    - Levene's test (p > 0.05 suggests homogeneous variance)
#
#    The script produces a summary interpretation for each test.

## Q: The original code used one big RData file. Why multiple files now?
# --------------------------------------------------------------
# A: Multiple files provide:
#    - Clear provenance (you know which script created which file)
#    - Smaller files (faster to load)
#    - Stage-appropriate outputs (processed data separate from results)
#    - Better organization (data/ vs. results/ vs. output/)
#
#    You CAN still save everything in one file if you prefer, but the
#    organized approach is generally better for larger projects.

## Q: What if I get an error saying a file doesn't exist?
# --------------------------------------------------------------
# A: Check:
#    1. Is your raw data in data/raw/?
#    2. Did you run previous scripts first? (Script 2 needs Script 1's output)
#    3. Are file names in config.R correct?
#    4. Is your working directory the project root?
#
#    The error message should tell you which file it's looking for.

## Q: Can I use this structure for other types of analyses?
# --------------------------------------------------------------
# A: Absolutely! The principles are universal:
#    - Modular structure (separate scripts for different stages)
#    - Configuration management (central settings file)
#    - Reusable functions (DRY principle)
#    - Documentation (README, comments)
#    - Validation (test assumptions)
#
#    Adapt the specific scripts to your analysis type, but the overall
#    structure works for ANY quantitative analysis.

# =============================================================================
# FINAL THOUGHTS
# =============================================================================

# The refactoring transformed your code from "working" to "maintainable and
# professional." This isn't just about making it look nice - it's about:
#
# 1. RELIABILITY: Catch errors early, validate assumptions
# 2. REPRODUCIBILITY: Anyone can run your analysis and get the same results
# 3. MAINTAINABILITY: Easy to fix bugs and add features
# 4. UNDERSTANDABILITY: Clear what each piece does and why
# 5. EXTENSIBILITY: Simple to adapt for new analyses
#
# These principles will serve you well in all your future data science work!
#
# Remember: Good code is not just code that works. Good code is code that:
# - Is easy to understand
# - Is easy to modify
# - Is easy to debug
# - Produces reliable results
# - Can be shared and reproduced
#
# The extra time spent on structure pays dividends every time you return
# to the code or share it with others.

# =============================================================================
