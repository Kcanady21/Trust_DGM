# =============================================================================
# MODELING FUNCTIONS FOR DISCONTINUOUS GROWTH MODELS
# =============================================================================
# This script contains functions for fitting and comparing DGM models.
# =============================================================================

source("config/config.R")
source("R/utilities.R")

# =============================================================================
# NULL MODEL FUNCTIONS
# =============================================================================

#' Fit Null Model (Random Intercepts Only)
#'
#' The null model has no predictors - just an overall mean and random intercepts
#' for each participant. This serves as a baseline for model comparisons and
#' allows us to calculate the ICC.
#'
#' Why start with a null model?
#' 1. It tells us how much variance is between vs within people
#' 2. It provides a baseline for comparing more complex models
#' 3. High ICC justifies using mixed models (confirms dependency in data)
#'
#' @param data Data frame with Trust and P_ID columns
#' @param outcome_var Name of outcome variable (default: "Trust")
#' @return Fitted lme model object
fit_null_model <- function(data, outcome_var = "Trust") {
  
  formula_str <- paste(outcome_var, "~ 1")
  
  model <- lme(
    fixed = as.formula(formula_str),
    random = ~ 1 | P_ID,
    data = data,
    method = "REML",
    na.action = na.omit
  )
  
  return(model)
}


# =============================================================================
# DISCONTINUOUS GROWTH MODEL FUNCTIONS
# =============================================================================

#' Fit Discontinuous Growth Model - Base Version
#'
#' Fits a DGM with development, dissolution, and restoration phases.
#' This is the core model structure for analyzing trust repair trajectories.
#'
#' The model includes:
#' - DEVELOPMENT: Linear growth in initial trust formation
#' - DISSOLUTION: Immediate drop at violation
#' - RESTORATION: Linear recovery after violation
#'
#' Random effects allow each person to have their own:
#' - Starting level (intercept)
#' - Development slope
#' - Response to violation
#' - Recovery slope
#'
#' @param data Data frame with required phase indicators
#' @param random_structure Formula for random effects (default includes slopes)
#' @param method Estimation method: "REML" or "ML"
#' @return Fitted lme model object
fit_dgm_base <- function(data, 
                         random_structure = ~ DEVELOPMENT + DISSOLUTION + RESTORATION | P_ID,
                         method = "REML") {
  
  model <- lme(
    fixed = Trust ~ DEVELOPMENT + DISSOLUTION + RESTORATION,
    random = random_structure,
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


#' Fit Discontinuous Growth Model - With Condition Interaction
#'
#' Extends the base DGM to test whether trust trajectories differ by experimental
#' condition. This allows us to ask: "Do different conditions show different
#' patterns of trust development, dissolution, or recovery?"
#'
#' @param data Data frame with required variables
#' @param random_structure Formula for random effects
#' @param method Estimation method
#' @return Fitted lme model object
fit_dgm_with_condition <- function(data,
                                   random_structure = ~ DEVELOPMENT + DISSOLUTION + RESTORATION | P_ID,
                                   method = "REML") {
  
  model <- lme(
    fixed = Trust ~ DEVELOPMENT + DISSOLUTION + RESTORATION +
      Condition:DISSOLUTION + Condition:RESTORATION,
    random = random_structure,
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


#' Fit Discontinuous Growth Model - Extended Restoration
#'
#' This version distinguishes between immediate and delayed restoration effects,
#' allowing for more nuanced modeling of the recovery process.
#'
#' Model structure:
#' - Development phase: Linear growth
#' - Dissolution: Immediate drop
#' - Initial restoration: Immediate recovery response
#' - Delayed restoration: Later-phase recovery
#'
#' This captures the idea that trust repair might happen in stages, with
#' different processes operating at different times.
#'
#' @param data Data frame
#' @param include_condition Logical, whether to include condition interactions
#' @param random_structure Formula for random effects
#' @param method Estimation method
#' @return Fitted lme model object
fit_dgm_extended <- function(data,
                             include_condition = FALSE,
                             random_structure = ~ DEVELOPMENT + DISSOLUTION + 
                               INITIAL_RESTORATION + DELAYED_RESTORATION | P_ID,
                             method = "REML") {
  
  if (include_condition) {
    fixed_formula <- Trust ~ DEVELOPMENT + DISSOLUTION + 
      INITIAL_RESTORATION + DELAYED_RESTORATION +
      Condition:INITIAL_RESTORATION + Condition:DELAYED_RESTORATION
  } else {
    fixed_formula <- Trust ~ DEVELOPMENT + DISSOLUTION + 
      INITIAL_RESTORATION + DELAYED_RESTORATION
  }
  
  model <- lme(
    fixed = fixed_formula,
    random = random_structure,
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


#' Fit Discontinuous Growth Model - With Retrospective Phase
#'
#' The most complex version, including a retrospective phase where participants
#' might re-evaluate trust after extended interaction post-violation.
#'
#' @param data Data frame
#' @param include_condition Logical, whether to include condition interactions
#' @param method Estimation method
#' @return Fitted lme model object
fit_dgm_retrospective <- function(data,
                                  include_condition = FALSE,
                                  method = "REML") {
  
  if (include_condition) {
    fixed_formula <- Trust ~ DEVELOPMENT + DISSOLUTION + 
      INITIAL_RESTORATION + DELAYED_RESTORATION + RETROSPECTIVE +
      Condition:INITIAL_RESTORATION + Condition:DELAYED_RESTORATION + 
      Condition:RETROSPECTIVE
  } else {
    fixed_formula <- Trust ~ DEVELOPMENT + DISSOLUTION + 
      INITIAL_RESTORATION + DELAYED_RESTORATION + RETROSPECTIVE
  }
  
  model <- lme(
    fixed = fixed_formula,
    random = ~ DEVELOPMENT + DISSOLUTION + INITIAL_RESTORATION + 
      DELAYED_RESTORATION + RETROSPECTIVE | P_ID,
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


# =============================================================================
# OMNIBUS MODEL (ALL AGENTS)
# =============================================================================

#' Fit Omnibus Model with Agent Interactions
#'
#' This model includes all agents simultaneously and tests whether trust
#' trajectories differ across agents. By including Agent as a factor with
#' interactions, we can test questions like:
#' - Do different agents show different violation responses?
#' - Do some agents recover faster than others?
#'
#' Important: Rocky is the reference group (first level of Agent factor),
#' so all other agents are compared to Rocky.
#'
#' @param data Data frame with all agents
#' @param method Estimation method
#' @return Fitted lme model object
fit_omnibus_model <- function(data, method = "REML") {
  
  # Ensure Agent is a factor with correct reference level
  data$Agent <- factor(data$Agent, levels = PARAMS$agents)
  
  model <- lme(
    fixed = Trust ~ DEVELOPMENT + DISSOLUTION + RESTORATION +
      Agent + Agent:DISSOLUTION + Agent:RESTORATION,
    random = ~ DEVELOPMENT + DISSOLUTION + RESTORATION | P_ID,
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


# =============================================================================
# MODEL COMPARISON FUNCTIONS
# =============================================================================

#' Compare Nested Models
#'
#' Uses likelihood ratio test to compare nested models. This answers the question:
#' "Does the more complex model fit significantly better than the simpler model?"
#'
#' Important: Models must be nested (one is a special case of the other) and
#' must use ML estimation (not REML) for valid comparison.
#'
#' @param model_simple Simpler model (fewer parameters)
#' @param model_complex More complex model (more parameters)
#' @return Data frame with comparison statistics
compare_nested_models <- function(model_simple, model_complex) {
  
  # Refit with ML if needed for comparison
  if (model_simple$method == "REML") {
    warning("Refitting models with ML for valid comparison")
    model_simple <- update(model_simple, method = "ML")
    model_complex <- update(model_complex, method = "ML")
  }
  
  # Perform likelihood ratio test
  comparison <- anova(model_simple, model_complex)
  
  # Extract key statistics
  results <- data.frame(
    Model = c("Simple", "Complex"),
    AIC = c(AIC(model_simple), AIC(model_complex)),
    BIC = c(BIC(model_simple), BIC(model_complex)),
    LogLik = c(logLik(model_simple), logLik(model_complex)),
    df = c(attr(logLik(model_simple), "df"), 
           attr(logLik(model_complex), "df"))
  )
  
  # Add test results
  results$ChiSq <- c(NA, comparison$L.Ratio[2])
  results$df_diff <- c(NA, comparison$df[2] - comparison$df[1])
  results$p_value <- c(NA, comparison$`p-value`[2])
  
  return(results)
}


#' Compare Multiple Models Using Information Criteria
#'
#' Compares several models using AIC and BIC. Unlike likelihood ratio tests,
#' this works for non-nested models.
#'
#' Lower AIC/BIC indicates better model fit, accounting for complexity.
#' Rule of thumb: ΔAIC > 10 suggests substantial improvement.
#'
#' @param ... Named model objects to compare
#' @return Data frame sorted by AIC
compare_multiple_models <- function(...) {
  
  models <- list(...)
  model_names <- names(models)
  
  if (is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(models))
  }
  
  # Extract fit statistics for each model
  results <- map_dfr(seq_along(models), function(i) {
    model <- models[[i]]
    data.frame(
      Model = model_names[i],
      AIC = AIC(model),
      BIC = BIC(model),
      LogLik = as.numeric(logLik(model)),
      df = attr(logLik(model), "df"),
      nobs = nobs(model)
    )
  })
  
  # Calculate delta statistics (difference from best model)
  results <- results %>%
    mutate(
      deltaAIC = AIC - min(AIC),
      deltaBIC = BIC - min(BIC),
      AIC_weight = exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC))
    ) %>%
    arrange(AIC)
  
  return(results)
}


# =============================================================================
# AUTOCORRELATION TESTING
# =============================================================================

#' Test for Autocorrelation in Residuals
#'
#' Checks whether residuals are correlated across time within participants.
#' If present, we may need to account for it in the model (e.g., AR(1) structure).
#'
#' The ACF (Autocorrelation Function) shows correlation between residuals
#' separated by different time lags. Significant autocorrelation at lag 1
#' suggests an AR(1) error structure might improve the model.
#'
#' @param model Fitted lme model
#' @param max_lag Maximum lag to test
#' @return List with ACF results and plot
test_autocorrelation <- function(model, max_lag = 6) {
  
  # Extract residuals and group information
  resid_df <- data.frame(
    residuals = residuals(model, type = "normalized"),
    P_ID = model$groups$P_ID
  )
  
  # Calculate ACF (this handles the grouping automatically)
  acf_obj <- ACF(model, maxLag = max_lag, resType = "normalized")
  
  # Create plot
  acf_plot <- ggplot(acf_obj, aes(x = lag, y = ACF)) +
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_hline(yintercept = c(-1.96/sqrt(nrow(model$data)), 
                               1.96/sqrt(nrow(model$data))),
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_point(size = 3) +
    labs(
      title = "Autocorrelation Function of Residuals",
      subtitle = "Dashed lines show 95% confidence bounds",
      x = "Lag",
      y = "Autocorrelation"
    ) +
    PLOT_THEME
  
  return(list(
    acf_values = acf_obj,
    plot = acf_plot
  ))
}


#' Fit Model with AR(1) Error Structure
#'
#' Adds an autoregressive correlation structure to account for temporal
#' dependency in residuals. Use this when autocorrelation tests suggest
#' residuals are correlated across adjacent timepoints.
#'
#' @param model_formula Formula for the model
#' @param data Data frame
#' @param random_formula Random effects formula
#' @param method Estimation method
#' @return Model with AR(1) correlation structure
fit_model_with_ar1 <- function(model_formula,
                               data,
                               random_formula = ~ DEVELOPMENT + DISSOLUTION + RESTORATION | P_ID,
                               method = "REML") {
  
  model <- lme(
    fixed = model_formula,
    random = random_formula,
    correlation = corAR1(form = ~ TimeShift | P_ID),  # AR(1) within person
    data = data,
    method = method,
    na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
  )
  
  return(model)
}


# =============================================================================
# MODEL SUMMARY FUNCTIONS
# =============================================================================

#' Extract Tidy Model Summary
#'
#' Creates a clean, formatted summary of model results suitable for tables.
#'
#' @param model Fitted model object
#' @return Data frame with fixed effects, SEs, t-values, and p-values
get_tidy_summary <- function(model) {
  
  coef_table <- summary(model)$tTable
  
  tidy_results <- data.frame(
    Term = rownames(coef_table),
    Estimate = coef_table[, "Value"],
    SE = coef_table[, "Std.Error"],
    df = coef_table[, "DF"],
    t_value = coef_table[, "t-value"],
    p_value = coef_table[, "p-value"]
  ) %>%
    mutate(
      Significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  
  rownames(tidy_results) <- NULL
  
  return(tidy_results)
}


#' Calculate R-squared for Mixed Models
#'
#' Computes marginal and conditional R-squared values.
#' - Marginal R²: Variance explained by fixed effects only
#' - Conditional R²: Variance explained by fixed + random effects
#'
#' @param model Fitted lme or lmer model
#' @return Named vector with R² values
calculate_r_squared <- function(model) {
  
  # Use MuMIn package for R² calculation
  r2 <- r.squaredGLMM(model)
  
  return(c(
    R2_marginal = r2[1, "R2m"],
    R2_conditional = r2[1, "R2c"]
  ))
}


cat("\n✓ Modeling functions loaded successfully\n\n")
