# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================
# This script contains reusable functions that are used across the analysis.

# Key principle: If you're copying and pasting code with minor changes,
# that's a sign you should write a function instead!
# =============================================================================

source("config/config.R")

# =============================================================================
# DATA VALIDATION FUNCTIONS
# =============================================================================

#' Validate Data Structure
#'
#' Checks that the dataset meets expected structure and quality requirements.
#' This catches data issues early before they cause problems in analysis.
#'
#' @param data A data frame to validate
#' @param required_cols Character vector of required column names
#' @param trust_range Numeric vector of length 2: c(min, max) for trust scale
#' @return List with validation results
#' @examples
#' validate_data_structure(df, required_cols = c("P_ID", "Trust"))
#'
validate_data_structure <- function(data, 
                                   required_cols = NULL,
                                   trust_range = c(1, 5)) {
  
  results <- list(
    valid = TRUE,
    messages = character(),
    warnings = character()
  )
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    results$valid <- FALSE
    results$messages <- c(results$messages, "Input is not a data frame")
    return(results)
  }
  
  # Check for required columns
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      results$valid <- FALSE
      results$messages <- c(
        results$messages,
        paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      )
    }
  }
  
  # Check for trust columns if present
  trust_cols <- grep("^Trust_", names(data), value = TRUE)
  if (length(trust_cols) > 0) {
    for (col in trust_cols) {
      # Check for values outside expected range
      out_of_range <- data[[col]][!is.na(data[[col]]) & 
                                   (data[[col]] < trust_range[1] | 
                                    data[[col]] > trust_range[2])]
      if (length(out_of_range) > 0) {
        results$warnings <- c(
          results$warnings,
          sprintf("%s has %d values outside range [%d, %d]",
                  col, length(out_of_range), trust_range[1], trust_range[2])
        )
      }
    }
  }
  
  # Check for duplicate participant IDs (if P_ID exists)
  if ("P_ID" %in% names(data)) {
    dups <- data$P_ID[duplicated(data$P_ID)]
    if (length(dups) > 0) {
      results$warnings <- c(
        results$warnings,
        sprintf("Found %d duplicate P_ID values", length(unique(dups)))
      )
    }
  }
  
  # Summary message
  if (results$valid) {
    results$messages <- c(results$messages, "Data structure validation passed")
  }
  
  return(results)
}


#' Print Validation Results
#'
#' Pretty-print the results from validate_data_structure()
#'
#' @param validation_results List returned from validate_data_structure()


print_validation_results <- function(validation_results) {
  cat("\n")
  cat("===============================================\n")
  cat("  DATA VALIDATION RESULTS\n")
  cat("===============================================\n")
  
  if (validation_results$valid) {
    cat("✓ Status: PASSED\n")
  } else {
    cat("✗ Status: FAILED\n")
  }
  
  if (length(validation_results$messages) > 0) {
    cat("\nMessages:\n")
    for (msg in validation_results$messages) {
      cat("  •", msg, "\n")
    }
  }
  
  if (length(validation_results$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warn in validation_results$warnings) {
      cat("  ⚠", warn, "\n")
    }
  }
  
  cat("===============================================\n\n")
}


# =============================================================================
# DATA TRANSFORMATION FUNCTIONS
# =============================================================================

#' Transform Trust Data to Long Format
#'
#' Instead of copying and pasting the pivot_longer code 5 times (once per agent),
#' we write it once in a function and call it as needed.
#' @param data Wide-format data frame
#' @param agent_name Name of the agent (e.g., "Rocky", "Boulder")
#' @return Long-format data frame with columns: P_ID, time, Trust_{agent_name}
#' @examples
#' rocky_long <- transform_agent_to_long(wide_df, "Rocky")
transform_agent_to_long <- function(data, agent_name) {
  
  # Validate inputs
  if (!agent_name %in% PARAMS$agents) {
    stop(sprintf("Invalid agent name: %s. Must be one of: %s",
                 agent_name, paste(PARAMS$agents, collapse = ", ")))
  }
  
  # Construct column names to select
  trust_cols <- paste0("Trust_", agent_name, "_T", PARAMS$timepoints)
  
  # Select participant ID and trust columns for this agent
  agent_data <- data %>%
    select(P_ID, all_of(trust_cols))
  
  # Transform to long format
  agent_long <- agent_data %>%
    pivot_longer(
      cols = starts_with(paste0("Trust_", agent_name, "_T")),
      names_to = "time_col",
      values_to = paste0("Trust_", agent_name)
    ) %>%
    mutate(
      # Extract time number from column name (e.g., "Trust_Rocky_T3" -> 3)
      Time = as.numeric(str_extract(time_col, "(?<=T)\\d+")),
      time_col = NULL  # Remove temporary column
    )
  
  return(agent_long)
}


#' Transform All Agents to Long Format
#'
#' Applies transform_agent_to_long() to all agents and combines results.
#'
#' @param data Wide-format data frame
#' @param agents Character vector of agent names
#' @return Long-format data frame with all agents
transform_all_agents_to_long <- function(data, agents = PARAMS$agents) {
  
  # Transform each agent separately
  # Use purrr::map() to apply function to each element of a vector 
  # and returns a list of results instead of writing for loop to do it
  agent_list <- map(agents, ~transform_agent_to_long(data, .x))
  
  # Join all agent data frames together
  # We start with the first agent and progressively join the others
  combined <- agent_list[[1]]
  
  for (i in 2:length(agent_list)) {
    combined <- combined %>%
      full_join(agent_list[[i]], by = c("P_ID", "Time"))
  }
  
  # Add back the Condition variable
  combined <- combined %>%
    left_join(select(data, P_ID, Condition), by = "P_ID") %>%
    select(P_ID, Condition, Time, everything())  # Reorder columns
  
  return(combined)
}


#' Create Phase Indicator Variables
#'
#' Adds discontinuous growth model phase indicators to the dataset.
#' These variables code different segments of the trust trajectory.
#'
#' @param data Long-format data frame with Time column
#' @return Data frame with added phase indicator columns
create_phase_indicators <- function(data) {
  
  data_with_phases <- data %>%
    mutate(
      # Apply each phase coding function from the config
      DEVELOPMENT = PHASE_CODING$DEVELOPMENT(Time),
      DISSOLUTION = PHASE_CODING$DISSOLUTION(Time),
      RESTORATION = PHASE_CODING$RESTORATION(Time),
      INITIAL_RESTORATION = PHASE_CODING$INITIAL_RESTORATION(Time),
      INITIAL_RESTORATION_SQ = PHASE_CODING$INITIAL_RESTORATION_SQ(Time),
      DELAYED_RESTORATION = PHASE_CODING$DELAYED_RESTORATION(Time),
      RETROSPECTIVE = PHASE_CODING$RETROSPECTIVE(Time)
    )
  
  return(data_with_phases)
}


#' Convert Wide Trust Data to Analysis-Ready Long Format
#'
#' This is the "master" transformation function that orchestrates the entire
#' data preparation pipeline. It calls other functions in sequence.
#'
#'
#' @param wide_data Wide-format data frame
#' @return Analysis-ready long-format data frame
prepare_analysis_data <- function(wide_data) {
  
  cat("Transforming data to long format...\n")
  
  # Step 1: Transform to from agent wide to agent long
  long_data <- transform_all_agents_to_long(wide_data)
  
  cat("  ✓ Transformed", nrow(wide_data), "participants to", 
      nrow(long_data), "observations\n")
  
  # Step 2: Further reshape so each row is one observation per agent per time (Agent long)
  long_data_final <- long_data %>%
    pivot_longer(
      cols = starts_with("Trust_"),
      names_to = "Agent",
      names_prefix = "Trust_",
      values_to = "Trust"
    ) %>%
    mutate(
      Agent = factor(Agent, levels = PARAMS$agents)
    )
  
  cat("  ✓ Reshaped to", nrow(long_data_final), "agent-time observations\n")
  
  # Step 3: Add phase indicators
  long_data_final <- create_phase_indicators(long_data_final)
  
  cat("  ✓ Added phase indicator variables\n")
  
  # Step 4: Ensure correct data types
  long_data_final <- long_data_final %>%
    mutate(
      Trust = as.numeric(Trust),
      DEVELOPMENT = as.numeric(DEVELOPMENT),
      DISSOLUTION = as.numeric(DISSOLUTION),
      RESTORATION = as.numeric(RESTORATION),
      INITIAL_RESTORATION = as.numeric(INITIAL_RESTORATION),
      INITIAL_RESTORATION_SQ = as.numeric(INITIAL_RESTORATION_SQ),
      DELAYED_RESTORATION = as.numeric(DELAYED_RESTORATION),
      RETROSPECTIVE = as.numeric(RETROSPECTIVE),
      Condition = factor(Condition),
      Agent = factor(Agent, levels = PARAMS$agents)
    )
  
  # Step 5: Create time index for autocorrelation testing
  # This groups by participant and agent, then creates a sequential index
  long_data_final <- long_data_final %>%
    group_by(P_ID, Agent) %>%
    arrange(Time) %>%
    mutate(TimeShift = row_number()) %>%
    ungroup()
  
  cat("  ✓ Ensured correct data types\n")
  cat("Data preparation complete!\n\n")
  
  return(long_data_final)
}


# =============================================================================
# DESCRIPTIVE STATISTICS FUNCTIONS
# =============================================================================

#' Calculate ICC for a Dataset
#'
#' Intraclass Correlation Coefficient measures the proportion of variance
#' that is between-subjects versus within-subjects.
#' 
#' High ICC means observations within a person are more similar to each other
#' than to observations from different people. This justifies using mixed models.
#'
#' @param data Data frame with Trust, P_ID columns
#' @return Named list with ICC1, ICC2, and additional statistics
calculate_icc <- function(data) {
  
  # Fit null model (random intercepts only)
  null_model <- lme(
    fixed = Trust ~ 1,
    random = ~ 1 | P_ID,
    data = data,
    method = "REML",
    na.action = na.omit
  )
  
  # Extract variance components
  var_corr <- VarCorr(null_model)
  between_var <- as.numeric(var_corr[1, 1])  # Between-subject variance
  within_var <- null_model$sigma^2            # Within-subject variance
  
  # Calculate group sizes
  group_sizes <- data %>%
    filter(!is.na(Trust)) %>%
    count(P_ID) %>%
    pull(n)
  k <- mean(group_sizes)  # Average group size
  
  # ICC(1): Reliability of a single observation
  # Answers: "How much does a single observation tell us about the person?"
  icc1 <- between_var / (between_var + within_var)
  
  # ICC(2): Reliability of group means
  # Answers: "How reliable are person-level averages?"
  icc2 <- between_var / (between_var + (within_var / k))
  
  return(list(
    ICC1 = icc1,
    ICC2 = icc2,
    between_var = between_var,
    within_var = within_var,
    total_var = between_var + within_var,
    avg_obs_per_subject = k,
    n_subjects = length(unique(data$P_ID[!is.na(data$Trust)]))
  ))
}


#' Calculate Descriptive Statistics by Group
#'
#' Computes means, SDs, and sample sizes for specified grouping variables.
#' This is useful for creating summary tables.
#'
#' @param data Data frame
#' @param value_col Name of the column containing values to summarize
#' @param group_cols Character vector of grouping variable names
#' @return Data frame with summary statistics
calculate_descriptives <- function(data, 
                                  value_col = "Trust",
                                  group_cols = c("Condition", "Time")) {
  
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      N = sum(!is.na(.data[[value_col]])),
      Mean = mean(.data[[value_col]], na.rm = TRUE),
      SD = sd(.data[[value_col]], na.rm = TRUE),
      SE = SD / sqrt(N),
      CI_lower = Mean - (1.96 * SE),
      CI_upper = Mean + (1.96 * SE),
      .groups = "drop"
    )
}


# =============================================================================
# REPORTING FUNCTIONS
# =============================================================================

#' Format Numbers for Reporting
#'
#' Formats numeric values according to APA style guidelines.
#'
#' @param x Numeric value or vector
#' @param digits Number of decimal places
#' @return Formatted character string
format_number <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}


#' Create APA-Style M(SD) String
#'
#' Formats mean and SD in APA style: M (SD)
#'
#' @param mean Mean value
#' @param sd Standard deviation value
#' @param digits Number of decimal places
#' @return Formatted string
format_mean_sd <- function(mean, sd, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (", "%.", digits, "f)"),
    mean, sd
  )
}


# =============================================================================
# DIAGNOSTIC PLOTTING FUNCTIONS
# =============================================================================

#' Create QQ Plot for Model Diagnostics
#'
#' Checks normality assumption by comparing residuals to theoretical normal distribution.
#'
#' @param model A fitted model object (lme or lmer)
#' @param title Plot title
#' @return A ggplot object
plot_qq <- function(model, title = "Q-Q Plot of Residuals") {
  
  residuals <- residuals(model, type = "pearson")
  
  qq_data <- data.frame(
    theoretical = qqnorm(residuals, plot.it = FALSE)$x,
    sample = qqnorm(residuals, plot.it = FALSE)$y
  )
  
  ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = title,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    PLOT_THEME
}


#' Create Residual vs Fitted Plot
#'
#' Checks homogeneity of variance assumption.
#'
#' @param model A fitted model object
#' @param title Plot title
#' @return A ggplot object
plot_residual_fitted <- function(model, title = "Residuals vs Fitted") {
  
  plot_data <- data.frame(
    fitted = fitted(model),
    residuals = residuals(model, type = "pearson")
  )
  
  ggplot(plot_data, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(se = TRUE, color = "blue") +
    labs(
      title = title,
      x = "Fitted Values",
      y = "Pearson Residuals"
    ) +
    PLOT_THEME
}


cat("\n✓ Utility functions loaded successfully\n\n")
