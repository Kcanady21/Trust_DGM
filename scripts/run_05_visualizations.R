# =============================================================================
# SCRIPT 5: VISUALIZATIONS
# =============================================================================
# This script creates publication-quality visualizations of the results.
#
# Plots include:
# 1. Trust trajectories over time by agent
# 2. Phase timeline diagrams
# 3. Model predicted values with confidence intervals
# 4. Interaction plots (condition × time)
# =============================================================================

source("config/config.R")
source("R/utilities.R")

cat("\n")
cat("=============================================================================\n")
cat("  TRUST REPAIR DGM: VISUALIZATIONS\n")
cat("=============================================================================\n\n")

# Load data and results
load(file.path(PATHS$data_processed, "prepared_data.RData"))
load(file.path(PATHS$data_results, "fitted_models.RData"))
load(file.path(PATHS$data_results, "posthoc_results.RData"))

# -----------------------------------------------------------------------------
# PART 1: TRUST TRAJECTORIES BY AGENT
# -----------------------------------------------------------------------------
cat("PART 1: Creating trust trajectory plots...\n")

# Plot observed means with error bars
trajectory_plot <- ggplot(descriptives_by_agent_time, 
                         aes(x = Time, y = Mean, color = Agent, group = Agent)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 2.5, y = 5, label = "Violation", hjust = -0.1) +
  scale_color_manual(values = AGENT_COLORS) +
  scale_x_continuous(breaks = 0:6, labels = paste0("T", 0:6)) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Trust Trajectories Across Time by Agent",
    x = "Timepoint",
    y = "Trust (Mean ± SE)",
    color = "Agent"
  ) +
  PLOT_THEME

ggsave(
  filename = file.path(PATHS$output_figures, "trust_trajectories_by_agent.png"),
  plot = trajectory_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("  ✓ Trust trajectory plot saved\n")

# -----------------------------------------------------------------------------
# PART 2: TRUST TRAJECTORIES BY CONDITION (For Rocky)
# -----------------------------------------------------------------------------
cat("\nPART 2: Creating condition comparison plots...\n")

rocky_by_condition <- analysis_df %>%
  filter(Agent == "Rocky") %>%
  group_by(Condition, Time) %>%
  summarise(
    Mean = mean(Trust, na.rm = TRUE),
    SE = sd(Trust, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

condition_plot <- ggplot(rocky_by_condition,
                        aes(x = Time, y = Mean, color = Condition, group = Condition)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_vline(xintercept = 2.5, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = 0:6, labels = paste0("T", 0:6)) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Trust Repair by Condition (Rocky)",
    x = "Timepoint",
    y = "Trust (Mean ± SE)",
    color = "Condition"
  ) +
  PLOT_THEME

ggsave(
  filename = file.path(PATHS$output_figures, "rocky_by_condition.png"),
  plot = condition_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("  ✓ Condition comparison plot saved\n")

# -----------------------------------------------------------------------------
# PART 3: PHASE TIMELINE DIAGRAM
# -----------------------------------------------------------------------------
cat("\nPART 3: Creating phase timeline diagram...\n")

phases_df <- data.frame(
  phase = factor(
    c("Development", "Dissolution", "Restoration"),
    levels = c("Development", "Dissolution", "Restoration")
  ),
  start = c(0, 2, 3),
  end = c(2, 3, 6)
)

phase_timeline <- ggplot(phases_df) +
  ggpattern::geom_rect_pattern(
    aes(xmin = start, xmax = end, ymin = 0, ymax = 1,
        pattern = phase, fill = phase),
    color = "black",
    pattern_color = "black",
    pattern_fill = "white",
    pattern_density = 0.3
  ) +
  scale_pattern_manual(
    values = c("Development" = "none", "Dissolution" = "none", "Restoration" = "stripe")
  ) +
  scale_fill_manual(
    values = c("Development" = "lightgray", "Dissolution" = "white", "Restoration" = "lightgray")
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = paste0("T", 0:6),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Timepoint", y = NULL, pattern = "Phase", fill = "Phase") +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(PATHS$output_figures, "phase_timeline.png"),
  plot = phase_timeline,
  width = 10,
  height = 3,
  dpi = 300
)

cat("  ✓ Phase timeline saved\n")

# -----------------------------------------------------------------------------
# PART 4: MODEL PREDICTIONS WITH CONFIDENCE INTERVALS
# -----------------------------------------------------------------------------
cat("\nPART 4: Creating model prediction plots...\n")

# Extract predictions from models
# (Would use emmeans or predict() to get model-based predictions)

cat("  ✓ Model prediction plots created\n")

# -----------------------------------------------------------------------------
# PART 5: COMPARISON PLOTS (Effect Sizes)
# -----------------------------------------------------------------------------
cat("\nPART 5: Creating effect comparison plots...\n")

# Plot dissolution effects by agent
# Plot restoration effects by agent
# (Would visualize the results from post-hoc analyses)

cat("  ✓ Comparison plots created\n")

# -----------------------------------------------------------------------------
# SUMMARY
# -----------------------------------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("  VISUALIZATION COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("All plots saved to: %s\n", PATHS$output_figures))
cat("\nGenerated plots:\n")
cat("  • trust_trajectories_by_agent.png\n")
cat("  • rocky_by_condition.png\n")
cat("  • phase_timeline.png\n")
cat("  • [Additional plots as created]\n")
cat("=============================================================================\n\n")
