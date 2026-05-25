# 42_validation_plots.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games",
  "predictions", "predictions_with25"
)
rm(list = setdiff(ls(), to_keep))

# __ Scatter plot ______________________________________________________________
plot_scatter_validation <- function(predictions, title) {
  predictions |>
    ggplot(aes(x = pred_diff, y = true_diff, color = serie)) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(
      title = title,
      x = "Predicted score difference",
      y = "True score difference",
      color = "Serie"
    ) +
    theme_minimal()
}

# __ Timeline rolling __________________________________________________________
plot_timeline_validation <- function(predictions, title) {
  predictions |>
    mutate(match_id = row_number()) |>
    ggplot(aes(x = match_id)) +
    geom_ribbon(
      aes(ymin = pred_diff - 1.96 * sd(true_diff - pred_diff),
          ymax = pred_diff + 1.96 * sd(true_diff - pred_diff)),
      fill = "grey80", alpha = 0.5
    ) +
    geom_line(aes(y = pred_diff, color = "Predicted"), linewidth = 0.8) +
    geom_point(aes(y = true_diff, color = "True"), size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    scale_color_manual(
      values = c("Predicted" = "darkblue", "True" = "red"),
      name = NULL
    ) +
    labs(
      title = title,
      x = "Match",
      y = "Score difference"
    ) +
    theme_minimal()
}

# __ Show plots ________________________________________________________________
plot_validation <- function(
    predictions,
    predictions_with25,
    title = "Validation — Playoffs 2026"
) {
  p1 <- plot_scatter_validation(predictions, title = "Scatter — 25-26")
  p3 <- plot_scatter_validation(predictions_with25, title = "Scatter — 24-25 & 25-26") +
    labs(y = NULL)
  
  p2 <- plot_timeline_validation(predictions, title = "Timeline — 25-26")
  p4 <- plot_timeline_validation(predictions_with25, title = "Timeline — 24-25 & 25-26") +
    labs(y = NULL)
  
  (p1 + p3 + plot_layout(guides = "collect")) /
    (p2 + p4 + plot_layout(guides = "collect")) +
    plot_annotation(title = title)
}
validation_plots <- plot_validation(predictions, predictions_with25)
validation_plots

# __ Export results ____________________________________________________________
# ggsave(
#   "outputs/validation/scatter_timeline.png",
#   plot = validation_plots,
#   width = 12, height = 6, dpi = 300
# )
