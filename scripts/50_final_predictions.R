# 50_final_predictions.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games", "all_teams",
  "games", "N", "Seed",
  "prepare_data", "predict_f426_one_game", "plot_SF_heatmaps"
)
rm(list = setdiff(ls(), to_keep))

# __ Prepare data ______________________________________________________________
train <- prepare_data(
  df = bind_rows(
    games$regular$`26`,
    games$playoffs$`26`
  ),
  team_ref = "OLY"
) |>
  arrange(date)

train_with25 <- prepare_data(
  df = bind_rows(
    games$regular$`25`,
    games$playoffs$`25`,
    games$regular$`26`,
    games$playoffs$`26`
  ),
  team_ref = "OLY"
) |>
  arrange(date)

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ team_home + team_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)

fit_with25 <- lm(
  score_diff ~ team_home + team_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train_with25
)

# __ Predictions _______________________________________________________________
Final_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Final WHE 26", team_A = "OLY", team_B = "RMB"
)

Final_with_for_OLY <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = TRUE,
  game_name = "Final HE 26", team_A = "OLY", team_B = "RMB"
)

Final_without_with25 <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit_with25, with_home_effect = FALSE,
  game_name = "Final WHE 25-26", team_A = "OLY", team_B = "RMB"
)

Final_with_for_OLY_with25 <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit_with25, with_home_effect = TRUE,
  game_name = "Final HE 25-26", team_A = "OLY", team_B = "RMB"
)

# __ Show winning predictions __________________________________________________
final_predictions_heatmap <- function (
    final_WHE_26, final_HE_26,
    final_WHE_2526, final_HE_2526
) {

  df <- tibble(
    `Training data` = c("25-26", "24-25 & 25-26"),
    `Without home effect` = c(
      final_WHE_26$summary$Probability,
      final_WHE_2526$summary$Probability
    ),
    `With home effect for OLY` = c(
      final_HE_26$summary$Probability,
      final_HE_2526$summary$Probability
    )
  )

  plot_df <- df |>
    pivot_longer(-`Training data`, names_to = "Scenario", values_to = "Probability") |>
    ggplot(aes(x = Scenario, y = `Training data`, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Probability, 2)), size = 5) +
    scale_fill_gradient(
      low = "white",
      high = "darkblue",
      limits = c(0, 1)
    ) +
    labs(
      title = "Probability of OLY winning the Final",
      x = NULL,
      y = NULL,
      fill = "Probability"
    ) +
    theme_minimal()
  
  return(plot_df)
}
prob_sumup <- final_predictions_heatmap(
  Final_without, Final_with_for_OLY,
  Final_without_with25, Final_with_for_OLY_with25
)
print(prob_sumup)

# __ Heatmaps __________________________________________________________________
plot_final_heatmaps <- function(
    final_WHE_26, final_HE_26, final_WHE_2526, final_HE_2526,
    title = "Final predictions"
) {
  
  get_max_prob <- function(mc) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      pull(prob) |>
      max()
  }
  
  max_prob <- max(
    get_max_prob(final_WHE_26$MC),
    get_max_prob(final_HE_26$MC),
    get_max_prob(final_WHE_2526$MC),
    get_max_prob(final_HE_2526$MC)
  )
  
  plot_MC_heatmap <- function(mc, team_A, team_B, title, pred_diff) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      ggplot(aes(x = diff, y = 1, fill = prob)) +
      geom_tile() +
      scale_fill_gradient(
        low = "white",
        high = "darkblue",
        limits = c(0, max_prob)
      ) +
      scale_x_continuous(breaks = seq(-15, 15, by = 5)) +
      labs(
        title = title,
        x = NULL,
        y = NULL,
        fill = "Probability"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) +
      geom_text(
        aes(
          label = ifelse(
            prob == max(prob),
            paste0(diff, "\n(", round(prob*100, 1), "%)"),
            ""
          )
        ),
        color = "white", size = 3
      ) +
      geom_vline(
        aes(xintercept = round(pred_diff), color = "Predicted diff"),
        linetype = "dashed"
      ) +
      scale_color_manual(values = c("Predicted diff" = "red"), name = NULL)
  }
  
  p1 <- plot_MC_heatmap(
    mc = final_WHE_26$MC,
    team_A = final_WHE_26$summary$Team_A,
    team_B = final_WHE_26$summary$Team_B,
    title = "Without home effect — 25-26",
    pred_diff = final_WHE_26$summary$Score_diff_pred
  )
  p2 <- plot_MC_heatmap(
    mc = final_HE_26$MC,
    team_A = final_HE_26$summary$Team_A,
    team_B = final_HE_26$summary$Team_B,
    title = "With home effect for Olympiacos — 25-26",
    pred_diff = final_HE_26$summary$Score_diff_pred
  )
  p3 <- plot_MC_heatmap(
    mc = final_WHE_2526$MC,
    team_A = final_WHE_2526$summary$Team_A,
    team_B = final_WHE_2526$summary$Team_B,
    title = "Without home effect — 24-25 & 25-26",
    pred_diff = final_WHE_2526$summary$Score_diff_pred
  )
  p4 <- plot_MC_heatmap(
    mc = final_HE_2526$MC,
    team_A = final_HE_2526$summary$Team_A,
    team_B = final_HE_2526$summary$Team_B,
    title = "With home effect for Olympiacos — 24-25 & 25-26",
    pred_diff = final_HE_2526$summary$Score_diff_pred
  )
  
  (p2 + p1) / (p4 + p3) +
    plot_layout(guides = "collect") +
    plot_annotation(title = title)
}
final_predictions_heatmap <- plot_final_heatmaps(
  Final_without,
  Final_with_for_OLY,
  Final_without_with25,
  Final_with_for_OLY_with25
)
print(final_predictions_heatmap)

# __ Export results ____________________________________________________________
# ggsave(
#   "outputs/final_predictions/prob_sumup.png",
#   plot = prob_sumup,
#   width = 12, height = 6, dpi = 300
# )
# ggsave(
#   "outputs/final_predictions/final_predictions_heatmap.png",
#   plot = final_predictions_heatmap,
#   width = 12, height = 6, dpi = 300
# )
