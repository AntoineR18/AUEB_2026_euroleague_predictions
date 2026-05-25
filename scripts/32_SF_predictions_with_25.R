# 32_SF_predictions_with_25.R

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
    games$regular$`25`,
    games$playoffs$`25`,
    games$regular$`26`,
    games$playoffs$`26` |> filter(!final4)
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

# __ Predict without home effect _______________________________________________
SF1_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
SF2_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Semi final 2", team_A = "VBC", team_B = "RMB"
)
SF_without <- bind_rows(
  SF1_without$summary,
  SF2_without$summary
)

# __ Predict with home effect for OLY __________________________________________
SF1_with <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = TRUE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
SF_with_for_OLY <- bind_rows(
  SF1_with$summary,
  SF2_without$summary
)

# __ Show predictions __________________________________________________________
cat(
  "Predictions for the Euroleague semi-finals considering 2025 season too :\n",
  "Without considering the home effect for Olympiacos :\n"
)
print(SF_without)
cat("Considering the home effect for Olympiacos :\n")
print(SF_with_for_OLY)

# __ Check predictions coherence _______________________________________________
mean_points <- left_join(
  train |>
    group_by(team = team_home) |>
    summarise(mean_diff_home = mean(score_diff)),
  train |>
    group_by(team = team_away) |>
    summarise(mean_diff_away = mean(-score_diff)),
  by = "team"
) |>
  mutate(mean_diff = (mean_diff_home + mean_diff_away) / 2) |>
  arrange(desc(mean_diff))
head(mean_points, 4)

# __ Heatmaps __________________________________________________________________
SF_heatmaps_with25 <- plot_SF_heatmaps(SF1_without, SF2_without, SF1_with)

# __ Export results ____________________________________________________________
# write_csv(SF_without, "outputs/SF_predictions_with25/SF_without_with25.csv")
# write_csv(SF_with_for_OLY, "outputs/SF_predictions_with25/SF_with_for_OLY_with25.csv")

# print(xtable(SF_without), file = "outputs/SF_predictions_with25/SF_without_with25.tex")
# print(xtable(SF_with_for_OLY), file = "outputs/SF_predictions_with25/SF_with_for_OLY_with25.tex")

# ggsave(
#   "outputs/SF_predictions_with25/SF_heatmaps_with25.png",
#   plot = SF_heatmaps_with25,
#   width = 12, height = 6, dpi = 300
# )
