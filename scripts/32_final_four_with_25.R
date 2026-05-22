# 32_final_four_with_25.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games", "all_teams",
  "train_reg25", "train_po25", "train_reg26", "po26",
  "N", "Seed",
  "prepare_data", "predict_f426_one_game", "plot_SF_heatmaps"
)
rm(list = setdiff(ls(), to_keep))

# __ Prepare data ______________________________________________________________
train <- prepare_data(
  df = bind_rows(train_reg25, train_po25, train_reg26, po26),
  team_ref = "OLY"
)

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

Final_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Final",
  team_A = SF1_without$summary$Winner,
  team_B = SF2_without$summary$Winner
)
f4_without <- bind_rows(
  SF_without,
  Final_without$summary
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

Final_with_for_OLY <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit,
  with_home_effect = "OLY" %in% SF_with_for_OLY$Winner,
  game_name = "Final",
  team_A = SF1_with$summary$Winner,
  team_B = SF2_without$summary$Winner
)
f4_with_for_OLY <- bind_rows(
  SF_with_for_OLY,
  Final_with_for_OLY$summary
)

# __ Show predictions __________________________________________________________
cat(
  "Predictions for the Final Four considering 2025 season too :\n",
  "Without considering the home effect for Olympiacos :\n"
)
print(f4_without)
cat("Considering the home effect for Olympiacos :\n")
print(f4_with_for_OLY)

# __ Check predictions coherence _______________________________________________
check_data <- left_join(
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

# __ Heatmaps __________________________________________________________________
SF_heatmaps <- plot_SF_heatmaps(SF1_without, SF2_without, SF1_with)

# __ Export results ____________________________________________________________
write_csv(f4_without, "outputs/f4_predictions_with25/f4_without_with25.csv")
write_csv(f4_with_for_OLY, "outputs/f4_predictions_with25/f4_with_for_OLY_with25.csv")
write_csv(check_data, "outputs/f4_predictions_with25/check_data_with25.csv")

print(xtable(f4_without), file = "outputs/f4_predictions_with25/f4_without_with25.tex")
print(xtable(f4_with_for_OLY), file = "outputs/f4_predictions_with25/f4_with_for_OLY_with25.tex")
print(xtable(check_data), file = "outputs/f4_predictions_with25/check_data_with25.tex")

ggsave(
  "outputs/f4_predictions_with25/SF_heatmaps_with25.png",
  plot = SF_heatmaps,
  width = 12, height = 6, dpi = 300
)
