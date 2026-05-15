# __ Adapted dataset ___________________________________________________________
prepare_lm_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}

train_reg25 <- prepare_lm_data(train_reg25, "OLY")

# __ Initial train _____________________________________________________________
fit_normal <- lm(score_diff ~ team_home + team_away + playoff, data = train_reg25)
print(summary(fit_normal))

# __ Sequential validation _____________________________________________________
train_rolling <- train_reg25
predictions <- vector("list", nrow(val))

for (i in 1:nrow(val)) {
  
  # Current match
  match_i <- prepare_lm_data(val[i, ], "OLY")
  
  known_home <- unique(as.character(train_rolling$team_home))
  known_away <- unique(as.character(train_rolling$team_away))
  
  match_home <- as.character(match_i$team_home)
  match_away <- as.character(match_i$team_away)
  
  if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
    predictions[[i]] <- NULL
    train_rolling <- bind_rows(train_rolling, match_i)
    fit_normal <- lm(score_diff ~ team_home + team_away + playoff, data = train_rolling)
    next
  }
  
  # 1. Predict BEFORE adding the result
  pred <- predict(fit_normal, newdata = match_i, interval = "prediction", level = 0.95)
  
  # 2. Store prediction vs truth
  predictions[[i]] <- tibble(
    date      = match_i$date,
    team_home = match_i$team_home,
    team_away = match_i$team_away,
    playoff   = match_i$playoff,
    true_diff = match_i$score_diff,
    pred_diff = pred[, "fit"],
    lower_95  = pred[, "lwr"],
    upper_95  = pred[, "upr"]
  )
  
  # 3. Add match to train and re-estimate
  train_rolling <- bind_rows(train_rolling, match_i)
  fit_normal <- lm(score_diff ~ team_home + team_away + playoff,
                   data = train_rolling)
}
rm(i)

print(summary(fit_normal))

# __ Metrics ___________________________________________________________________
results <- bind_rows(predictions)

rmse <- sqrt(mean((results$true_diff - results$pred_diff)^2))
cat("RMSE :", round(rmse, 3), "\n")
