# __ Adapted dataset ___________________________________________________________
prepare_glm_data <- function(df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}
train_reg25_OLS <- prepare_glm_data(train_reg25, "OLY")

# __ Initial train _____________________________________________________________
fit_logit <- glm(win ~ team_home + team_away + playoff,
                 data = train_reg25,
                 family = binomial)
summary(fit_logit)

# __ Sequential validation _____________________________________________________
train_rolling <- train_reg25
predictions <- vector("list", nrow(val))

for (i in 1:nrow(val)) {
  
  # Current match
  match_i <- prepare_glm_data(val[i, ], "OLY")
  
  # New teams issue
  known_home <- unique(as.character(train_rolling$team_home))
  known_away <- unique(as.character(train_rolling$team_away))
  
  match_home <- as.character(match_i$team_home)
  match_away <- as.character(match_i$team_away)
  
  if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
    predictions[[i]] <- NULL
    train_rolling <- bind_rows(train_rolling, match_i)
    fit_logit <- glm(win ~ team_home + team_away + playoff,
                     data = train_rolling,
                     family = binomial)
    next
  }
  
  # 1. Predict BEFORE adding the result
  pred_prob <- predict(fit_logit, newdata = match_i, type = "response")
  
  # 2. Store prediction vs truth
  predictions[[i]] <- tibble(
    date      = match_i$date,
    team_home = match_i$team_home,
    team_away = match_i$team_away,
    playoff   = match_i$playoff,
    true_win  = match_i$win,
    pred_prob = pred_prob
  )
  
  # 3. Add match to train and re-estimate
  train_rolling <- bind_rows(train_rolling, match_i)
  fit_logit <- glm(win ~ team_home + team_away + playoff,
                   data = train_rolling,
                   family = binomial)
}
print(summary(fit_logit))

# __ Metrics ___________________________________________________________________
results_logit <- bind_rows(predictions)

results_logit <- results_logit |>
  mutate(pred_win = ifelse(pred_prob > 0.5, 1, 0))

accuracy <- mean(results_logit$pred_win == results_logit$true_win)
cat("Accuracy :", round(accuracy, 4), "\n")