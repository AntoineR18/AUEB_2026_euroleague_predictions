# __ Adapted dataset ___________________________________________________________
prepare_glm_data <- function (df) {
  df |>
    pivot_longer(
      cols = starts_with("team"),
      names_to = "side",
      values_to = "team"
      ) |>
    mutate(
      side = ifelse(side == "team_home", 1, -1),
      score_diff = ifelse(side == 1, score_diff, -score_diff),
      win = as.numeric(score_diff > 0)
      )
}
train_reg25 <- prepare_glm_data(train_reg25)

# __ Initial train _____________________________________________________________
fit_BT <- glm(win ~ team + side + playoff,
              data = train_reg25,
              contrasts = list(team = contr.sum),
              family = binomial)
print(summary(fit_BT))

# __ Sequential validation _____________________________________________________
train_rolling <- train_reg25
predictions <- vector("list", nrow(val))

for (i in seq_len(nrow(val))) {
  
  # Current match
  match_i <- prepare_glm_data(val[i,])
  
  # New teams issue
  known_teams <- as.character(unique(train_rolling$team))
  
  if (!all(match_i$team %in% known_teams)) {
    predictions[[i]] <- NULL
    train_rolling <- bind_rows(train_rolling, match_i)
    fit_BT <- glm(win ~ team + side + playoff,
                  data = train_rolling,
                  contrasts = list(team = contr.sum),
                  family = binomial)
  next
  }
  
  # Prediction
  pred_prob <- predict(fit_BT, newdata = match_i, type = "response")
  
  # Store prediction vs truth
  predictions[[i]] <- tibble(
    date = match_i[1]$date,
    team_home = match_i$team[1],
    team_away = match_i$team[2],
    playoff   = match_i$playoff[1],
    true_win  = match_i$win[1],
    pred_prob = pred_prob
  )
  
  # Add match to train and re-estimate
  train_rolling <- bind_rows(train_rolling, match_i)
  fit_BT <- glm(win ~ team + side + playoff,
                data = train_rolling,
                contrasts = list(team = contr.sum),
                family = binomial)
  
                              
}
print(summary(fit_BT))
