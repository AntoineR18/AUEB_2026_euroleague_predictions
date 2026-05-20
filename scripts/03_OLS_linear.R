# __ Adapted dataset ___________________________________________________________
prepare_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}
train <- prepare_data(train, "OLY")

# __ Initial train _____________________________________________________________
fit <- lm(
  score_diff ~ team_home + team_away + playoff + total_diff_home + total_diff_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)
summary(fit)

# __ Sequential validation _____________________________________________________
validate <- function (train, val, fit) {
  
  predictions <- vector("list", nrow(val))
  
  for (i in 1:nrow(val)) {
    
    # Current match
    match_i <- prepare_data(val[i, ], "OLY")
    
    known_home <- unique(as.character(train$team_home))
    known_away <- unique(as.character(train$team_away))
    
    match_home <- as.character(match_i$team_home)
    match_away <- as.character(match_i$team_away)
    
    if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
      predictions[[i]] <- NULL
      train <- bind_rows(train, match_i) |>
        prepare_data("OLY")
      fit <- lm(
        score_diff ~ team_home + team_away + playoff + total_diff_home + total_diff_away,
        contrasts = list(
          team_home = contr.sum,
          team_away = contr.sum
        ),
        data = train
      )
      next
    }
    
    # 1. Predict BEFORE adding the result
    pred <- predict(
      fit,
      newdata = match_i,
      interval = "prediction",
      level = 0.95
    )
    
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
    train <- bind_rows(train, match_i) |>
      prepare_data("OLY")
    fit <- lm(
      score_diff ~ team_home + team_away + playoff + total_diff_home + total_diff_away,
      contrasts = list(
        team_home = contr.sum,
        team_away = contr.sum
      ),
      data = train
    )
  }
  
  return(list(
    train_rolling = train,
    fit = fit,
    predictions = bind_rows(predictions)
  ))
}

validated <- validate(train, val, fit)
fit <- validated$fit
train_rolling <- validated$train_rolling

summary(fit)

predictions_po26 <- validated$predictions |>
  filter(date >= as.Date("2026-04-28"))

# __ Final Four predictions ____________________________________________________
predict_without_home_effect <- function (
    td = total_diff$po26,
    team_A,
    team_B,
    fit
) {
  game_1 <- tibble(
    team_home = team_A,
    team_away = team_B,
    playoff = 1,
    total_diff_home = td |>
      filter(team == team_A) |>
      pull(total_diff_weighted),
    total_diff_away = td |>
      filter(team == team_B) |>
      pull(total_diff_weighted)
  ) |>
    prepare_data("OLY")
  
  game_2 <- tibble(
    team_home = team_B,
    team_away = team_A,
    playoff = 1,
    total_diff_home = td |>
      filter(team == team_B) |>
      pull(total_diff_weighted),
    total_diff_away = td |>
      filter(team == team_A) |>
      pull(total_diff_weighted)
  ) |>
    prepare_data("OLY")
  
  pred_1 <- predict(fit, newdata = game_1)
  pred_2 <- predict(fit, newdata = game_2)
  
  return(as.numeric((pred_1 - pred_2) / 2))
}

predict_f4_26 <- function(
    td = total_diff$po26,
    fit
) {
  
  sigma <- summary(fit)$sigma
  n <- 10000
  set.seed(1807)
  
  pred_SF1 <- predict_without_home_effect(
    td = td,
    team_A = "OLY",
    team_B = "FBB",
    fit = fit
  )
  MC_SF1 <- rnorm(n, pred_SF1, sigma)
  prob_SF1 <- mean(MC_SF1 > 0)
  if (prob_SF1 > 0.5) {winner1 <- "OLY"}
  else {winner1 <- "FBB"}
  
  pred_SF2 <- predict_without_home_effect(
    td = td,
    team_A = "VBC",
    team_B = "RMB",
    fit = fit
  )
  MC_SF2 <- rnorm(n, pred_SF2, sigma)
  prob_SF2 <- mean(MC_SF2 > 0)
  if (prob_SF2 > 0.5) {winner2 <- "VBC"}
  else {winner2 <- "RMB"}
  
  pred_final <- predict_without_home_effect(
    td = td,
    team_A = winner1,
    team_B = winner2,
    fit = fit
  )
  MC_final <- rnorm(n, pred_final, sigma)
  prob_final <- mean(MC_final > 0)

  f4 <- data.frame(
    Game = c("Semi-final 1", "Semi-final 2", "Final"),
    Team_A = c("OLY", "VBC", winner1),
    Team_B = c("FBB", "RMB", winner2),
    Score_diff_pred = c(pred_SF1, pred_SF2, pred_final),
    Probability = c(prob_SF1, prob_SF2, prob_final)
  )
  return(f4)
}

predictions_f4 <- predict_f4_26(fit = fit)
