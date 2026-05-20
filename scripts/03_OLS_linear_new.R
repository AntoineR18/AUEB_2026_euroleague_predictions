# 03_OLS_linear_new.R

#__ Adapt data ________________________________________________________________
prepare_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    ) |>
    droplevels()
}
train <- prepare_data(train, "OLY")
val <- prepare_data(val, "OLY")

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ 
    team_home + team_away + 
    playoff + # wins_A + wins_B +
    total_diff_home + total_diff_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)
summary(fit)

# __ Predictions without home effect for Final Four games ______________________
predict_without_home_effect <- function (
    td = total_diff,
    model = fit,
    season,
    team_A,
    team_B
) {
  
  # 1. Get correct season
  td <- td[[paste0("playoffs", season)]]
  
  # 2.1. Create home game
  game_1 <- tibble(
    team_home = team_A,
    team_away = team_B,
    playoff = TRUE,
    wins_A = 0,
    wins_B = 0,
    total_diff_home = td |>
      filter(team == team_A) |>
      pull(total_diff_weighted),
    total_diff_away = td |>
      filter(team == team_B) |>
      pull(total_diff_weighted)
  ) |>
    prepare_data("OLY")
  
  # 2.2. Create away game
  game_2 <- tibble(
    team_home = team_B,
    team_away = team_A,
    playoff = TRUE,
    wins_A = 0,
    wins_B = 0,
    total_diff_home = td |>
      filter(team == team_B) |>
      pull(total_diff_weighted),
    total_diff_away = td |>
      filter(team == team_A) |>
      pull(total_diff_weighted)
  ) |>
    prepare_data("OLY")
  
  # 3. Predict according to model
  pred_1 <- predict(
    model,
    newdata = game_1,
    interval = "prediction",
    level = 0.95
  )
  pred_2 <- predict(
    model,
    newdata = game_2,
    interval = "prediction",
    level = 0.95
  )
  
  return(
    matrix(
      (pred_1 - pred_2) / 2,
      nrow = 1,
      dimnames = list(NULL, c("fit", "lwr", "upr"))
    )
  )
}

# __ Sequential validation _____________________________________________________
validate <- function (train, val, fit) {
  
  # 0. Initialize predictions
  predictions <- vector("list", nrow(val))

  for (i in 1:nrow(val)) {

    # 1. Prepare current match
    match_i <- prepare_data(val[i, ], "OLY")
  
    # 2. Solve unknown teams issue
    known_home <- unique(as.character(train$team_home))
    known_away <- unique(as.character(train$team_away))

    match_home <- as.character(match_i$team_home)
    match_away <- as.character(match_i$team_away)

    if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
      predictions[[i]] <- NULL
      train <- bind_rows(train, match_i) |>
        prepare_data("OLY")
      fit <- lm(
        score_diff ~
          team_home + team_away +
          playoff + #wins_A + wins_B +
          total_diff_home + total_diff_away,
        contrasts = list(
          team_home = contr.sum,
          team_away = contr.sum
        ),
        data = train
      )
      next
    }

    # 3. Predict score_diff 
    if (match_i$final_four) {
      pred <- predict_without_home_effect(
        season = substr(format(match_i$date, "%Y"), 3, 4),
        team_A = match_i$team_home,
        team_B = match_i$team_away
      )
    } else {
      pred <- predict(
        fit,
        newdata = match_i,
        interval = "prediction",
        level = 0.95
      )
    }

    # 4. Store prediction vs truth
    predictions[[i]] <- match_i |>
      select(!c("serie", "pts_home", "pts_away")) |>
      rename(true_diff = score_diff) |>
      mutate(
        pred_diff = pred[, "fit"],
        lower_95 = pred[, "lwr"],
        upper_95 = pred[, "upr"]
      )

    # 4. Feed model with current match
    train <- bind_rows(train, match_i) |>
      prepare_data("OLY")
    
    fit <- lm(
      score_diff ~
        team_home + team_away +
        playoff + # wins_A + wins_B +
        total_diff_home + total_diff_away,
      contrasts = list(
        team_home = contr.sum,
        team_away = contr.sum
      ),
      data = train
    )
  }

  return(list(
    train = train,
    fit = fit,
    predictions = bind_rows(predictions)
  ))
}

validated <- validate(train, val, fit)
fit <- validated$fit
train <- validated$train

summary(fit)
 
# __ Final Four predictions ____________________________________________________
predict_f4_26 <- function(
    td = total_diff$po26,
    fit
) {

  sigma <- summary(fit)$sigma
  n <- 10000
  set.seed(1807)

  pred_SF1 <- predict_without_home_effect(
    season = "26",
    team_A = "OLY",
    team_B = "FBB"
  )[, "fit"]
  MC_SF1 <- rnorm(n, pred_SF1, sigma)
  prob_SF1 <- mean(MC_SF1 > 0)
  if (prob_SF1 > 0.5) {winner1 <- "OLY"}
  else {winner1 <- "FBB"}

  pred_SF2 <- predict_without_home_effect(
    season = "26",
    team_A = "VBC",
    team_B = "RMB"
  )[, "fit"]
  MC_SF2 <- rnorm(n, pred_SF2, sigma)
  prob_SF2 <- mean(MC_SF2 > 0)
  if (prob_SF2 > 0.5) {winner2 <- "VBC"}
  else {winner2 <- "RMB"}

  pred_final <- predict_without_home_effect(
    season = "26",
    team_A = winner1,
    team_B = winner2
  )[, "fit"]
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

# # __ Interpretations ___________________________________________________________
# show_coefs <- function(model = fit) {
# 
#   # Home
#   coefs_home <- coef(model)[
#     grepl("^team_home", names(coef(model)))
#   ]
# 
#   effects_home <- c(
#     coefs_home,
#     -sum(coefs_home)
#   )
# 
#   # Away
#   coefs_away <- coef(model)[
#     grepl("^team_away", names(coef(model)))
#   ]
# 
#   effects_away <- c(
#     coefs_away,
#     -sum(coefs_away)
#   )
# 
#   teams <- levels(droplevels(train$team_home))
# 
#   tibble(
#     team = teams,
#     home_effect = effects_home,
#     away_effect = effects_away,
#     net_home_adv = effects_home - effects_away
#   ) |>
#     arrange(desc(net_home_adv))
# }
# show_coefs()
