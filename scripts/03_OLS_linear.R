# __ Adapted dataset ___________________________________________________________
prepare_data_OLS <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}

train_OLS <- prepare_data_OLS(train, "OLY")
train_OLS <- train_OLS[c(
  "date",
  "team_home", "pts_home",
  "team_away", "pts_away" ,
  "score_diff", "win" ,"playoff"
)]

# __ Initial train _____________________________________________________________
fit_OLS <- lm(
  score_diff ~ team_home + team_away + playoff,
  data = train_OLS
)

summary(fit_OLS)

fit_OLS_initial <- fit_OLS

# __ Sequential validation _____________________________________________________
validate_OLS <- function (
    train = train_OLS,
    val_ = val,
    model = fit_OLS
) {
  
  predictions <- vector("list", nrow(val_))
  
  for (i in 1:nrow(val_)) {
    
    # Current match
    match_i <- prepare_data_OLS(val_[i, ], "OLY")
    
    known_home <- unique(as.character(train$team_home))
    known_away <- unique(as.character(train$team_away))
    
    match_home <- as.character(match_i$team_home)
    match_away <- as.character(match_i$team_away)
    
    if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
      predictions[[i]] <- NULL
      train <- bind_rows(train, match_i)
      model <- lm(
        score_diff ~ team_home + team_away + playoff,
        data = train
      )
      next
    }
    
    # 1. Predict BEFORE adding the result
    pred <- predict(
      model,
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
    train <- bind_rows(train, match_i)
    model <- lm(
      score_diff ~ team_home + team_away + playoff,
      data = train
    )
  }
  
  return(list(
    train_rolling_OLS = train,
    fit_OLS_validated = model
  ))
}

val_OLS <- validate_OLS()
fit_OLS <- val_OLS$fit_OLS_validated
train_rolling_OLS <- val_OLS$train_rolling_OLS

summary(fit_OLS)

fit_OLS_validated <- fit_OLS

# __ Playoffs predictions ______________________________________________________
predict_po26 <- function (
    df = playoffs26_pred,
    model = fit_OLS,
    train = train_rolling_OLS
) {
  
  for (i in 1:5) {
    
    for (s in unique(df$serie)) {
      
      idx <- which(df$serie == s & df$match_number == i)
      
      if (
        max(df[df$serie == s, "win_A"], na.rm = TRUE) >= 3 ||
        max(df[df$serie == s, "win_B"], na.rm = TRUE) >= 3
      ) {
        next
      }
      
      home <- if (i %in% c(1, 2, 5)) "A" else "B"
      
      match_i <- tibble(
        team_home = ifelse(
          home == "A",
          subset(df, serie == s & match_number == i)$team_A,
          subset(df, serie == s & match_number == i)$team_B
        ),
        team_away = ifelse(
          home == "A",
          subset(df, serie == s & match_number == i)$team_B,
          subset(df, serie == s & match_number == i)$team_A
        ),
        playoff = 1
      )
      
      match_i <- prepare_data_OLS(match_i, "OLY")
      pred <- predict(model, newdata = match_i)
      
      df[idx, "match_played"] <- TRUE
      df[idx, "score_diff"] <- pred
      
      if (pred > 0) {
        df[idx, "win_A"] <- max(
          df[df$serie == s, "win_A"],
          na.rm = TRUE
        ) + 1
       
      } else {
        df[idx, "win_B"] <- max(
          df[df$serie == s, "win_B"],
          na.rm = TRUE
        ) + 1
        }
      
      if (nrow(playoffs26_true |> filter(serie == s, match_number == i)) > 0) {
        train <- bind_rows(
          train,
          tibble(
            team_home = match_i$team_home,
            team_away = match_i$team_away,
            score_diff = playoffs26_true |>
              filter(serie == s, match_number == i) |>
              pull(score_diff),
            playoff = 1
          )
        )
        
        model <- lm(
          score_diff ~ team_home + team_away + playoff,
          data = train
        )
      }
    }
  }
  return(list(
    playoffs26_pred = df,
    fit_OLS = model,
    train_rolling_OLS = train
  ))
}

predicted_po26 <- predict_po26()
playoffs26_pred <- predicted_po26$playoffs26_pred
train_rolling_OLS <- predicted_po26$train_rolling_OLS
fit_OLS <- predicted_po26$fit_OLS

summary(fit_OLS)

fit_OLS_after_po26 <- fit_OLS

# __ Final Four predictions ____________________________________________________
deduce_winners <- function (df = playoffs26_true) {
  
  winners <- list()
  
  for (i in 1:4) {
    
    winner <- df |>
      filter(serie == paste0("QF",i), win_A == 3 | win_B == 3) |>
      mutate(
        winner = case_when(
          win_A == 3 ~ team_A,
          win_B == 3 ~ team_B
        )) |>
      pull(winner)
    
    winners[[paste0("QF",i)]] = winner
  }
  return(winners)
}

predict_without_home_effect <- function (team_A, team_B, model) {
  
  game_1 <- tibble(
    team_home = team_A,
    team_away = team_B,
    playoff = 1
  ) |>
    prepare_data_OLS("OLY")
  
  game_2 <- tibble(
    team_home = team_B,
    team_away = team_A,
    playoff = 1
  ) |>
    prepare_data_OLS("OLY")
  
  pred_1 <- predict(model, newdata = game_1)
  pred_2 <- predict(model, newdata = game_2)
  
  return((pred_1 - pred_2) / 2)
}

predict_final4_26 <- function(
    po = playoffs26_true,
    model = fit_OLS
) {
  teams <- deduce_winners(po)
  
  ff4  <- data.frame(
    game = c("SF1", "SF2", "Final"),
    team_A = c(teams$`QF1`, teams$`QF2`, NA_character_),
    team_B = c(teams$`QF4`, teams$`QF3`, NA_character_),
    score_diff = NA_integer_,
    winner = NA_character_
  )
  
  pred_SF1 <- predict_without_home_effect(teams$`QF1`, teams$`QF4`, fit_OLS)

  ff4[ff4$game == "SF1", "score_diff"] <- pred_SF1
  
  if (pred_SF1 > 0) {
    ff4[ff4$game == "SF1", "winner"] <- ff4[ff4$game == "SF1", "team_A"]
  } else {
    ff4[ff4$game == "SF1", "winner"] <- ff4[ff4$game == "SF1", "team_B"]
  }

  pred_SF2 <- predict_without_home_effect(teams$`QF2`, teams$`QF3`, model)
  
  ff4[ff4$game == "SF2", "score_diff"] <- pred_SF2
  
  if (pred_SF2 > 0) {
    ff4[ff4$game == "SF2", "winner"] <- ff4[ff4$game == "SF2", "team_A"]  
  } else {
    ff4[ff4$game == "SF2", "winner"] <- ff4[ff4$game == "SF2", "team_B"]
  }
  
  ff4[ff4$game == "Final", "team_A"] <- ff4[ff4$game == "SF1", "winner"]
  ff4[ff4$game == "Final", "team_B"] <- ff4[ff4$game == "SF2", "winner"]
  
  pred_final <- predict_without_home_effect(
    ff4[ff4$game == "Final", "team_A"],
    ff4[ff4$game == "Final", "team_B"],
    model
  )
  ff4[ff4$game == "Final", "score_diff"] <- pred_final
  if (pred_final > 0) {
    ff4[ff4$game == "Final", "winner"] <- ff4[ff4$game == "Final", "team_A"]
  } else {
    ff4[ff4$game == "Final", "winner"] <- ff4[ff4$game == "Final", "team_B"]
  }
  return(ff4)
}

ff4 <- predict_final4_26()

