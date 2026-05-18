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

summary(val_OLS$fit_OLS)


# __ Playoffs predictions ______________________________________________________
predict_po26 <- function (
    df = playoffs26,
    model = fit_OLS,
    train = val_OLS$train_rolling_OLS
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
      
      train <- bind_rows(
        train,
        tibble(
          team_home = match_i$team_home,
          team_away = match_i$team_away,
          score_diff = pred,
          playoff = 1
        )
      )
      
      model <- lm(
        score_diff ~ team_home + team_away + playoff,
        data = train
      )
    }
  }
  
  return(list(
    playoffs26 = df,
    fit_OLS = model,
    train_rolling_OLS = train
  ))
}

predicted_po26 <- predict_po26()

summary(predicted_po26$playoffs26)

# __ Final Four predictions
deduce_winners <- function (df = predicted_po26$playoffs) {
  winners <- list()
  for (i in 1:4) {
    winner <- df |>
      filter(serie == paste0("QF",i),
             match_played,
             win_A == 3 | win_B == 3) |>
      mutate(
        winner = case_when(
          win_A == 3 ~ team_A,
          win_B == 3 ~ team_B
        )
      ) |>
      pull(winner)
    winners[[paste0("QF",i)]] = winner
  }
  return(winners)
}
ff4_teams26 <- deduce_winners()

deduce_final426 <- function(
    df = playoffs26,
    teams = ff4_teams26,
    model = fit_OLS
) {
  ff4  <- data.frame(
    game = c("SF1", "SF2", "Final"),
    team_A = c(ff4_teams$`QF1`, ff4_teams$`QF2`, NA_character_),
    team_B = c(ff4_teams$`QF4`, ff4_teams$`QF3`, NA_character_),
    score_diff = NA_integer_,
    winner = NA_character_
  )
}
