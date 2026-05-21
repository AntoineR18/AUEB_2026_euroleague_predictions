# 03_validation.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "games", "team_codes", "all_teams",
  "train_reg26", "po26"
)
rm(list = setdiff(ls(), to_keep))

# __ Adapt data ________________________________________________________________
prepare_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}
train <- prepare_data(train_reg26, team_ref = "OLY")
val <- po26

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ 
    team_home + team_away,
  # total_diff_home + total_diff_away,
  data = train
)
summary(fit)

# __ Sequential validation using MC estimation _________________________________
N <- 10000; Seed <- 1807

validate_MC <- function (n = N, seed = Seed, train, val, model) {
  
  set.seed(seed)
  
  train_rolling <- train
  
  # 0. Initialize predictions
  predictions <- vector("list", nrow(val))
  
  for (i in 1:nrow(val)) {
    
    # 1. Prepare current match
    match_i <- prepare_data(val[i, ], "OLY")
    
    # 2. Solve unknown teams issue
    known_home <- unique(as.character(train_rolling$team_home))
    known_away <- unique(as.character(train_rolling$team_away))
    
    match_home <- as.character(match_i$team_home)
    match_away <- as.character(match_i$team_away)
    
    if (!(match_home %in% known_home) || !(match_away %in% known_away)) {
      predictions[[i]] <- NULL
      train_rolling <- bind_rows(train_rolling, match_i) |>
        prepare_data("OLY")
      model <- lm(
        score_diff ~
          team_home + team_away +
          # playoff + wins_A + wins_B +
          total_diff_home + total_diff_away,
        contrasts = list(
          team_home = contr.sum,
          team_away = contr.sum
        ),
        data = train_rolling
      )
      next
    }
    
    # 3. Predict score_diff with MC estimation
    sigma <- summary(model)$sigma
    if (match_i$final_four) {
      pred <- predict_without_home_effect(
        model = model,
        season = substr(format(match_i$date, "%Y"), 3, 4),
        team_A = match_i$team_home,
        team_B = match_i$team_away
      )[, "fit"]
    } else {
      pred <- predict(
        model,
        newdata = match_i,
        interval = "prediction",
        level = 0.95
      )[, "fit"]
    }
    MC_estimation <- rnorm(n, pred, sigma)
    prob <- mean(MC_estimation > 0)
    
    # 4. Store prediction vs truth
    predictions[[i]] <- match_i |>
      select(!c("serie", "pts_home", "pts_away")) |>
      rename(true_diff = score_diff) |>
      mutate(
        pred_diff = pred,
        prob_win = prob
      )
    
    # 4. Feed model with current match
    train_rolling <- bind_rows(train_rolling, match_i) |>
      prepare_data("OLY")
    
    model <- lm(
      score_diff ~
        team_home + team_away +
        # playoff + wins_A + wins_B +
        total_diff_home + total_diff_away,
      contrasts = list(
        team_home = contr.sum,
        team_away = contr.sum
      ),
      data = train_rolling
    )
  }
  
  return(list(
    train_rolling = train_rolling,
    fit = model,
    predictions = bind_rows(predictions)
  ))
}