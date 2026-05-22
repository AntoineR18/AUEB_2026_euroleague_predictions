# 41_validation.R

# __ Clean environment _________________________________________________________
to_keep <- c(
  "raw_games", "all_teams",
  "train_reg25", "train_po25", "train_reg26", "po26",
  "N", "Seed",
  "prepare_data"
)
rm(list = setdiff(ls(), to_keep))

# __ Prepare data ______________________________________________________________
train <- prepare_data(train_reg26, team_ref = "OLY")
train_with25 <- prepare_data(
  bind_rows(train_reg25, train_po25, train_reg26),
  team_ref = "OLY"
)
val <- po26

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ 
    team_home + team_away,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)

fit_with25 <- lm(score_diff ~ team_home + team_away, data = train_with25)
summary(fit_with25)

# __ Sequential validation using MC estimation _________________________________
validate_MC <- function (n = N, seed = Seed, train, val, model) {
  
  set.seed(seed)
  
  train_rolling <- train
  predictions <- vector("list", nrow(val))
  
  # Get standard deviation from model
  sigma <- summary(model)$sigma
  
  for (i in 1:nrow(val)) {
    
    # Prepare current match
    match_i <- prepare_data(val[i, ], "OLY")
    
    # Predict score_diff with MC estimation
    pred <- predict(
      model,
      newdata = match_i,
      interval = "prediction",
      level = 0.95
    )[, "fit"]
    
    MC_estimation <- rnorm(n, pred, sigma)
    prob <- mean(MC_estimation > 0)
    
    # Store prediction vs truth
    predictions[[i]] <- match_i |>
      rename(true_diff = score_diff) |>
      mutate(
        pred_diff = pred,
        prob_win = prob
      )
    
    # Feed model with current match
    train_rolling <- bind_rows(train_rolling, match_i) |>
      prepare_data("OLY")
    
    model <- lm(
      score_diff ~
        team_home + team_away,
      data = train_rolling
    )
  }
  
  return(list(
    train_rolling = train_rolling,
    fit = model,
    predictions = bind_rows(predictions) |>
      select(
        serie, wins_A, wins_B,
        team_home, team_away,
        true_diff, pred_diff, prob_win
      ) |>
      arrange(serie)
  ))
}

validated <- validate_MC(
  n = N, seed = Seed, train = train, val = val, model = fit
)
fit <- validated$fit
predictions <- validated$predictions

validated_with25 <- validate_MC(
  n = N, seed = Seed, train = train_with25, val = val, model = fit_with25
)
fit_with25 <- validated_with25$fit
predictions_with25 <- validated_with25$predictions

# __ Comparisons _______________________________________________________________
compute_metrics <- function(predictions, train_data) {
  predictions |>
    summarise(
      Accuracy = sum(sign(true_diff) == sign(pred_diff)) / n(),
      MAE = mean(abs(true_diff - pred_diff)),
      RMSE = sqrt(mean((true_diff - pred_diff)^2))
    ) |>
    mutate(Train_data = train_data)
}
metrics <- as.data.frame(bind_rows(
  compute_metrics(predictions, "25-26"),
  compute_metrics(predictions_with25, "24-25 & 25-26")
))[c("Train_data", "Accuracy", "MAE", "RMSE")]
print(metrics)

# __ Export results ____________________________________________________________
write_csv(predictions, "outputs/validation/pred_po26.csv")
write_csv(predictions_with25, "outputs/validation/pred_po26_with25.csv")
write_csv(metrics, "outputs/validation/metrics.csv")

print(xtable(predictions), file = "outputs/validation/pred_po26.tex")
print(xtable(predictions_with25), file = "outputs/validation/pred_po26_with25.tex")
print(xtable(metrics), file = "outputs/validation/metrics.tex")
