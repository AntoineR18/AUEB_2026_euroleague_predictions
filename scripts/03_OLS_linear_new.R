# 03_OLS_linear_new.R

#__ Adapt data ________________________________________________________________
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

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ 
    team_home + team_away + 
    # playoff + wins_A + wins_B +
    total_diff_home + total_diff_away,
  # contrasts = list(
  #   team_home = contr.sum,
  #   team_away = contr.sum
  # ),
  data = train
)
summary(fit)

fit_trained_without_intercept <- lm(
  score_diff ~
    team_home + team_away +
    # playoff + wins_A + wins_B +
    total_diff_home + total_diff_away
  ,
  contrasts = list(
    team_home = contr.sum,
    team_away = contr.sum
  ),
  data = train
)
summary(fit_trained_without_intercept)

# __ Predictions without home effect for Final Four games ______________________
predict_without_home_effect <- function (
    td = total_diff,
    model,
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

# __ Clean environment _________________________________________________________
to_keep <- c(
  "games", "team_codes", "all_teams",
  "train", "val_po26", "total_diff",
  "prepare_data", "train", "fit", "predict_without_home_effect")
rm(list = setdiff(ls(), to_keep))

# __ Sequential validation using MC estimation _________________________________
n <- 10000
seed <- 1807

validate_MC <- function (n, seed, train, val, model) {

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

validated <- validate_MC(n, seed, train, val_po26, fit)
train_rolling <- validated$train_rolling
fit <- validated$fit
predictions_po26 <- validated$predictions

# validated_trained_without_intercept <-
#   validate_MC(n, seed, train, val_po26, fit_trained_without_intercept)
# fit_trained_without_intercept <-
#   validated_trained_without_intercept$fit
# predictions_po26_trained_without_intercept <-
#   validated_trained_without_intercept$predictions

# __ Final Four prediction _____________________________________________________
predict_f426_one_game <- function(
    n,
    seed,
    td = total_diff$po26,
    model,
    game_name,
    team_A,
    team_B,
    intercept
) {

  sigma <- summary(model)$sigma
  set.seed(seed)

  pred <- predict_without_home_effect(
    model = model,
    season = "26",
    team_A = team_A,
    team_B = team_B
  )[, "fit"]
  if (!intercept) {pred <- pred - model$coefficients[[1]]}
  MC <- rnorm(n, pred, sigma)
  prob <- mean(MC > 0)
  if (prob > 0.5) {winner <- team_A} else {winner <- team_B}

  game <- data.frame(
    Game = game_name,
    Team_A = team_A,
    Team_B = team_B,
    Score_diff_pred = pred,
    Probability = prob,
    Winner = winner
  )
  return(list(
    summary = game,
    MC = MC # list(SF1 = MC_SF1, SF2 = MC_SF2, third = MC_3rd,final = MC_final)
  ))
}

# __ Predict without intercept _________________________________________________
predicted_SF1_without <- predict_f426_one_game(
  n, seed, model = fit, intercept = FALSE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
predicted_SF2_without <- predict_f426_one_game(
  n, seed, model = fit, intercept = FALSE,
  game_name = "Semi final 2", team_A = "VBC", team_B = "RMB"
)
predictions_SF_without <- bind_rows(
  predicted_SF1_without$summary,
  predicted_SF2_without$summary
)

# __ Predict with intercept for OLY ____________________________________________
predicted_SF1_with <- predict_f426_one_game(
  n, seed, model = fit, intercept = TRUE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
predictions_SF_with_for_OLY <- bind_rows(
  predicted_SF1_with$summary,
  predicted_SF2_without$summary
)

# __ Predict with intercept ____________________________________________________
predicted_SF2_with <- predict_f426_one_game(
  n, seed, model = fit, intercept = TRUE,
  game_name = "Semi final 2", team_A = "VBC", team_B = "RMB"
)
predictions_SF_with <- bind_rows(
  predicted_SF1_with$summary,
  predicted_SF2_with$summary
)

# __ Adapt final four game data to bind_rows ___________________________________
build_f4_obs <- function(
  team_A, team_B, pred_diff, td = total_diff[["playoffs26"]]
) {
  tibble(
    date = as.Date(NA),
    playoff = TRUE,
    final_four = TRUE,
    serie = NA_character_,
    team_home = team_A,
    pts_home = NA_real_,
    team_away = team_B,
    pts_away = NA_real_,
    score_diff = pred_diff,
    wins_A = 0L,
    wins_B = 0L,
    total_diff_home = td |> filter(team == team_A) |> pull(total_diff_weighted),
    total_diff_away = td |> filter(team == team_B) |> pull(total_diff_weighted)
  )
}

# __ Scenario without intercept ________________________________________________
sf1_obs_without <- build_f4_obs(
  team_A = "OLY", team_B = "FBB",
  pred_diff = predicted_SF1_without$summary$Score_diff_pred
)
sf2_obs_without <- build_f4_obs(
  team_A = "VBC", team_B = "RMB",
  pred_diff = predicted_SF2_without$summary$Score_diff_pred
)

new_obs_without <- bind_rows(sf1_obs_without, sf2_obs_without)

train_f4_without <- bind_rows(train_rolling, new_obs_without) |>
  prepare_data("OLY")
fit_f4_without <- lm(
  score_diff ~ team_home + team_away + total_diff_home + total_diff_away,
  contrasts = list(team_home = contr.sum, team_away = contr.sum),
  data = train_f4_without
)

predictions_F_without <- predict_f426_one_game(
  n, seed, model = fit_f4_without, intercept = FALSE,
  game_name = "Final",
  team_A = predictions_SF_without$Winner[[1]],
  team_B = predictions_SF_without$Winner[[2]]
)$summary

predictions_f4_without <- bind_rows(
  predictions_SF_without,
  predictions_F_without
)

cat("Without considering the intercept :\n")
print(predictions_f4_without)

# __ Scenario with intercept for OLY ___________________________________________
sf1_obs_with <- build_f4_obs(
  team_A = "OLY", team_B = "FBB",
  pred_diff = predicted_SF1_with$summary$Score_diff_pred
)

new_obs_with_for_OLY <- bind_rows(sf1_obs_with, sf2_obs_without)

train_f4_with_for_OLY <- bind_rows(train_rolling, new_obs_with_for_OLY) |>
  prepare_data("OLY")
fit_f4_with_for_OLY <- lm(
  score_diff ~ team_home + team_away + total_diff_home + total_diff_away,
  contrasts = list(team_home = contr.sum, team_away = contr.sum),
  data = train_f4_with_for_OLY
)

predictions_F_with_for_OLY <- predict_f426_one_game(
  n, seed, model = fit_f4_with_for_OLY, intercept = TRUE,
  game_name = "Final",
  team_A = predictions_SF_with_for_OLY$Winner[[1]],
  team_B = predictions_SF_with_for_OLY$Winner[[2]]
)$summary

predictions_f4_with_for_OLY <- bind_rows(
  predictions_SF_with_for_OLY,
  predictions_F_with_for_OLY
)

cat("Considering the intercept for Olympiacos only :\n")
print(predictions_f4_with_for_OLY)

# __ Heatmaps __________________________________________________________________
# plot_f4_heatmaps <- function(predicted_f426) {
#
#   MC <- predicted_f426$MC
#   summary <- predicted_f426$summary
#
#   get_max_prob <- function(mc) {
#     tibble(diff = round(mc)) |>
#       filter(diff >= -15, diff <= 15) |>
#       count(diff) |>
#       mutate(prob = n / sum(n)) |>
#       pull(prob) |>
#       max()
#   }
#
#   max_prob <- max(
#     get_max_prob(MC$SF1),
#     get_max_prob(MC$SF2)
#   )
#
#   plot_MC_heatmap <- function(mc, team_A, team_B, title, pred_diff) {
#     tibble(diff = round(mc)) |>
#       filter(diff >= -15, diff <= 15) |>
#       count(diff) |>
#       mutate(prob = n / sum(n)) |>
#       ggplot(aes(x = diff, y = 1, fill = prob)) +
#       geom_tile() +
#       scale_fill_gradient(
#         low = "white",
#         high = "darkblue",
#         limits = c(0, max_prob)
#       ) +
#       scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
#       labs(
#         title = title,
#         x = paste0("Score difference (", team_A, " - ", team_B, ")"),
#         y = NULL,
#         fill = "Probability"
#       ) +
#       theme_minimal() +
#       theme(axis.text.y = element_blank()) +
#       geom_text(
#         aes(
#           label = ifelse(
#             prob == max(prob),
#             paste0(diff, "\n(", round(prob*100, 1), "%)"),
#             ""
#           )
#         ),
#         color = "white", size = 3
#       ) +
#       geom_vline(
#         aes(xintercept = round(pred_diff), color = "Predicted diff"),
#         linetype = "dashed"
#       ) +
#       scale_color_manual(values = c("Predicted diff" = "red"), name = NULL)
#   }
#
#   titles <- c("Semi-final 1", "Semi-final 2")
#
#   plots <- imap(MC, function(mc, name) {
#     i <- which(names(MC) == name)
#     plot_MC_heatmap(
#       mc = mc,
#       team_A = summary$Team_A[[i]],
#       team_B = summary$Team_B[[i]],
#       title = titles[[i]],
#       pred_diff = summary$Score_diff_pred[[i]]
#     )
#   })
#
#   plots$SF1 + plots$SF2 +
#     plot_layout(guides = "collect")
# }
#
# ggsave(
#   "outputs/heatmaps_f4_SF.png",
#   plot = plot_f4_heatmaps(predicted_f426),
#   width = 12, height = 6, dpi = 300
# )

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
