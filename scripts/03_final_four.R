# 03_final_four.R

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
train <- prepare_data(
  df = bind_rows(train_reg26, po26),
  team_ref = "OLY"
)

# __ Train model _______________________________________________________________
fit <- lm(
  score_diff ~ 
    team_home + team_away,
    # total_diff_home + total_diff_away,
  data = train
)
summary(fit)

# __ Final Four prediction with Monte Carlo estimation _________________________
predict_f426_one_game <- function(
    n, seed,
    model, with_home_effect,
    game_name, team_A, team_B
) {
  
  set.seed(seed)
  
  # Get standard deviation from model
  sigma <- summary(model)$sigma
  
  # Predict home game
  game_home <- tibble(
    team_home = team_A,
    team_away = team_B
  ) |>
    prepare_data("OLY")
  
  pred_home <- as.numeric(
    predict(
      model,
      newdata = game_home,
      interval = "prediction",
      level = 0.95
    )[, "fit"]
  )
  
  if (with_home_effect) {
    
    pred <- pred_home
    
  } else {
    
    # Predict away game
    game_away <- tibble(
      team_home = team_B,
      team_away = team_A
    ) |>
      prepare_data("OLY")
    
    pred_away <- as.numeric(
      predict(
        model,
        newdata = game_away,
        interval = "prediction",
        level = 0.95
      )[, "fit"]
    )
    
    pred <- (pred_home - pred_away) / 2
    
  }
  
  # Generate Monte Carlo estimation
  MC <- rnorm(n, pred, sigma)
  prob <- mean(MC > 0)
  
  # Deduce winner 
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
    MC = MC
  ))
}

N <- 10000; Seed <- 1807

# __ Predict without home effect _______________________________________________
SF1_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
SF2_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Semi final 2", team_A = "VBC", team_B = "RMB"
)
SF_without <- bind_rows(
  SF1_without$summary,
  SF2_without$summary
)

Final_without <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = FALSE,
  game_name = "Final",
  team_A = SF1_without$summary$Winner,
  team_B = SF2_without$summary$Winner
)
f4_without <- bind_rows(
  SF_without,
  Final_without$summary
)

# __ Predict with home effect for OLY __________________________________________
SF1_with <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit, with_home_effect = TRUE,
  game_name = "Semi final 1", team_A = "OLY", team_B = "FBB"
)
SF_with_for_OLY <- bind_rows(
  SF1_with$summary,
  SF2_without$summary
)

Final_with_for_OLY <- predict_f426_one_game(
  n = N, seed = Seed,
  model = fit,
  with_home_effect = "OLY" %in% SF_with_for_OLY$Winner,
  game_name = "Final",
  team_A = SF1_with$summary$Winner,
  team_B = SF2_without$summary$Winner
)
f4_with_for_OLY <- bind_rows(
  SF_with_for_OLY,
  Final_with_for_OLY$summary
)

# __ Show predictions __________________________________________________________
cat(
  "Predictions for the Final Four :\n",
  "Without considering the home effect for Olympiacos :\n"
)
print(f4_without)
cat("Considering the home effect for Olympiacos :\n")
print(f4_with_for_OLY)

# __ Check predictions coherence _______________________________________________
check_data <- left_join(
  train |>
    group_by(team = team_home) |>
    summarise(mean_diff_home = mean(score_diff)),
  train |>
    group_by(team = team_away) |>
    summarise(mean_diff_away = mean(-score_diff)),
  by = "team"
) |>
  mutate(mean_diff = (mean_diff_home + mean_diff_away) / 2) |>
  arrange(desc(mean_diff))

# __ Heatmaps __________________________________________________________________
plot_SF_heatmaps <- function(SF1_without, SF2_without, SF1_with) {
  
  get_max_prob <- function(mc) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      pull(prob) |>
      max()
  }
  
  max_prob <- max(
    get_max_prob(SF1_without$MC),
    get_max_prob(SF2_without$MC),
    get_max_prob(SF1_with$MC)
  )
  
  plot_MC_heatmap <- function(mc, team_A, team_B, title, pred_diff) {
    tibble(diff = round(mc)) |>
      filter(diff >= -15, diff <= 15) |>
      count(diff) |>
      mutate(prob = n / sum(n)) |>
      ggplot(aes(x = diff, y = 1, fill = prob)) +
      geom_tile() +
      scale_fill_gradient(
        low = "white",
        high = "darkblue",
        limits = c(0, max_prob)
      ) +
      scale_x_continuous(breaks = seq(-15, 15, by = 5)) +
      labs(
        title = title,
        x = paste0("Score difference (", team_A, " - ", team_B, ")"),
        y = NULL,
        fill = "Probability"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) +
      geom_text(
        aes(
          label = ifelse(
            prob == max(prob),
            paste0(diff, "\n(", round(prob*100, 1), "%)"),
            ""
          )
        ),
        color = "white", size = 3
      ) +
      geom_vline(
        aes(xintercept = round(pred_diff), color = "Predicted diff"),
        linetype = "dashed"
      ) +
      scale_color_manual(values = c("Predicted diff" = "red"), name = NULL)
  }
  
  p1 <- plot_MC_heatmap(
    mc = SF1_without$MC,
    team_A = SF1_without$summary$Team_A,
    team_B = SF1_without$summary$Team_B,
    title = "SF1 — Without home effect",
    pred_diff = SF1_without$summary$Score_diff_pred
  )
  p2 <- plot_MC_heatmap(
    mc = SF2_without$MC,
    team_A = SF2_without$summary$Team_A,
    team_B = SF2_without$summary$Team_B,
    title = "SF2 — Without home effect",
    pred_diff = SF2_without$summary$Score_diff_pred
  )
  p3 <- plot_MC_heatmap(
    mc = SF1_with$MC,
    team_A = SF1_with$summary$Team_A,
    team_B = SF1_with$summary$Team_B,
    title = "SF1 — With home effect for OLY",
    pred_diff = SF1_with$summary$Score_diff_pred
  )
  
  (p1 + p3) / p2 +
    plot_layout(guides = "collect")
}
plot_SF_heatmaps(SF1_without, SF2_without, SF1_with)

ggsave(
  "outputs/heatmaps_SF.png",
  plot = plot_SF_heatmaps(SF1_without, SF2_without, SF1_with_for_OLY),
  width = 12, height = 6, dpi = 300
)


# __ Clean environment _________________________________________________________
to_keep <- c(
  "games", "team_codes", "all_teams",
  "train_reg26", "po26",
  "train", "fit",
  "SF1_without", "SF1_with", "SF2_without", "SF_without", "SF_with_for_OLY",
  "check_data")
rm(list = setdiff(ls(), to_keep))
