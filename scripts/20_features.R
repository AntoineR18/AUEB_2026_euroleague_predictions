# 020_features.R

# __ Clean environment _________________________________________________________
to_keep <- c("raw_games", "games", "all_teams")
rm(list = setdiff(ls(), to_keep))

# __ Define train & validation data ____________________________________________

train_reg25 <- games$regular$`25`|>
  arrange(date)
train_po25 <- games$playoffs$`25`|>
  arrange(date)
train_reg26 <- games$regular$`26` |>
  arrange(date)
po26 <- games$playoffs$`26`|>
  arrange(date)

# __ Add previous seasons effect _______________________________________________
choose_weights <- c(0.70, 0.15, 0.10, 0.05)

create_td <- function (
    chosen_weights = choose_weights, 
    season = "26"
) {
  
  # 1. Create weighted_seasons
  seasons <- as.character(as.numeric(season) - 1:4)
  weighted_seasons <- setNames(chosen_weights, seasons)
  
  # 2. Initialize df
  df <- tibble()
  for (s in seasons) {
    df <- bind_rows(
      df,
      games$regular[[s]] |>
        mutate(season = s, phase = "regular"),
      games$playoffs[[s]] |>
        mutate(season = s, phase = "playoffs")
    )
  }
  df <- df |>
    arrange(date)
  
  # 3. Create home & away
  home <- df |>
    select(
      season,
      team = team_home,
      pts_team = pts_home,
      pts_opp = pts_away
    )
  away <- df |>
    select(
      season,
      team = team_away,
      pts_team = pts_away,
      pts_opp = pts_home
    )
  
  # 4. Compute total score difference
  df <- bind_rows(home, away) |>
    
    # 4.1. Compute on each season
    group_by(team, season) |>
    summarise(
      total_diff = sum(pts_team - pts_opp),
      .groups = "drop"
    ) |>
    
    # 4.2. Compute weighted season
    mutate(weight = weighted_seasons[season]) |>
    group_by(team) |>
    summarise(total_diff_weighted = sum(total_diff * weight)) |>
    
    # 4.3. Delete useless teams
    filter(team %in% all_teams)
  
  return(df)
}
total_diff <- create_td()

join_td <- function(df, td) {
  
  total_diff_home <- td |>
    rename(
      total_diff_home = total_diff_weighted
    )
  
  total_diff_away <- td |>
    rename(
      total_diff_away = total_diff_weighted
    )
  
  df  <- df |>
    left_join(
      total_diff_home,
      by = c("team_home" = "team")
    ) |>
    left_join(
      total_diff_away,
      by = c("team_away" = "team")
    ) |>
    mutate(
      across(
        c(total_diff_home, total_diff_away),
        ~ replace_na(.x, 0)
      )
    )
  return(df)
}
train_reg26 <- join_td(train_reg26, total_diff)
po26 <- join_td(po26, total_diff)

# __ Set up random framework ___________________________________________________
N <- 10000
Seed <- 1807

# __ Useful functions __________________________________________________________
prepare_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}

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
