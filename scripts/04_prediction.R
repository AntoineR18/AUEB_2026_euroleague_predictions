# __ Matchups __________________________________________________________________
matchups <- tibble(
  team_A = c("OLY", "VBC", "RMB", "FBB"),
  team_B = c("MON", "PAO", "HTA", "ZAL")
)

# __ Neutral court probability _________________________________________________
neutral_prob <- function(team_A, team_B, model) {
  
  # A at home
  m1 <- prepare_lm_data(
    tibble(team_home = team_A, team_away = team_B, playoff = 1, 
           score_diff = 0, win = 1), "OLY")
  p1 <- predict(model, newdata = m1, type = "response")
  
  # B at home
  m2 <- prepare_lm_data(
    tibble(team_home = team_B, team_away = team_A, playoff = 1,
           score_diff = 0, win = 1), "OLY")
  p2 <- predict(model, newdata = m2, type = "response")
  
  # Average to cancel home effect
  (p1 + (1 - p2)) / 2
}

# __ BO5 simulation ____________________________________________________________
simulate_bo5 <- function(p_A_home, p_A_away, n_sim = 10000) {
  # Match order : A, A, B, B, A
  home_sequence <- c("A", "A", "B", "B", "A")
  
  wins_A <- integer(n_sim)
  for (s in 1:n_sim) {
    score_A <- 0
    score_B <- 0
    game <- 1
    while (score_A < 3 && score_B < 3) {
      p <- ifelse(home_sequence[game] == "A", p_A_home, p_A_away)
      if (runif(1) < p) score_A <- score_A + 1
      else score_B <- score_B + 1
      game <- game + 1
    }
    wins_A[s] <- score_A == 3
  }
  mean(wins_A)
}

# __ Results ___________________________________________________________________
set.seed(1807)
results_sim <- matchups |>
  rowwise() |>
  mutate(
    p_A_home = as.numeric(predict(fit_logit, newdata = prepare_lm_data(
      tibble(team_home = team_A, team_away = team_B, 
             playoff = 1, score_diff = 0, win = 1), "OLY"),
      type = "response")),
    p_A_away = as.numeric(predict(fit_logit, newdata = prepare_lm_data(
      tibble(team_home = team_B, team_away = team_A,
             playoff = 1, score_diff = 0, win = 1), "OLY"),
      type = "response")),
    p_A_away = 1 - p_A_away,
    prob_A   = simulate_bo5(p_A_home, p_A_away)
  ) |>
  ungroup() |>
  mutate(prob_B = 1 - prob_A)

print(results_sim)
