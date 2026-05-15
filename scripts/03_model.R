# __ Adapted dataset ___________________________________________________________
prepare_lm_data <- function (df, team_ref) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref)
    )
}

train_reg25 <- prepare_lm_data(train_reg25, "OLY")

# __ Initial training __________________________________________________________
fit <- lm(score_diff ~ team_home + team_away + playoff, data = train_reg25)
summary(fit)


# # ── Design matrix ─────────────────────────────────────────────────────────────
# n <- nrow(df_combined)
# d <- length(teams25)
# 
# X <- matrix(0, n, d)
# colnames(X) <- teams25
# 
# for (i in 1:n) {
#   X[i, df_combined$team_home[i]] <-  1
#   X[i, df_combined$team_away[i]] <- -1
# }
# 
# X <- cbind(X, playoff = df_combined$playoff, form_diff = df_combined$form_diff)
# 
# # ── Model fitting ─────────────────────────────────────────────────────────────
# leader25 <- record25$team[1]
# 
# df_model <- as.data.frame(X[, -which(colnames(X) == leader25)])
# df_model$score_diff <- df_combined$score_diff
# 
# fit <- lm(score_diff ~ . - 1, data = df_model)
# summary(fit)
# 
# # ── Prediction function ───────────────────────────────────────────────────────
# predict_score_diff <- function(team_home, team_away, fit, is_playoff = 1) {
#   alpha <- coef(fit)
#   alpha_home <- ifelse(team_home %in% names(alpha), alpha[team_home], 0)
#   alpha_away <- ifelse(team_away %in% names(alpha), alpha[team_away], 0)
#   playoff_eff <- alpha["playoff"] * is_playoff
#   
#   unname(alpha_home - alpha_away + playoff_eff)
# }