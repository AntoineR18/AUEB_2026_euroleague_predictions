# __ Adapted dataset ___________________________________________________________
prepare_OLS_records <- function (
    df = train,
    team_ref = "OLY",
    records = record_hist
) {
  df |>
    mutate(
      team_home = factor(team_home, levels = all_teams),
      team_away = factor(team_away, levels = all_teams),
      team_home = relevel(team_home, ref = team_ref),
      team_away = relevel(team_away, ref = team_ref),
      record_home = ,
      record_away = 
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
  score_diff ~ team_home + team_away + playoff + ,
  data = train_OLS
)

summary(fit_OLS)

fit_OLS_initial <- fit_OLS