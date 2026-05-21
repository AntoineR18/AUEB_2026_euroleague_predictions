# 02_features_new.R

# __ Define train & validation data ____________________________________________

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

# __ Clean environment _________________________________________________________
to_keep <- c(
  "games", "team_codes", "all_teams",
  "train_reg26", "po26", "choose_weights", "total_diff")
rm(list = setdiff(ls(), to_keep))
