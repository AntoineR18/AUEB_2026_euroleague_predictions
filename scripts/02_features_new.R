# 02_features_new.R

# __ Define train & validation data ____________________________________________
train_reg25 <- games$regular$`25`

val_po25 <- games$playoffs$`25` |>
  arrange(date)
val_reg26 <- games$regular$`26` |>
  arrange(date)
val_po26 <- games$playoffs$`26`|>
  arrange(date)

# __ Add previous seasons effect _______________________________________________
choose_weights <- c(0.70, 0.15, 0.10, 0.05)
create_total_diff <- function (chosen_weights = choose_weights) {
  
  create_td_single <- function (
    chosen_weights = choose_weights, 
    season,
    phase
  ) {
    
    # 1. Create weighted_seasons
    if (phase == "regular") {
      seasons <- as.character(as.numeric(season) - 1:4)
    } else {
      seasons <- as.character(as.numeric(season) - 0:3)
    }
    weighted_seasons <- setNames(chosen_weights, seasons)
    
    # 2. Initialize df
    df <- tibble()
    for (s in seasons) {
      if (phase == "playoffs" & s == seasons[1]) {
        df <- bind_rows(
          df,
          games$regular[[s]] |>
            mutate(season = s, phase = "regular")
        )
      } else {
        df <- bind_rows(
          df,
          games$regular[[s]] |>
            mutate(season = s, phase = "regular"),
          games$playoffs[[s]] |>
            mutate(season = s, phase = "playoffs")
        )
      }
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
  
  # 5. Generalize to every case
  total_diff <- list()
  for (p in c("regular", "playoffs")) {
    for (s in c("25", "26")) {
      total_diff[[paste0(p, s)]] <- create_td_single(season = s, phase = p)
    }
  }
  
  return(total_diff)
}
total_diff <- create_total_diff()

join_total_diff <- function (
    total_diff,
    train_reg25,
    val_po25,
    val_reg26,
    val_po26
) {
  
  datasets <- list(
    regular25 = train_reg25,
    playoffs25 = val_po25,
    regular26 = val_reg26,
    playoffs26 = val_po26
  )
  
  join_td_single <- function(df, td) {
    
    total_diff_home <- td |>
      rename(
        total_diff_home = total_diff_weighted
      )
    
    total_diff_away <- td |>
      rename(
        total_diff_away = total_diff_weighted
      )
    
    df |>
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
  }
  
  datasets <- imap(datasets, function(df, phase) {
    join_td_single(df, total_diff[[phase]])
  })

  return(
    list(
      train = datasets$regular25,
      val = bind_rows(
        datasets$playoffs25,
        datasets$regular26,
        datasets$playoffs26
      ) |>
        arrange(date)
    )
  )
}
joined_games <- join_total_diff(
  total_diff,
  train_reg25,
  val_po25,
  val_reg26,
  val_po26
)
train <- joined_games$train
val <- joined_games$val

# __ Clean environment _________________________________________________________
to_keep <- c(
  "games", "team_codes", "all_teams",
  "train", "val", "total_diff")
rm(list = setdiff(ls(), to_keep))
