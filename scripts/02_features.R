# __ Train & validation data ___________________________________________________
clean_matches <- function(df, type, season) {
  
  Sys.setlocale("LC_TIME", "C")
  
  df_clean <- df |>
    rename(
      date = Date,
      team_home = Opp,
      pts_home = `PTS...5`,
      team_away = Team,
      pts_away = `PTS...3`
    ) |>
    filter(!is.na(pts_home)) |>
    mutate(
      date = trimws(date),
      date = as.Date(date, "%a %b %d %Y"),
      team_home = recode(team_home, !!!team_codes[[season]]),
      team_away = recode(team_away, !!!team_codes[[season]]),
      score_diff = pts_home - pts_away,
      win = ifelse(score_diff > 0, 1, 0),
      playoff = ifelse(type == "reg", 0, 1)
    )
}

games <- imap(games, function(type_list, type) {
  imap(type_list, function(df, season) {
    clean_matches(df, type, season)
  })
})

train <- clean_matches(games$regular$`25`, "reg", "25")

val <- bind_rows(
  clean_matches(games$playoffs$`25`, "po", "25"),
  clean_matches(games$regular$`26`, "reg", "26")
) |>
  arrange(date)

# __ Test data _________________________________________________________________
matches_po26 <- list(
  QF1 = c("OLY", "MON"),
  QF2 = c("VBC", "PAO"),
  QF3 = c("RMB", "HTA"),
  QF4 = c("FBB", "ZAL")
)

playoffs26_pred <- expand.grid(
  serie = names(matches_po26),
  match_number = 1:5
) |>
  transform(
    match_played = FALSE,
    team_A = sapply(serie, function(s) matches_po26[[s]][1]),
    team_B = sapply(serie, function(s) matches_po26[[s]][2]),
    score_diff = NA_integer_,
    win_A = 0L,
    win_B = 0L
  )

create_true_po26 <- function (df = playoffs26_pred) {
  
  score_diffs <- list(
    "QF1" = c(21, 30, 23),
    "QF2" = c(-1, -2, 4, 3, 17),
    "QF3" = c(4, 27, -7, 6),
    "QF4" = c(11, 12, -3, 4)
  )
  for (s in unique(df$serie)) {
    for (i in 1:5) {
      
      score_diff <- score_diffs[[s]][i]
      
      if (!is.na(score_diff)) {
        df[df$serie == s & df$match_number == i, "match_played"] <- TRUE
        df[df$serie == s & df$match_number == i, "score_diff"] <- score_diff
        if (score_diff > 0) {
          df[df$serie == s & df$match_number == i, "win_A"] <-
            max(df[df$serie == s, "win_A"]) + 1 
        } else {
          df[df$serie == s & df$match_number == i, "win_B"] <-
            max(df[df$serie == s, "win_B"]) + 1 
        }
      }
    }
  }
  df <- df |> 
    filter(match_played) |>
    select(!match_played)
  return(df)
}

playoffs26_true <- create_true_po26()

# __ Records ___________________________________________________________________
create_records <- function (df = standings) {
  
  records <- list()
  
  for (i in 21:26) {
    
    records[[paste0(i)]] <- standings[[paste0(i)]] |>
      rename(team = Team, record = `W/L%`) |>
      select(team, record) |>
      mutate(team = str_trim(str_remove(team, "\\*")))
  }
  
  records <- records |>
    imap(
      ~ .x |>
        mutate(team = recode(team, !!!team_codes[[.y]])) |>
        filter(team %in% all_teams) |>
        arrange(team)
    )
  return(records)
}

records <- create_records()
  
# __ Weighted historical form score ____________________________________________
weights25 <- c("24" = 0.70, "23" = 0.15, "22" = 0.10, "21" = 0.05)
weights26 <- c("25" = 0.70, "24" = 0.15, "23" = 0.10, "22" = 0.05)
weights_po26 <- c("26" = 0.70, "25" = 0.15, "24" = 0.10, "23" = 0.05)

record_hist <- bind_rows(
  records$`26` |> mutate(season = "26"),
  records$`25` |> mutate(season = "25"),
  records$`24` |> mutate(season = "24"),
  records$`23` |> mutate(season = "23"),
  records$`22` |> mutate(season = "22"),
  records$`21` |> mutate(season = "21")
) |>
  filter(team %in% all_teams) |>
  mutate(
    weight25 = weights25[season],
    weight26 = weights26[season],
    weight_po26 = weights_po26[season],
    score_diff = sum(games$regular[[paste0(season)]])
  ) |>
  group_by(team) |>
  summarise(
    form25 = sum(
      record * weight25, na.rm = TRUE) /
      sum(weight25, na.rm = TRUE),
    form26 = sum(
      record * weight26, na.rm = TRUE) /
      sum(weight26, na.rm = TRUE)
  ) |>
  mutate(
    across(c(form25, form26), ~ replace_na(.x, mean(.x, na.rm = TRUE)))
  )

