# __ Train & validation data ___________________________________________________
clean_matches <- function(df, type, season) {
  Sys.setlocale("LC_TIME", "C")
  df_clean <- df |>
    rename(date = Date,
           team_home = Opp, pts_home = `PTS...5`,
           team_away = Team, pts_away = `PTS...3`) |>
    filter(!is.na(pts_home)) |>
    mutate(date = trimws(date),
           date = as.Date(date, "%a %b %d %Y"),
           team_home = recode(team_home, !!!team_codes[[season]]),
           team_away = recode(team_away, !!!team_codes[[season]]),
           score_diff = pts_home - pts_away,
           win = ifelse(score_diff > 0, 1, 0),
           playoff = ifelse(type == "reg", 0, 1)
           )
}

train <- clean_matches(regular25, "reg", "25")

val <- bind_rows(
  clean_matches(playoffs25, "po", "25"),
  clean_matches(regular26, "reg", "26")) |>
  arrange(date)

# __ Test data _________________________________________________________________
matches_po26 <- list(
  QF1 = c("OLY", "MON"),
  QF2 = c("VBC", "PAO"),
  QF3 = c("RMB", "HTA"),
  QF4 = c("FBB", "ZAL")
)

playoffs26 <- expand.grid(
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

# final_four <- 
  
# # __ Records ___________________________________________________________________
# records <- list()
# for (i in 21:26) {
#   records[[paste0(i)]] <- standings[[paste0(i)]] |>
#     rename(team = Team, record = `W/L%`) |>
#     select(team, record) |>
#     mutate(team = str_trim(str_remove(team, "\\*")))
# }
# rm(i)
# 
# records <- imap(
#   records,
#   ~ .x |>
#     mutate(team = recode(team, !!!team_codes[[.y]])) |>
#     filter(team %in% teams) |>
#     arrange(team)
#   )

# ── Weighted historical form score ────────────────────────────────────────────
# weights25 <- c("24" = 0.70, "23" = 0.15, "22" = 0.10, "21" = 0.05)
#
# record_hist <- bind_rows(
#   record24 |> mutate(season = "24"),
#   record23 |> mutate(season = "23"),
#   record22 |> mutate(season = "22"),
#   record21 |> mutate(season = "21")
# ) |>
#   filter(team %in% teams25) |>
#   mutate(weight = weights[season]) |>
#   group_by(team) |>
#   summarise(form = sum(record * weight) / sum(weight))
#
# record_hist <- record_hist |>
#   bind_rows(tibble(team = "PBB", form = mean(record_hist$form))) |>
#   arrange(desc(form))