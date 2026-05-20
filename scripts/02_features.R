# __ Team name harmonization ___________________________________________________
create_team_codes <- function () {
  team_codes <- list(
    `21` = c(
      "LDLC ASVEL"                = "ASV",
      "Bayern München"            = "BAY",
      "Alba Berlin"               = "BER",
      "TD Systems Baskonia"       = "BKN",
      "Crvena zvezda mts"         = "CZV",
      "Anadolu Efes"              = "EFS",
      "Fenerbahçe Beko"           = "FBB",
      "Barcelona"                 = "FCB",
      "AX Armani Exchange Milan"  = "MIL",
      "Maccabi Playtika Tel Aviv" = "MTA",
      "Olympiacos"                = "OLY",
      "Panathinaikos OPAP"        = "PAO",
      "Real Madrid"               = "RMB",
      "Valencia Basket"           = "VBC",
      "Žalgiris"                  = "ZAL"
    ),
    
    `22` = c(
      "LDLC ASVEL"                = "ASV",
      "Bayern München"            = "BAY",
      "Alba Berlin"               = "BER",
      "Bitci Baskonia"            = "BKN",
      "Crvena zvezda mts"         = "CZV",
      "Anadolu Efes"              = "EFS",
      "Fenerbahçe Beko"           = "FBB",
      "Barcelona"                 = "FCB",
      "AIX Armani Exchange Milan" = "MIL",
      "AS Monaco"                 = "MON",
      "Maccabi Tel Aviv"          = "MTA",
      "Olympiacos"                = "OLY",
      "Panathinaikos OPAP"        = "PAO",
      "Real Madrid"               = "RMB",
      "Žalgiris"                  = "ZAL"
    ),
    
    
    `23` = c(
      "LDLC ASVEL"               = "ASV",
      "Bayern München"           = "BAY",
      "Alba Berlin"              = "BER",
      "Cazoo Baskonia"           = "BKN",
      "Crvena zvezda mts"        = "CZV",
      "Anadolu Efes"             = "EFS",
      "Fenerbahçe Beko"          = "FBB",
      "Barcelona"                    = "FCB",
      "EA7 Emporio Armani Milan" = "MIL",
      "AS Monaco"                = "MON",
      "Maccabi Tel Aviv"         = "MTA",
      "Olympiacos"               = "OLY",
      "Panathinaikos"            = "PAO",
      "Partizan Mozzart Bet"     = "PAR",
      "Real Madrid"              = "RMB",
      "Valencia Basket"          = "VBC",
      "Virtus Segafredo Bologna" = "VIR",
      "Žalgiris"                 = "ZAL"
    ),
    
    `24` = c(
      "LDLC ASVEL"                = "ASV",
      "Bayern München"            = "BAY",
      "ALBA Berlin"               = "BER",
      "Baskonia"                  = "BKN",
      "Crvena zvezda Meridianbet" = "CZV",
      "Anadolu Efes"              = "EFS",
      "Fenerbahçe Beko"           = "FBB",
      "Barcelona"                 = "FCB",
      "EA7 Emporio Armani Milano" = "MIL",
      "AS Monaco"                 = "MON",
      "Maccabi Playtika Tel Aviv" = "MTA",
      "Olympiacos"                = "OLY",
      "Panathinaikos"             = "PAO",
      "Partizan Mozzart Bet"      = "PAR",
      "Real Madrid"               = "RMB",
      "Valencia Basket"           = "VBC",
      "Virtus Segafredo Bologna"  = "VIR",
      "Žalgiris"                  = "ZAL"
    ),
    
    `25` = c(
      "LDLC ASVEL"                = "ASV",
      "Bayern München"            = "BAY",
      "ALBA Berlin"               = "BER",
      "Baskonia"                  = "BKN",
      "Crvena zvezda Meridianbet" = "CZV",
      "Anadolu Efes"              = "EFS",
      "Fenerbahçe Beko"           = "FBB",
      "Barcelona"                 = "FCB",
      "EA7 Emporio Armani Milano" = "MIL",
      "AS Monaco"                 = "MON",
      "Maccabi Playtika Tel Aviv" = "MTA",
      "Olympiacos"                = "OLY",
      "Panathinaikos AKTOR"       = "PAO",
      "Partizan Mozzart Bet"      = "PAR",
      "Paris Basketball"          = "PBB",
      "Real Madrid"               = "RMB",
      "Virtus Segafredo Bologna"  = "VIR",
      "Žalgiris"                  = "ZAL"
    ),
    
    `26` = c(
      "LDLC ASVEL"                = "ASV",
      "Bayern München"            = "BAY",
      "Baskonia"                  = "BKN",
      "Crvena zvezda Meridianbet" = "CZV",
      "Dubai"                     = "DUB",
      "Anadolu Efes"              = "EFS",
      "Fenerbahçe Beko"           = "FBB",
      "Barcelona"                 = "FCB",
      "Hapoel IBI Tel Aviv"       = "HTA",
      "EA7 Emporio Armani Milano" = "MIL",
      "AS Monaco"                 = "MON",
      "Maccabi Rapyd Tel Aviv"    = "MTA",
      "Olympiacos"                = "OLY",
      "Panathinaikos AKTOR"       = "PAO",
      "Partizan Mozzart Bet"      = "PAR",
      "Paris Basketball"          = "PBB",
      "Real Madrid"               = "RMB",
      "Valencia Basket"           = "VBC",
      "Virtus Olidata Bologna"    = "VIR",
      "Žalgiris"                  = "ZAL"
    )
  )
}
team_codes <- create_team_codes()
all_teams <- unique(unlist(team_codes))

# __ Processed data ____________________________________________________________
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
      playoff = ifelse(type == "regular", 0, 1)
    )
}

games <- imap(games, function(type_list, type) {
  imap(type_list, function(df, season) {
    clean_matches(df, type, season)
  })
})

# __ Train & validation data ___________________________________________________
train_reg25 <- games$regular$`25`

val_po25 <- games$playoffs$`25` |>
  arrange(date)
val_reg26 <- games$regular$`26` |>
  arrange(date)
val_po26 <- games$playoffs$`26`|>
  arrange(date)

# __ Previous seasons effect ___________________________________________________
create_total_diff <- function (season, phase) {
  
  poids <- c(0.70, 0.15, 0.10, 0.05)
  seasons <- if (phase == "reg") {
    as.character(as.numeric(season) - 1:4)
  } else {
    as.character(as.numeric(season) - 0:3)
  }
  weights <- setNames(poids, seasons)
  df <- NULL
  
  for (s in seasons) {
    if (s == seasons[1]) {
      df <- bind_rows(
        df,
        games$regular[[s]] |>
          mutate(season = s, type = "reg")
      )
    } else {
      df <- bind_rows(
        df,
        games$regular[[s]] |>
          mutate(season = s, type = "reg"),
        games$playoffs[[s]] |>
          mutate(season = s, type = "po")
      )
    }
  }
  home <- df |>
    select(season, team = team_home, pts_team = pts_home, pts_opp = pts_away)
  away <- df |>
    select(season, team = team_away, pts_team = pts_away, pts_opp = pts_home)
  df <- bind_rows(home, away) |>
    group_by(team, season) |>
    summarise(total_diff = sum(pts_team - pts_opp), .groups = "drop") |>
    mutate(weight = weights[season]) |>
    group_by(team) |>
    summarise(total_diff_weighted = sum(total_diff * weight)) |>
    filter(team %in% all_teams)
  return(df)
}
total_diff <- list(
  "reg25"   = create_total_diff("25", "reg"),
  "po25" = create_total_diff("25", "po"),
  "reg26"   = create_total_diff("26", "reg"),
  "po26" = create_total_diff("26", "po")
)

join_total_diff <- function (train_reg25, val_po25, val_reg26, val_po26) {
  
  for (s in c("reg25", "po25", "reg26", "po26")) {
    
    total_diff_home <- total_diff[[s]] |>
      rename(total_diff_home = total_diff_weighted)
    total_diff_away <- total_diff[[s]] |>
      rename(total_diff_away = total_diff_weighted)
    
    switch (s,
      "reg25" = train_reg25 <- train_reg25 |>
        left_join(total_diff_home, by = c("team_home" = "team")) |>
        left_join(total_diff_away, by = c("team_away" = "team")) |>
        mutate(across(c(total_diff_home, total_diff_away), ~ replace_na(.x, 0))),
      "po25" = val_po25 <- val_po25 |>
        left_join(total_diff_home, by = c("team_home" = "team")) |>
        left_join(total_diff_away, by = c("team_away" = "team")) |>
        mutate(across(c(total_diff_home, total_diff_away), ~ replace_na(.x, 0))),
      "reg26" = val_reg26 <- val_reg26 |>
        left_join(total_diff_home, by = c("team_home" = "team")) |>
        left_join(total_diff_away, by = c("team_away" = "team")) |>
        mutate(across(c(total_diff_home, total_diff_away), ~ replace_na(.x, 0))),
      "po26" = val_po26 <- val_po26 |>
        left_join(total_diff_home, by = c("team_home" = "team")) |>
        left_join(total_diff_away, by = c("team_away" = "team")) |>
        mutate(across(c(total_diff_home, total_diff_away), ~ replace_na(.x, 0)))
    )
  }
  return(list(
    train = train_reg25,
    val = bind_rows(val_po25, val_reg26, val_po26) |>
      arrange(date)
  ))
}
joined_games <- join_total_diff(train_reg25, val_po25, val_reg26, val_po26)
train <- joined_games$train
val <- joined_games$val