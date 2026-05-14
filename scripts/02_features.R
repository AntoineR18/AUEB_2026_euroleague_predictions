# __ Regular & playoffs ________________________________________________________
clean_matches <- function(df) {
  df_clean <- df |>
    rename(date = Date,
           team_home = Team, pts_home = `PTS...3`,
           team_away = Opp, pts_away = `PTS...5`) |>
    filter(!is.na(pts_home)) |>
    mutate(score_diff = pts_home - pts_away,
           date = trimws(date),
           date = as.Date(date, "%a %b %d %Y"))
}

regular25 <- clean_matches(regular25)
regular26 <- clean_matches(regular26)
playoffs25 <- clean_matches(playoffs25)


# __ Records ___________________________________________________________________
records <- list()
for (i in 21:26) {
  records[[paste0(i)]] <- standings[[paste0(i)]] |>
    rename(team = Team, record = `W/L%`) |>
    select(team, record) |>
    mutate(team = str_trim(str_remove(team, "\\*")))
}
rm(i)

# __ Team name harmonization ___________________________________________________
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
    "Barça"                    = "FCB",
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

records <- imap(
  records,
  ~ .x |>
    mutate(team = recode(team, !!!team_codes[[.y]])) |>
    filter(team %in% teams) |>
    arrange(team)
  )

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