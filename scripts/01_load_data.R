# 01_load_data.R

# __ Load raw data _____________________________________________________________
load_data <- function (phase, season) {
  
  path <-  file.path(
    "raw_data", phase,
    paste0("eul", "_", season, "_", phase, ".csv")
  )
  read_csv(path)
}

load_games <- function () {
  
  reg <- list()
  po <- list()
  
  for (i in 21:26) {
    reg[[paste0(i)]] <- load_data("reg", paste0(i-1, "-", i))
    po[[paste0(i)]] <- load_data("po", paste0(i-1, "-", i))
  }
  return(list(
    regular = reg,
    playoffs = po
  ))
}
games <- load_games()

# __ Harmonize team names ______________________________________________________
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
      "Barcelona"                = "FCB",
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
}
team_codes <- create_team_codes()
all_teams <- unique(unlist(team_codes))

# __ Preprocess data ___________________________________________________________
clean_games <- function(df, phase, season) {

  Sys.setlocale("LC_TIME", "C")

  df_clean <- df |>

    # 1. Rename existing columns
    rename(
      date = Date,
      team_home = Opp,
      pts_home = `PTS...5`,
      team_away = Team,
      pts_away = `PTS...3`
    ) |>

    # 2. Delete postponed & canceled games
    filter(!is.na(pts_home)) |>

    # 3. update existing variables and create new ones
    mutate(

      date = as.Date(date, "%a %b %d %Y"),
      team_home = recode(team_home, !!!team_codes[[season]]),
      team_away = recode(team_away, !!!team_codes[[season]]),

      score_diff = pts_home - pts_away,
      playoff = phase == "playoffs",
      serie = NA_character_,
      wins_A = 0,
      wins_B = 0,
      final_four = FALSE
    )

  # 4. Rearrange columns
  df_clean <- df_clean[c(
    "date",
    "playoff", "final_four", "serie",
    "team_home", "pts_home", "team_away", "pts_away",
    "score_diff",
    "wins_A", "wins_B"
  )]

  return(df_clean)
}
games <- imap(games, function(phase_list, phase) {
  imap(phase_list, function(df, season) {
    clean_games(df, phase, season)
  })
})

verify_codes <- function(df, season) {

  codes <- unname(unlist(team_codes[[season]]))

  list(
    unknown_home = setdiff(unique(df$team_home), codes),
    unknown_away = setdiff(unique(df$team_away), codes)
  )
}
imap(games, function(phase_list, phase) {
  imap(phase_list, function(df, season) {
    verify_codes(df, season)
  })
})

precise_po_f4 <- function (df) {

  n <- nrow(df)

  # 1. Check final four existence
  last4 <- df[(n-3):n, ]
  n_teams <- length(unique(c(last4$team_home, last4$team_away)))
  is_f4 <- n_teams == 4

  # 2. Detail playoffs
  po <- if (is_f4) {
    df[1:(n-4), ]
  } else {
    df
  } |>
    mutate(final_four = FALSE) |>

    # 2.1. Group by series
    mutate(
      serie = paste(
        pmin(team_home, team_away),
        pmax(team_home, team_away),
        sep = "-"
      )
    ) |>
    group_by(serie) |>

    # 2.2. Compute wins before game
    mutate(
      wins_A = lag(
        cumsum(
          team_home == pmin(team_home, team_away) & score_diff > 0 |
            team_away == pmin(team_home, team_away) & score_diff < 0
        ),
        default = 0
      ),
      wins_B = lag(
        cumsum(
          team_home == pmax(team_home, team_away) & score_diff > 0 |
            team_away == pmax(team_home, team_away) & score_diff < 0
        ),
        default = 0
      )
    ) |>

    # 2.3. Ungroup
    ungroup()


  # 3. Manage final four
  f4 <- if (is_f4) {
    df[(n-3):n, ] |>
      mutate(
        final_four = TRUE,
        wins_A = 0L,
        wins_B = 0L)
  } else {
    tibble()
  }

  return(bind_rows(po, f4))
}
games$playoffs <- imap(games$playoffs, function(df, season) {
  precise_po_f4(df)
})

# __ Clean environment _________________________________________________________
to_keep <- c("games", "team_codes", "all_teams")
rm(list = setdiff(ls(), to_keep))
