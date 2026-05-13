library(readr)
library(tidyverse)

rm(list = ls())

# ── Regular season data 24-25 ─────────────────────────────────────────────────

reg25 <- read_csv("data/raw/eul/reg/eul_24-25_reg.csv") |>
  rename(team_home = Team, pts_home = `PTS...3`,
         team_away = Opp,  pts_away = `PTS...5`) |>
  filter(!is.na(pts_home)) |>
  mutate(score_diff = pts_home - pts_away)

# ── Team dictionary ───────────────────────────────────────────────────────────

teams_map <- c(
  "BER" = "ALBA Berlin",
  "EFS" = "Anadolu Efes",
  "FCB" = "Barcelona",
  "BKN" = "Baskonia",
  "BAY" = "Bayern München",
  "CZV" = "Crvena zvezda Meridianbet",
  "MIL" = "EA7 Emporio Armani Milano",
  "FBB" = "Fenerbahçe Beko",
  "ASV" = "LDLC ASVEL",
  "MTA" = "Maccabi Playtika Tel Aviv",
  "MON" = "AS Monaco",
  "OLY" = "Olympiacos",
  "PAO" = "Panathinaikos AKTOR",
  "PBB" = "Paris Basketball",
  "PAR" = "Partizan Mozzart Bet",
  "RMB" = "Real Madrid",
  "VIR" = "Virtus Segafredo Bologna",
  "ZAL" = "Žalgiris"
)

teams25 <- names(teams_map)

# ── Loading standings ─────────────────────────────────────────────────────────

load_record <- function(path) {
  read_csv(path, skip = 1) |>
    select(`...1`, `W/L%`) |>
    rename(team = `...1`, record = `W/L%`) |>
    mutate(team = str_trim(str_remove(team, "\\*")))
}

record22 <- load_record("data/raw/eul/standings/eul_21-22_standings.csv")
record23 <- load_record("data/raw/eul/standings/eul_22-23_standings.csv")
record24 <- load_record("data/raw/eul/standings/eul_23-24_standings.csv")
record25 <- load_record("data/raw/eul/standings/eul_24-25_standings.csv")

# ── Team name harmonization ───────────────────────────────────────────────────

record22 <- record22 |>
  mutate(team = recode(team,
                       "Barcelona"                 = "FCB",
                       "Olympiacos"                = "OLY",
                       "AIX Armani Exchange Milan" = "MIL",
                       "Real Madrid"               = "RMB",
                       "Maccabi Tel Aviv"          = "MTA",
                       "Anadolu Efes"              = "EFS",
                       "AS Monaco"                 = "MON",
                       "Bayern München"            = "BAY",
                       "Bitci Baskonia"            = "BKN",
                       "Alba Berlin"               = "BER",
                       "Crvena zvezda mts"         = "CZV",
                       "Fenerbahçe Beko"           = "FBB",
                       "Panathinaikos OPAP"        = "PAO",
                       "LDLC ASVEL"                = "ASV",
                       "Žalgiris"                  = "ZAL"
  ))

record23 <- record23 |>
  mutate(team = recode(team,
                       "Olympiacos"               = "OLY",
                       "Barça"                    = "FCB",
                       "Real Madrid"              = "RMB",
                       "AS Monaco"                = "MON",
                       "Maccabi Tel Aviv"         = "MTA",
                       "Partizan Mozzart Bet"     = "PAR",
                       "Žalgiris"                 = "ZAL",
                       "Fenerbahçe Beko"          = "FBB",
                       "Cazoo Baskonia"           = "BKN",
                       "Crvena zvezda mts"        = "CZV",
                       "Anadolu Efes"             = "EFS",
                       "EA7 Emporio Armani Milan" = "MIL",
                       "Virtus Segafredo Bologna" = "VIR",
                       "Bayern München"           = "BAY",
                       "Alba Berlin"              = "BER",
                       "Panathinaikos"            = "PAO",
                       "LDLC ASVEL"               = "ASV"
  ))

record24 <- record24 |>
  mutate(team = recode(team,
                       "Real Madrid"               = "RMB",
                       "Panathinaikos"             = "PAO",
                       "AS Monaco"                 = "MON",
                       "Barcelona"                 = "FCB",
                       "Olympiacos"                = "OLY",
                       "Fenerbahçe Beko"           = "FBB",
                       "Maccabi Playtika Tel Aviv" = "MTA",
                       "Baskonia"                  = "BKN",
                       "Anadolu Efes"              = "EFS",
                       "Virtus Segafredo Bologna"  = "VIR",
                       "Partizan Mozzart Bet"      = "PAR",
                       "EA7 Emporio Armani Milano" = "MIL",
                       "Žalgiris"                  = "ZAL",
                       "Bayern München"            = "BAY",
                       "Crvena zvezda Meridianbet" = "CZV",
                       "LDLC ASVEL"                = "ASV",
                       "ALBA Berlin"               = "BER"
  ))

record25 <- record25 |>
  mutate(team = recode(team,
                       "ALBA Berlin"               = "BER",
                       "Anadolu Efes"              = "EFS",
                       "Barcelona"                 = "FCB",
                       "Baskonia"                  = "BKN",
                       "Bayern München"            = "BAY",
                       "Crvena zvezda Meridianbet" = "CZV",
                       "EA7 Emporio Armani Milano" = "MIL",
                       "Fenerbahçe Beko"           = "FBB",
                       "LDLC ASVEL"                = "ASV",
                       "Maccabi Playtika Tel Aviv" = "MTA",
                       "AS Monaco"                 = "MON",
                       "Olympiacos"                = "OLY",
                       "Panathinaikos AKTOR"       = "PAO",
                       "Paris Basketball"          = "PBB",
                       "Partizan Mozzart Bet"      = "PAR",
                       "Real Madrid"               = "RMB",
                       "Virtus Segafredo Bologna"  = "VIR",
                       "Žalgiris"                  = "ZAL"
  ))

# ── Weighted historical form score ────────────────────────────────────────────

weights <- c("25" = 0.70, "24" = 0.15, "23" = 0.10, "22" = 0.05)

record_hist <- bind_rows(
  record25 |> mutate(season = "25"),
  record24 |> mutate(season = "24"),
  record23 |> mutate(season = "23"),
  record22 |> mutate(season = "22")
) |>
  filter(team %in% teams25) |>
  mutate(weight = weights[season]) |>
  group_by(team) |>
  summarise(form = sum(record * weight) / sum(weight)) |>
  arrange(desc(form))