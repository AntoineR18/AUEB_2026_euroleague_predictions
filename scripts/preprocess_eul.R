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

teams_map_inv <- setNames(teams25, teams_map)

# ── Loading standings ─────────────────────────────────────────────────────────

load_record <- function(path) {
  read_csv(path, skip = 1) |>
    select(`...1`, `W/L%`) |>
    rename(team = `...1`, record = `W/L%`) |>
    mutate(team = str_trim(str_remove(team, "\\*")))
}

record21 <- load_record("data/raw/eul/standings/eul_20-21_standings.csv")
record22 <- load_record("data/raw/eul/standings/eul_21-22_standings.csv")
record23 <- load_record("data/raw/eul/standings/eul_22-23_standings.csv")
record24 <- load_record("data/raw/eul/standings/eul_23-24_standings.csv")
record25 <- load_record("data/raw/eul/standings/eul_24-25_standings.csv")

# ── Team name harmonization ───────────────────────────────────────────────────

record21 <- record21 |>
  mutate(team = recode(team,
                       "Barcelona" = "FCB",
                       "Anadolu Efes" = "EFS",
                       "AX Armani Exchange Milan" = "MIL",
                       "Bayern München" = "BAY",
                       "Real Madrid" = "RMB",
                       "Fenerbahçe Beko" = "FBB",
                       "TD Systems Baskonia" = "BKN",
                       "Žalgiris" = "ZAL",
                       "Olympiacos" = "OLY",
                       "Maccabi Playtika Tel Aviv" = "MTA",
                       "LDLC ASVEL" = "ASV",
                       "ALBA Berlin" = "BER",
                       "Panathinaikos OPAP" = "PAO",
                       "Crvena zvezda mts" = "CZV"
  ))
  
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

reg25 <- reg25 |>
  mutate(
    team_home = teams_map_inv[team_home],
    team_away = teams_map_inv[team_away]
  )

# ── Weighted historical form score ────────────────────────────────────────────

weights <- c("24" = 0.70, "23" = 0.15, "22" = 0.10, "21" = 0.05)

record_hist <- bind_rows(
  record25 |> mutate(season = "24"),
  record24 |> mutate(season = "23"),
  record23 |> mutate(season = "22"),
  record22 |> mutate(season = "21")
) |>
  filter(team %in% teams25) |>
  mutate(weight = weights[season]) |>
  group_by(team) |>
  summarise(form = sum(record * weight) / sum(weight)) |>
  arrange(desc(form))

# ── Design matrix  ────────────────────────────────────────────────

n <- nrow(reg25)
d <- length(teams25)

X <- matrix(0, n, d)
colnames(X) <- teams25

for (i in 1:n) {
  X[i, reg25$team_home[i]] <- 1
  X[i, reg25$team_away[i]] <- -1
}

# ── Model fitting ─────────────────────────────────────────────────────────────

leader25 <- record25$team[1]

df_model <- as.data.frame(X[, -which(colnames(X) == leader25)])
df_model$score_diff <- reg25$score_diff

fit <- lm(score_diff ~ . - 1, data = df_model)
summary(fit)

# ── Predictions ───────────────────────────────────────────────────────────────

predict_score_diff <- function(team_home, team_away, fit) {
  alpha <- coef(fit)
  alpha_home <- ifelse(team_home %in% names(alpha), alpha[team_home], 0)
  alpha_away <- ifelse(team_away %in% names(alpha), alpha[team_away], 0)
  
  unname(alpha_home - alpha_away)
}

play25 <- read_csv("data/raw/eul/po/eul_24-25_po.csv") |>
  rename(team_home = Team, pts_home = PTS...3,
         team_away = Opp,  pts_away = PTS...5) |>
  mutate(
    score_diff = pts_home - pts_away,
    team_home = teams_map_inv[team_home],
    team_away = teams_map_inv[team_away]
  )

play25 <- play25 |>
  mutate(pred = mapply(predict_score_diff, team_home, team_away, MoreArgs = list(fit = fit)))

play25 
reg25
