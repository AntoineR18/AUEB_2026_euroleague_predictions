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
all_teams <- unique(unlist(team_codes))

# __ Raw data __________________________________________________________________
load_data <- function (type, season) {
  
  path <-  file.path(
    "data", "raw", "eul", type,
    paste0("eul", "_", season, "_", type, ".csv")
  )
  read_csv(path)
}

load_games <- function () {
  
  reg <- list()
  po <- list()
  
  for (i in 21:26) {
    reg[[paste0(i)]] <- load_data("reg", paste0(i-1, "-", i))
    if (i == 26) {next}
    po[[paste0(i)]] <- load_data("po", paste0(i-1, "-", i))
  }
  return(list(
    regular = reg,
    playoffs = po
  ))
}
games <- load_games()

load_standings <- function () {
  
  standings <- list()
  
  for (i in 21:26){
    standings[[paste0(i)]] <- load_data("standings", paste0(i-1, "-", i))
  }
  return(standings)
}
standings <- load_standings()

