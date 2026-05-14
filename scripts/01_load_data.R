# __ Team dictionary ___________________________________________________________
teams_map <- c(
  "ASV" = "LDLC ASVEL",
  "BAY" = "Bayern München",
  "BER" = "ALBA Berlin",
  "BKN" = "Baskonia",
  "CZV" = "Crvena zvezda Meridianbet",
  "DUB" = "Dubai",
  "EFS" = "Anadolu Efes",
  "FBB" = "Fenerbahçe Beko",
  "FCB" = "Barcelona",
  "HTA" = "Hapoel IBI Tel Aviv",
  "MIL" = "EA7 Emporio Armani Milano",
  "MON" = "AS Monaco",
  "MTA" = "Maccabi Playtika Tel Aviv",
  "OLY" = "Olympiacos",
  "PAO" = "Panathinaikos AKTOR",
  "PAR" = "Partizan Mozzart Bet",
  "PBB" = "Paris Basketball",
  "RMB" = "Real Madrid",
  "VBC" = "Valencia Basket",
  "VIR" = "Virtus Segafredo Bologna",
  "ZAL" = "Žalgiris"
)
teams <- names(teams_map)

# __ Raw data __________________________________________________________________
load_data <- function(type, season){
  path <-  file.path("data", "raw", "eul", type,
                     paste0("eul", "_", season, "_", type, ".csv"))
  read_csv(path)
}

regular25 <- load_data("reg", "24-25")
playoffs25 <- load_data("po", "24-25")
regular26 <- load_data("reg", "25-26")

standings <- list()
for (i in 21:26){
  standings[[paste0(i)]] <- load_data("standings", paste0(i-1, "-", i))
}
rm(i)
