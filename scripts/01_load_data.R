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
    po[[paste0(i)]] <- load_data("po", paste0(i-1, "-", i))
  }
  return(list(
    regular = reg,
    playoffs = po
  ))
}
games <- load_games()

# load_standings <- function () {
#   
#   standings <- list()
#   
#   for (i in 21:26){
#     standings[[paste0(i)]] <- load_data("standings", paste0(i-1, "-", i))
#   }
#   return(standings)
# }
# standings <- load_standings()
