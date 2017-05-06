library(readr)

JTR.jtr <- read_csv("data-raw/jtr_export.csv", col_names = FALSE,
                    col_types = cols(X2 = col_datetime(format = "%Y-%m-%d %H:%M:%S"), X3 = col_datetime(format = "%Y-%m-%d %H:%M:%S"), X6 = col_character()))

##################################################################################################
## SOURCE: http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
# added NA check
# load XML package
library(XML)

# Convenience function to convert html codes
html2txt <- function(str) {
  if (is.na(str)) {
    ''
  } else {
    xpathApply(htmlParse(str, asText=TRUE),
               "//body//text()",
               xmlValue)[[1]]
  }
}
##################################################################################################

library(dplyr, warn.conflicts = FALSE)

# fix char encoding
JTR.jtr$X6[JTR.jtr$X6 == '-'] <- NA
JTR.jtr <- JTR.jtr %>% mutate(X1  = sapply(X1 , FUN = html2txt),
                              X5  = sapply(X5 , FUN = html2txt),
                              X6  = sapply(X6 , FUN = html2txt),
                              X7  = sapply(X7 , FUN = html2txt),
                              X8  = sapply(X8 , FUN = html2txt),
                              X9  = sapply(X9 , FUN = html2txt),
                              X11 = sapply(X11, FUN = html2txt),
                              X12 = sapply(X12, FUN = html2txt),
                              X13 = sapply(X13, FUN = html2txt))
# clean PLZ
JTR.jtr <- JTR.jtr %>% mutate(X6 = substr(X6,1,5))

# code tournament and team together
Country <- union(JTR.jtr$X5, JTR.jtr$X12)
City    <- union(JTR.jtr$X7, JTR.jtr$X13)
JTR.jtr <- JTR.jtr %>% transmute(TournamentName = as.factor(X1),
                                 TournamentStart = X2,
                                 TournamentEnd = X3,
                                 maxParticipants = X4,
                                 TournamentCountry = factor(X5, levels = Country),
                                 TournamentPostalCode = as.factor(X6),
                                 TournamentCity = factor(X7, levels = City),
                                 TournamentStreet = as.factor(X8),
                                 TournamentPlace = as.factor(X9),
                                 Rank = X10,
                                 TeamName = as.factor(X11),
                                 TeamCountry = factor(X12, levels = Country),
                                 TeamCity = factor(X13, levels = City))

JTR.Tournaments <- JTR.jtr %>% group_by(TournamentName, TournamentStart, TournamentEnd, TournamentCountry, TournamentPostalCode, TournamentCity, TournamentStreet, TournamentPlace, maxParticipants) %>%
                              summarise(nParticipants = n()) %>% arrange(TournamentStart, TournamentName)

JTR.Tournaments <- JTR.Tournaments %>% ungroup() %>% mutate(TournamentID = row_number())

# tournament name collisions?
# nrow(JTR.Tournaments)
# length(levels(JTR.Tournaments$TournamentName)) #!!!
#
# TournamentsNameMult <- JTR.Tournaments %>% group_by(TournamentName) %>% summarise(n = n()) %>% filter(n > 1) %>% select(TournamentName)
# JTR.Tournaments %>% filter(TournamentName %in% TournamentsNameMult$TournamentName)

##################################################################################################
# get geo information
# BASED ON: https://www.r-bloggers.com/search-and-draw-cities-on-a-map-using-openstreetmap-and-r/
library(RJSONIO)

geoCodeOSM <- function(Street, City, Country) {
  cleanCityName  <- gsub(' ', '%20', City)
  if (!is.na(Street)) {
    cleanStreeName <- gsub(' ', '%20', Street)
    url <- paste(
      "http://nominatim.openstreetmap.org/search?"
      , "limit=9&format=json"
      , "&street="
      , cleanStreeName
      , "&city="
      , cleanCityName
      , "&country="
      , Country
      , sep="")
  } else {
    url <- paste(
      "http://nominatim.openstreetmap.org/search?"
      , "limit=9&format=json"
      , "&city="
      , cleanCityName
      , "&country="
      , Country
      , sep="")
  }
  # return(url);
  resOSM <- fromJSON(url)
  if (length(resOSM) > 0) {
    return(c(resOSM[[1]]$lon, resOSM[[1]]$lat))
  } else return(rep(NA,2))
}
##################################################################################################

geocodeTournaments <- apply(JTR.Tournaments, 1, FUN = function(trow) { geoCodeOSM(trow[7], trow[6], trow[4]) })

JTR.Tournaments$TournamentLongitude <- as.numeric(geocodeTournaments[1, ])
JTR.Tournaments$TournamentLatitude  <- as.numeric(geocodeTournaments[2, ])
# rm(geocodeTournaments)

# missing geo?
# sum(is.na(JTR.Tournaments$TournamentLatitude)) / nrow(JTR.Tournaments)
#
# JTR.Tournaments %>% filter(is.na(TournamentLatitude))

##################################################################################################

JTR.Teams <- JTR.jtr %>% group_by(TeamName, TeamCountry, TeamCity) %>% summarise(nParticipations = n())
JTR.Teams <- JTR.Teams %>% ungroup() %>% mutate(TeamID = row_number())

geocodeTeams <- apply(JTR.Teams, 1, FUN = function(trow) { geoCodeOSM(Street = NA, trow[3], trow[2]) })
save(geocodeTournaments,geocodeTeams, file = "geocodes.RData")

JTR.Teams$TeamLongitude <- as.numeric(geocodeTeams[1, ])
JTR.Teams$TeamLatitude  <- as.numeric(geocodeTeams[2, ])
# rm(geocodeTeams)

# missing geo?
# sum(is.na(JTR.Teams$TeamLatitude)) / nrow(JTR.Teams)
#
# JTR.Teams %>% filter(is.na(TeamLatitude))

##################################################################################################

JTR.jtr <- JTR.jtr %>%  left_join(JTR.Tournaments) %>%
                        left_join(JTR.Teams)
JTR.Results <- JTR.jtr %>% transmute(TournamentID, TeamID, Rank)

##################################################################################################
#save(jtr, Tournaments, Results, Teams, file = "JTR.RData")
devtools::use_data(JTR.jtr, JTR.Tournaments, JTR.Teams, JTR.Results, overwrite = TRUE)
save(JTR.jtr, JTR.Tournaments, JTR.Teams, JTR.Results, file = "jtr.RData")

