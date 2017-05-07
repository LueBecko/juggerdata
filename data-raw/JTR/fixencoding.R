# fix encoding (warning when checking the package: found non-ASCII strings)

load("data/JTR.jtr.rda")
load("data/JTR.Teams.rda")
load("data/JTR.Tournaments.rda")
load("data/JTR.Results.rda")

## based on: http://stackoverflow.com/questions/18837855/making-non-ascii-data-suitable-for-cran
Fix.Encoding <- function(strfactor) {
  Encoding(levels(strfactor)) <- "latin1"
  levels(strfactor) <- iconv(
    levels(strfactor),
    "latin1",
    "UTF-8"
  )
  return(strfactor)
}

JTR.jtr$TournamentName <- Fix.Encoding(JTR.jtr$TournamentName)
JTR.jtr$TournamentCountry <- Fix.Encoding(JTR.jtr$TournamentCountry)
JTR.jtr$TournamentCity <- Fix.Encoding(JTR.jtr$TournamentCity)
JTR.jtr$TournamentStreet <- Fix.Encoding(JTR.jtr$TournamentStreet)
JTR.jtr$TournamentPlace <- Fix.Encoding(JTR.jtr$TournamentPlace)
JTR.jtr$TeamName <- Fix.Encoding(JTR.jtr$TeamName)
JTR.jtr$TeamCountry <- Fix.Encoding(JTR.jtr$TeamCountry)
JTR.jtr$TeamCity <- Fix.Encoding(JTR.jtr$TeamCity)

JTR.Tournaments$TournamentName <- Fix.Encoding(JTR.Tournaments$TournamentName)
JTR.Tournaments$TournamentCountry <- Fix.Encoding(JTR.Tournaments$TournamentCountry)
JTR.Tournaments$TournamentCity <- Fix.Encoding(JTR.Tournaments$TournamentCity)
JTR.Tournaments$TournamentStreet <- Fix.Encoding(JTR.Tournaments$TournamentStreet)
JTR.Tournaments$TournamentPlace <- Fix.Encoding(JTR.Tournaments$TournamentPlace)

JTR.Teams$TeamName <- as.factor(JTR.Teams$TeamName)
JTR.Teams$TeamName <- Fix.Encoding(JTR.Teams$TeamName)
JTR.Teams$TeamCountry <- Fix.Encoding(JTR.Teams$TeamCountry)
JTR.Teams$TeamCity <- Fix.Encoding(JTR.Teams$TeamCity)

devtools::use_data(JTR.jtr, JTR.Tournaments, JTR.Teams, JTR.Results, overwrite = TRUE)
