#' JTR-data
#'
#' A data set of jugger tournament participation
#' and results over a long time preiode (2009-2016).
#'
#' @format A data frame with 2941 rows and 19 variables:
#' \describe{
#'   \item{TournamentID}{unique tournament key}
#'   \item{TournamentName}{Name (not always unique)}
#'   \item{TournamentStart}{announced starting timepoint}
#'   \item{TournamentEnd}{announced end timepoint}
#'   \item{TournamentCountry}{country name}
#'   \item{TournamentCity}{city name}
#'   \item{TournamentPostalCode}{postal code (as text)}
#'   \item{TournamentStreet}{street (text)}
#'   \item{TournamentPlace}{specific name of the location}
#'   \item{TournamentLatitude}{geographic latitude of the tournament}
#'   \item{TournamentLongitude}{geographic longitude of the tournament}
#'   \item{maxParticipants}{maximal allowed number of participants (0 means no limit)}
#'   \item{nParticipants}{actual number of participants}
#'   \item{TeamID}{unique team key}
#'   \item{TeamName}{Name of the team}
#'   \item{TeamCountry}{country name}
#'   \item{TeamCity}{city name}
#'   \item{TeamLatitude}{geographic latitude of the team (coutnry, city)}
#'   \item{TeamLongitude}{geographic longitude of the team (coutnry, city)}
#'   \item{nParticipations}{number of registered torunament participation of each team}
#'   \item{Rank}{rank of the team in the tournament}
#' }
#' @source \url{http://turniere.jugger.org/}
"JTR.jtr"

#' JTR-tournament data
#'
#' A data set of jugger tournaments over a long time preiode (2009-2016).
#'
#' @format A data frame with 217 rows and 13 variables:
#' \describe{
#'   \item{TournamentID}{unique tournament key}
#'   \item{TournamentName}{Name (not always unique)}
#'   \item{TournamentStart}{announced starting timepoint}
#'   \item{TournamentEnd}{announced end timepoint}
#'   \item{TournamentCountry}{country name}
#'   \item{TournamentCity}{city name}
#'   \item{TournamentPostalCode}{postal code (as text)}
#'   \item{TournamentStreet}{street (text)}
#'   \item{TournamentPlace}{specific name of the location}
#'   \item{TournamentLatitude}{geographic latitude of the tournament}
#'   \item{TournamentLongitude}{geographic longitude of the tournament}
#'   \item{maxParticipants}{maximal allowed number of participants (0 means no limit)}
#'   \item{nParticipants}{actual number of participants}
#' }
#' @source \url{http://turniere.jugger.org/}
"JTR.Tournaments"

#' JTR-team data
#'
#' A data set of jugger teams registered in the JTR
#' from a long time preiode (2009-2016).
#'
#' @format A data frame with 474 rows and 7 variables:
#' \describe{
#'   \item{TeamID}{unique team key}
#'   \item{TeamName}{Name of the team}
#'   \item{TeamCountry}{country name}
#'   \item{TeamCity}{city name}
#'   \item{TeamLatitude}{geographic latitude of the team (coutnry, city)}
#'   \item{TeamLongitude}{geographic longitude of the team (coutnry, city)}
#'   \item{nParticipations}{number of registered torunament participation of each team}
#' }
#' @source \url{http://turniere.jugger.org/}
"JTR.Teams"

#' JTR-torunament result data
#'
#' A data set of jugger tournament results
#' over a long time preiode (2009-2016).
#'
#' Note: This data set is not very useful  for standalone analysis and should be
#'       joined with at least one of JTR.Tournaments or JTR.Teams
#'       (JTR.Results %>% left_join(JTR.Tournaments) %>% left_join(JTR.Teams))
#'
#' @format A data frame with 2941 rows and 19 variables:
#' \describe{
#'   \item{TournamentID}{unique tournament key}
#'   \item{TeamID}{unique team key}
#'   \item{Rank}{rank of the team in the tournament}
#' }
#' @source \url{http://turniere.jugger.org/}
"JTR.Results"
