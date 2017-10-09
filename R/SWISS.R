#' Read files of the SWISS tournament software
#'
#' \code{readSwissTournament} returns all entries of a SWISS tournament.
#' \code{summary} returns a ranking summary of a SWISS tournament.
#'
#' The SWISS tournament software is a old (2008) piece of software that allows
#' to manage a jugger tournament as a variant of the SWISS system. It returns a
#' XML file with all tournament information, teams, rounds, matches and results.
#' This function reads those information into R for analysis purposes.
#'
#' @param filename (character) name of the XML file returned by the program
#' @param tournament a valid SwissTournament object
#' @param fullReport (logical) print a full ranking over each round or only the latest ranking
#'
#' @return \code{readSwissTournament} returns a S3 object SwissTournament, which is implemente as a list with the following entries:
#' \itemize{
#' \item tournamentVersion (integer)
#' \item scoreCalculation (character)
#' \item rankingComparator (character)
#' \item teams (a data.frame of teams)
#'   \itemize{
#'     \item entryId (integer)
#'     \item teamName (character)
#'     \item teamCity (character)
#'   }
#' \item rounds (a data.frame of all rounds and matches)
#'   \itemize{
#'     \item round (integer)
#'     \item game (interger)
#'     \item entryId1 (integer)
#'     \item entryId2 (integer)
#'     \item points1 (integer)
#'     \item points2 (integer)
#'     \item finished (logical)
#'   }
#' }
#' \code{summary} returns an object of class SwissTournamentSummary, which is a derived class of SwissTournament and contains all fields of the original SwissTorunament plus the following fields:
#' \itemize{
#' \item stats (list)
#'   \itemize{
#'     \item teamsParticipating (integer)
#'     \item roundsPlayed (integer)
#'     \item roundsSceduled (integer)
#'     \item matchesPlayed (integer)
#'     \item matchesScheduled (integer)
#'   }
#' \item rankings (a data.frame of rankings and related values)
#'   \itemize{
#'     \item round (integer)
#'     \item rank (integer)
#'     \item entryId (integer)
#'     \item opp (a list of integers) each entry indicates a opponent team thats already been played against
#'     \item scoreCum (integer) cumulated game scores with respect to selected scoring function
#'     \item pointsCum (integer) cumulated points scored
#'     \item pointsDiffCum (integer) cumulated point differences
#'     \item BHZ (integer) Buchholzzahl
#'   }
#' }
#'
#' @author Christian Beck
#'
#' @examples
#'
#' \dontrun{
#' # reads the tournament
#' hanseaticjuggercup2012 <- readSwissTournament("HanseaticJuggerCupR6")
#'
#' # ad hoc information about rankings
#' summary(hanseaticjuggercup2012)
#' }
readSwissTournament <- function(filename) {
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(file.exists(filename))

  swiss <- xml2::read_xml(filename)

  # embedd schema for validation
  schemaText <- '<?xml version="1.0" encoding="UTF-8"?>
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" attributeFormDefault="unqualified">
  <xs:element name="tournament">
  <xs:complexType>
  <xs:sequence>
  <xs:element name="scoreCalculator">
  <xs:complexType>
  <xs:attribute name="type">
  <xs:simpleType>
  <xs:restriction base="xs:string">
      <xs:enumeration value="KO"/>
      <xs:enumeration value="TwoPoints"/>
      <xs:enumeration value="ThreePoints"/>
  </xs:restriction>
  </xs:simpleType>
  </xs:attribute>
  </xs:complexType>
  </xs:element>
  <xs:element name="rankingComparator">
  <xs:complexType>
  <xs:attribute name="type">
  <xs:simpleType>
  <xs:restriction base="xs:string">
      <xs:enumeration value="Buchholz"/>
  </xs:restriction>
  </xs:simpleType>
  </xs:attribute>
  </xs:complexType>
  </xs:element>
  <xs:element name="teams">
  <xs:complexType>
  <xs:sequence>
  <xs:element name="team" maxOccurs="unbounded">
  <xs:complexType>
  <xs:attribute name="name" type="xs:string"></xs:attribute>
  <xs:attribute name="city" type="xs:string"></xs:attribute>
  </xs:complexType>
  </xs:element>
  </xs:sequence>
  </xs:complexType>
  </xs:element>
  <xs:element name="rounds">
  <xs:complexType>
  <xs:sequence>
  <xs:element name="round" maxOccurs="unbounded">
  <xs:complexType>
  <xs:sequence>
  <xs:element name="match" maxOccurs="unbounded">
  <xs:complexType>
  <xs:attribute name="teamA" type="xs:int"></xs:attribute>
  <xs:attribute name="teamB" type="xs:int"></xs:attribute>
  <xs:attribute name="pointsA" type="xs:int"></xs:attribute>
  <xs:attribute name="pointsB" type="xs:int"></xs:attribute>
  <xs:attribute name="finished" type="xs:string"></xs:attribute>
  </xs:complexType>
  </xs:element>
  </xs:sequence>
  </xs:complexType>
  </xs:element>
  </xs:sequence>
  </xs:complexType>
  </xs:element>
  </xs:sequence>
  <xs:attribute name="version" type="xs:int"></xs:attribute>
  </xs:complexType>
  </xs:element>
  </xs:schema>'
  schema <- xml2::read_xml(schemaText)

  assertthat::assert_that(xml2::xml_validate(swiss, schema))
  rm(schema, schemaText)

  nTeams <- xml2::xml_length(xml2::xml_children(swiss)[3])

  teams <- data.frame(entryId = 1:nTeams,
                      teamName = xml2::xml_attr( xml2::xml_children( xml2::xml_children(swiss)[3] ), "name" ),
                      teamCity = xml2::xml_attr( xml2::xml_children( xml2::xml_children(swiss)[3] ), "city" ))

  rounds <- data.frame(round = integer(),
                       game = integer(),
                       entryId1 = integer(),
                       entryId2 = integer(),
                       points1 = integer(),
                       points2 = integer(),
                       finished = logical())

  nRounds <- xml2::xml_length( xml2::xml_children(swiss)[4] )
  for (rIdx in 1:nRounds) {
    nGames <- xml2::xml_length( xml2::xml_children(xml2::xml_children(swiss)[4]))[rIdx]
    round <- data.frame(round = rIdx,
                        game = 1:nGames,
                        entryId1 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[rIdx]), "teamA"), base = 10) + as.integer(1),
                        entryId2 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[rIdx]), "teamB"), base = 10) + as.integer(1),
                        points1 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[rIdx]), "pointsA"), base = 10),
                        points2 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[rIdx]), "pointsB"), base = 10),
                        finished = toupper( xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[rIdx]), "finished") ) == "TRUE"
    )

    rounds <- rbind(rounds, round)
  }
  rm(rIdx, round, nGames, nRounds)

  tournament <- list(tournamentVersion = strtoi(xml2::xml_attr(swiss, "version"), base = 10),
                     scoreCalculation = xml2::xml_attr( xml2::xml_children(swiss)[1], "type"),
                     rankingComparator = xml2::xml_attr( xml2::xml_children(swiss)[2], "type"),
                     teams = teams,
                     rounds = rounds)

  class(tournament) <- "SwissTournament"
  return(tournament)
}

#' @rdname readSwissTournament
summary.SwissTournament <- function(tournament) {
  assertthat::assert_that(class(tournament) == 'SwissTournament')

  ## scoring functions
  scoreKO <- function(won, draw) {
    return (ifelse(won, 1, 0))
  }

  scoreTwoPoints <- function(won, draw) {
    return (ifelse(won, 2, ifelse(draw, 1, 0)))
  }

  scoreThreePoints <- function(won, draw) {
    return (ifelse(won, 3, ifelse(draw, 1, 0)))
  }

  scoreInvalid <- function(won, draw) {
    stop("Unknown scoring function. Please review your data.")
  }

  scoreFunction <- switch(EXPR = tournament$scoreCalculation,
                          KO = scoreKO,
                          TwoPoints = scoreTwoPoints,
                          ThreePoints = scoreThreePoints,
                          scoreInvalid)

  ## comparator functions
  comparatorBuchholzzahl <- function(games) {
    ordering <- order(games$scoreCum, games$BHZ, games$pointsDiffCum, games$pointsCum,
                     decreasing = rep(TRUE, 4))
    return(order(ordering))
  }

  comparatorInvalid <- function(games) {
    stop("Unknown ranking comparator. Please review your data.")
  }

  comparatorFunction <- switch(EXPR = tournament$rankingComparator,
                               Buchholz = comparatorBuchholzzahl,
                               comparatorInvalid)

  # games
  teamsParticipating <- nrow(tournament$teams)

  roundsPlayed <- length(setdiff(unique(tournament$rounds$round),
                                 unique(tournament$rounds$round[!tournament$rounds$finished])))
  roundsScheduled <- length(unique(tournament$rounds$round))
  matchesPlayed <- sum(tournament$rounds$finished)
  matchesScheduled <- nrow(tournament$rounds)

  rounds <- tournament$rounds[tournament$rounds$finished,]
  rounds$won1 <- rounds$points1  > rounds$points2
  rounds$won2 <- rounds$points2  > rounds$points1
  rounds$draw <- rounds$points1 == rounds$points2
  rounds$pointsDiff1 <- rounds$points1 - rounds$points2
  rounds$pointsDiff2 <- rounds$points2 - rounds$points1
  rounds$score1 <- scoreFunction(rounds$won1, rounds$draw)
  rounds$score2 <- scoreFunction(rounds$won2, rounds$draw)

  rounds <- rbind(data.frame(round = rounds$round,
                             entryId = rounds$entryId1,
                             points = rounds$points1,
                             won = rounds$won1,
                             draw = rounds$draw,
                             pointsDiff = rounds$pointsDiff1,
                             score = rounds$score1,
                             opp = rounds$entryId2),
                  data.frame(round = rounds$round,
                             entryId = rounds$entryId2,
                             points = rounds$points2,
                             won = rounds$won2,
                             draw = rounds$draw,
                             pointsDiff = rounds$pointsDiff2,
                             score = rounds$score2,
                             opp = rounds$entryId1) )

  # fill gaps for exact ranking computation
  for (rIdx in 1:roundsScheduled) {
    toAdd <- setdiff(tournament$teams$entryId, rounds$entryId[rounds$round == rIdx])
    if (length(toAdd) > 0) {
      rounds <- rbind(rounds, data.frame(round = rIdx,
                                         entryId = toAdd,
                                         points = 0,
                                         won = FALSE,
                                         draw = FALSE,
                                         pointsDiff = 0,
                                         score = 0,
                                         opp = NA))
    }
  }

  rounds <- rounds[order(rounds$round), ]

  for (tIdx in tournament$teams$entryId) {
    sel <- rounds$entryId == tIdx
    games <- rounds[sel, ]
    opp <- list()

    # cumulate results
    rounds$scoreCum[sel] <- cumsum(games$score)
    rounds$pointsCum[sel] <- cumsum(games$points)
    rounds$pointsDiffCum[sel] <- cumsum(games$pointsDiff)

    if (nrow(games) > 0) {
      for (g in 1:nrow(games)) {

        if (g == 1) {
          opp[[g]] <- games$opp[g]
        } else {
          # if (!is.na(games$opp[g])) {
            opp[[g]] <- c(games$opp[g], opp[[g - 1]])
          # }
        }
      }
    }
    rounds$opp[sel] <- opp
  }

  # computed BHZ (reversed from program)
  if (nrow(rounds) > 0) {
    for (rIdx in 1:nrow(rounds)) {
      rounds$BHZ[rIdx] <- sum(rounds$scoreCum[rounds$round == rounds$round[rIdx] & rounds$entryId %in% rounds$opp[rIdx][[1]]])
    }
  }

  # apply ranking comparator
  if (matchesPlayed > 0) {
    for (rIdx in 1:roundsScheduled) {
      sel <- rounds$round == rIdx
      rounds$rank[sel] <- comparatorFunction(rounds[sel,])
    }
  } else {
    rounds$rank <- integer(length = nrow(rounds))
  }

  rounds <- rounds[order(rounds$rank),]
  rounds <- rounds[order(rounds$round),]

  tournamentSummary <- append(tournament, list(rankings = rounds[,c(1,13,2,8:12)],
                                               stats = list(teamsParticipating = teamsParticipating,
                                                            roundsPlayed = roundsPlayed, roundsScheduled = roundsScheduled,
                                                             matchesPlayed = matchesPlayed, matchesScheduled = matchesScheduled)))

  class(tournamentSummary) <- c("SwissTournamentSummary", "SwissTournament")
  return(tournamentSummary)
}

#' @rdname readSwissTournament
print.SwissTournament <- function(tournament, fullReport = FALSE) {
  print(summary(tournament), fullReport = fullReport)
}

#' @rdname readSwissTournament
print.SwissTournamentSummary <- function(tournamentSummary, fullReport = FALSE) {
  assertthat::assert_that(all(class(tournamentSummary) == c("SwissTournamentSummary", "SwissTournament")))

  rankingsOut <- tournamentSummary$rankings[,c(-4)]
  rankingsOut$teamName <- tournamentSummary$teams$teamName[match(rankingsOut$entryId, tournamentSummary$teams$entryId)]

  cat("=======================================\n")
  cat(" Swiss-tournament summary\n\n")
  cat(paste(" * tournament-version: ", toString(tournamentSummary$tournamentVersion), "\n", sep = ""))
  cat(paste(" * score-calculation: ", tournamentSummary$scoreCalculation, "\n", sep = ""))
  cat(paste(" * ranking-comparator: ", tournamentSummary$rankingComparator, "\n", sep = ""))
  cat(paste("\n Setup a tournament for ", toString(tournamentSummary$stats$teamsParticipating), " teams\n", sep = ""))
  cat(paste("  with ", toString(tournamentSummary$stats$roundsPlayed), " complete rounds played (", toString(tournamentSummary$stats$roundsScheduled), " scheduled)\n", sep = ""))
  cat(paste("  and ", toString(tournamentSummary$stats$matchesPlayed), " finished games played (", toString(tournamentSummary$stats$matchesScheduled), " scheduled)\n", sep = ""))
  cat("\n=======================================\n")
  cat(" Participating teams\n")
  write.table(tournamentSummary$teams, quote = FALSE, sep = "\t", row.names = FALSE)
  cat("\n=======================================\n")
  if (nrow(rankingsOut) > 0) {
    if (fullReport) {
      for (rIdx in tournament_summary$stats$roundsScheduled:1) {
        cat("---------------------------------------\n")
        cat(paste(" Ranking of round ", as.character(rIdx), "\n", sep = ""))
        write.table(rankings.out[rankingsOut$round == rIdx, c(1:3,8,4:7)], quote = FALSE, sep = "\t", row.names = FALSE)
      }
    } else {
      cat(" Latest Ranking\n")
      write.table(rankingsOut[rankingsOut$round == max(rankingsOut$round), c(1:3,8,4:7)], quote = FALSE, sep = "\t", row.names = FALSE)
    }
  } else {
    cat(" No ranking information present due to insufficient number of games\n")
  }
}
