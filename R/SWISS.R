#' Read files of the SWISS tournament software
#'
#' \code{read_SWISS_tournament} returns all entries of a SWISS tournament.
#'
#' The SWISS tournament software is a old (2008) piece of software that allows
#' to manage a jugger tournament as a variant of the SWISS system. It returns a
#' XML file with all tournament information, teams, rounds, matches and results.
#' This function reads those information into R for analysis purposes.
#'
#' @param filename name of the XML file returned by the program
#' @return a S3 object SWISS_torunament, which is implemente as a list with the following entries:
#' \itemize{
#' \item tournament_version (integer)
#' \item score_calculation (character)
#' \item ranking_comparator (character)
#' \item teams (a data.frame of teams)
#'   \itemize{
#'     \item entry_id (integer)
#'     \item team_name (character)
#'     \item team_city (character)
#'   }
#' \item rounds (a data.frame of all rounds and matches)
#'   \itemize{
#'     \item round (integer)
#'     \item game (interger)
#'     \item entry_id1 (integer)
#'     \item entry_id2 (integer)
#'     \item points1 (integer)
#'     \item points2 (integer)
#'     \item finished (logical)
#'   }
#' }
#'
#' @author Christian Beck
#'
#' @examples
#'
#' \dontrun{
#' hanseaticjuggercup2014 <- read_SWISS_tournament("HanseaticJuggerCupR6")
#' }
read_SWISS_tournament <- function(filename) {
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(file.exists(filename))

  swiss <- xml2::read_xml(filename)

  # embedd schema for validation
  schema_text <- '<?xml version="1.0" encoding="UTF-8"?>
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
  schema <- xml2::read_xml(schema_text)

  assertthat::assert_that(xml2::xml_validate(swiss, schema))
  rm(schema, schema_text)

  n_teams <- xml2::xml_length(xml2::xml_children(swiss)[3])

  teams <- data.frame(entry_id = 1:n_teams,
                      team_name = xml2::xml_attr( xml2::xml_children( xml2::xml_children(swiss)[3] ), "name" ),
                      team_city = xml2::xml_attr( xml2::xml_children( xml2::xml_children(swiss)[3] ), "city" ))

  rounds <- data.frame(round = integer(),
                       game = integer(),
                       entry_id1 = integer(),
                       entry_id2 = integer(),
                       points1 = integer(),
                       points2 = integer(),
                       finished = logical())

  n_rounds <- xml2::xml_length( xml2::xml_children(swiss)[4] )
  for (r in 1:n_rounds) {
    n_games <- xml2::xml_length( xml2::xml_children(xml2::xml_children(swiss)[4]))[r]
    round <- data.frame(round = r,
                        game = 1:n_games,
                        entry_id1 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[r]), "teamA"), base = 10) + as.integer(1),
                        entry_id2 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[r]), "teamB"), base = 10) + as.integer(1),
                        points1 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[r]), "pointsA"), base = 10),
                        points2 = strtoi(xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[r]), "pointsB"), base = 10),
                        finished = toupper( xml2::xml_attr( xml2::xml_children( xml2::xml_children( xml2::xml_children(swiss)[4])[r]), "finished") ) == "TRUE"
    )

    rounds <- rbind(rounds, round)
  }
  rm(r, round, n_games, n_rounds)

  tournament <- list(tournament_version = strtoi(xml2::xml_attr(swiss, "version"), base = 10),
                     score_calculation = xml2::xml_attr( xml2::xml_children(swiss)[1], "type"),
                     ranking_comparator = xml2::xml_attr( xml2::xml_children(swiss)[2], "type"),
                     teams = teams,
                     rounds = rounds)

  class(tournament) <- "SWISS_tournament"
  return(tournament)
}


summary.SWISS_tournament <- function(tournament) {
  assertthat::assert_that(class(tournament) == 'SWISS_tournament')

  ## scoring functions
  score_KO <- function(won, draw) {
    return (ifelse(won, 1, 0))
  }

  score_TwoPoints <- function(won, draw) {
    return (ifelse(won, 2, ifelse(draw, 1, 0)))
  }

  score_ThreePoints <- function(won, draw) {
    return (ifelse(won, 3, ifelse(draw, 1, 0)))
  }

  score_invalid <- function(won, draw) {
    stop("Unknown scoring function. Please review your data.")
  }

  score_apply <- switch(EXPR = tournament$score_calculation,
                        KO = score_KO,
                        TwoPoints = score_TwoPoints,
                        ThreePoints = score_ThreePoints,
                        score_invalid)

  ## comparator functions
  comparator_Buchholzzahl <- function(games) {
    ranking <- order(games$score_cum, games$BHZ, games$points_diff_cum, games$points_cum,
                     decreasing = rep(TRUE, 4))
    return(ranking)
  }

  comparator_invalid <- function(games) {
    stop("Unknown ranking comparator. Please review your data.")
  }

  comparator_apply <- switch(EXPR = tournament$ranking_comparator,
                             Buchholz = comparator_Buchholzzahl,
                             comparator_invalid)

  # games
  teams_participating <- nrow(tournament$teams)

  rounds_played <- length(setdiff(unique(tournament$rounds$round),
                                  unique(tournament$rounds$round[!tournament$rounds$finished])))
  rounds_scheduled <- length(unique(tournament$rounds$round))
  matches_played <- sum(tournament$rounds$finished)
  matches_scheduled <- nrow(tournament$rounds)

  rounds <- tournament$rounds[tournament$rounds$finished,]
  rounds$won1 <- rounds$points1  > rounds$points2
  rounds$won2 <- rounds$points2  > rounds$points1
  rounds$draw <- rounds$points1 == rounds$points2
  rounds$points_diff1 <- rounds$points1 - rounds$points2
  rounds$points_diff2 <- rounds$points2 - rounds$points1
  rounds$score1 <- score_apply(rounds$won1, rounds$draw)
  rounds$score2 <- score_apply(rounds$won2, rounds$draw)

  rounds <- rbind(data.frame(round = rounds$round,
                             entry_id = rounds$entry_id1,
                             points = rounds$points1,
                             won = rounds$won1,
                             draw = rounds$draw,
                             points_diff = rounds$points_diff1,
                             score = rounds$score1,
                             opp = rounds$entry_id2),
                  data.frame(round = rounds$round,
                             entry_id = rounds$entry_id2,
                             points = rounds$points2,
                             won = rounds$won2,
                             draw = rounds$draw,
                             points_diff = rounds$points_diff2,
                             score = rounds$score2,
                             opp = rounds$entry_id1) )

  rounds <- rounds[order(rounds$round), ]

  if (nrow(rounds) == 0) {
    rounds$score_cum <- list()
    rounds$points_cum <- list()
    rounds$points_diff_cum <- list()
    rounds$BHZ <- list()
  }

  for (t in tournament$teams$entry_id) {
    sel <- rounds$entry_id == t
    games <- rounds[sel, ]
    opp <- list()

    # cumulate results
    rounds$score_cum[sel] <- cumsum(games$score)
    rounds$points_cum[sel] <- cumsum(games$points)
    rounds$points_diff_cum[sel] <- cumsum(games$points_diff)

    if (nrow(games) > 0) {
      for (g in 1:nrow(games)) {

        if (g == 1) {
          opp[[g]] <- games$opp[g]
        } else {
          opp[[g]] <- c(games$opp[g], opp[[g - 1]])
        }
      }
    }
    rounds$opp[sel] <- opp
  }

  # computed BHZ (reversed from program)
  if (nrow(rounds) > 0) {
    for (r in 1:nrow(rounds)) {
      rounds$BHZ[r] <- sum(rounds$score_cum[rounds$round == rounds$round[r] & rounds$entry_id %in% rounds$opp[r][[1]]])
    }
  }

  # apply ranking comparator
  if (rounds_played > 0) {
    for (r in 1:rounds_played) {
      sel <- rounds$round == r
      rounds$rank[sel] <- comparator_apply(rounds[sel,])
    }
  } else {
    rounds$rank <- list()
  }

  rounds <- rounds[order(rounds$rank),]
  rounds <- rounds[order(rounds$round),]

  # tournament_summary <- list(stats = list(teams_participating = teams_participating,
  #                                         rounds_played = rounds_played, rounds_scheduled = rounds_scheduled,
  #                                         matches_played = matches_played, matches_scheduled = matches_scheduled),
  #                            params = list(tournament_version = tournament$tournament_version,
  #                                          score_calculation = tournament$score_calculation,
  #                                          ranking_comparator = tournament$ranking_comparator),
  #                            teams = tournament$teams,
  #                            games = tournament$rounds,
  #                            rankings = rounds[,c(1,2,7:12)])

  tournament_summary <- append(tournament, list(rankings = rounds[,c(1,2,7:12)],
                                                stats = list(teams_participating = teams_participating,
                                                             rounds_played = rounds_played, rounds_scheduled = rounds_scheduled,
                                                             matches_played = matches_played, matches_scheduled = matches_scheduled)))

  class(tournament_summary) <- c("SWISS_tournament_summary", "SWISS_tournament")
  return(tournament_summary)
}

print.SWISS_tournament <- function(tournament, full_report = FALSE) {
  print(summary(tournament), full_report = full_report)
}

print.SWISS_tournament_summary <- function(tournament_summary, full_report = FALSE) {
  assertthat::assert_that(all(class(tournament_summary) == c("SWISS_tournament_summary", "SWISS_tournament")))

  rankings.out <- tournament_summary$rankings[,c(-3,-4)]
  rankings.out$team_name <- tournament_summary$teams$team_name[match(rankings.out$entry_id, tournament_summary$teams$entry_id)]

  cat("=======================================\n")
  cat(" SWISS-tournament summary\n\n")
  cat(paste(" * tournament-version: ", toString(tournament_summary$tournament_version), "\n", sep = ""))
  cat(paste(" * score-calculation: ", tournament_summary$score_calculation, "\n", sep = ""))
  cat(paste(" * ranking-comparator: ", tournament_summary$ranking_comparator, "\n", sep = ""))
  cat(paste("\n Setup a tournament for ", toString(tournament_summary$stats$teams_participating), " teams\n", sep = ""))
  cat(paste("  with ", toString(tournament_summary$stats$rounds_played), " complete rounds played (", toString(tournament_summary$stats$rounds_scheduled), " scheduled)\n", sep = ""))
  cat(paste("  and ", toString(tournament_summary$stats$matches_played), " finished games played (", toString(tournament_summary$stats$matches_scheduled), " scheduled)\n", sep = ""))
  cat("\n=======================================\n")
  cat(" Participating teams\n")
  write.table(tournament_summary$teams, quote = FALSE, sep = "\t", row.names = FALSE)
  cat("\n=======================================\n")
  if (nrow(rankings.out) > 0) {
    if (full_report) {
      for (r in tournament_summary$stats$rounds_played:1) {
        cat("---------------------------------------\n")
        cat(paste(" Ranking of round ", as.character(r), "\n", sep = ""))
        write.table(rankings.out[rankings.out$round == r,c(1:2,7,3:6)], quote = FALSE, sep = "\t", row.names = FALSE)
      }
    } else {
      cat(" Latest Ranking\n")
      write.table(rankings.out[rankings.out$round == max(rankings.out$round),c(1:2,7,3:6)], quote = FALSE, sep = "\t", row.names = FALSE)
    }
  } else {
    cat(" No ranking information present due to insufficient number of games\n")
  }
}
