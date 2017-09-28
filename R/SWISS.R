#' Read files of the SWISS tournament software
#'
#' \code{read_SWISS_tournament} returns all entries of a SWISS tournament.
#'
#' The SWISS tournament software is a old (2008) piece of software that allows
#' to manage a jugger tournament as a variant of the SWISS system. It returns a
#' XML file with all tournament information, teams, rounds, matches and results.
#' This function reads those information into R for analysis purposes.
#'
#' @param filename name of the XML file rturned by the program
#' @return a list with the following entries: tournament_version (integer),
#' score_calculation (character), ranking_comparator (character), teams
#' (a data.frame of teams) and rounds (a data.frame of all rounds and matches)
#'
#' teams has the columns: entry_id (integer), team_name (character), team_city (character)
#' rounds has the columns: round (integer), game (interger), entry_id1 (integer),
#' entry_id2 (integer), points1 (integer), points2 (integer), finished (logical)
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
  <xs:attribute name="type" type="xs:string"></xs:attribute>
  </xs:complexType>
  </xs:element>
  <xs:element name="rankingComparator">
  <xs:complexType>
  <xs:attribute name="type" type="xs:string"></xs:attribute>
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

  return(tournament)
}
