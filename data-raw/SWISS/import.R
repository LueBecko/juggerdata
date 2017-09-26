library(tidyverse)
library(xml2)

swiss0 <- read_xml("data-raw/SWISS/Hanseatic Jugger Cup")

tournament.setup <- list(tournament.version = strtoi(xml_attr(swiss0, "version"), base = 10),
                         scoreCalculation = xml_attr( xml_children(swiss0)[1], "type"),
                         rankingComparator = xml_attr( xml_children(swiss0)[2], "type") )

n.teams <- xml_length(xml_children(swiss0)[3])

teams <- data.frame(EntryID = 1:n.teams,
                    TeamName = xml_attr( xml_children( xml_children(swiss0)[3] ), "name" ),
                    TeamCity = xml_attr( xml_children( xml_children(swiss0)[3] ), "city" ))

rounds <- data.frame(round = integer(), game = integer(), EntryID1 = integer(), EntryID2 = integer(), points1 = integer(), points2 = integer(), finished = logical())
n.rounds <- xml_length( xml_children(swiss0)[4] )
for (r in 1:n.rounds) {
  n.games <- xml_length( xml_children(xml_children(swiss0)[4]))[r]
  round <- data.frame(round = r,
                      game = 1:n.games,
                      EntryID1 = strtoi(xml_attr( xml_children( xml_children( xml_children(swiss0)[4] )[r] ), "teamA"), base = 10) + 1,
                      EntryID2 = strtoi(xml_attr( xml_children( xml_children( xml_children(swiss0)[4] )[r] ), "teamB"), base = 10) + 1,
                      points1 = strtoi(xml_attr( xml_children( xml_children( xml_children(swiss0)[4] )[r] ), "pointsA"), base = 10),
                      points2 = strtoi(xml_attr( xml_children( xml_children( xml_children(swiss0)[4] )[r] ), "pointsA"), base = 10),
                      finished = toupper( xml_attr( xml_children( xml_children( xml_children(swiss0)[4] )[r] ), "finished") ) == "TRUE"
                      )

  rounds <- rbind(rounds, round)
}
rm(r, round, n.games, n.rounds)

rounds <- rounds %>%
  left_join(teams, by = c("EntryID1" = "EntryID")) %>%
  left_join(teams, by = c("EntryID2" = "EntryID"), suffix = c("1", "2")) %>%
  select(round, game, EntryID1, TeamName1, TeamCity1, points1, EntryID2, TeamName2, TeamCity2, points2, finished)
