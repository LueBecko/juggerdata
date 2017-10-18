
context("SWISS-tournament data import")

#### readSWISSTournament base
test_that("checks on filename input", {
  expect_error(readSwissTournament(NA), regexp = "filename is not a character vector") # no char
  expect_error(readSwissTournament(1), regexp = "filename is not a character vector") # no char
  expect_error(readSwissTournament(""), regexp = "Path '' does not exist") # no file
  expect_error(readSwissTournament(":::definitely not a filename:::"), regexp = "Path ':::definitely not a filename:::' does not exist") # no file
})

# using stubs to check correct file usage
test_that("checks on XML to process", {
  expect_error(readSwissTournament(file.path("testSWISSfiles","invalid1")), "StartTag: invalid element name \\[68\\]") # not XML
  expect_error(readSwissTournament(file.path("testSWISSfiles","invalid2")), "xml2::xml_validate\\(x = swiss, schema = schema\\) is not TRUE") # XML not in schema
})

#### readSwissTournament with data
emptyResults <- list(tournamentVersion = as.integer(1),
                     scoreCalculation = "TwoPoints",
                     rankingComparator = "Buchholz",
                     teams = data.frame(entryId = as.integer(1:2),
                                        teamName = paste("T", strtoi(1:2), sep = ""),
                                        teamCity = paste("S", strtoi(1:2), sep = ""),
                                        stringsAsFactors = FALSE),
                     rounds = data.frame(round = as.integer(1),
                                         game = as.integer(1),
                                         entryId1 = as.integer(1),
                                         entryId2 = as.integer(2),
                                         points1 = as.integer(0),
                                         points2 = as.integer(0),
                                         finished = FALSE))
class(emptyResults) <- "SwissTournament"

exampleResults  <- list(tournamentVersion = as.integer(1),
                        scoreCalculation = "TwoPoints",
                        rankingComparator = "Buchholz",
                        teams = data.frame(entryId = as.integer(1:14),
                                           teamName = c("MixTem",
                                                        "LMS",
                                                        "Victim",
                                                        "Blutgrätsche",
                                                        "Kmikze Eulen",
                                                        "leere Menge",
                                                        "GÄG",
                                                        "WoR",
                                                        "Bildungsurlub",
                                                        "Pompfenjäger",
                                                        "FKK",
                                                        "Torpedo",
                                                        "Keiler",
                                                        "Mixerei"),
                                           teamCity = c("","","","","","","","Rotenburg","","","","","Oldenburg",""),
                                           stringsAsFactors = FALSE),
                        rounds = data.frame(round = as.integer(c(rep(1, 7), rep(2, 7))),
                                            game = as.integer(c(1:7, 1:7)),
                                            entryId1  = as.integer(c(13,  2, 14,  5,  4, 10,  7,  6,  4, 12, 14, 11,  9,  1)),
                                            entryId2  = as.integer(c( 8,  9, 11,  6,  3, 12,  1,  7,  2, 13,  8, 10,  3,  5)),
                                            points1   = as.integer(c(10, 13,  9,  2, 15,  4, 18, 10,  7, 11,  8,  8,  4, 11)),
                                            points2   = as.integer(c( 4,  1,  8, 18,  2, 12,  3, 10,  9, 10,  7,  3, 11,  5)),
                                            finished  = rep(TRUE, 14)))
class(exampleResults) <- "SwissTournament"

partialResults <- list(tournamentVersion = as.integer(1),
                       scoreCalculation = "TwoPoints",
                       rankingComparator = "Buchholz",
                       teams = data.frame(entryId = as.integer(1:4),
                                          teamName = paste("T", strtoi(1:4), sep = ""),
                                          teamCity = paste("S", strtoi(1:4), sep = ""),
                                          stringsAsFactors = FALSE),
                       rounds = data.frame(round = as.integer(1),
                                           game = as.integer(1:2),
                                           entryId1 = as.integer(c(1,3)),
                                           entryId2 = as.integer(c(2,4)),
                                           points1 = as.integer(1),
                                           points2 = as.integer(0),
                                           finished = c(TRUE, FALSE)))
class(partialResults) <- "SwissTournament"

# using stubs to check correct output
test_that("checks on valid data output", {
  expect_silent(readSwissTournament(file.path("testSWISSfiles","valid1")))
  expect_s3_class(readSwissTournament(file.path("testSWISSfiles","valid1")), "SwissTournament")
  expect_identical(lapply(readSwissTournament(file.path("testSWISSfiles","valid1")), class), list(tournamentVersion = "integer", scoreCalculation = "character", rankingComparator = "character", teams = "data.frame", rounds = "data.frame"))
  expect_identical(lapply(readSwissTournament(file.path("testSWISSfiles","valid1"))$teams, class), list(entryId = "integer", teamName = "character", teamCity = "character"))
  expect_identical(lapply(readSwissTournament(file.path("testSWISSfiles","valid1"))$rounds, class), list(round = "integer", game = "integer", entryId1 = "integer", entryId2 = "integer", points1 = "integer", points2 = "integer", finished = "logical"))
  expect_identical(readSwissTournament(file.path("testSWISSfiles","valid1")), emptyResults)
  expect_identical(readSwissTournament(file.path("testSWISSfiles","valid2")), exampleResults)
  expect_identical(readSwissTournament(file.path("testSWISSfiles","valid3")), partialResults)
})

test_that("checks on correctness of data structure", {
  tournament <- readSwissTournament(file.path("testSWISSfiles","valid2"))
  expect_true(all(tournament$rounds$round > 0))
  expect_true(all(tournament$rounds$game > 0))
  expect_true(!any(duplicated(tournament$rounds[,c("round","game")])))
  expect_true(all(tournament$rounds$entryId1 %in% tournament$teams$entryId))
  expect_true(all(tournament$rounds$entryId2 %in% tournament$teams$entryId))
  expect_true(all(tournament$rounds$points1 >= 0))
  expect_true(all(tournament$rounds$points2 >= 0))

  tournament <- readSwissTournament(file.path("testSWISSfiles","valid3"))
  expect_true(all(tournament$rounds$round > 0))
  expect_true(all(tournament$rounds$game > 0))
  expect_true(!any(duplicated(tournament$rounds[,c("round","game")])))
  expect_true(all(tournament$rounds$entryId1 %in% tournament$teams$entryId))
  expect_true(all(tournament$rounds$entryId2 %in% tournament$teams$entryId))
  expect_true(all(tournament$rounds$points1 >= 0))
  expect_true(all(tournament$rounds$points2 >= 0))
})

context("SWISS-tournament auxiliary helper functions")
test_that("checks correctness scoring helper functions", {
  ## score functions
  expect_error(scoreInvalid(TRUE,FALSE), regexp = "Unknown scoring function. Please review your data.")
  expect_error(scoreInvalid("dog",TRUE), regexp = "is.logical(won) is not TRUE", fixed = TRUE)
  expect_error(scoreInvalid(TRUE,"cat"), regexp = "is.logical(draw) is not TRUE", fixed = TRUE)
  expect_error(scoreInvalid(TRUE,TRUE), regexp = "Elements 1 of !(won & draw) are not true", fixed = TRUE)

  expect_error(scoreKO("dog",TRUE), regexp = "is.logical(won) is not TRUE", fixed = TRUE)
  expect_error(scoreKO(TRUE,"cat"), regexp = "is.logical(draw) is not TRUE", fixed = TRUE)
  expect_error(scoreKO(TRUE,TRUE), regexp = "Elements 1 of !(won & draw) are not true", fixed = TRUE)

  expect_error(scoreTwoPoints("dog",TRUE), regexp = "is.logical(won) is not TRUE", fixed = TRUE)
  expect_error(scoreTwoPoints(TRUE,"cat"), regexp = "is.logical(draw) is not TRUE", fixed = TRUE)
  expect_error(scoreTwoPoints(TRUE,TRUE), regexp = "Elements 1 of !(won & draw) are not true", fixed = TRUE)

  expect_error(scoreThreePoints("dog",TRUE), regexp = "is.logical(won) is not TRUE", fixed = TRUE)
  expect_error(scoreThreePoints(TRUE,"cat"), regexp = "is.logical(draw) is not TRUE", fixed = TRUE)
  expect_error(scoreThreePoints(TRUE,TRUE), regexp = "Elements 1 of !(won & draw) are not true", fixed = TRUE)

  expect_type(scoreKO(TRUE,FALSE), "integer")
  expect_type(scoreTwoPoints(TRUE,FALSE), "integer")
  expect_type(scoreThreePoints(TRUE,FALSE), "integer")
  # expect_type(scoreKO(rep(TRUE,10),rep(FALSE,10)), "integer")
  # expect_type(scoreTwoPoints(rep(TRUE,10),rep(FALSE,10)), "integer")
  # expect_type(scoreThreePoints(rep(TRUE,10),rep(FALSE,10)), "integer")

  expect_silent(scoreKO(TRUE,FALSE))
  expect_silent(scoreTwoPoints(TRUE,FALSE))
  expect_silent(scoreThreePoints(TRUE,FALSE))

  expect_equal(scoreKO(FALSE,FALSE), as.integer(0))
  expect_equal(scoreKO(FALSE,TRUE), as.integer(0))
  expect_equal(scoreKO(TRUE,FALSE), as.integer(1))

  expect_equal(scoreTwoPoints(FALSE,FALSE), as.integer(0))
  expect_equal(scoreTwoPoints(FALSE,TRUE), as.integer(1))
  expect_equal(scoreTwoPoints(TRUE,FALSE), as.integer(2))

  expect_equal(scoreThreePoints(FALSE,FALSE), as.integer(0))
  expect_equal(scoreThreePoints(FALSE,TRUE), as.integer(1))
  expect_equal(scoreThreePoints(TRUE,FALSE), as.integer(3))

  for (l in c(1,2,5,10,100)) {
    expect_length(scoreKO(rep(FALSE,l),rep(FALSE,l)), l)
    expect_length(scoreTwoPoints(rep(FALSE,l),rep(FALSE,l)), l)
    expect_length(scoreThreePoints(rep(FALSE,l),rep(FALSE,l)), l)
  }
})

test_that("checks correctness comparator helper functions", {
  ## comparator functions
  expect_error(comparatorInvalid(TRUE), regexp = "games is not a data frame")
  expect_error(comparatorInvalid(data.frame()), regexp = "nrow(games) not greater than 0", fixed = TRUE)
  expect_error(comparatorInvalid(data.frame(entryId = 1:14)), regexp = 'Elements 1, 2, 3, 4 of c("scoreCum", "BHZ", "pointsDiffCum", "pointsCum") %in% colnames(games) are not true', fixed = TRUE)
  expect_error(comparatorInvalid(data.frame(entryId = 1:14, scoreCum = 'ui', BHZ = 'ui', pointsDiffCum = 'ui', pointsCum = 'ui'))) #, regexp = "")
  expect_error(comparatorInvalid(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)), regexp = "Unknown ranking comparator. Please review your data.")

  expect_error(comparatorBuchholzzahl(TRUE), regexp = "games is not a data frame")
  expect_error(comparatorBuchholzzahl(data.frame()), regexp = "nrow(games) not greater than 0", fixed = TRUE)
  expect_error(comparatorBuchholzzahl(data.frame(entryId = 1:14)), regexp = 'Elements 1, 2, 3, 4 of c("scoreCum", "BHZ", "pointsDiffCum", "pointsCum") %in% colnames(games) are not true', fixed = TRUE)
  expect_error(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 'ui')))
  expect_silent(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)))
  expect_type(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)), "integer")
  for (l in as.integer(c(1,2,5,10,100))) {
    expect_length(comparatorBuchholzzahl(data.frame(entryId = as.integer(1:l), scoreCum = as.integer(1 + (1:l)), BHZ = as.integer(1 + (1:l)), pointsDiffCum = as.integer(3 + (1:l)), pointsCum = as.integer(4 + (1:l)))), l)
    expect_identical(comparatorBuchholzzahl(data.frame(entryId = as.integer(1:l), scoreCum = as.integer(1 + (1:l)), BHZ = as.integer(1 + (1:l)), pointsDiffCum = as.integer(3 + (1:l)), pointsCum = as.integer(4 + (1:l)))), l:1)
  }
})

############
context("SWISS-tournament analysis functions")
test_that("checks correctness of the analysis and print functions", {
  expect_output(print.SwissTournament(exampleResults))
  expect_output(print.SwissTournament(emptyResults))
  expect_silent(summary.SwissTournament(exampleResults))
  expect_silent(summary.SwissTournament(emptyResults))
  expect_output(print.SwissTournamentSummary(summary(exampleResults)))
  expect_output(print.SwissTournamentSummary(summary(emptyResults)))
  expect_s3_class(summary(exampleResults), c("SwissTournament", "SwissTournamentSummary"))

  tournamentsummary <- summary(exampleResults)
  expect_identical(lapply(tournamentsummary, class), list(tournamentVersion = "integer", scoreCalculation = "character", rankingComparator = "character", teams = "data.frame", rounds = "data.frame", rankings = "data.frame", stats = "list"))
  expect_identical(lapply(tournamentsummary$rankings, class), list(round = "integer", rank = "integer", entryId = "integer", opp = "list", scoreCum = "integer", pointsCum = "integer", pointsDiffCum = "integer", BHZ = "integer"))
  expect_identical(lapply(tournamentsummary$stats, class), list(teamsParticipating = "integer", roundsPlayed = "integer", roundsScheduled = "integer", matchesPlayed = "integer", matchesScheduled = "integer"))

  expect_true(nrow(tournamentsummary$teams) == tournamentsummary$stats$teamsParticipating)

  expect_true(all(tournamentsummary$rankings$round %in% 1:tournamentsummary$stats$roundsScheduled))
  expect_true(all(tournamentsummary$rankings$rank %in% 0:tournamentsummary$stats$teamsParticipating))
  expect_true(all(tournamentsummary$rankings$entryId %in% tournamentsummary$teams$entryId))
  expect_true(all(unlist(tournamentsummary$rankings$opp) %in% c(NA, tournamentsummary$teams$entryId))) # NA if no game was played

  ##
  tournamentsummary <- summary(emptyResults)
  expect_identical(lapply(tournamentsummary, class), list(tournamentVersion = "integer", scoreCalculation = "character", rankingComparator = "character", teams = "data.frame", rounds = "data.frame", rankings = "data.frame", stats = "list"))
  expect_identical(lapply(tournamentsummary$rankings, class), list(round = "integer", rank = "integer", entryId = "integer", opp = "list", scoreCum = "integer", pointsCum = "integer", pointsDiffCum = "integer", BHZ = "integer"))
  expect_identical(lapply(tournamentsummary$stats, class), list(teamsParticipating = "integer", roundsPlayed = "integer", roundsScheduled = "integer", matchesPlayed = "integer", matchesScheduled = "integer"))

  expect_true(nrow(tournamentsummary$teams) == tournamentsummary$stats$teamsParticipating)

  expect_true(all(tournamentsummary$rankings$round %in% 1:tournamentsummary$stats$roundsScheduled))
  expect_true(all(tournamentsummary$rankings$rank %in% 0:tournamentsummary$stats$teamsParticipating))
  expect_true(all(tournamentsummary$rankings$entryId %in% tournamentsummary$teams$entryId))
  expect_true(all(unlist(tournamentsummary$rankings$opp) %in% c(NA, tournamentsummary$teams$entryId))) # NA if no game was played

  ##
  tournamentsummary <- summary(partialResults)
  expect_identical(lapply(tournamentsummary, class), list(tournamentVersion = "integer", scoreCalculation = "character", rankingComparator = "character", teams = "data.frame", rounds = "data.frame", rankings = "data.frame", stats = "list"))
  expect_identical(lapply(tournamentsummary$rankings, class), list(round = "integer", rank = "integer", entryId = "integer", opp = "list", scoreCum = "integer", pointsCum = "integer", pointsDiffCum = "integer", BHZ = "integer"))
  expect_identical(lapply(tournamentsummary$stats, class), list(teamsParticipating = "integer", roundsPlayed = "integer", roundsScheduled = "integer", matchesPlayed = "integer", matchesScheduled = "integer"))

  expect_true(nrow(tournamentsummary$teams) == tournamentsummary$stats$teamsParticipating)

  expect_true(all(tournamentsummary$rankings$round %in% 1:tournamentsummary$stats$roundsScheduled))
  expect_true(all(tournamentsummary$rankings$rank %in% 0:tournamentsummary$stats$teamsParticipating))
  expect_true(all(tournamentsummary$rankings$entryId %in% tournamentsummary$teams$entryId))
  expect_true(all(unlist(tournamentsummary$rankings$opp) %in% c(NA, tournamentsummary$teams$entryId))) # NA if no game was played
})
