
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
                                        teamName = as.factor(paste("T", strtoi(1:2), sep = "")),
                                        teamCity = as.factor(paste("S", strtoi(1:2), sep = ""))),
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
                                           teamName = as.factor(c("MixTem",
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
                                                                  "Mixerei")),
                                           teamCity = as.factor(c("","","","","","","","Rotenburg","","","","","Oldenburg",""))),
                        rounds = data.frame(round = as.integer(c(rep(1, 7), rep(2, 7))),
                                            game = as.integer(c(1:7, 1:7)),
                                            entryId1  = as.integer(c(13,  2, 14,  5,  4, 10,  7,  6,  4, 12, 14, 11,  9,  1)),
                                            entryId2  = as.integer(c( 8,  9, 11,  6,  3, 12,  1,  7,  2, 13,  8, 10,  3,  5)),
                                            points1   = as.integer(c(10, 13,  9,  2, 15,  4, 18, 10,  7, 11,  8,  8,  4, 11)),
                                            points2   = as.integer(c( 4,  1,  8, 18,  2, 12,  3, 10,  9, 10,  7,  3, 11,  5)),
                                            finished  = rep(TRUE, 14)))
class(exampleResults) <- "SwissTournament"

# using stubs to check correct output
test_that("checks on valid data output", {
  expect_silent(readSwissTournament(file.path("testSWISSfiles","valid1")))
  expect_s3_class(readSwissTournament(file.path("testSWISSfiles","valid1")), "SwissTournament")
  expect_identical(readSwissTournament(file.path("testSWISSfiles","valid1")), emptyResults)
  expect_identical(readSwissTournament(file.path("testSWISSfiles","valid2")), exampleResults)
})

context("SWISS-tournament functions")
#### Aux helper functions (they are only called internally so no explicit asserts of data types is performed within)
test_that("checks correctness of the helper functions", {
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

  ## comparator functions
  expect_error(comparatorInvalid(TRUE))
  expect_error(comparatorInvalid(data.frame()))
  expect_error(comparatorInvalid(data.frame(entryId = 1:14)))
  expect_error(comparatorInvalid(data.frame(entryId = 1:14, scoreCum = 'ui')))
  expect_error(comparatorInvalid(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)))

  expect_error(comparatorBuchholzzahl(TRUE))
  expect_error(comparatorBuchholzzahl(data.frame()))
  expect_error(comparatorBuchholzzahl(data.frame(entryId = 1:14)))
  expect_error(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 'ui')))
  expect_silent(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)))
  expect_type(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)), "integer")
  expect_length(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)), 14)
  expect_identical(comparatorBuchholzzahl(data.frame(entryId = 1:14, scoreCum = 2:15, BHZ = 3:16, pointsDiffCum = 4:17, pointsCum = 5:18)), 14:1)
})

