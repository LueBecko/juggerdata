library(juggerdata)
library(xml2)

context("SWISS-tournament data import")

test_that("checks on filename input", {
  expect_error(read_SWISS_tournament(NA), regexp = "filename is not a character vector") # no char
  expect_error(read_SWISS_tournament(1), regexp = "filename is not a character vector") # no char
  expect_error(read_SWISS_tournament(""), regexp = "Path '' does not exist") # no file
  expect_error(read_SWISS_tournament(":::definitely not a filename:::"), regexp = "Path ':::definitely not a filename:::' does not exist") # no file
})

# using stubs to check correct file usage
test_that("checks on XML to process", {
  expect_error(read_SWISS_tournament(file.path("testSWISSfiles","invalid1")), "StartTag: invalid element name \\[68\\]") # not XML
  expect_error(read_SWISS_tournament(file.path("testSWISSfiles","invalid2")), "xml2::xml_validate\\(x = swiss, schema = schema\\) is not TRUE") # XML not in schema
})

###################################
empty_results <- list(tournament_version = as.integer(1),
                      score_calculation = "TwoPoints",
                      ranking_comparator = "Buchholz",
                      teams = data.frame(entry_id = as.integer(1:2),
                                         team_name = as.factor(paste("T", strtoi(1:2), sep = "")),
                                         team_city = as.factor(paste("S", strtoi(1:2), sep = ""))),
                      rounds = data.frame(round = as.integer(1),
                                          game = as.integer(1),
                                          entry_id1 = as.integer(1),
                                          entry_id2 = as.integer(2),
                                          points1 = as.integer(0),
                                          points2 = as.integer(0),
                                          finished = FALSE))

example_results <- list(tournament_version = as.integer(1),
                        score_calculation = "TwoPoints",
                        ranking_comparator = "Buchholz",
                        teams = data.frame(entry_id = as.integer(1:14),
                                           team_name = as.factor(c("MixTem",
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
                                           team_city = as.factor(c("","","","","","","","Rotenburg","","","","","Oldenburg",""))),
                        rounds = data.frame(round = as.integer(c(rep(1, 7), rep(2, 7))),
                                            game = as.integer(c(1:7, 1:7)),
                                            entry_id1 = as.integer(c(13,  2, 14,  5,  4, 10,  7,  6,  4, 12, 14, 11,  9,  1)),
                                            entry_id2 = as.integer(c( 8,  9, 11,  6,  3, 12,  1,  7,  2, 13,  8, 10,  3,  5)),
                                            points1   = as.integer(c(10, 13,  9,  2, 15,  4, 18, 10,  7, 11,  8,  8,  4, 11)),
                                            points2   = as.integer(c( 4,  1,  8, 18,  2, 12,  3, 10,  9, 10,  7,  3, 11,  5)),
                                            finished  = rep(TRUE, 14)))


# using stubs to check correct output
test_that("checks on valid data output", {
  expect_identical(read_SWISS_tournament(file.path("testSWISSfiles","valid1")), empty_results)
  expect_identical(read_SWISS_tournament(file.path("testSWISSfiles","valid2")), example_results)
})

