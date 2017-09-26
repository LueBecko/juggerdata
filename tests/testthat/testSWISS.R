library(juggerdata)
library(testthatsomemore)
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
  within_file_structure(list(invalid1 = '<<ui', invalid2 = '<elefant type="african" />'), {
    expect_error(read_SWISS_tournament(file.path(tempdir,"invalid1")), "StartTag: invalid element name \\[68\\]") # not XML
    expect_error(read_SWISS_tournament(file.path(tempdir,"invalid2")), "xml2::xml_validate\\(x = swiss, schema = schema\\) is not TRUE") # XML not in schema
  })
})

###################################
empty_tournament <- '<tournament version="1">
	<scoreCalculator type="TwoPoints"/>
<rankingComparator type="Buchholz"/>
<teams>
<team name="T1" city="S1"/>
<team name="T2" city="S2"/>
</teams>
<rounds>
<round>
<match teamA="0" teamB="1" pointsA="0" pointsB="0" finished="false"/>
</round>
</rounds>
</tournament>'
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

example_tournament <- '<tournament version="1">
<scoreCalculator type="TwoPoints"/>
<rankingComparator type="Buchholz"/>
<teams>
<team name="MixTem" city=""/>
<team name="LMS" city=""/>
<team name="Victim" city=""/>
<team name="Blutgrätsche" city=""/>
<team name="Kmikze Eulen" city=""/>
<team name="leere Menge" city=""/>
<team name="GÄG" city=""/>
<team name="WoR" city="Rotenburg"/>
<team name="Bildungsurlub" city=""/>
<team name="Pompfenjäger" city=""/>
<team name="FKK" city=""/>
<team name="Torpedo" city=""/>
<team name="Keiler" city="Oldenburg"/>
<team name="Mixerei" city=""/>
</teams>
<rounds>
<round>
<match teamA="12" teamB="7" pointsA="10" pointsB="4" finished="true"/>
<match teamA="1" teamB="8" pointsA="13" pointsB="1" finished="true"/>
<match teamA="13" teamB="10" pointsA="9" pointsB="8" finished="true"/>
<match teamA="4" teamB="5" pointsA="2" pointsB="18" finished="true"/>
<match teamA="3" teamB="2" pointsA="15" pointsB="2" finished="true"/>
<match teamA="9" teamB="11" pointsA="4" pointsB="12" finished="true"/>
<match teamA="6" teamB="0" pointsA="18" pointsB="3" finished="true"/>
</round>
<round>
<match teamA="5" teamB="6" pointsA="10" pointsB="10" finished="true"/>
<match teamA="3" teamB="1" pointsA="7" pointsB="9" finished="true"/>
<match teamA="11" teamB="12" pointsA="11" pointsB="10" finished="true"/>
<match teamA="13" teamB="7" pointsA="8" pointsB="7" finished="true"/>
<match teamA="10" teamB="9" pointsA="8" pointsB="3" finished="true"/>
<match teamA="8" teamB="2" pointsA="4" pointsB="11" finished="true"/>
<match teamA="0" teamB="4" pointsA="11" pointsB="5" finished="true"/>
</round>
</rounds>
</tournament>'
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
  within_file_structure(list(valid1 = empty_tournament, valid2 = example_tournament), {
    expect_identical(read_SWISS_tournament(file.path(tempdir,"valid1")), empty_results)
    expect_identical(read_SWISS_tournament(file.path(tempdir,"valid2")), example_results)
  })
})

