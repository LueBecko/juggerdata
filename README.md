
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/juggerdata)](https://cran.r-project.org/package=juggerdata) [![Travis-CI Build Status](https://travis-ci.org/LueBecko/juggerdata.svg?branch=master)](https://travis-ci.org/LueBecko/juggerdata) [![Coverage status](https://img.shields.io/codecov/c/github/LueBecko/juggerdata/master.svg)](https://codecov.io/github/LueBecko/juggerdata?branch=master)

juggerdata - The package
========================

A package that was created together with the ongoing blog <https://juggerdata.blogspot.de/>

juggerdata strives to deliver a single starting point for all Jugger related data-analysis. It contains several jugger-related datasets that can used in many analysis scenarios. This package is intended to be used as a central repository to enable quick analysis and reproducability. Accordingly the data in this package is well documented and as tidy and cleaned as possible.

Licensing
---------

This package is now (starting with version 1.1.0) licensed under the MIT license. Feel free to do whatever you like with this package and the data within. No warranty can be given for any function or validity of the data.

The JTR data within the package was part of previous builds which were licensed as CC0 - public domain. Since raw data can not be properly licensed this was the only logical license. These data sets therefore remain under this license, while the newly added code will get the MIT license - just for warranty reasons.

Installation
------------

You can install juggerdata from github with:

``` r
# install.packages("devtools")
devtools::install_github("LueBecko/juggerdata")
```

Datasets
--------

This package contains the following datasets:

-   `JTR`: JTR-data from the years 2009 till early 2017 (<http://turniere.jugger.org/>, thanks to Jan 'Ace' Schliep)
    -   `JTR.Tournaments`: A comprehensive list of tournaments and their meta-data
    -   `JTR.Teams`: A comprehensive list of registered teams and their meta-data
    -   `JTR.Results`: A comprehensive list of tournament participation and results
    -   `JTR.jtr`: The above datasets joined together into one

The package will be updated on a regular basis and more datasets will follow.

Quick-Start:

``` r
library(juggerdata)

# number of teams and tournaments?
nrow(JTR.Teams)
nrow(JTR.Tournaments)

# first ever tournament on record?
min(JTR.Tournaments$TournamentStart)
```

Datasets are loaded into your workspace when you first invoke them.

Functions
---------

This packages also delivers a number of function for dealing with various Jugger and data related tasks.

-   `readSwissTournament`: a function to read result files from a software that was used to manage jugger tournaments with a swiss system (it actually contains also a powerful summary function and two S3 classes that allow accessing all results and rankings of such an tournament)

More functionality is sure to come.
