# juggerdata - The package

A package that was created together with the ongoing blog https://juggerdata.blogspot.de/

It contains several jugger-related datasets that can used in many analysis scenarios. This package is intended to be used as a central repository to enable quick analysis and reproducability. Accordingly the data in this package is well documented and as tidy and cleaned as possible.

## Licensing

This package is licensed under the CC0 license - so it's public domain. Feel free to do whatever you like with this package and the data within. No warranty can be given for any function or validity of the data.

It would also be very nice if you cite this package and it's author as the source when you use it, but no obligation to do so arises from the license.

## Datasets
This package contains the following datasets:

* `JTR`: JTR-data from the years 2009 till early 2017 (http://turniere.jugger.org/)
    * `JTR.Tournaments`: A comprehensive list of tournaments and their meta-data
    * `JTR.Teams`: A comprehensive list of registered teams and their meta-data
    * `JTR.Results`: A comprehensive list of tournament participation and results
    * `JTR.jtr`: The above datasets joined together into one

The package will be updated on a regular basis and more datasets will follow.

## Getting started

This package is available on github and can be installed with two short lines:

```r
install.packages("devtools")
devtools::install_github("LueBecko/juggerdata")

# loading
library(juggerdata)
```

Datasets are loaded into your workspace when you first invoke them.
