<img src="man/figures/logo.png" align="right" height="105"/>

# OnThisDay

<!-- badges: start -->

[![R-CMD-check](https://github.com/Louis-Jarvis/OnThisDay/actions/workflows/check-release.yaml/badge.svg)](https://github.com/Louis-Jarvis/OnThisDay/actions/workflows/check-release.yaml)
[![Codecov test coverage](https://codecov.io/gh/r-lib/covr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/r-lib/covr?branch=master)


<!-- badges: end -->

The goal of OnThisDay is to Scrape Wikipedia's "On This Day" section and return a summary of the events.

## Installation

Download the `tar.gz` from [github](https://github.com/Louis-Jarvis/OnThisDay). Windows users will need Rtools [(cran link here)](https://cran.r-project.org/bin/windows/Rtools/) to build the package from source.

``` r
devtools::install("./OnThisDay_1.0.0.tar.gz", dependencies = TRUE)
#or 
remotes::install_local("./OnThisDay_1.0.0.tar.gz", dependencies = True)
```

You can install the development version of `OnThisDay` like so:

``` r
install.packages(renv)
devtools::install_github("https://github.com/Louis-Jarvis/OnThisDay") # requires a github PAT

# To load all the dependencies
renv::restore()
```

## Example

``` r
library(OnThisDay)

Sys.Date()
#> "2023-03-12"

# Run This to get a daily fact from wikipedia!
getDailyFacts()

#> -- Guess What Happened On March 12! --------------------------------------------
#> Today's Date: Sunday 12 March 2023
#> 
#> -- On This Day... --
#> 
#> *  1669
#> Mount Etna in Sicily began erupting, eventually producing the largest lava flow in the volcano's history, and damaging Catania and other towns.
#> 
#> *  1843
#> During a period of activity known as the Great Eruption, Eta Carinae (pictured) briefly became the second-brightest star in the night sky.
#> 
#> *  1984
#> The anime film Nausica of the Valley of the Wind by Hayao Miyazaki was released.
#> 
#> *  2011
#> A massive earthquake and tsunami struck northeastern Japan and triggered a nuclear disaster at the Fukushima Daiichi Nuclear Power Plant.
#> 
#> *  2012
#> U.S. Army soldier Robert Bales murdered sixteen civilians and wounded six others in Kandahar Province, Afghanistan.
#> 
#> -- Famous Births/Deaths --
#> 
#> *  Benjamin Tupper
#> Born - 1738
#> 
#> *  Jane Meade Welch
#> Born - 1854
#> 
#> *  Helen Rollason
#> Born - 1956
#> 
#> See for yourself at <https://en.wikipedia.org/wiki/Main_Page>
#> -------------------------------------------------------------------------------
```
