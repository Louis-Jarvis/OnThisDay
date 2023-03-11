<img src="man/figures/logo.png" align="right" height="105"/>

# OnThisDay

<!-- badges: start -->
[![R-CMD-check](https://github.com/Louis-Jarvis/OnThisDay/actions/workflows/check-release.yaml/badge.svg)](https://github.com/Louis-Jarvis/OnThisDay/actions/workflows/check-release.yaml)
<!-- badges: end -->

The goal of OnThisDay is to Scrape Wikipedia's "On This Day" section and return a summary of the events.

##Installation

``` r
devtools::install(xxx.tar.gz, dependencies = TRUE)
```

Windows users will need Rtools [cran link](https://cran.r-project.org/bin/windows/Rtools/)

You can install the development version of OnThisDay like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?

# To load all the dependencies
renv::restore()
```

## Inst

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OnThisDay)

# Run This to get a fact from wikipedia!
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
#> The anime film Nausica of the Valley of the Wind by Hayao Miyazaki was released.
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
--------------------------------------------------------------------------------
```
