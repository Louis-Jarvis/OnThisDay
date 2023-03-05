<div style="text-align:center">
  <img src="OnThisDay-logo.png" width="25%"/>
</div>

# OnThisDay

<!-- badges: start -->

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

#> -- Guess What Happened On This Very Day! 
#>----------------------------------------------
#>
#> -- Festivals / National Days of Importance / Holidays --
#>
#> March 5: Learn from Lei Feng Day in China; St Piran's Day in Cornwall, England
#>
#> -- On This Day... --
#>
#> *  1279
#> The Livonian branch of the Teutonic Order suffered a great loss when 71 knights died in the Battle of
#> Aizkraukle.
#>
#> *  1824
#> The First Anglo-Burmese War, the longest and most expensive war in British Indian history, began.
#>
#> *  1966
#> BOAC Flight 911 disintegrated and crashed near Mount Fuji shortly after departure from Tokyo International
#> Airport, killing all 113 passengers and 11 crew members on board.
#>
#> *  1975
#> Computer hobbyists in Silicon Valley held the first meeting of the Homebrew Computer Club (founder
#> pictured), whose members went on to have great influence on the development of the personal computer.
#>
#>-- Famous People --
#>
#> *  Gerardus Mercator
#> Born On This Day
#>
#> *  Alessandro Volta
#> Died On This Day
#>
#> *  Elaine Paige
#> Born On This Day
#>
#> See for yourself at <https://en.wikipedia.org/wiki/Main_Page>
#> --------------------------------------------------------------------------------------
```
