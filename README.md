
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apportion <a href="https://christopherkenny.github.io/apportion/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/apportion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christopherkenny/apportion/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`apportion` provides different apportionment methods for allocating
seats across states. These methods are also sometimes used for
allocating seats by votes in proportional representation systems.

## Installation

You can install the development version of `apportion` like so:

``` r
remotes::install_github('christopherkenny/apportion')
```

## Example

`apportion` has several apportionment methods, each of which take two
required arguments: - `size`: the number of seats to apportion across
units - `pop`: a vector of population sizes for each unit

``` r
library(apportion)
## basic example code
data('state_2020')

app_adams(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 50  8  5  2 27 14  2  3 16  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  2  9 37  5  1 11 10  3  8  1
```

Implemented methods include:

- the Adams Method (`app_adams()`)
- the Balinski Young Method (`app_balinski_young()`)
- the Dean Method (`app_dean()`)
- the D’Hondt Method (`app_dhondt()`)
- the Hamilton-Vinton Method (`app_hamilton_vinton()`)
- the Huntington-Hill Method (`app_huntington_hill()`)
- the Jefferson Method (`app_jefferson()`)
- the Webster Method (`app_webster()`)
