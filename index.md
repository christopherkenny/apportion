# apportion

`apportion` provides different apportionment methods for allocating
seats across states. These methods are also sometimes used for
allocating seats by votes in proportional representation systems.

## Installation

You can install `apportion` with:

``` r
install.packages("apportion")
```

## Example

`apportion` has several apportionment methods, each of which take two
required arguments: - `size`: the number of seats to apportion across
units - `pop`: population sizes or proportions for each unit

Several methods also support an `init` argument for applying
apportionment on top of an initial allocation.

``` r
library(apportion)
## basic example code
data('state_2020')

app_huntington_hill(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  2 17  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  1  9 38  4  1 11 10  2  8  1
```

Implemented methods:

- the Adams method
  ([`app_adams()`](https://christopherkenny.github.io/apportion/reference/app_adams.md))
- the Balinski Young method
  ([`app_balinski_young()`](https://christopherkenny.github.io/apportion/reference/app_balinski_young.md))
- the Dean method
  ([`app_dean()`](https://christopherkenny.github.io/apportion/reference/app_dean.md))
- the D’Hondt (Jefferson, greatest divisors) method
  ([`app_dhondt()`](https://christopherkenny.github.io/apportion/reference/app_dhondt.md)
  and
  [`app_jefferson()`](https://christopherkenny.github.io/apportion/reference/app_dhondt.md))
- the Hamilton-Vinton (Hare) method
  ([`app_hamilton_vinton()`](https://christopherkenny.github.io/apportion/reference/app_hamilton_vinton.md))
- the Huntington-Hill (Equal proportions) method
  ([`app_huntington_hill()`](https://christopherkenny.github.io/apportion/reference/app_huntington_hill.md))
- the Webster (Sainte-Laguë) method
  ([`app_webster()`](https://christopherkenny.github.io/apportion/reference/app_webster.md))
