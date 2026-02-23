# Apportion by the Webster (Sainte-Laguë) Method

A divisor method that uses standard arithmetic-mean rounding, used for
US Congressional apportionment in 1842 and from 1911 to 1931. Also used
in Norway and Sweden for parliamentary seat allocation (where it is
known as the Sainte-Laguë method).

## Usage

``` r
app_webster(size, pop, init = NULL)
```

## Arguments

- size:

  An integer number of seats to apportion across units, or a vector of
  numbers of seats, one for each column of `pop`. Must be non-negative.

- pop:

  A vector or matrix of population sizes or proportions for each unit.
  If a matrix is provided, the apportionment algorithm is applied
  columnwise: each row is a unit and each column is a replicate. For
  example, with congressional apportionment, the matrix would have 50
  rows and as many columns as hypothetical census population scenarios.

- init:

  A vector or matrix of the same size as `pop` with the initial number
  of seats allocated to each unit. Defaults to zero for all units.

## Value

An integer vector or matrix of the same dimensions as `pop`, containing
the number of seats apportioned to each unit.

## Details

The Webster method finds a common divisor \\d\\ such that
standard-rounded quotients sum to the desired house size: \$\$\sum\_{i}
\text{round}\\\left(\frac{p_i}{d}\right) = H\$\$ where \\p_i\\ is the
population of unit \\i\\ and \\H\\ is the total number of seats
(`size`). Quotients are rounded at the arithmetic mean \\n + 0.5\\ of
consecutive integers \\n\\ and \\n + 1\\.

Among the classical divisor methods, Webster is considered the most
statistically unbiased: it does not systematically favor either large or
small units. It minimizes the expected absolute deviation from exact
quotas when populations are drawn from a wide range of sizes.

## Examples

``` r
app_webster(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  2 17  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  1  3  4  2 12  3 27 14  1 16  5  6 17  1  7  1  9 38  4  1 11 10  2  8  1
```
