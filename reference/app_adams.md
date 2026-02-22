# Apportion by the Adams Method

A divisor method that uses ceiling (round-up) rounding, proposed by John
Quincy Adams. It was never adopted for US Congressional apportionment.

## Usage

``` r
app_adams(size, pop)
```

## Arguments

- size:

  An integer number of seats to apportion across units, or a vector of
  numbers of seats, one for each column of `pop`. Must be non-negative.

- pop:

  A vector or matrix of population sizes for each unit. If a matrix is
  provided, the apportionment algorithm is applied columnwise: each row
  is a unit and each column is a replicate. For example, with
  congressional apportionment, the matrix would have 50 rows and as many
  columns as hypothetical census population scenarios.

## Value

An integer vector or matrix of the same dimensions as `pop`, containing
the number of seats apportioned to each unit.

## Details

The Adams method finds a common divisor \\d\\ such that ceiling-rounded
quotients sum to the desired house size: \$\$\sum\_{i} \left\lceil
\frac{p_i}{d} \right\rceil = H\$\$ where \\p_i\\ is the population of
unit \\i\\ and \\H\\ is the total number of seats (`size`). The divisor
\\d\\ is adjusted iteratively until this condition is met.

Because every non-zero fractional remainder is always rounded up, the
Adams method is the most generous to small units among the classical
divisor methods. Any unit with a positive population receives at least
one seat, which can over-represent small states or parties relative to
their population share.

## Examples

``` r
app_adams(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 50  8  5  2 27 14  2  3 16  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  2  9 37  5  1 11 10  3  8  1
```
