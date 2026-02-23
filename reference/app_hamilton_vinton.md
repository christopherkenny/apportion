# Apportion by the Hamilton-Vinton Method

A largest-remainder quota method used for US Congressional apportionment
from 1850 to 1900. Also known as the Hamilton method or the Vinton
method. Equivalent to the largest remainder method using the Hare quota.

## Usage

``` r
app_hamilton_vinton(size, pop)
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

## Value

An integer vector or matrix of the same dimensions as `pop`, containing
the number of seats apportioned to each unit.

## Details

The Hamilton-Vinton method first computes the exact quota for each unit:
\$\$q_i = \frac{p_i \cdot H}{P}\$\$ where \\p_i\\ is the population of
unit \\i\\, \\H\\ is the total number of seats (`size`), and \\P =
\sum_i p_i\\ is the total population. Each unit initially receives
\\\lfloor q_i \rfloor\\ seats. Any remaining seats are awarded one at a
time to the units with the largest fractional remainders \\q_i - \lfloor
q_i \rfloor\\.

The method satisfies the *quota property*: no unit ever receives fewer
than \\\lfloor q_i \rfloor\\ or more than \\\lceil q_i \rceil\\ seats.
However, it is susceptible to the "Alabama paradox," in which increasing
the total house size can paradoxically cause a unit to lose a seat.

## Examples

``` r
app_hamilton_vinton(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  2 17  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  1  3  4  2 12  3 27 14  1 16  5  6 17  1  7  1  9 38  4  1 11 10  2  8  1
```
