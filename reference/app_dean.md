# Apportion by the Dean Method

A divisor method that rounds at the harmonic mean of consecutive
integers.

## Usage

``` r
app_dean(size, pop)
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

The Dean method finds a common divisor \\d\\ and rounds each quotient at
the harmonic mean of the two surrounding integers. A quotient \\p_i /
d\\ that falls between integers \\n\\ and \\n + 1\\ is rounded up to
\\n + 1\\ if it exceeds the harmonic mean: \$\$H(n,\\ n+1) =
\frac{2n(n+1)}{2n+1}\$\$ and rounded down to \\n\\ otherwise. The
divisor is adjusted iteratively until the total allocation equals
`size`.

Among the classical divisor methods, the Dean method minimizes the
absolute difference in average district population (i.e., \\p_i / s_i\\,
where \\s_i\\ is the number of seats awarded to unit \\i\\) between any
two units. It falls between the Webster and Huntington-Hill methods in
its treatment of small versus large units, giving slightly more seats to
small units than Webster but fewer than Huntington-Hill.

## Examples

``` r
app_dean(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  3 17  9  4  4  6  6  2  8  9 13  7  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  1  9 38  4  1 11 10  2  8  1
```
