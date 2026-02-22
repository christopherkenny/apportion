# Apportion by the Huntington-Hill (Equal proportions) Method

The current method used for US Congressional apportionment, in
continuous use since the Apportionment Act of 1941. Also known as the
method of equal proportions.

## Usage

``` r
app_huntington_hill(size, pop, init = NULL, thresh = 0)
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

- init:

  A vector or matrix of the same size as `pop` with the initial number
  of seats allocated to each unit. Defaults to zero for all units.

- thresh:

  A population threshold for assigning seats. Units with population
  below this threshold receive zero seats, by default. Only affects the
  default value of `init`; if `init` is provided, `thresh` is ignored.

## Value

An integer vector or matrix of the same dimensions as `pop`, containing
the number of seats apportioned to each unit.

## Details

The Huntington-Hill method is a sequential priority method. Starting
with one seat allocated to each unit (a constitutional minimum for
Congressional apportionment), it repeatedly awards the next seat to the
unit with the highest priority value: \$\$P_i(n) =
\frac{p_i}{\sqrt{n(n + 1)}}\$\$ where \\p_i\\ is the population of unit
\\i\\ and \\n\\ is the number of seats currently held by unit \\i\\.

This is equivalent to a divisor method that rounds each quotient at the
geometric mean of the two surrounding integers: \\p_i / d\\ is rounded
up to \\n + 1\\ if it exceeds \\\sqrt{n(n+1)}\\, and down to \\n\\
otherwise. Among the divisor methods, Huntington-Hill minimizes the
maximum relative difference in representation between any two units.

## References

Huntington, E. V. (1928). The apportionment of representatives in
Congress. *Transactions of the American Mathematical Society*, 30(1),
85–110. [doi:10.2307/1989268](https://doi.org/10.2307/1989268)

## Examples

``` r
app_huntington_hill(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  2 17  9  4  4  6  6  2  8  9 13  8  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  1  9 38  4  1 11 10  2  8  1
```
