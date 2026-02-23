# Apportion by the D'Hondt (Jefferson, greatest divisors) Method

A sequential priority method widely used for proportional representation
elections, including in Belgium, Spain, Portugal, the Netherlands,
Austria, and many other countries. Mathematically equivalent to the
Jefferson method, a procedure described by Thomas Jefferson, and also
known as the greatest divisors method. Compared with the
Webster/Sainte-Laguë method, D'Hondt/Jefferson tends to give a slight
advantage to larger units.

## Usage

``` r
app_dhondt(size, pop, init = NULL)

app_jefferson(size, pop, init = NULL)
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

The D'Hondt method allocates seats sequentially. At each step, the next
seat is awarded to the unit (party or state) with the highest quotient:
\$\$Q_i = \frac{p_i}{n_i + 1}\$\$ where \\p_i\\ is the population or
vote total of unit \\i\\ and \\n_i\\ is the number of seats it currently
holds. This process repeats until all `size` seats have been awarded.

The Jefferson method finds a common divisor \\d\\ such that
floor-rounded quotients sum to the desired house size: \$\$\sum\_{i}
\left\lfloor \frac{p_i}{d} \right\rfloor = H\$\$ where \\p_i\\ is the
population of unit \\i\\ and \\H\\ is the total number of seats
(`size`). The divisor \\d\\ is decreased iteratively until this
condition is met.

The Jefferson method is mathematically equivalent to the D'Hondt method:
both produce the same allocation. The Jefferson divisor formulation and
the sequential D'Hondt priority \\p_i / (n_i + 1)\\ are two perspectives
on the same apportionment rule.

## Examples

``` r
app_dhondt(size = 435, pop = state_2020$pop)
#>  [1]  6  1  9  4 54  8  5  1 29 14  2  2 17  9  4  4  6  6  1  8  9 14  7  4  8
#> [26]  1  2  4  1 12  2 28 14  1 16  5  5 18  1  7  1  9 40  4  0 12 10  2  8  0
```
