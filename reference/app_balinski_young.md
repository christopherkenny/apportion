# Apportion by the Balinski-Young Method

A quota method that satisfies the **quota property** and avoids the
Alabama paradox, proposed by Michel Balinski and H. Peyton Young.

## Usage

``` r
app_balinski_young(size, pop, init = NULL)
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

## Value

An integer vector or matrix of the same dimensions as `pop`, containing
the number of seats apportioned to each unit.

## Details

Let the exact quota for unit \\i\\ be: \$\$q_i = \frac{p_i \cdot
H}{P}\$\$ where \\p_i\\ is the population of unit \\i\\, \\H\\ is the
total number of seats (`size`), and \\P = \sum_i p_i\\ is the total
population.

The method guarantees the *quota property*: each unit receives either
\\\lfloor q_i \rfloor\\ or \\\lceil q_i \rceil\\ seats, so no unit is
over- or under-represented by more than one seat relative to its exact
quota. Seats are awarded sequentially using the D'Hondt priority \\p_i /
(1 + n_i)\\, but with an upper quota cap: once a unit has received
\\\lceil q_i \rceil\\ seats it is excluded from further consideration.
This cap prevents the runaway over-representation that the unconstrained
Jefferson/D'Hondt method can produce, while preserving freedom from the
"Alabama paradox," in which increasing the total house size can
paradoxically cause a unit to lose a seat.

Balinski and Young proved that no apportionment method can
simultaneously satisfy the quota property and avoid all paradoxes; this
method is a practical compromise that prioritizes the quota property.

## References

Balinski, M. L., & Young, H. P. (2001). *Fair Representation: Meeting
the Ideal of One Man, One Vote* (2nd ed.). Brookings Institution Press.

## Examples

``` r
app_balinski_young(size = 435, pop = state_2020$pop)
#>  [1]  6  1  9  4 54  8  5  1 29 14  2  2 17  9  4  4  6  6  1  8  9 14  7  4  8
#> [26]  1  2  4  1 12  2 28 14  1 16  5  5 18  1  7  1  9 40  4  0 12 10  2  8  0
```
