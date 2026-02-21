# Apportion by the Dean Method

A divisor method that rounds at the harmonic mean of consecutive
integers, proposed by astronomer James Dean. It was never adopted for US
Congressional apportionment.

## Usage

``` r
app_dean(size, pop)
```

## Arguments

- size:

  An integer number of seats to apportion across units

- pop:

  A vector of population sizes for each unit

## Value

An integer vector of the same length as `pop` with the number of seats
apportioned to each unit.

## Details

The Dean method finds a common divisor \\d\\ and rounds each quotient at
the **harmonic mean** of the two surrounding integers. A quotient \\p_i
/ d\\ that falls between integers \\n\\ and \\n + 1\\ is rounded up to
\\n + 1\\ if it exceeds the harmonic mean: \$\$H(n,\\ n+1) =
\frac{2n(n+1)}{2n+1}\$\$ and rounded down to \\n\\ otherwise. The
divisor is adjusted iteratively until the total allocation equals
`size`.

Among the classical divisor methods, the Dean method minimizes the
absolute difference in **average district population** (i.e., \\p_i /
s_i\\, where \\s_i\\ is the number of seats awarded to unit \\i\\)
between any two units. It falls between the Webster and Huntington-Hill
methods in its treatment of small versus large units, giving slightly
more seats to small units than Webster but fewer than Huntington-Hill.

## Examples

``` r
app_dean(size = 435, pop = state_2020$pop)
#>  [1]  7  1  9  4 52  8  5  1 28 14  2  3 17  9  4  4  6  6  2  8  9 13  7  4  8
#> [26]  2  3  4  2 12  3 26 14  1 15  5  6 17  2  7  1  9 38  4  1 11 10  2  8  1
```
