# 2020 State Population Data

tibble with columns:

- `GEOID`: Federal Information Processing Standards code

- `name`: State name

- `pop`: 2020 census population

- `abb`: Two-letter postal abbreviation

## Examples

``` r
head(state_2020)
#> # A tibble: 6 × 4
#>   GEOID name            pop abb  
#>   <chr> <chr>         <dbl> <chr>
#> 1 01    Alabama     5024279 AL   
#> 2 02    Alaska       733391 AK   
#> 3 04    Arizona     7151502 AZ   
#> 4 05    Arkansas    3011524 AR   
#> 5 06    California 39538223 CA   
#> 6 08    Colorado    5773714 CO   
```
