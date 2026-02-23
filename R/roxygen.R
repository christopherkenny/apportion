template_var_size <- function() { # nocov start
  "An integer number of seats to apportion across units, or a vector of numbers
  of seats, one for each column of `pop`. Must be non-negative."
} # nocov end

template_var_pop <- function() { # nocov start
  "A vector or matrix of population sizes or proportions for each unit.
  If a matrix is provided, the apportionment algorithm is applied columnwise:
  each row is a unit and each column is a replicate. For example, with
  congressional apportionment, the matrix would have 50 rows and as many columns
  as hypothetical census population scenarios."
} # nocov end

template_var_init <- function() { # nocov start
  "A vector or matrix of the same size as `pop` with the initial number of seats
  allocated to each unit. Defaults to zero for all units."
} # nocov end

template_var_return <- function() { # nocov start
  "An integer vector or matrix of the same dimensions as `pop`, containing the
  number of seats apportioned to each unit."
} # nocov end
