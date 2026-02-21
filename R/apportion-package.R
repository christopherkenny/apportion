#' @keywords internal
"_PACKAGE"

#' @param size An integer number of seats to apportion across units, or a vector
#'   of numbers of seats, one for each column of `pop`. Must be non-negative.
#' @param pop A vector or matrix of population sizes for each unit. If a matrix
#'   is provided, the apportionment algorithm is applied columnwise:
#'   each row is a unit and each column is a replicate. For example, with
#'   congressional apportionment, the matrix would have 50 rows and as many
#'   columns as hypothetical census population scenarios.
#' @param init A vector or matrix of the same size as `pop` with the initial
#'   number of seats allocated to each unit. Defaults to zero for all units.
#'
#' @return An integer vector or matrix of the same dimensions as `pop`,
#'   containing the number of seats apportioned to each unit.
#'
#' @name app-params
NULL

## usethis namespace: start
#' @importFrom utils globalVariables
#' @useDynLib apportion, .registration = TRUE
## usethis namespace: end
NULL

globalVariables(c("type"))

restore_app = function(output, input) {
  dim(output) <- dim(input)
  if (is.matrix(output) && (!is.null(rownames(input)) || !is.null(colnames(input)))) {
    dimnames(output) <- dimnames(input)
  } else if (!is.null(names(input))) {
    names(output) <- names(input)
  }
  output
}

make_init = function(init, pop) {
  if (is.null(init)) {
    if (is.matrix(pop)) {
      matrix(0L, nrow(pop), ncol(pop))
    } else {
      matrix(0L, length(pop), 1)
    }
  } else {
    as.matrix(init)
  }
}

make_size = function(size, pop) {
  if (length(size) == 1L) {
    rep(as.integer(size), if (is.matrix(pop)) ncol(pop) else 1L)
  } else {
    as.integer(size)
  }
}