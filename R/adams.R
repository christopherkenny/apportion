#' Apportion by the Adams Method
#'
#' A divisor method that uses ceiling (round-up) rounding, proposed by John
#' Quincy Adams. It was never adopted for US Congressional apportionment.
#'
#' @details
#' The Adams method finds a common divisor \eqn{d} such that ceiling-rounded
#' quotients sum to the desired house size:
#' \deqn{\sum_{i} \left\lceil \frac{p_i}{d} \right\rceil = H}
#' where \eqn{p_i} is the population of unit \eqn{i} and \eqn{H} is the total
#' number of seats (`size`). The divisor \eqn{d} is adjusted iteratively until
#' this condition is met.
#'
#' Because every non-zero fractional remainder is always rounded up, the Adams
#' method is the most generous to small units among the classical divisor
#' methods. Any unit with a positive population receives at least one seat,
#' which can over-represent small states or parties relative to their population
#' share.
#'
#' @param size An integer number of seats to apportion across units
#' @param pop A vector or matrix of population sizes for each unit. If a matrix
#'   is provided, the apportionment algorithm is applied columnwise:
#'   each row is a unit and each column is a replicate. For example, with
#'   congressional apportionment, the matrix would have 50 rows and as many
#'   columns as hypothetical census population scenarios.
#'
#' @return An integer vector of the same length as `pop` with the number of
#'   seats apportioned to each unit.
#'
#' @examples
#' app_adams(size = 435, pop = state_2020$pop)
#' @export
app_adams <- function(size, pop) {
  apprt <- run_adams(as.integer(size), as.matrix(pop))
  restore_app(apprt, pop)
}

run_adams <- quickr::quick(
  function(n_tot, pop) {
    declare(type(n_tot = integer(1)), type(pop = double(NA, NA)))
    out = matrix(0L, nrow = nrow(pop), ncol = ncol(pop))

    for (k in seq_len(ncol(pop))) {
      div <- floor(sum(pop[, k]) / n_tot)

      apprt <- ceiling(pop[, k] / div)
      rem <- n_tot - sum(apprt)

      while (rem != 0) {
        diff <- ifelse(rem < 0L, 1L, -1L)
        div <- div + diff
        apprt <- ceiling(pop[, k] / div)
        rem <- n_tot - sum(apprt)
      }

      out[, k] = apprt
    }

    out
  },
  name = "adams"
)