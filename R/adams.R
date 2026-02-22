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
#' @param size `r template_var_size()`
#' @param pop `r template_var_pop()`
#'
#' @return `r template_var_return()`
#' @export
#'
#' @examples
#' app_adams(size = 435, pop = state_2020$pop)
app_adams <- function(size, pop) {
  if (any(size < 0)) {
    stop("`size` must be non-negative.")
  }
  apprt <- run_adams(make_size(size, pop), as.matrix(pop))
  restore_app(apprt, pop)
}

run_adams <- quick(
  function(n_tot, pop) {
    declare(type(n_tot = integer(m)), type(pop = double(NA, m)))
    out = matrix(0L, nrow = nrow(pop), ncol = ncol(pop))

    for (k in seq_len(ncol(pop))) {
      div <- floor(sum(pop[, k]) / n_tot[k])

      apprt <- ceiling(pop[, k] / div)
      rem <- n_tot[k] - sum(apprt)

      while (rem != 0) {
        diff <- ifelse(rem < 0L, 1L, -1L)
        div <- div + diff
        apprt <- ceiling(pop[, k] / div)
        rem <- n_tot[k] - sum(apprt)
      }

      out[, k] = apprt
    }

    out
  },
  name = "adams"
)
