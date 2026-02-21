#' Apportion by the Dean Method
#'
#' A divisor method that rounds at the harmonic mean of consecutive integers.
#'
#' @details
#' The Dean method finds a common divisor \eqn{d} and rounds each quotient at
#' the harmonic mean of the two surrounding integers. A quotient
#' \eqn{p_i / d} that falls between integers \eqn{n} and \eqn{n + 1} is
#' rounded up to \eqn{n + 1} if it exceeds the harmonic mean:
#' \deqn{H(n,\, n+1) = \frac{2n(n+1)}{2n+1}}
#' and rounded down to \eqn{n} otherwise. The divisor is adjusted iteratively
#' until the total allocation equals `size`.
#'
#' Among the classical divisor methods, the Dean method minimizes the absolute
#' difference in average district population (i.e., \eqn{p_i / s_i}, where
#' \eqn{s_i} is the number of seats awarded to unit \eqn{i}) between any two
#' units. It falls between the Webster and Huntington-Hill methods in its
#' treatment of small versus large units, giving slightly more seats to small
#' units than Webster but fewer than Huntington-Hill.
#'
#' @inheritParams app-params
#' @inherit app-params return
#'
#' @examples
#' app_dean(size = 435, pop = state_2020$pop)
#' @export
app_dean <- function(size, pop) {
  if (any(size < 0)) {
    stop("`size` must be non-negative.")
  }
  apprt <- run_dean(make_size(size, pop), as.matrix(pop))
  restore_app(apprt, pop)
}

run_dean <- quickr::quick(
  function(n_tot, pop) {
    declare(type(n_tot = integer(m)), type(pop = double(NA, m)))
    out <- matrix(0L, nrow = nrow(pop), ncol = ncol(pop))

    for (k in seq_len(ncol(pop))) {
      div <- floor(sum(pop[, k]) / n_tot[k])

      v <- pop[, k] / div
      fc <- 2 * floor(v) * ceiling(v) / (floor(v) + ceiling(v))
      apprt <- ifelse(v < fc, floor(v), ceiling(v))
      rem <- n_tot[k] - sum(apprt)

      while (rem != 0) {
        diff <- ifelse(rem < 0, 1L, -1L)
        div <- div + diff
        v <- pop[, k] / div
        fc <- 2 * floor(v) * ceiling(v) / (floor(v) + ceiling(v))
        apprt <- ifelse(v < fc, floor(v), ceiling(v))
        rem <- n_tot[k] - sum(apprt)
      }

      out[, k] <- apprt
    }

    out
  },
  name = "dean"
)
