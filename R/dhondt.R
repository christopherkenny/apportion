#' Apportion by the D'Hondt (Jefferson, greatest divisors) Method
#'
#' A sequential priority method widely used for proportional representation
#' elections, including in Belgium, Spain, Portugal, the Netherlands, Austria,
#' and many other countries. Mathematically equivalent to the Jefferson method,
#' a procedure described by Thomas Jefferson, and also known as the greatest
#' divisors method.
#' Compared with the Webster/Sainte-Laguë method, D'Hondt/Jefferson tends to
#' give a slight advantage to larger units.
#'
#' @details
#' The D'Hondt method allocates seats sequentially. At each step, the next seat
#' is awarded to the unit (party or state) with the highest quotient:
#' \deqn{Q_i = \frac{p_i}{n_i + 1}}
#' where \eqn{p_i} is the population or vote total of unit \eqn{i} and
#' \eqn{n_i} is the number of seats it currently holds. This process repeats
#' until all `size` seats have been awarded.
#'
#' The Jefferson method finds a common divisor \eqn{d} such that floor-rounded
#' quotients sum to the desired house size:
#' \deqn{\sum_{i} \left\lfloor \frac{p_i}{d} \right\rfloor = H}
#' where \eqn{p_i} is the population of unit \eqn{i} and \eqn{H} is the total
#' number of seats (`size`). The divisor \eqn{d} is decreased iteratively until
#' this condition is met.
#'
#' The Jefferson method is mathematically equivalent to the D'Hondt method: both
#' produce the same allocation. The Jefferson divisor formulation and the
#' sequential D'Hondt priority \eqn{p_i / (n_i + 1)} are two perspectives on
#' the same apportionment rule.
#'
#' @inheritParams app_adams
#' @param init A vector or matrix of the same size as `pop` with the initial
#'   number of seats allocated to each unit. Defaults to zero for all units.
#' @inherit app_adams return
#'
#' @export
#'
#' @examples
#' app_dhondt(size = 435, pop = state_2020$pop)
app_dhondt <- function(size, pop, init = NULL) {
  if (any(size < 0)) {
    stop("`size` must be positive.")
  }
  apprt <- run_dhondt(make_size(size, pop), as.matrix(pop), make_init(init, pop))
  restore_app(apprt, pop)
}

run_dhondt <- quickr::quick(
  function(n_tot, pop, apprt) {
    declare(type(n_tot = integer(m)), type(pop = double(n, m)), type(apprt = integer(n, m)))

    for (k in seq_len(ncol(pop))) {
      rem = n_tot[k] - sum(apprt[, k])
      while (rem > 0) {
        quotient <- pop[, k] / (apprt[, k] + 1)
        idx <- which.max(quotient)
        apprt[idx, k] <- apprt[idx, k] + 1L
        rem <- rem - 1L
      }
    }

    apprt
  },
  name = "dhondt"
)

#' @export
#' @rdname app_dhondt
app_jefferson <- app_dhondt
