#' Apportion by the Balinski-Young Method
#'
#' A quota method that satisfies the **quota property** and avoids the Alabama
#' paradox, proposed by Michel Balinski and H. Peyton Young.
#'
#' @details
#' Let the exact quota for unit \eqn{i} be:
#' \deqn{q_i = \frac{p_i \cdot H}{P}}
#' where \eqn{p_i} is the population of unit \eqn{i}, \eqn{H} is the total
#' number of seats (`size`), and \eqn{P = \sum_i p_i} is the total population.
#'
#' The method guarantees the *quota property*: each unit receives either
#' \eqn{\lfloor q_i \rfloor} or \eqn{\lceil q_i \rceil} seats, so no unit is
#' over- or under-represented by more than one seat relative to its exact quota.
#' Seats are awarded sequentially using the D'Hondt priority
#' \eqn{p_i / (1 + n_i)}, but with an upper quota cap: once a unit has
#' received \eqn{\lceil q_i \rceil} seats it is excluded from further
#' consideration. This cap prevents the runaway over-representation that the
#' unconstrained Jefferson/D'Hondt method can produce, while preserving freedom
#' from the "Alabama paradox," in which increasing
#' the total house size can paradoxically cause a unit to lose a seat.
#'
#' Balinski and Young proved that no apportionment method can simultaneously
#' satisfy the quota property and avoid all paradoxes; this method is a
#' practical compromise that prioritizes the quota property.
#'
#' @inheritParams app_adams
#' @inheritParams app_dhondt
#' @inherit app_adams return
#'
#' @references
#' Balinski, M. L., & Young, H. P. (2001). *Fair Representation: Meeting the
#' Ideal of One Man, One Vote* (2nd ed.). Brookings Institution Press.
#'
#' @examples
#' app_balinski_young(size = 435, pop = state_2020$pop)
#' @export
app_balinski_young <- function(size, pop, init = NULL) {
  if (any(size < 0)) {
    stop("`size` must be positive.")
  }
  apprt <- run_balinski_young(make_size(size, pop), as.matrix(pop), make_init(init, pop))
  restore_app(apprt, pop)
}

run_balinski_young <- quickr::quick(
  function(n_tot, pop, apprt) {
    declare(type(n_tot = integer(m)), type(pop = double(n, m)), type(apprt = integer(n, m)))

    for (k in seq_len(ncol(pop))) {
      total_pop <- sum(pop[, k])
      rem <- n_tot[k] - sum(apprt[, k])

      while (rem > 0L) {
        v <- pop[, k] / (1 + apprt[, k])
        v[pop[, k] < pop[, k] * (n_tot[k] - rem) / total_pop] <- 0
        apprt[which.max(v), k] <- apprt[which.max(v), k] + 1L
        rem <- rem - 1L
      }
    }

    apprt
  },
  name = "balinski_young"
)
