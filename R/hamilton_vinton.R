#' Apportion by the Hamilton-Vinton Method
#'
#' A largest-remainder quota method used for US Congressional apportionment
#' from 1850 to 1900. Also known as the Hamilton method or the Vinton method.
#' Equivalent to the largest remainder method using the Hare quota.
#'
#' @details
#' The Hamilton-Vinton method first computes the exact quota for each unit:
#' \deqn{q_i = \frac{p_i \cdot H}{P}}
#' where \eqn{p_i} is the population of unit \eqn{i}, \eqn{H} is the total
#' number of seats (`size`), and \eqn{P = \sum_i p_i} is the total population.
#' Each unit initially receives \eqn{\lfloor q_i \rfloor} seats. Any remaining
#' seats are awarded one at a time to the units with the largest fractional
#' remainders \eqn{q_i - \lfloor q_i \rfloor}.
#'
#' The method satisfies the *quota property*: no unit ever receives fewer than
#' \eqn{\lfloor q_i \rfloor} or more than \eqn{\lceil q_i \rceil} seats.
#' However, it is susceptible to the "Alabama paradox," in which increasing
#' the total house size can paradoxically cause a unit to lose a seat.
#'
#' @inheritParams app-params
#' @inherit app-params return
#'
#' @examples
#' app_hamilton_vinton(size = 435, pop = state_2020$pop)
#' @export
app_hamilton_vinton <- function(size, pop) {
  if (any(size < 0)) {
    stop("`size` must be non-negative.")
  }
  apprt <- run_hamilton_vinton(make_size(size, pop), as.matrix(pop))
  restore_app(apprt, pop)
}

run_hamilton_vinton <- quickr::quick(
  function(n_tot, pop) {
    declare(type(n_tot = integer(m)), type(pop = double(NA, m)))
    out <- matrix(0L, nrow = nrow(pop), ncol = ncol(pop))

    for (k in seq_len(ncol(pop))) {
      total_pop <- sum(pop[, k])
      denom <- total_pop / n_tot[k]
      quotient <- pop[, k] / denom
      apprt <- floor(quotient)
      apprt[apprt == 0] <- 1L
      rem <- n_tot[k] - sum(apprt)
      remainder <- quotient - apprt

      while (rem > 0) {
        idx <- which.max(remainder)
        apprt[idx] <- apprt[idx] + 1L
        remainder[idx] <- 0
        rem <- rem - 1L
      }

      out[, k] <- apprt
    }

    out
  },
  name = "hamilton_vinton"
)
