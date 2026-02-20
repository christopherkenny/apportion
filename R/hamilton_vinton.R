#' Apportion by the Hamilton-Vinton Method
#'
#' A largest-remainder quota method used for US Congressional apportionment
#' from 1850 to 1900. Also known as the Hamilton method or the Vinton method.
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
#' @inheritParams app_adams
#'
#' @return An integer vector of the same length as `pop` with the number of
#'   seats apportioned to each unit.
#'
#' @examples
#' app_hamilton_vinton(size = 435, pop = state_2020$pop)
#' @export
app_hamilton_vinton <- function(size, pop) {
  apprt <- rep.int(1L, times = length(pop))

  if (size < 0) {
    stop('{.arg size} must be positive.')
  }

  total_pop <- sum(pop)

  denom <- total_pop / size

  quotient <- pop / denom

  apprt <- floor(quotient)

  apprt[apprt == 0] <- 1L

  rem <- size - sum(apprt)

  remainder <- quotient - apprt
  while (rem > 0) {
    apprt[which.max(remainder)] <- apprt[which.max(remainder)] + 1L
    remainder[which.max(remainder)] <- 0
    rem <- rem - 1L
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
