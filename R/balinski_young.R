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
#'
#' @return An integer vector of the same length as `pop` with the number of
#'   seats apportioned to each unit.
#'
#' @references
#' Balinski, M. L., & Young, H. P. (2001). *Fair Representation: Meeting the
#' Ideal of One Man, One Vote* (2nd ed.). Brookings Institution Press.
#'
#' @examples
#' app_balinski_young(size = 435, pop = state_2020$pop)
#' @export
app_balinski_young <- function(size, pop) {

  total_pop <- sum(pop)

  apprt <- integer(length(pop))

  for (h in seq_len(size)) {
    v <- pop / (1 + apprt)
    v[pop < pop * h / total_pop] <- 0
    apprt[which.max(v)] <- apprt[which.max(v)] + 1L
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
