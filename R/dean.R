#' Apportion by the Dean Method
#'
#' A divisor method that rounds at the harmonic mean of consecutive integers,
#' proposed by astronomer James Dean. It was never adopted for US Congressional
#' apportionment.
#'
#' @details
#' The Dean method finds a common divisor \eqn{d} and rounds each quotient at
#' the **harmonic mean** of the two surrounding integers. A quotient
#' \eqn{p_i / d} that falls between integers \eqn{n} and \eqn{n + 1} is
#' rounded up to \eqn{n + 1} if it exceeds the harmonic mean:
#' \deqn{H(n,\, n+1) = \frac{2n(n+1)}{2n+1}}
#' and rounded down to \eqn{n} otherwise. The divisor is adjusted iteratively
#' until the total allocation equals `size`.
#'
#' Among the classical divisor methods, the Dean method minimizes the absolute
#' difference in **average district population** (i.e., \eqn{p_i / s_i}, where
#' \eqn{s_i} is the number of seats awarded to unit \eqn{i}) between any two
#' units. It falls between the Webster and Huntington-Hill methods in its
#' treatment of small versus large units, giving slightly more seats to small
#' units than Webster but fewer than Huntington-Hill.
#'
#' @inheritParams app_adams
#'
#' @return An integer vector of the same length as `pop` with the number of
#'   seats apportioned to each unit.
#'
#' @examples
#' app_dean(size = 435, pop = state_2020$pop)
#' @export
app_dean <- function(size, pop) {

  div <- floor(sum(pop) / size)

  apprt <- round_harm(pop / div)
  rem <- size - sum(apprt)

  while (rem != 0) {
    diff <- ifelse(rem < 0, 1L, -1L)
    div <- div + diff
    apprt <- round_harm(pop / div)
    rem <- size - sum(apprt)
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}

fc_harm <- function(v) {
  2 * floor(v) * ceiling(v) / (floor(v) + ceiling(v))
}

round_harm <- function(v) {
  ifelse(v < fc_harm(v), floor(v), ceiling(v))
}

