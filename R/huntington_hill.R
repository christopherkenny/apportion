#' Apportion by the Huntington-Hill (Equal proportions) Method
#'
#' The current method used for US Congressional apportionment, in continuous
#' use since the Apportionment Act of 1941. Also known as the method of equal
#' proportions.
#'
#' @details
#' The Huntington-Hill method is a sequential priority method. Starting with one
#' seat allocated to each unit (a constitutional minimum for Congressional
#' apportionment), it repeatedly awards the next seat to the unit with the
#' highest priority value:
#' \deqn{P_i(n) = \frac{p_i}{\sqrt{n(n + 1)}}}
#' where \eqn{p_i} is the population of unit \eqn{i} and \eqn{n} is the number
#' of seats currently held by unit \eqn{i}.
#'
#' This is equivalent to a divisor method that rounds each quotient at the
#' geometric mean of the two surrounding integers: \eqn{p_i / d} is rounded
#' up to \eqn{n + 1} if it exceeds \eqn{\sqrt{n(n+1)}}, and down to \eqn{n}
#' otherwise. Among the divisor methods, Huntington-Hill minimizes the maximum
#' relative difference in representation between any two units.
#'
#' @inheritParams app-params
#' @inherit app-params return
#'
#' @references
#' Huntington, E. V. (1928). The apportionment of representatives in Congress.
#' *Transactions of the American Mathematical Society*, 30(1), 85--110.
#' \doi{10.2307/1989268}
#'
#' @examples
#' app_huntington_hill(size = 435, pop = state_2020$pop)
#' @export
app_huntington_hill <- function(size, pop) {

  # init ----
  apprt <- rep.int(1L, times = length(pop))
  rem <- size - sum(apprt)
  if (rem < 0) {
    stop('There are more entries in {.arg pop} than {.arg size} is large.')
  }

  # run priorities ----
  prios <- integer(length = length(pop))
  while (rem > 0) {
    prios <- pop / sqrt(apprt * (apprt + 1))
    apprt[which.max(prios)] <- apprt[which.max(prios)] + 1L
    rem <- rem - 1L
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}

