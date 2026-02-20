#' Apportion by the D'Hondt (Jefferson) Method
#'
#' A sequential priority method widely used for **proportional representation**
#' elections, including in Belgium, Spain, Portugal, the Netherlands, Austria,
#' and many other countries. Mathematically equivalent to the Jefferson method,
#' a procedure described by Thomas Jefferson.
#' Compared with the Webster/Sainte-Laguë method, D'Hondt/Jefferson tends to
#' give a slight advantage to **larger** units.
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
#'
#' @return An integer vector of the same length as `pop` with the number of
#'   seats apportioned to each unit.
#' @export
#'
#' @examples
#' app_dhondt(size = 435, pop = state_2020$pop)
app_dhondt <- function(size, pop) {
  apprt <- rep.int(0, times = length(pop))

  if (size < 0) {
    stop('{.arg size} must be positive.')
  }

  for (i in seq_len(size)) {
    quotient <- pop / (apprt + 1)
    apprt[which.max(quotient)] <- apprt[which.max(quotient)] + 1L
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}

#' @export
#' @rdname app_dhondt
app_jefferson <- app_dhondt
