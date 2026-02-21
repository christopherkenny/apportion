#' Apportion by the Webster (Sainte-Laguë) Method
#'
#' A divisor method that uses standard arithmetic-mean rounding, used for US
#' Congressional apportionment in 1842 and from 1911 to 1931. Also used in
#' Norway and Sweden for parliamentary seat allocation (where it is known as the
#' Sainte-Laguë method).
#'
#' @details
#' The Webster method finds a common divisor \eqn{d} such that
#' standard-rounded quotients sum to the desired house size:
#' \deqn{\sum_{i} \text{round}\!\left(\frac{p_i}{d}\right) = H}
#' where \eqn{p_i} is the population of unit \eqn{i} and \eqn{H} is the total
#' number of seats (`size`). Quotients are rounded at the arithmetic mean
#' \eqn{n + 0.5} of consecutive integers \eqn{n} and \eqn{n + 1}.
#'
#' Among the classical divisor methods, Webster is considered the most
#' statistically unbiased: it does not systematically favor either large or
#' small units. It minimizes the expected absolute deviation from exact quotas
#' when populations are drawn from a wide range of sizes.
#'
#' @inheritParams app-params
#' @inherit app-params return
#'
#' @examples
#' app_webster(size = 435, pop = state_2020$pop)
#' @export
app_webster <- function(size, pop) {

  div <- floor(sum(pop) / size)

  apprt <- round(pop / div)
  rem <- size - sum(apprt)

  while (rem != 0) {
    diff <- ifelse(rem < 0, 1L, -1L)
    div <- div + diff
    apprt <- round(pop / div)
    rem <- size - sum(apprt)
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
