#' Apportion by the Dean Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_dean(size = 435, pop = state_2020$pop)
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

