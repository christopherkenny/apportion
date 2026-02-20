#' Apportion by the Adams Method
#'
#' @param size An integer number of seats to apportion across units
#' @param pop A vector of population sizes for each unit
#'
#' @return An integer vector of the same length as `pop` with the number of seats apportioned to each unit.
#' @export
#'
#' @examples
#' app_adams(size = 435, pop = state_2020$pop)
app_adams <- function(size, pop) {

  div <- floor(sum(pop) / size)

  apprt <- ceiling(pop / div)
  rem <- size - sum(apprt)

  while (rem != 0) {
    diff <- ifelse(rem < 0, 1L, -1L)
    div <- div + diff
    apprt <- ceiling(pop / div)
    rem <- size - sum(apprt)
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
