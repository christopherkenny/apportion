#' Apportion by the Webster Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_webster(size = 435, pop = state_2020$pop)
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
