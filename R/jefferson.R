#' Apportion by the Jefferson Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_jefferson(size = 435, pop = state_2020$pop)
app_jefferson <- function(size, pop) {

  div <- floor(sum(pop) / size)

  apprt <- floor(pop / div)
  rem <- size - sum(apprt)

  while (rem != 0) {
    div <- div - 1L
    apprt <- floor(pop / div)
    rem <- size - sum(apprt)
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
