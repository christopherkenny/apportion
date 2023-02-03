#' Apportion by the D'Hondt Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
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
