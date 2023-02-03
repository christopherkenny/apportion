#' Apportion by the Balinski Young Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_balinski_young(size = 435, pop = state_2020$pop)
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
