#' Apportion by the Huntington-Hill Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_huntington_hill(size = 435, pop = state_2020$pop)
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

