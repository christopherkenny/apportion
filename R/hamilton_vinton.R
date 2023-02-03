#' Apportion by the Hamilton-Vinton Method
#'
#' @param size number of seats to apportion across units
#' @param pop a vector of population sizes for each unit
#'
#' @return integer vector
#' @export
#'
#' @examples
#' app_hamilton_vinton(size = 435, pop = state_2020$pop)
app_hamilton_vinton <- function(size, pop) {
  # init ----
  apprt <- rep.int(1L, times = length(pop))

  if (size < 0) {
    stop('{.arg size} must be positive.')
  }

  total_pop <- sum(pop)

  denom <- total_pop / size

  quotient <- pop / denom

  apprt <- floor(quotient)

  apprt[apprt == 0] <- 1L

  rem <- size - sum(apprt)

  remainder <- quotient - apprt
  while (rem > 0) {
    apprt[which.max(remainder)] <- apprt[which.max(remainder)] + 1L
    remainder[which.max(remainder)] <- 0
    rem <- rem - 1L
  }

  if (!is.null(names(pop))) {
    names(apprt) <- names(pop)
  }

  apprt
}
