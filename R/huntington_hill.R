#' Apportion by the Huntington-Hill (Equal proportions) Method
#'
#' The current method used for US Congressional apportionment, in continuous
#' use since the Apportionment Act of 1941. Also known as the method of equal
#' proportions.
#'
#' @details
#' The Huntington-Hill method is a sequential priority method. Starting with one
#' seat allocated to each unit (a constitutional minimum for Congressional
#' apportionment), it repeatedly awards the next seat to the unit with the
#' highest priority value:
#' \deqn{P_i(n) = \frac{p_i}{\sqrt{n(n + 1)}}}
#' where \eqn{p_i} is the population of unit \eqn{i} and \eqn{n} is the number
#' of seats currently held by unit \eqn{i}.
#'
#' This is equivalent to a divisor method that rounds each quotient at the
#' geometric mean of the two surrounding integers: \eqn{p_i / d} is rounded
#' up to \eqn{n + 1} if it exceeds \eqn{\sqrt{n(n+1)}}, and down to \eqn{n}
#' otherwise. Among the divisor methods, Huntington-Hill minimizes the maximum
#' relative difference in representation between any two units.
#'
#' @inheritParams app_adams
#' @inheritParams app_dhondt
#' @param thresh A population threshold for assigning seats. Units with
#'   population below this threshold receive zero seats, by default. Only affects
#'   the default value of `init`; if `init` is provided, `thresh` is ignored.
#' @inherit app_adams return
#'
#' @references
#' Huntington, E. V. (1928). The apportionment of representatives in Congress.
#' *Transactions of the American Mathematical Society*, 30(1), 85--110.
#' \doi{10.2307/1989268}
#'
#' @examples
#' app_huntington_hill(size = 435, pop = state_2020$pop)
#' @export
app_huntington_hill <- function(size, pop, init = NULL, thresh = 0) {
  if (any(size < 0)) {
    stop("`size` must be positive.")
  }
  init = make_init(init, pop)
  init[pop > thresh] <- 1L
  apprt <- run_huntington_hill(make_size(size, pop), as.matrix(pop), init)
  restore_app(apprt, pop)
}

run_huntington_hill <- quickr::quick(
  function(n_tot, pop, apprt) {
    declare(type(n_tot = integer(m)), type(pop = double(n, m)), type(apprt = integer(n, m)))

    for (k in seq_len(ncol(pop))) {
      is_zero <- apprt[, k] == 0L
      rem <- n_tot[k] - sum(apprt[!is_zero, k])

      while (rem > 0) {
        prios <- pop[, k] / sqrt(apprt[, k] * (apprt[, k] + 1))
        best <- 0L
        idx <- 0L
        for (j in seq_along(prios)) {
            if (!is_zero[j] && prios[j] > best) {
                best <- prios[j]
                idx <- j
            }
        }
        apprt[idx, k] <- apprt[idx, k] + 1L
        rem <- rem - 1L
      }
    }

    apprt
  },
  name = "huntington_hill"
)
