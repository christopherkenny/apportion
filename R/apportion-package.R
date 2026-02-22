#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils globalVariables
#' @importFrom quickr quick
#' @useDynLib apportion, .registration = TRUE
## usethis namespace: end
NULL

globalVariables(c("type"))

restore_app = function(output, input) {
  dim(output) <- dim(input)
  if (is.matrix(output) && (!is.null(rownames(input)) || !is.null(colnames(input)))) {
    dimnames(output) <- dimnames(input)
  } else if (!is.null(names(input))) {
    names(output) <- names(input)
  }
  output
}

make_init = function(init, pop) {
  if (is.null(init)) {
    if (is.matrix(pop)) {
      matrix(0L, nrow(pop), ncol(pop))
    } else {
      matrix(0L, length(pop), 1)
    }
  } else {
    as.matrix(init)
  }
}

make_size = function(size, pop) {
  if (length(size) == 1L) {
    rep(as.integer(size), if (is.matrix(pop)) ncol(pop) else 1L)
  } else {
    as.integer(size)
  }
}