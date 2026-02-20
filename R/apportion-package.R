#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils globalVariables
#' @useDynLib apportion, .registration = TRUE
## usethis namespace: end
NULL

globalVariables(c("type"))

restore_app = function(output, input) {
    if (is.matrix(output) && (!is.null(rownames(input)) || !is.null(colnames(input)))) {
        dimnames(output) <- dimnames(input)
    } else if (!is.null(names(input))) {
        names(output) <- names(input)
    }
    dim(output) <- dim(input)
    output
}