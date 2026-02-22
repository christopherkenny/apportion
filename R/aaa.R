# Wrapper to avoid importing quickr in the package
quick <- function(fun, name) {
  # During development: delegate to quickr for compilation
  if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
    if (requireNamespace("quickr", quietly = TRUE)) {
      return(quickr::quick(fun, name))
    }
    warning("quickr is not installed; returning uncompiled function")
    return(fun)
  }

  # In installed package: return pre-compiled .External wrapper
  body(fun) <- as.call(c(
    quote(.External),
    as.name(paste0(name, "_")),
    lapply(names(formals(fun)), as.name)
  ))
  fun
}
