#' List R symbols
#'
#' Get the R symbols available after the datashield.assign calls in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#'
#' @export
datashield.symbols <- function(conns) {
  datashield.sessions(conns)
  if (is.list(conns)) {
    lapply(conns, FUN=datashield.symbols)
  } else {
    dsListSymbols(conns)
  }
}

#' Remove a R symbol
#'
#' Remove a symbol from the current Datashield session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @export
datashield.rm <- function(conns, symbol) {
  if (is.list(conns)) {
    res <- lapply(conns, FUN=datashield.rm, symbol)
  } else {
    res <- dsRmSymbol(conns, symbol)
  }
}
