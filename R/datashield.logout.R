#' @title Logout from DataSHIELD R sessions
#'
#' @description Clear the Datashield R sessions and logout from DataSHIELD data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param save Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#' @export
datashield.logout <- function(conns, save=NULL) {
  if (is.list(conns)) {
    ignore <- lapply(conns, function(c){datashield.logout(c, save=save)})
  } else if (!is.null(conns)) {
    dsDisconnect(conns, save)
  }
}
