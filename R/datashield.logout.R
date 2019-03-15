#' @title Logout from DataSHIELD R sessions
#'
#' @description Clear the Datashield R sessions and logout from DataSHIELD data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param save Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#' @export
datashield.logout <- function(conns, save=NULL) {
  if (is.list(conns)) {
    pb <- .newProgress(total = 1 + length(conns))
    lapply(conns, function(c) {
      .tickProgress(pb, tokens = list(what = paste0("Logout ", c@name)))
      datashield.logout(c, save)
    })
    ignore <- .tickProgress(pb, tokens = list(what = "Logged out from all servers"))
  } else if (!is.null(conns)) {
    saveId <- save
    if (!is.null(save)) {
      saveId <- paste0(conns@name, ":", save)
    }
    tryCatch(dsDisconnect(conns, saveId))
  }
}
