#' List of the tables
#'
#' Get the list of all the tables from the different data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @return Table unique names from all the servers.
#' @examples
#' \dontrun{
#'   datashield.tables(conns)
#' }
#' @export
datashield.tables <- function(conns) {
  if (is.list(conns)) {
    lapply(conns, function(c) { dsListTables(c) })
  } else {
    dsListTables(conns)
  }
}

#' List of the resources
#'
#' Get the list of all the resources from the different data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @return Resource unique names from all the servers.
#' @examples
#' \dontrun{
#'   datashield.resources(conns)
#' }
#' @export
datashield.resources <- function(conns) {
  if (is.list(conns)) {
    lapply(conns, function(c) { dsListResources(c) })
  } else {
    dsListResources(conns)
  }
}
