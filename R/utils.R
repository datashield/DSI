#' Makes a typical logindata data frame a list of tables named by the server in which they are defined.
#' Makes a character vector of table names a list named by the connections.
#' @keywords internal
.asNamedListOfTables <- function(conns, value) {
  rval <- value
  if (is.data.frame(value) && !is.null(value$table) && !is.null(value$server)) {
    rval <- as.character(value$table)
    names(rval) <- value$server
  } else if (is.character(value)) {
    if (length(value) == 1) {
      rval <- rep(value, length(conns))
    }
    cs <- .asNamedListOfConnections(conns)
    names(rval) <- unlist(lapply(cs, function(c) c@name))
  }
  rval
}

#' Makes a single \code{\link{DSConnection-class}} object or a named list of \code{\link{DSConnection-class}} objects.
#' @keywords internal
.asNamedListOfConnections <- function(conns) {
  cs <- conns
  if (!is.list(conns) && .isDSConnection(conns)) {
    cs <- list()
    cs[[conns@name]] <- conns
  }
  cs
}
