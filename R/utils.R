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

#' Create a new progress instance with default settings.
#' @import progress
#' @keywords internal
.newProgress <- function(format = "  :what [:bar] :percent /:elapsed", clear = getOption("datashield.progress.clear", FALSE), total, width = 100) {
  progress::progress_bar$new(format = format, clear = clear, total = total, width = width)
}

#' Output the progress status if option "datashield.progress" is allows to.
#' @keywords internal
.tickProgress <- function(progress, tokens = list()) {
  if (getOption("datashield.progress", TRUE)) progress$tick(tokens = tokens)
}
