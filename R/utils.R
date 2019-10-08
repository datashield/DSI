#' Makes a typical logindata data frame a list of tables named by the server in which they are defined.
#' Makes a character vector of table names a list named by the connections.
#' @keywords internal
.asNamedListOfTables <- function(conns, value) {
  .asNamedListOfValues(conns, value, "table")
}

#' Makes a typical logindata data frame a list of resources named by the server in which they are defined.
#' Makes a character vector of resource names a list named by the connections.
#' @keywords internal
.asNamedListOfResources <- function(conns, value) {
  .asNamedListOfValues(conns, value, "resource")
}

#' Makes a typical logindata data frame a list of items named by the server in which they are defined.
#' Makes a character vector of item names a list named by the connections.
#' @keywords internal
.asNamedListOfValues <- function(conns, value, colname) {
  rval <- value
  if (is.data.frame(value) && !is.null(value[[colname]]) && !is.null(value$server)) {
    rval <- as.character(value[[colname]])
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

#' Deparse language expression
#' @keywords internal
.deparse <- function(expr) {
  if (is.language(expr)) {
    deparse(expr)
  } else {
    expr
  }
}

#' Clear some cache
#' @keywords internal
.clearCache <- function(env=getOption("datashield.env", globalenv())) {
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.pkg_status", envir = env)) {
    rm(".datashield.pkg_status", envir = env)
  }
}
