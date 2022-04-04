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
.asNamedListOfValues <- function(conns, value, colname = NULL) {
  rval <- value
  if (is.data.frame(value) && !is.null(value[[colname]]) && !is.null(value$server)) {
    rval <- as.character(value[[colname]])
    names(rval) <- value$server
  } else if (is.character(value)) {
    if (!is.vector(value) || length(value) == 1) {
      rval <- rep(value, length(conns))
    }
    cs <- .asNamedListOfConnections(conns)
    names(rval) <- unlist(lapply(cs, function(c) c@name))
  } else if (is.function(value) || is.language(value)) {
    rval <- list()
    cs <- .asNamedListOfConnections(conns)
    for (n in unlist(lapply(cs, function(c) c@name)))
      rval[[n]] <- value
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

#' @keywords internal
.filterConnectionsByName <- function(conns, names) {
  fconns <- list()
  for (n in names)
    if (!is.null(conns[[n]]))
      fconns[[n]] <- conns[[n]]
  fconns
}

#' Create a new progress instance with default settings.
#' @import progress
#' @keywords internal
.newProgress <- function(format = "  :what [:bar] :percent /:elapsed", clear = getOption("datashield.progress.clear", FALSE), total, width = 100) {
  pb <- progress::progress_bar$new(format = format, clear = clear, total = total, width = width, show_after = 0)
  pb$tick(0, tokens = list(what = ''))
  pb
}

#' Update and increment the progress status if option "datashield.progress" is TRUE.
#' @keywords internal
.tickProgress <- function(progress, tokens = list()) {
  if (getOption("datashield.progress", TRUE)) progress$tick(tokens = tokens)
}

#' Update the progress status if option "datashield.progress" is TRUE.
#' @keywords internal
.updateProgress <- function(progress, step, total, tokens = list()) {
  if (getOption("datashield.progress", TRUE)) progress$update(ratio = step/total, tokens = tokens)
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
  if (exists(".datashield.pkg_status", envir = env)) {
    rm(".datashield.pkg_status", envir = env)
  }
}

#' Clear last errors
#' @keywords internal
.clearLastErrors <- function(env=getOption("datashield.env", globalenv())) {
  if (exists(".datashield.last_errors", envir = env)) {
    rm(".datashield.last_errors", envir = env)
  }
}

#' Check if there are last errors
#' @keywords internal
.hasLastErrors <- function(name, env=getOption("datashield.env", globalenv())) {
  if (exists(".datashield.last_errors", envir = env)) {
    errs <- get(".datashield.last_errors", envir = env)
    !is.null(errs[[name]])
  } else {
    FALSE
  }
}

#' Check if there are last errors
#' @keywords internal
.checkLastErrors <- function(env=getOption("datashield.env", globalenv())) {
  if (exists(".datashield.last_errors", envir = env)) {
    if (getOption("datashield.errors.stop", TRUE)) {
      stop("There are some DataSHIELD errors, list them with datashield.errors()", call. = FALSE)
    } else {
      warning("There are some DataSHIELD errors, list them with datashield.errors()", call. = FALSE)
    }
  }
}

#' Append error message to last errors vector
#' @keywords internal
.appendError <- function(name, msg, env=getOption("datashield.env", globalenv())) {
  if (exists(".datashield.last_errors", envir = env)) {
    errs <- get(".datashield.last_errors", envir = env)
    if (is.null(errs[[name]])) {
      errs[[name]] <- msg
    } else {
      errs[[name]] <- append(errs[[name]], msg)
    }
    assign(".datashield.last_errors", value = errs, envir = env)
  } else {
    errs <- list()
    errs[[name]] <- msg
    assign(".datashield.last_errors", value = errs, envir = env)
  }
}

#' Check if a callback parameter is a valid function
#' @keywords internal
.is.callback <- function(cb) {
  !is.null(cb) && is.function(cb)
}

#' Get time to sleep depending on the numer of previous iterations
#' @keywords internal
.getSleepTime <- function(checks) {
  t0 <- getOption("datashield.polling.sleep.0", 0.05)
  # wait 1s after 1s
  t1 <- getOption("datashield.polling.sleep.1", 1)
  # wait 2s after 10s
  t10 <- getOption("datashield.polling.sleep.10", t1 * 2)
  # wait 10s after 1min
  t60 <- getOption("datashield.polling.sleep.60", t1 * 10)
  # wait 1min after 10mins
  t600 <- getOption("datashield.polling.sleep.600", t1 * 60)
  # wait 10min after 1h
  t3600 <- getOption("datashield.polling.sleep.3600", t1 * 600)
  n1 <- 1 / t0
  n10 <- n1 + 9 / t1
  n60 <- n10 + 50 / t10
  n600 <- n60 + 540 / t60
  n3600 <- n600 + 3000 / t600
  t <- t0
  if (checks>=n1 && checks<n10) {
    t <- t1
  } else if (checks>=n10 && checks<n60) {
    t <- t10
  } else if (checks>=n60 && checks<n600) {
    t <- t60
  } else if (checks>=n600 && checks<n3600) {
    t <- t600
  } else if (checks>=n3600) {
    t <- t3600
  }
  t
}
