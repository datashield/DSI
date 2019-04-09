#' Data assignment
#'
#' Assign a table or an expression result to a R symbol in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a table reference in data repositories (see
#'   \code{\link{datashield.assign.table}} for more details) or a R expression with allowed assign
#'  functions calls.
#' @param variables List of variable names or Javascript expression that selects the variables of a table
#'   (ignored if value does not refere to a table). See javascript documentation:
#'   \url{http://opaldoc.obiba.org/en/latest/magma-user-guide/variable/}
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#'   Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (if supported
#'   by data repository).
#' @param async Whether the result of the call should be retrieved asynchronously (TRUE means that calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests).
#'
#' @examples
#' \dontrun{
#' # assign a list of variables from table HOP
#' datashield.assign(conn, symbol="D", value="demo.HOP",
#'   variables=list("GENDER","LAB_GLUC"))
#'
#' # assign all the variables matching 'LAB' from table HOP
#' datashield.assign(conn, symbol="D", value="demo.HOP",
#'   variables="name().matches('LAB_')")
#' }
#' @export
datashield.assign <- function(conns, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE) {
  if(is.language(value) || is.function(value)) {
    datashield.assign.expr(conns, symbol, value, async)
  } else {
    datashield.assign.table(conns, symbol, value, variables, missings, identifiers, async)
  }
}

#' Table assignment
#'
#' Assign a table to a R symbol in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of a table in the data repository (can be a vector or must be
#'   the same in each data repository); or a named list of fully qualified table names (one per server
#'   name); or a data frame with 'server' and 'table' columns (such as the one that is used in
#'   \code{\link{datashield.login}})
#' @param variables List of variable names or Javascript expression that selects the variables of
#'   a table. See javascript documentation:
#'   \url{http://opaldoc.obiba.org/en/latest/magma-user-guide/variable/}
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#'   Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (if supported
#'   by the data repository).
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @examples
#' \dontrun{
#' # assign a list of variables from table HOP
#' datashield.assign.table(conn, symbol="D", table="demo.HOP",
#'   variables=list("GENDER","LAB_GLUC"))
#'
#' # assign all the variables matching 'LAB' from table HOP
#' datashield.assign.table(conn, symbol="D", table="demo.HOP",
#'   variables="name().matches('LAB_')")
#'
#' # assign the tables that are defined in the logindata ('server' and 'table' columns are
#' # expected) data frame that is used in datashield.login() function. Connections names
#' # and server names must match.
#' datashield.assign.table(conns, "D", logindata)
#'
#' # assign the tables that are defined in the provided named list. Connections names
#' # and server names must match.
#' datashield.assign.table(conns, "D", list(server1="datashield.CNSIM1", server2="datashield.CNSIM2"))
#' }
#' @export
datashield.assign.table <- function(conns, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE) {
    # prepare tables as a named list
    tables <- .asNamedListOfTables(conns, table)

    if (is.list(conns)) {
    results <- list()
    async <- lapply(conns, function(conn) { ifelse(async, dsIsAsync(conn)$assignTable, FALSE) })
    pb <- .newProgress(total = 1 + length(conns))
    # async first
    for (n in names(conns)) {
      if(async[[n]]) {
        results[[n]] <- dsAssignTable(conns[[n]], symbol, tables[[n]], variables, missings, identifiers, async=TRUE)
      }
    }
    # not async (blocking calls)
    for (n in names(conns)) {
      if(!async[[n]]) {
        .tickProgress(pb, tokens = list(what = paste0("Assigning ", conns[[n]]@name, " (", symbol, " <- `", tables[[n]], "`)")))
        results[[n]] <- dsAssignTable(conns[[n]], symbol, tables[[n]], variables, missings, identifiers, async=FALSE)
      }
    }
    lapply(names(conns), function(n) {
      if(async[[n]]) .tickProgress(pb, tokens = list(what = paste0("Assigning table ", conns[[n]]@name, " (", symbol, " <- `", tables[[n]], "`)")))
      dsGetInfo(results[[n]])
    })
    ignore <- .tickProgress(pb, tokens = list(what = paste0("Assigned all table (", symbol, " <- ...)")))
  } else {
    res <- dsAssignTable(conns, symbol, tables[[conns@name]], variables, missings, identifiers)
    ignore <- dsGetInfo(res)
  }
}

#' Expression result assignment
#'
#' Assign the result of the execution of an expression to a R symbol in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @param expr R expression with allowed assign functions calls.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @examples
#' \dontrun{
#' # assign a  o
#' datashield.assign.expr(o, symbol="G", expr=quote(as.numeric(D$GENDER)))
#' }
#' @export
datashield.assign.expr <- function(conns, symbol, expr, async=TRUE) {
  if (is.list(conns)) {
    results <- list()
    async <- lapply(conns, function(conn) { ifelse(async, dsIsAsync(conn)$assignExpr, FALSE) })
    pb <- .newProgress(total = 1 + length(conns))
    # async first
    for (n in names(conns)) {
      if(async[[n]]) {
        results[[n]] <- dsAssignExpr(conns[[n]], symbol, expr, async=TRUE)
      }
    }
    dexpr <- .deparse(expr)
    # not async (blocking calls)
    for (n in names(conns)) {
      if(!async[[n]]) {
        .tickProgress(pb, tokens = list(what = paste0("Assigning expr. ", conns[[n]]@name, " (", symbol, " <- ", dexpr, ")")))
        results[[n]] <- dsAssignExpr(conns[[n]], symbol, expr, async=FALSE)
      }
    }
    lapply(names(conns), function(n) {
      if(async[[n]]) .tickProgress(pb, tokens = list(what = paste0("Assigning ", conns[[n]]@name, " (", symbol, " <- ", dexpr, ")")))
      dsGetInfo(results[[n]])
    })
    ignore <- .tickProgress(pb, tokens = list(what = paste0("Assigned expr. (", symbol, " <- ", dexpr, ")")))
  } else {
    res <- dsAssignExpr(conns, symbol, expr)
    ignore <- dsGetInfo(res)
  }
}
