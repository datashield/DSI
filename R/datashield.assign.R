#' Data assignment
#'
#' Assign a table or an expression result to a R symbol in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table reference in data repositories
#'   (must be the same in each data repository) or a R expression with allowed assign functions calls.
#' @param variables List of variable names or Javascript expression that selects the variables of a table
#'   (ignored if value does not refere to a table). See javascript documentation:
#'   \url{http://opaldoc.obiba.org/en/latest/magma-user-guide/variable/}
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#'   Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (if supported
#'   by data repository).
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
datashield.assign <- function(conns, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL) {
  if(is.language(value) || is.function(value)) {
    datashield.assign.expr(conns, symbol, value)
  } else {
    datashield.assign.table(conns, symbol, value, variables, missings, identifiers)
  }
}

#' Table assignment
#'
#' Assign a table to a R symbol in the Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of a table in the data repository
#'   (must be the same in each data repository).
#' @param variables List of variable names or Javascript expression that selects the variables of
#'   a table. See javascript documentation:
#'   \url{http://opaldoc.obiba.org/en/latest/magma-user-guide/variable/}
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#'   Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (if supported
#'   by the data repository).
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
#' }
#' @export
datashield.assign.table <- function(conns, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL) {
  if (is.list(conns)) {
    ress <- lapply(conns, FUN=dsAssignTable, symbol, table, variables, missings, identifiers)
    ignore <- lapply(ress, function(r) {
      dsGetInfo(r)
    })
  } else {
    res <- dsAssignTable(conns, symbol, table, variables, missings, identifiers)
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
#'
#' @examples
#' \dontrun{
#' # assign a  o
#' datashield.assign.expr(o, symbol="G", expr=quote(as.numeric(D$GENDER)))
#' }
#' @export
datashield.assign.expr <- function(conns, symbol, expr) {
  if (is.list(conns)) {
    ress <- lapply(conns, FUN=dsAssignExpr, symbol, expr)
    ignore <- lapply(ress, function(r) {
      dsGetInfo(r)
    })
  } else {
    res <- dsAssignExpr(conns, symbol, expr)
    ignore <- dsGetInfo(res)
  }
}
