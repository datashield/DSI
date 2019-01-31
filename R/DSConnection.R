#' @include hidden.R
NULL

#' DSConnection class
#'
#' This virtual class encapsulates the connection to a DataSHIELD-aware data repository,
#' and it provides access to data assignments and aggregagtions etc.
#'
#' @section Implementation note:
#' Individual drivers are free to implement single or multiple simultaneous
#' connections.
#'
#' @docType class
#' @name DSConnection-class
#' @family DS classes
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' con
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
#' @include DSObject.R
setClass("DSConnection", contains = c("DSObject", "VIRTUAL"))

#' List remote tables
#'
#' List remote tables from the data repository. Returns the unquoted names of remote tables
#' accessible through this connection.
#'
#' @template methods
#' @templateVar method_name dsListTables
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsListTables(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListTables",
           def = function(conn) standardGeneric("dsListTables"),
           valueClass = "character")

#' Check remote table exists
#'
#' Check if a remote table exists in the data repository. Returns a logical indicating the existence of a
#' remote table accessible through this connection.
#'
#' @template methods
#' @templateVar method_name dsHasTable
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#' @param table the table fully qualified name
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsHasTable(con, "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsHasTable",
           def = function(conn, table) standardGeneric("dsHasTable"),
           valueClass = "logical")

#' Assign a remote table
#'
#' Assign a remote table from the data repository to a symbol in the DataSHIELD R session.
#' The table to be assigned must exist for the DataSHIELD user.
#'
#' @template methods
#' @templateVar method_name dsAssignTable
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of a table in the data repository.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAssignTable",
           def = function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL) standardGeneric("dsAssignTable"),
           valueClass = "DSResult")

#' Assign an expression result
#'
#' Assign the result of the evaluation of an expression to a symbol the DataSHIELD R session
#' The assignment expression must satisfy the data repository's DataSHIELD configuration.
#'
#' @template methods
#' @templateVar method_name dsAssignExpr
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAssignExpr",
           def = function(conn, symbol, expr) standardGeneric("dsAssignExpr"),
           valueClass = "DSResult")

#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression. The aggregation expression
#' must satisfy the data repository's DataSHIELD configuration.
#'
#' @template methods
#' @templateVar method_name dsAggregate
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param expr Expression to evaluate.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsAggregate(con, as.symbol("meanDS(D$WEIGHT)"))
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAggregate",
           def = function(conn, expr) standardGeneric("dsAggregate"),
           valueClass = "DSResult")

#' List symbols
#'
#' After assignments have been performed, some symbols live in the DataSHIELD R session on the server side.
#'
#' @template methods
#' @templateVar method_name dsListSymbols
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsListSymbols(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListSymbols",
           def = function(conn) standardGeneric("dsListSymbols"),
           valueClass = "character")

#' Remove a symbol
#'
#' After removal, the data identified by the symbol will not be accessible in the DataSHIELD R session on the server side.
#'
#' @template methods
#' @templateVar method_name dsRmSymbol
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsRmSymbol(con, "D")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsRmSymbol",
           def = function(conn, symbol) standardGeneric("dsRmSymbol"))

#' Asynchronous result support
#'
#' When a \code{\link{DSResult-class}} object is returned on aggregation or assignment operation,
#' the raw result can be accessed asynchronously, allowing parallelization of DataSHIELD calls
#' over multpile servers. The returned named list of logicals will specify if asynchronicity is supported for:
#' aggregation operation ('aggregate'), table assignment operation ('assignTable'),
#' expression assignment operation ('assignExpr').
#'
#' @template methods
#' @templateVar method_name dsIsAsync
#'
#' @param conn an object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsIsAsync(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsIsAsync",
           def = function(conn) standardGeneric("dsIsAsync"),
           valueClass = "list")

#' Disconnect (close) a connection
#'
#' This closes the connection, discards all pending work, and frees
#' resources (e.g., memory, sockets).
#'
#' @template methods
#' @templateVar method_name dsDisconnect
#'
#' @param conn An object inheriting from \code{\link{DSConnection-class}}.
#' @param save Save DataSHIELD session in data repository with provided identifier string.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsDisconnect",
           def = function(conn, save = NULL) standardGeneric("dsDisconnect"))
