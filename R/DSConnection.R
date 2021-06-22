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
#' con <- dsConnect(DSOpal::Opal(), "server1", 
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' con
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
#' @include DSObject.R
setClass("DSConnection", representation(name = "character"), contains = c("DSObject", "VIRTUAL"))

#' List remote tables
#'
#' List remote tables from the data repository. Returns the unquoted names of remote tables
#' accessible through this connection.
#'
#' @template methods
#' @templateVar method_name dsListTables
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @return A character vector of table names.
#' 
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
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
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param table the table fully qualified name
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsHasTable(con, "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsHasTable",
           def = function(conn, table) standardGeneric("dsHasTable"),
           valueClass = "logical")

#' List remote resources
#'
#' List remote resources from the data repository. Returns the unquoted names of remote resources
#' accessible through this connection.
#'
#' @template methods
#' @templateVar method_name dsListResources
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsListResources(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListResources",
           def = function(conn) standardGeneric("dsListResources"),
           valueClass = "character")

#' Check remote resource exists
#'
#' Check if a remote resource reference exists in the data repository. Returns a logical indicating the existence of a
#' remote resource accessible through this connection.
#'
#' @template methods
#' @templateVar method_name dsHasResource
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param resource the resource fully qualified name
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsHasResource(con, "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsHasResource",
           def = function(conn, resource) standardGeneric("dsHasResource"),
           valueClass = "logical")

#' Assign a data table
#'
#' Assign a data table from the data repository to a symbol in the DataSHIELD R session.
#' The table to be assigned must exist (i.e. proper permissions apply) for the DataSHIELD user.
#'
#' @template methods
#' @templateVar method_name dsAssignTable
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of a table in the data repository.
#' @param variables List of variable names or Javascript expression that selects the variables of
#'   a table. See javascript documentation:
#'   \url{http://opaldoc.obiba.org/en/latest/magma-user-guide/variable/}
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (if supported
#'   by the data repository).
#' @param id.name Name of the column that will contain the entity identifiers. If not specified, the identifiers
#'   will be the data frame row names. When specified this column can be used to perform joins between data frames.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAssignTable",
           def = function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=TRUE) standardGeneric("dsAssignTable"),
           valueClass = "DSResult")

#' Assign a resource object
#'
#' Assign a resource object of class 'ResourceClient' from the data repository to a symbol in the DataSHIELD R session.
#' The resource reference to be assigned must exist (i.e. proper permissions apply) for the DataSHIELD user.
#'
#' @template methods
#' @templateVar method_name dsAssignResource
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param resource Fully qualified name of a resource reference in the data repository.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsAssignResource(con, "D", "test.CNSIM")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAssignResource",
           def = function(conn, symbol, resource, async=TRUE) standardGeneric("dsAssignResource"),
           valueClass = "DSResult")

#' Assign an expression result
#'
#' Assign the result of the evaluation of an expression to a symbol the DataSHIELD R session
#' The assignment expression must satisfy the data repository's DataSHIELD configuration.
#'
#' @template methods
#' @templateVar method_name dsAssignExpr
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAssignExpr",
           def = function(conn, symbol, expr, async=TRUE) standardGeneric("dsAssignExpr"),
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
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsAggregate(con, as.symbol("meanDS(D$WEIGHT)"))
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsAggregate",
           def = function(conn, expr, async=TRUE) standardGeneric("dsAggregate"),
           valueClass = "DSResult")

#' List symbols
#'
#' After assignments have been performed, some symbols live in the DataSHIELD R session on the server side.
#'
#' @template methods
#' @templateVar method_name dsListSymbols
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
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
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsAssignTable(con, "D", "test.CNSIM")
#' dsRmSymbol(con, "D")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsRmSymbol",
           def = function(conn, symbol) standardGeneric("dsRmSymbol"))

#' Get the DataSHIELD profiles
#'
#' Get the list of DataSHIELD profiles that have been configured on the remote data repository.
#'
#' @template methods
#' @templateVar method_name dsListProfiles
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @return A list containing the "available" character vector of profile names and the "current" profile (in case a default one was assigned).
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsListProfiles(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListProfiles",
           def = function(conn) standardGeneric("dsListProfiles"),
           valueClass = "list")

#' Get the DataSHIELD methods
#'
#' Get the list of DataSHIELD methods that have been configured on the remote data repository.
#'
#' @template methods
#' @templateVar method_name dsListMethods
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param type Type of the method: "aggregate" (default) or "assign".
#'
#' @return A data.frame with columns: name, type ('aggregate' or 'assign'), class ('function' or 'script'), value, package, version.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsListMethods(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListMethods",
           def = function(conn, type = "aggregate") standardGeneric("dsListMethods"),
           valueClass = "data.frame")

#' Get the DataSHIELD packages
#'
#' Get the list of DataSHIELD packages with their version, that have been configured on the remote data repository.
#'
#' @template methods
#' @templateVar method_name dsListPackages
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @return A data.frame with columns: name, version.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsListPackages(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListPackages",
           def = function(conn) standardGeneric("dsListPackages"),
           valueClass = "data.frame")


#' Get the DataSHIELD workspaces
#'
#' Get the list of DataSHIELD workspaces, that have been saved on the remote data repository.
#'
#' @template methods
#' @templateVar method_name dsListWorkspaces
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @return A data.frame with columns: name, lastAccessDate, size.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsListWorkspaces",
           def = function(conn) standardGeneric("dsListWorkspaces"),
           valueClass = "data.frame")

#' Save the DataSHIELD R session in a workspace
#'
#' Save the DataSHIELD R session in a workspace on the remote data repository.
#'
#' @template methods
#' @templateVar method_name dsSaveWorkspace
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param name Name of the workspace
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsSaveWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsSaveWorkspace",
           def = function(conn, name) standardGeneric("dsSaveWorkspace"))

#' Remove a DataSHIELD workspace
#'
#' Remove a DataSHIELD workspace from the remote data repository. Ignore if no
#' such workspace exists.
#'
#' @template methods
#' @templateVar method_name dsRmWorkspace
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#' @param name Name of the workspace
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsSaveWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsRmWorkspace(con, "foo")
#' dsListWorkspaces(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsRmWorkspace",
           def = function(conn, name) standardGeneric("dsRmWorkspace"))

#' Asynchronous result support
#'
#' When a \code{\link{DSResult-class}} object is returned on aggregation or assignment operation,
#' the raw result can be accessed asynchronously, allowing parallelization of DataSHIELD calls
#' over multpile servers. The returned named list of logicals will specify if asynchronicity is supported for:
#' aggregation operation ('aggregate'), table assignment operation ('assignTable'),
#' resource assignment operation ('assignResource') and expression assignment operation ('assignExpr').
#'
#' @template methods
#' @templateVar method_name dsIsAsync
#'
#' @param conn An object that inherits from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsIsAsync(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsIsAsync",
           def = function(conn) standardGeneric("dsIsAsync"),
           valueClass = "list")

#' Keep a connection alive
#'
#' As the DataSHIELD sessions are working in parallel, this function helps at keeping
#' idle connections alive while others are working. Any communication failure must
#' be silently processed.
#'
#' @template methods
#' @templateVar method_name dsKeepAlive
#'
#' @param conn An object inheriting from \code{\link{DSConnection-class}}.
#'
#' @family DSConnection generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsKeepAlive(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsKeepAlive",
           def = function(conn) standardGeneric("dsKeepAlive"))

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
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsDisconnect",
           def = function(conn, save = NULL) standardGeneric("dsDisconnect"))
