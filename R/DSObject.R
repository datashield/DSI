#' DSObject class
#'
#' Base class for all other DataSHIELD classes (e.g., drivers, connections). This
#' is a virtual Class: No objects may be created from it.
#'
#' More generally, DataSHIELD defines a very small set of classes and generics that
#' allows users and applications perform meta-analysis with a common interface.  The
#' virtual classes are `DSDriver` that individual drivers extend,
#' `DSConnection` that represent instances of DataSHIELD-aware data repository connections, and
#' `DSResult` that represent the result of a DataSHIELD operation. These three
#' classes extend the basic class of `DSObject`, which serves as the root
#' or parent of the class hierarchy.
#'
#' @section Implementation notes:
#' An implementation MUST provide methods for the following generics:
#'
#' \itemize{
#'   \item \code{\link{dsGetInfo}}
#' }
#'
#' It MAY also provide methods for:
#'
#' \itemize{
#'   \item \code{\link{summary}} Print a concise description of the
#'     object. The default method invokes `dsGetInfo(dsObj)` and prints
#'     the name-value pairs one per line. Individual implementations may
#'     tailor this appropriately.
#' }
#'
#' @docType class
#' @family DS classes
#' @examples
#' \dontrun{
#' drv <- DSOpal::Opal()
#' con <- dsConnect(drv, 
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#'
#' rs <- dsAssign(con, "Project.TableA")
#' is(drv, "DSObject")  ## True
#' is(con, "DSObject")  ## True
#' is(rs, "DSObject")   ## True
#'
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
#' @name DSObject-class
setClass("DSObject", "VIRTUAL")

#' Get DataSHIELD-aware data repository metadata
#'
#' @section Implementation notes:
#' For `DSDriver` subclasses, this should include the version of the
#' package (`driver.version`) and the version of the underlying client
#' library (`client.version`).
#'
#' For `DSConnection` objects this should report the version of
#' the data repository application (`repo.version`) and its name (`repo.name`),
#' the database name (`dbname`), username, (`username`), host (`host`), port (`port`), etc.
#' It MAY also include any other arguments related to the connection
#' (e.g., thread id, socket or TCP connection type). It MUST NOT include the
#' password.
#'
#' For `DSResult` objects, this should include the R expression
#' being executed (an expression object tailored by the implementation of DSI) 
#' and if the query is complete (a result object tailored by the implementation of DSI).
#'
#' @param dsObj An object inheriting from \code{\link{DSObject-class}},
#'  i.e. \code{\link{DSDriver-class}}, \code{\link{DSConnection-class}},
#'  or a \code{\link{DSResult-class}}.
#' @param ... Other arguments to methods.
#' @family DSDriver generics
#' @family DSConnection generics
#' @family DSResult generics
#' @return a named list
#' @import methods
#' @export
setGeneric("dsGetInfo",
           def = function(dsObj, ...) standardGeneric("dsGetInfo"),
           valueClass = "list")
