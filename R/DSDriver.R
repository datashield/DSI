#' DSDriver class
#'
#' Base class for all DataSHIELD-aware data repositories drivers (e.g., Opal, ...).
#' The virtual class `DSDriver` defines the operations for creating
#' connections.
#'
#' @docType class
#' @name DSDriver-class
#' @family DS classes
#' @family DSDriver generics
#' @import methods
#' @export
#' @include DSObject.R
setClass("DSDriver", contains = c("DSObject", "VIRTUAL"))

#' Create a connection to a DataSHIELD-aware data repository
#'
#' Connect to a data repository going through the appropriate authentication procedure.
#' Some implementations may allow you to have multiple connections open, so you
#' may invoke this function repeatedly assigning its output to different
#' objects.
#' The authentication mechanism is left unspecified, so check the
#' documentation of individual drivers for details.
#'
#' @template methods
#' @templateVar method_name dsConnect
#'
#' @param drv an object that inherits from \code{\link{DSDriver-class}}.
#' @param ... authentication arguments needed by the data repository instance; these
#'   typically include `user`, `password`, `host`, `port`, `dbname`, etc.
#'   For details see the appropriate `DSDriver`.
#' @seealso \code{\link{dsDisconnect}} to disconnect from a data repository.
#' @family DSDriver generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "username", "password", "https://opal.example.org")
#' con
#' dsListTables(con)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsConnect",
           def = function(drv, ...) standardGeneric("dsConnect"),
           valueClass = "DSConnection")
