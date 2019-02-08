#' @include hidden.R
NULL

#' DSResult class
#'
#' This virtual class describes the result and state of execution of
#' a DataSHIELD request (aggregation or assignment).
#'
#' @section Implementation notes:
#' Individual drivers are free to allow single or multiple
#' active results per connection.
#'
#' The default show method displays a summary of the query using other
#' DS generics.
#'
#' @name DSResult-class
#' @docType class
#' @family DS classes
#' @family DSResult generics
#' @import methods
#' @export
#' @include DSObject.R
setClass("DSResult", contains = c("DSObject", "VIRTUAL"))

#' Get the raw result
#'
#' Wait for the result to be available and fetch the result from a previous assignment or aggregation operation that may have been
#' run asynchronously, in which case it is a one-shot call. When the assignment or aggregation operation was not asynchronous,
#' the result is wrapped in the object and can be fetched mutliple times.
#'
#' @template methods
#' @templateVar method_name dsFetch
#'
#' @param res An object inheriting from \code{\link{DSResult-class}}.
#'
#' @family DSResult generics
#' @examples
#' \dontrun{
#' con <- dbConnect(DSOpal::Opal(), "server1",
#'   "username", "password", "https://opal.example.org")
#' dsAssignExpr(con, "C", as.symbol("c(1, 2, 3)"))
#' res <- dsAggregate(con, as.symbol("length(C)"))
#' length <- dsFetch(res)
#' dsDisconnect(con)
#' }
#' @import methods
#' @export
setGeneric("dsFetch",
           def = function(res) standardGeneric("dsFetch"))
