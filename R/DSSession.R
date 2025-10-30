#' DSSession class
#'
#' This virtual class describes the state of the R session.
#'
#' The default show method displays a summary of the R session state using other
#' DS generics.
#'
#' @name DSSession-class
#' @docType class
#' @family DS classes
#' @family DSSession generics
#' @export
#' @include DSObject.R
setClass("DSSession", contains = c("DSObject", "VIRTUAL"))

#' Get whether the remote R session is up and running
#'
#' Get whether the remote R session is up and running, ready to accept R commands.
#' The primary use of this function is to know whether the session is ready after it has been
#' created in an asynchronous way.
#'
#' @param session An object inheriting from \code{\link{DSSession-class}}.
#' @return A logical
#'
#' @family DSSession generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' session <- dsSession(con, async = TRUE)
#' ready <- dsIsReady(session)
#' while (!ready) {
#'   Sys.sleep(1)
#'   ready <- dsIsReady(session)
#'   cat(".")
#' }
#' dsDisconnect(con)
#' }
#' @export
setGeneric("dsIsReady",
           def = function(session) standardGeneric("dsIsReady"))

#' Get the state of the remote R session
#'
#' Get a human-readable message that informs about the state of the remote R session.
#' The primary use of this function is to inform the user about the session state process 
#' after it has been created in an asynchronous way.
#'  
#' @param session An object inheriting from \code{\link{DSSession-class}}.
#' @return A character string
#'
#' @family DSSession generics
#' @examples
#' \dontrun{
#' con <- dsConnect(DSOpal::Opal(), "server1",
#'   username = "dsuser", password = "password", url = "https://opal-demo.obiba.org")
#' session <- dsSession(con, async = TRUE)
#' ready <- dsIsReady(session)
#' while (!ready) {
#'   Sys.sleep(1)
#'   ready <- dsIsReady(session)
#'   cat(dsStateMessage(session), "\n")
#' }
#' dsDisconnect(con)
#' }
#' @export
setGeneric("dsStateMessage",
           def = function(session) standardGeneric("dsStateMessage"))
