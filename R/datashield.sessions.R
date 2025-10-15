#' R/DataSHIELD remote sessions
#'
#' Ensure that the remote R sessions are up and running during the analysis.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param async Whether the remote R/DataSHIELD session should be created asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' @param success Callback function that will be called each time an R session has been created from a connection. 
#'   The expected function signature is the connection/study name. Default is NULL (no callback).
#' @param error Callback function that will be called each time the R session creation request has failed. 
#'   The expected function signature is the connection/study name and the error message. Default is NULL (no callback).
#' @param errors.print Boolean, whether to print datashield errors in the console or return a message indicating that they can be retrieved using `datashield.errors`.
#'
#' @examples
#'\dontrun{
#' # call sessions function on server side asynchronously
#' # i.e. each study connection will create a remote R session in parallel
#' datashield.sessions(conns)
#' 
#' # call sessions function with callback functions
#' result <- datashield.sessions(conns,
#'   success = function(server) {
#'     # do something with server's success
#'   },
#'   error = function(server, error) {
#'     # do something with server's error
#'   })
#' }
#'
#' @export
datashield.sessions <- function(conns, async=TRUE, success=NULL, error=NULL, errors.print = getOption("datashield.errors.print", FALSE)) {
  .clearLastErrors()

  if (is.list(conns)) {
    # filter conns supporting session API and not having connection
    fconns <- Filter(function(conn) { !is.null(dsIsAsync(conn)$session) && !dsHasSession(conn) }, conns)
    if (length(fconns) == 0) {
      return(invisible(NULL))
    }
    
    sessions <- list()
    async <- lapply(fconns, function(conn) { ifelse(async, dsIsAsync(conn)$session, FALSE) })
    pb <- .newProgress(total = 1 + length(fconns))
    # async first
    for (n in names(fconns)) {
      if(async[[n]]) {
        tryCatch({
          sessions[[n]] <- dsSession(fconns[[n]], async=TRUE)
        }, error = function(e) {
          .appendError(n, conditionMessage(e))
          if (.is.callback(error)) {
            error(n, conditionMessage(e))
          }
        })
      }
    }
    # not async (blocking calls)
    for (n in names(fconns)) {
      if(!async[[n]]) {
        tryCatch({
          .tickProgress(pb, tokens = list(what = paste0("Session ", fconns[[n]]@name)))
          sessions[[n]] <- dsSession(fconns[[n]], async=FALSE)
        }, error = function(e) {
          .appendError(n, conditionMessage(e))
          if (.is.callback(error)) {
            error(n, conditionMessage(e))
          }
        })
      }
    }
    # polling
    completed <- replicate(length(fconns), FALSE)
    names(completed) <- names(fconns)
    checks <- 1
    while (!all(completed)) {
      messages <- c()
      for (n in names(fconns)) {
        if (!completed[[n]]) {
          if (!.hasLastErrors(n)) {
            tryCatch({
              msg <- paste0(fconns[[n]]@name, ": ", dsStateMessage(sessions[[n]]))
              messages <- append(messages, msg)
              if(async[[n]]) {
                completed[[n]] <- dsIsReady(sessions[[n]])
                if (completed[[n]]) {
                  .tickProgress(pb, tokens = list(what = paste0(fconns[[n]]@name, ": ", msg)))
                }
              } else {
                completed[[n]] <- TRUE
                .tickProgress(pb, tokens = list(what = paste0(fconns[[n]]@name, ": ", msg)))
              }
              if (completed[[n]] && .is.callback(success)) {
                success(n)
              }
            }, error = function(e) {
              .appendError(n, conditionMessage(e))
              completed[[n]] <- TRUE
              if (.is.callback(error)) {
                error(n, conditionMessage(e))
              }
            })
          } else {
            completed[[n]] <- TRUE
          }
        } else {
          # heart beat request
          dsKeepAlive(fconns[[n]])
        }
      }
      if (!all(completed)) {
        .updateProgress(pb, step = length(subset(completed, completed == TRUE)), total = length(fconns), tokens = list(what = paste(messages, collapse = ", ")))
        Sys.sleep(.getSleepTime(checks))
        checks <- checks + 1
      }
    }
    ignore <- .tickProgress(pb, tokens = list(what = paste0("All R sessions ready")))
  } else if (!is.null(dsIsAsync(conns)$session)) {
    tryCatch({
      if (!dsHasSession(conns)) {
        dsSession(conns, async = FALSE)
      }
      if (.is.callback(success)) {
        success(conns@name)
      }
    }, error = function(e) {
      .appendError(conns@name, conditionMessage(e))
      if (.is.callback(error)) {
        error(conns@name, conditionMessage(e))
      }
    })
  }
  .handle_errors(errors.print)
  invisible(NULL)
}
