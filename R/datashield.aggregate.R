#' Data aggregation
#'
#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' @param success Callback function that will be called each time an aggregation result is received from a connection. 
#'   The expected function signature is the connection/study name and the result value. Default is NULL (no callback).
#' @param error Callback function that will be called each time the aggregation request has failed. 
#'   The expected function signature is the connection/study name and the error message. Default is NULL (no callback).
#' @param return_errors Boolean, whether to print datashield errors in the console or return a message indicating that they can be retrieved using `datashield.errors`.
#' @return The result of the aggregation
#'
#' @examples
#'\dontrun{
#' # call aggregate function on server side asynchronously
#' # i.e. each study connection will process the request in parallel
#' result <- datashield.aggregate(conns, expr = quote(someFunction(D, 123)))
#'
#' # call aggregate function on server side synchronously, i.e. each study 
#' # connection will be called, one after the other, in a blocking way 
#' result <- datashield.aggregate(conns, expr = quote(someFunction(D, 123)), async = FALSE)
#' 
#' # call aggregate functions that are defined in the provided named list. 
#' # Connections are filtered by the list names.
#' result <- datashield.aggregate(conns,
#'   list(server1=quote(someFunction(D, 123)), server2=quote(someFunction(G, 456))))
#' 
#' # call aggregate function with callback functions
#' result <- datashield.aggregate(conns, expr = quote(someFunction(D, 123)),
#'   success = function(server, res) {
#'     # do something with server's result value
#'   },
#'   error = function(server, error) {
#'     # do something with server's error message
#'   })
#' }
#'
#' @export
datashield.aggregate <- function(conns, expr, async=TRUE, success=NULL, error=NULL, return_errors = getOption("datashield.return_errors", TRUE)) {
  .clearLastErrors()
  rval <- NULL
  
  # prepare expressions as a named list
  exprs <- .asNamedListOfValues(conns, expr)
  
  if (is.list(conns)) {
    # filter connections to aggregate 
    fconns <- .filterConnectionsByName(conns, names(exprs))
    
    results <- list()
    async <- lapply(fconns, function(conn) { ifelse(async, dsIsAsync(conn)$aggregate, FALSE) })
    pb <- .newProgress(total = 1 + length(fconns))
    # async first
    for (n in names(fconns)) {
      if(async[[n]]) {
        tryCatch({
          results[[n]] <- dsAggregate(fconns[[n]], exprs[[n]], async=TRUE)
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
          .tickProgress(pb, tokens = list(what = paste0("Aggregating ", fconns[[n]]@name, " (", .deparse(exprs[[n]]), ")")))
          results[[n]] <- dsAggregate(fconns[[n]], exprs[[n]], async=FALSE)
        }, error = function(e) {
          .appendError(n, conditionMessage(e))
          if (.is.callback(error)) {
            error(n, conditionMessage(e))
          }
        })
      }
    }
    # polling
    rval <- replicate(length(fconns), NULL)
    names(rval) <- names(fconns)
    completed <- replicate(length(fconns), FALSE)
    names(completed) <- names(fconns)
    checks <- 1
    while (!all(completed)) {
      for (n in names(fconns)) {
        dexpr <- .deparse(exprs[[n]])
        if (!completed[[n]]) {
          tryCatch({
            if (!.hasLastErrors(n)) {
              .updateProgress(pb, step = length(subset(completed, completed == TRUE)), total = length(fconns), tokens = list(what = paste0("Checking ", fconns[[n]]@name, " (", dexpr, ")")))
              if (async[[n]]) {
                completed[[n]] <- dsIsCompleted(results[[n]])
                if (completed[[n]]) {
                  .tickProgress(pb, tokens = list(what = paste0("Getting aggregate ", fconns[[n]]@name, " (", dexpr, ")")))
                  rval[[n]] <- dsFetch(results[[n]])  
                  if (.is.callback(success)) {
                    success(n, rval[[n]])
                  }
                }
              } else {
                completed[[n]] <- TRUE
                .tickProgress(pb, tokens = list(what = paste0("Getting aggregate ", fconns[[n]]@name, " (", dexpr, ")")))
                rval[[n]] <- dsFetch(results[[n]]) 
                if (.is.callback(success)) {
                  success(n, rval[[n]])
                }
              }
            } else {
              completed[[n]] <- TRUE
              rval[[n]] <- NULL
            }
          }, error = function(e) {
            .appendError(n, conditionMessage(e))
            completed[[n]] <- TRUE
            rval[[n]] <- NULL
            if (.is.callback(error)) {
              error(n, conditionMessage(e))
            }
          })
        } else {
          # heart beat request
          dsKeepAlive(fconns[[n]])
        }
      }
      if (!all(completed)) {
        .updateProgress(pb, step = length(subset(completed, completed == TRUE)), total = length(fconns), tokens = list(what = paste0("Waiting... ", " (", ifelse(is.vector(expr), "...", .deparse(expr)), ")")))
        Sys.sleep(.getSleepTime(checks))
        checks <- checks + 1
      }
    }
    .tickProgress(pb, tokens = list(what = paste0("Aggregated (", ifelse(is.vector(expr), "...", .deparse(expr)), ")")))
  } else {
    rval <- tryCatch({
      res <- dsAggregate(conns, exprs[[conns@name]])
      dsFetch(res)
    }, error = function(e) {
      .appendError(conns@name, conditionMessage(e))
      if (.is.callback(error)) {
        error(conns@name, conditionMessage(e))
      }
      NULL
    })
    if (.is.callback(success) && !.hasLastErrors(conns@name)) {
      success(conns@name, rval)
    }
  }
  if(return_errors == TRUE){
    returned_errors <- datashield.errors(type = "assign")
    if(!is.null(returned_errors)) {
      cli_abort(c("There are some DataSHIELD errors: ", returned_errors), call = NULL)  
    }
  } else if(return_errors == FALSE){
    .checkLastErrors()
  }
  invisible(NULL)
  rval
}
