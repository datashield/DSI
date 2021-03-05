#' Data aggregation
#'
#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @return The result of the aggregation
#'
#' @examples
#'\dontrun{
#' # call aggregate function on server side
#' datashield.aggregate(conns, expr = quote(someFunction(D, 123)))
#' 
#' # call aggregate functions that are defined in the provided named list. 
#' # Connections are filtered by the list names.
#' datashield.aggregate(conns,
#'   list(server1=quote(someFunction(D, 123)), server2=quote(someFunction(G, 456)))
#' }
#'
#' @export
datashield.aggregate <- function(conns, expr, async=TRUE) {
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
          .appendError(n, e$message)
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
          .appendError(n, e$message)
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
                }
              } else {
                completed[[n]] <- TRUE
                .tickProgress(pb, tokens = list(what = paste0("Getting aggregate ", fconns[[n]]@name, " (", dexpr, ")")))
                rval[[n]] <- dsFetch(results[[n]])
              }
            } else {
              completed[[n]] <- TRUE
              rval[[n]] <- NULL
            }
          }, error = function(e) {
            .appendError(n, e$message)
            completed[[n]] <- TRUE
            rval[[n]] <- NULL
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
      .appendError(conns@name, e$message)
      NULL
    })
  }
  .checkLastErrors()
  rval
}
