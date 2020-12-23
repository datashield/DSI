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
#'}
#'
#' @export
datashield.aggregate <- function(conns, expr, async=TRUE) {
  .clearLastErrors()
  rval <- NULL
  if (is.list(conns)) {
    results <- list()
    async <- lapply(conns, function(conn) { ifelse(async, dsIsAsync(conn)$aggregate, FALSE) })
    pb <- .newProgress(total = 1 + length(conns))
    # async first
    for (n in names(conns)) {
      if(async[[n]]) {
        tryCatch({
          results[[n]] <- dsAggregate(conns[[n]], expr, async=TRUE)
        }, error = function(e) {
          .appendError(n, e$message)
        })
      }
    }
    dexpr <- .deparse(expr)
    # not async (blocking calls)
    for (n in names(conns)) {
      if(!async[[n]]) {
        tryCatch({
          .tickProgress(pb, tokens = list(what = paste0("Aggregating ", conns[[n]]@name, " (", dexpr, ")")))
          results[[n]] <- dsAggregate(conns[[n]], expr, async=FALSE)
        }, error = function(e) {
          .appendError(n, e$message)
        })
      }
    }
    # polling
    rval <- replicate(length(conns), NULL)
    names(rval) <- names(conns)
    completed <- replicate(length(conns), FALSE)
    names(completed) <- names(conns)
    checks <- 1
    while (!all(completed)) {
      for (n in names(conns)) {
        if (!completed[[n]]) {
          tryCatch({
            if (!.hasLastErrors(n)) {
              if (async[[n]]) {
                .updateProgress(pb, step = length(subset(completed, completed == TRUE)), total = length(conns), tokens = list(what = paste0("Checking ", conns[[n]]@name, " (", dexpr, ")")))
                completed[[n]] <- dsIsCompleted(results[[n]])
                if (completed[[n]]) {
                  .tickProgress(pb, tokens = list(what = paste0("Getting aggregate ", conns[[n]]@name, " (", dexpr, ")")))
                  rval[[n]] <- dsFetch(results[[n]])  
                }
              } else {
                completed[[n]] <- TRUE
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
          dsKeepAlive(conns[[n]])
        }
      }
      if (!all(completed)) {
        .updateProgress(pb, step = length(subset(completed, completed == TRUE)), total = length(conns), tokens = list(what = paste0("Waiting... ", " (", dexpr, ")")))
        Sys.sleep(.getSleepTime(checks))
        checks <- checks + 1
      }
    }
    .tickProgress(pb, tokens = list(what = paste0("Aggregated (", dexpr, ")")))
  } else {
    rval <- tryCatch({
      res <- dsAggregate(conns, expr)
      dsFetch(res)
    }, error = function(e) {
      .appendError(conns@name, e$message)
      NULL
    })
  }
  .checkLastErrors()
  rval
}
