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
    rval <- lapply(names(conns), function(n) {
      tryCatch({
        if (!.hasLastErrors(n)) {
          if(async[[n]]) {
            .tickProgress(pb, tokens = list(what = paste0("Aggregating ", conns[[n]]@name, " (", dexpr, ")")))
            dsFetch(results[[n]])
          } else {
            dsFetch(results[[n]])
          }
        } else {
          NULL
        }
      }, error = function(e) {
        .appendError(n, e$message)
        NULL
      })
    })
    .tickProgress(pb, tokens = list(what = paste0("Aggregated (", dexpr, ")")))
    names(rval) <- names(conns)
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
