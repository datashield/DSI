#' Data aggregation
#'
#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param expr Expression to evaluate.
#'
#' @return The result of the aggregation.
#'
#' @export
datashield.aggregate <- function(conns, expr) {
  if (is.list(conns)) {
    ress <- lapply(conns, FUN=dsAggregate, expr)
    lapply(ress, function(r) {
      dsFetch(r)
    })
  } else {
    res <- dsAggregate(conns, expr)
    dsFetch(res)
  }
}
