#' List R last errors
#'
#' Get the R last errors available after the datashield.assign or datashield.aggregate calls in the Datashield R session.
#'
#' @export
datashield.errors <- function() {
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.last_errors", envir = env)) {
    get(".datashield.last_errors", envir = env)
  } else {
    NULL
  }
}
