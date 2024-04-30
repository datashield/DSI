#' List R last errors
#'
#' Get the R last errors available after the datashield.assign or datashield.aggregate calls in the Datashield R session.
#'
#' @export
datashield.errors <- function() {
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.last_errors", envir = env)) {
    errors <- get(".datashield.last_errors", envir = env)
    .format_errors(errors)
  } else {
    NULL
  }
}

.format_errors <- function(errors){
  errors <- errors %>% imap(~paste0("Error in server ", .y, "\n", .x, "\n\n"))
  errors %>% walk(~cli_alert_warning(.x))
}
