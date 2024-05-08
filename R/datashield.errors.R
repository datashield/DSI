#' List R last errors
#'
#' Get the R last errors available after the datashield.assign or datashield.aggregate calls in the Datashield R session.
#'
#' @export
datashield.errors <- function() {
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.last_errors", envir = env)) {
    errors <- get(".datashield.last_errors", envir = env)
    neat <- .format_errors(errors)
    cli_bullets(neat)
  } else {
    NULL
  }
}

.format_errors <- function(errors){
  errors <- errors %>% imap_chr(~paste0(.y, ": ", .x, "\n"))
  names(errors) <- rep("x", length(errors))
  return(errors)
}
