#' List R last errors
#'
#' Get the R last errors available after the datashield.assign or datashield.aggregate calls in the Datashield R session.
#'
#' @export
datashield.errors <- function() {
  .inform_once(.new_errors_message(), "error_id")
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.last_errors", envir = env)) {
    get(".datashield.last_errors", envir = env)
  } else {
    NULL
  }
}

.new_errors_message <- function() {
  msg <- c(
    "Errors can now be automatically printed, rather than requiring a call to 
  `datashield.errors()`.",
    "To enable this behavior, run `options('datashield.return_errors' = FALSE)`"
  )
  names(msg) <- c("i", ">")
  return(msg)
}

inform_env <- new.env()
.inform_once <- function(msg, id = msg) {
  if (exists(id, envir = inform_env, inherits = FALSE)) {
    return(invisible(NULL))
  }
  inform_env[[id]] <- TRUE
  cli_bullets(msg)
  cli_inform(col_silver("This message is displayed once per session."))
  cat("\n")
}
