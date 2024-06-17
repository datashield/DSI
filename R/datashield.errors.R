#' datashield.errors
#'
#' Retrieve and display the last errors occurred in a DataSHIELD session.
#'
#' This function retrieves the last errors occurred in a DataSHIELD session
#' and displays them in a formatted manner using bullet points.
#'
#' @return NULL if no errors are found, otherwise prints the errors.
#' @importFrom cli cli_bullets
#' @export
datashield.errors <- function(type = "message") {
  env <- getOption("datashield.env", globalenv())
  if (exists(".datashield.last_errors", envir = env)) {
    errors <- get(".datashield.last_errors", envir = env)
    neat <- .format_errors(errors)
    if(type == "assign") {
      return(neat) 
    } else if(type == "message"){
      cli_bullets(neat) 
    }
      
  } else {
    NULL
  }
}

#' Format Errors
#'
#' Format errors into a character vector with specified prefix.
#'
#' This function formats a list of errors into a character vector with each
#' error message prefixed by a cross.
#'
#' @param errors A list of errors to be formatted.
#' @return A character vector containing formatted error messages.
#' @importFrom dplyr %>%
#' @importFrom purrr imap_chr
#' @importFrom stringr str_replace_all
#' @noRd
.format_errors <- function(errors){
  errors <- errors %>% map(~str_replace_all(.x, "\\{", "("))
  errors <- errors %>% map(~str_replace_all(.x, "\\}", ")"))
  errors <- errors %>% imap_chr(~paste0(.y, ": ", .x, "\n"))
  names(errors) <- rep("x", length(errors))
  return(errors)
}
  
  
  


