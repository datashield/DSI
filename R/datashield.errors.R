#' datashield.errors
#'
#' Retrieve and display the last errors occurred in a DataSHIELD session.
#'
#' This function retrieves the last errors occurred in a DataSHIELD session
#' and displays them in a formatted manner using bullet points.
#'
#' @param type Specify format for return message. If type == "message", a formatted message is 
#' returned. If type == "assign" a named vector is returned which will be formatted and returned 
#' within datashield.assign.
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
#' @noRd
.format_errors <- function(errors){
  no_brackets <- .remove_curly_brackets(errors)
  errors <- unlist(no_brackets)
  cohorts <- .format_cohort_colour(errors)
  errors <- paste0(cohorts, errors, "\f")
  names(errors) <- rep("x", length(errors))
  return(errors)
}

#' Format Cohort Names with Color
#'
#' This function formats cohort names with ANSI escape codes to make the text bold and yellow.
#'
#' @param errors A named character vector where the names represent cohort names and the values are error messages.
#' @return A character vector with formatted cohort names.
#' @noRd
.format_cohort_colour <- function(errors){
  cohorts <- names(errors)
  formatted <- paste0("\033[1m\033[33m", cohorts, "\033[39m", ": ")
  return(formatted)
}

#' Remove Curly Brackets from Strings
#'
#' This function replaces all curly brackets in the input strings with parentheses.
#'
#' @param errors A list of strings that may contain curly brackets.
#' @return A list of strings with curly brackets replaced by parentheses.
.remove_curly_brackets <- function(errors) {
  errors <- lapply(errors, function(x) gsub("\\{", "(", x))
  errors <- errors <- lapply(errors, function(x) gsub("\\{", ")", x))
  return(errors)
}
