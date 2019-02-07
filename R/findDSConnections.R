#' Searches for \code{\link{DSConnection-class}} objects in the environment
#'
#' If the user does not set the argument 'datasources', this function
#' is called to searches for opal login objects in the environment. If more than one
#' login object is found a prompt asks the user to choose one and if none is found
#' the process stops.
#'
#' @return returns a list of \code{\link{DSConnection-class}} objects or stops the process
#' @export
#'
findDSConnections <- function() {
  found <- .getDSConnections()
  if (found$flag == 1) {
    return (found$conns)
  } else if (found$flag == 0) {
    stop(" Are you logged in to any server? Please provide a valid DSConnection object! ", call.=FALSE)
  } else {
    message(paste0("More than one list of DSConnection objects were found: '", paste(found$conns,collapse="', '"), "'!"))
    userInput <- readline("Please enter the name of the DSConnection objects list you want to use: ")
    datasources <- eval(parse(text=userInput))
    if (.isDSConnection(datasources[[1]])) {
      return (datasources)
    } else {
      stop("End of process: you failed to enter a valid connection object", call.=FALSE)
    }
  }
}

#' Gets the opal objects
#'
#' The function searches for a list containing object of type \code{\link{DSConnection-class}}
#' in the current environment; if more than one list is found it return the lastest.
#' This way no matter what the user calls his opal login object it will be captured.
#'
#' @return a list of opal object obtained after login into the servers
#' @import rlang
#' @keywords internal
.getDSConnections <- function() {
  # get the names of all the objects in the current work environment
  symbols <- ls(name=rlang::global_env())

  # check which of the object is a list (the opal objects are kept in a list)
  if (length(symbols) > 0) {
    connlist <- c()
    flag <- 0
    for (i in 1:length(symbols)) {
      obj <- eval(parse(text=symbols[i]))
      if ("list" %in% class(obj)) {
        # if an object is not an empty list check if its elements are of type DSConnection
        if (length(obj) > 0) {
          if (.isDSConnection(obj[[1]])) {
            connlist <- append(connlist, symbols[i])
            flag <- 1
          }
        }
      }
    }
    if (flag == 1) {
      if (length(connlist) > 1) {
        flag <- 2
        return(list(flag=flag, conns=connlist))
      } else {
        pp <- connlist[[1]]
        conns <- eval(parse(text=pp))
        return(list(flag=flag, conns=conns))
      }
    }
  }
  return(list(flag=0, conns=NULL))
}

#' Check if provided object is a S4 class instance and if this class inherits from \code{\link{DSConnection-class}}.
#' @keywords internal
.isDSConnection <- function(obj) {
  if (isS4(obj)) {
    cls <- getClass(class(obj)[[1]])
    "DSConnection" %in% names(cls@contains)
  } else {
    FALSE
  }
}
