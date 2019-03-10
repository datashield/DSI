#' Searches for DSConnection objects in the environment
#'
#' If the user does not set the argument 'datasources' in the client side analysis functions, this function
#' is called to search for \code{\link{DSConnection-class}} objects in the environment (default
#' environment is the Global one). If one set of DSConnection objects is found, it is assigned
#' to 'default.connections' symbol in the analytic environment. If more than one set of DSConnection
#' objects is found and none of them is called 'default.connections', the function stops and
#' suggests user to use the \link{setDefaultDSConnections} function.
#'
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @return returns a list of \code{\link{DSConnection-class}} objects or stops the process
#' @export
findDSConnections <- function(env=getOption("datashield.env", globalenv())) {
  found <- .getDSConnections(env)
  if (found$flag == 1) {
    return (found$conns)
  } else if (found$flag == 0) {
    stop(" Are you logged in to any server? Please provide a valid DSConnection object! ", call.=FALSE)
  } else {
    for (j in 1:length(found$conns)) {
      if(found$conns[[j]] == "default.connections"){
        datasources <- eval(parse(text=found$conns[[j]]), envir=env)
        return(datasources)
      }
    }
    message(paste0("More than one list of DSConnection objects were found with no default specified: '", paste(found$conns, collapse="', '"), "'!"))
    stop("Please specify a default list of DSConnection objects using setDefaultDSConnection()", call.=FALSE)
  }
}

#' Gets the \code{\link{DSConnection-class}} objects
#'
#' The function searches for a list containing object of type \code{\link{DSConnection-class}}
#' in the current environment; if more than one list is found it return the lastest.
#' This way no matter what the user calls his connection object list it will be captured.
#'
#' @param env The environment where to search for the connection symbols.
#' @return A list of \code{\link{DSConnection-class}} objects obtained after login into the servers
#' @keywords internal
.getDSConnections <- function(env) {
  # get the names of all the objects in the current work environment
  symbols <- ls(name=env)

  # check which of the object is a list (the connection objects are kept in a list)
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
