#' List the DSConnection objects in the analytic environment
#'
#' This function identifies and prints all \code{\link{DSConnection-class}} objects
#' in the analytic environment. If there are no DSConnection servers in the analytic
#' environment \link{datashield.connections} reminds the user that they have to login to a valid set of
#' DataSHIELD aware servers. If there is only one set of DSConnections, it copies that one set and
#' names the copy 'default.connections'. This default set will then be used by
#' default by all subsequent calls to client-side functions. If there is more than one set of DSConnections
#' in the analytic environment, \link{datashield.connections} tells the user that they can either explicitly specify the
#' DSConnections to be used by each client-side function by providing an explicit "datasources=" argument
#' for each call, or can alternatively use the \link{datashield.connections_default} function to specify a default
#' set of DSConnections to be used by all client-side calls unless over-ruled by the 'datasources=' argument.
#'
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @return Returns a list of \code{\link{DSConnection-class}} objects and advises
#' the user how best to respond depending whether there are zero, one or multiple connections detected.
#'
#' @family Connections management
#' @export
datashield.connections <- function(env=getOption("datashield.env", globalenv())) {
  found <- .getDSConnections(env)
  if (found$flag == 0) {
    stop("Are you logged in to any server? Please provide a valid DSConnection object!", call.=FALSE)
  }

  if (found$flag==1) {
    message(paste0(" There is only one set of DSConnection objects available that is:\n"))
    print(found$conns)
    base::assign("default.connections", found$conns, envir = env)
    return(message(paste0(" This set of DSConnection objects has been copied to create 'default.connections',\n which all DataSHIELD functions will now use by default.\n If you want to change the default DSConnection objects,\n please run the function datashield.connections_default() again.\n")))
  }

  if (found$flag > 1) {
    message(paste0(" There is more than one set of DSConnection objects available, these are: '", paste(found$conns, collapse="', '"), "'."))
    if ("default.connections" %in% found$conns) {
      message(" Showing 'default.connections':\n")
      print(datashield.connections_default(env = env))
    } else {
      message(paste0("\n You can either choose to specify the DSConnection objects you wish to use\n for each individual function call using the argument:\n 'datasources=name of DSConnection objects' [no inverted commas]\n or else use the datashield.connections_default() function\n to create the object 'default.connections', which all\n DataSHIELD functions will then use by default."))
    }
  } else {
    stop("End of process: please enter a valid login object with no inverted commas", call.=FALSE)
  }
}

#' Set or get the default list of DSConnection objects in the analytic environment
#'
#' By default if there is only one set of \code{\link{DSConnection-class}} objects that is available for
#' analysis, all DataSHIELD client-side functions will use that full set of DSConnections unless
#' the 'datasources=' argument has been set and specifies that a particular subset of those
#' DSConnections should be used instead. The correct identification of the full
#' single set of opals is based on the \link{datashield.connections_find} function.
#' To illustrate, if the single set of Opals is called 'study.opals' and consists of
#' six servers numbered studies[1] to studies[6] then all client-side functions will
#' use data from all six of these 'studies' unless, say, datasources=studies[c(2,5)] is
#' declared and only data from the second and fifth studies will then be used.
#' On the other hand, if there is more than one set of DSConnections in the analytic environment
#' client-side functions will be unable to determine which set to use. The function \link{datashield.connections_find}
#' has therefore been written so that if one of the DSConnection sets is called 'default.connections'
#' then that set - i.e. 'default.connections' - will be selected by default by all DataSHIELD
#' client-side functions. If there is more than one set of DSConnections in the analytic environment
#' and NONE of these is called 'default.connections', the function \link{datashield.connections_find} will fail.
#' Therefore datashield.connections_default copies the provided set of DSConnections as 'default.connections'.
#' This set will then be selected by default by all client-side functions, unless it is deleted and
#' an alternative set of DSConnections is copied and named 'default.connections'. Regardless
#' how many sets of DSConnections exist and regardless whether any of them may be called 'default.connections',
#' the 'datasources=' argument overrides the defaults and allows the user to base his/her analysis
#' on any set of DSConnections and any subset of those DSConnections.
#'
#' @param name Symbol name that identifies the set of \code{\link{DSConnection-class}} objects to be used by default. If not provided,
#'  the 'default.connections' variable value is returned.
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @return The 'default.connections' value from the analytic environment or NULL if the 'default.connections' symbol is not defined.
#'
#' @family Connections management
#' @export
datashield.connections_default <- function(name=NULL, env=getOption("datashield.env", globalenv())) {
  if(is.null(name)) {
    if (base::exists("default.connections", envir = env)) {
      return(base::get("default.connections", envir = env))
    } else {
      return(NULL)
    }
  }
  .clearCache(env)
  base::assign("default.connections", base::get(name, envir = env), envir = env)
}

#' Search for DSConnection objects in the analytic environment
#'
#' If the user does not set the argument 'datasources' in the client side analysis functions, this function
#' is called to search for \code{\link{DSConnection-class}} objects in the environment (default
#' environment is the Global one). If one set of DSConnection objects is found, it is assigned
#' to 'default.connections' symbol in the analytic environment. If more than one set of DSConnection
#' objects is found and none of them is called 'default.connections', the function stops and
#' suggests user to use the \link{datashield.connections_default} function.
#'
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @return Returns a list of \code{\link{DSConnection-class}} objects or stops the process
#'
#' @family Connections management
#' @export
datashield.connections_find <- function(env=getOption("datashield.env", globalenv())) {
  found <- .getDSConnections(env)
  if (found$flag == 1) {
    return (found$conns)
  } else if (found$flag == 0) {
    stop(" Are you logged in to any server? Please provide a valid DSConnection object! ", call.=FALSE)
  } else {
    for (j in 1:length(found$conns)) {
      if(found$conns[[j]] == "default.connections"){
        datasources <- base::get(found$conns[[j]], envir=env)
        return(datasources)
      }
    }
    message(paste0("More than one list of DSConnection objects were found with no default specified: '", paste(found$conns, collapse="', '"), "'!"))
    stop("Please specify a default list of DSConnection objects using datashield.connections_default()", call.=FALSE)
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
  symbols <- base::ls(name=env)

  # check which of the object is a list (the connection objects are kept in a list)
  if (length(symbols) > 0) {
    connlist <- c()
    flag <- 0
    for (i in 1:length(symbols)) {
      obj <- base::get(symbols[i], envir = env)
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
        conns <- base::get(pp, envir = env)
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
