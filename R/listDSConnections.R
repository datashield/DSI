#' List the DSConnection objects in the analytic environment
#'
#' This function identifies and prints all \code{\link{DSConnection-class}} objects
#' in the analytic environment. If there are no DSConnection servers in the analytic
#' environment listDSConnections reminds the user that they have to login to a valid set of
#' DataSHIELD aware servers. If there is only one set of DSConnections, it copies that one set and
#' names the copy 'default.connections'. This default set will then be used by
#' default by all subsequent calls to client-side functions. If there is more than one set of DSConnections
#' in the analytic environment, listDSConnections tells the user that they can either explicitly specify the
#' DSConnections to be used by each client-side function by providing an explicit "datasources=" argument
#' for each call, or can alternatively use the \link{setDefaultDSConnections} function to specify a default
#' set of DSConnections to be used by all client-side calls unless over-ruled by the 'datasources=' argument.
#'
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @return returns a list of \code{\link{DSConnection-class}} objects and advises
#' the user how best to respond depending whether there are zero, one or multiple connections detected.
#' @export
listDSConnections <- function(env=getOption("datashield.env", globalenv())) {
  found <- .getDSConnections(env)
  if (found$flag == 0) {
    stop("Are you logged in to any server? Please provide a valid DSConnection object!", call.=FALSE)
  }

  if (found$flag==1) {
    message(paste0(" There is only one set of DSConnection objects available that is:\n"))
    print(found$conns)
    base::assign("default.connections", found$conns, envir = env)
    return(message(paste0(" This set of DSConnection objects has been copied to create 'default.connections',\n which all DataSHIELD functions will now use by default.\n If you want to change the default DSConnection objects,\n please run the function setDefaultDSConnections() again.\n")))
  }

  if (found$flag > 1) {
    message(paste0(" There is more than one set of DSConnection objects available, these are: '", paste(found$conns, collapse="', '"), "'!!"))
    return(message(paste0("\n You can either choose to specify the DSConnection objects you wish to use\n for each individual function call using the argument:\n 'datasources=name of DSConnection objects' [no inverted commas]\n or else use the setDefaultDSConnections() function\n to create the object 'default.connections', which all\n DataSHIELD functions will then use by default.")))
  } else {
    stop("End of process: please enter a valid login object with no inverted commas", call.=FALSE)
  }
}
