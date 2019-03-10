#' Set the default DSConnection objects in the analytic environment
#'
#' By default if there is only one set of \code{\link{DSConnection-class}} objects that is available for
#' analysis, all DataSHIELD client-side functions will use that full set of DSConnections unless
#' the 'datasources=' argument has been set and specifies that a particular subset of those
#' DSConnections should be used instead. The correct identification of the full
#' single set of opals is based on the \link{findDSConnections} function.
#' To illustrate, if the single set of Opals is called 'study.opals' and consists of
#' six servers numbered studies[1] to studies[6] then all client-side functions will
#' use data from all six of these 'studies' unless, say, datasources=studies[c(2,5)] is
#' declared and only data from the second and fifth studies will then be used.
#' On the other hand, if there is more than one set of DSConnections in the analytic environment
#' client-side functions will be unable to determine which set to use. The function link{findDSConnections}
#' has therefore been written so that if one of the DSConnection sets is called 'default.connections'
#' then that set - i.e. 'default.connections' - will be selected by default by all DataSHIELD
#' client-side functions. If there is more than one set of DSConnections in the analytic environment
#' and NONE of these is called 'default.connections', the function link{findDSConnections} will fail.
#' Therefore setDefaultDSConnections copies the provided set of DSConnections as 'default.connections'.
#' This set will then be selected by default by all client-side functions, unless it is deleted and
#' an alternative set of DSConnections is copied and named 'default.connections'. Regardless
#' how many sets of DSConnections exist and regardless whether any of them may be called 'default.connections',
#' the 'datasources=' argument overrides the defaults and allows the user to base his/her analysis
#' on any set of DSConnections and any subset of those DSConnections.
#'
#' @param name Symbol name that identifies the set of \code{\link{DSConnection-class}} objects to be used by default.
#' @param env The environment where to search for the connection symbols. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @export
setDefaultDSConnections <- function(name, env=getOption("datashield.env", globalenv())) {
  if(is.null(name)) {
    stop(" \n\n Please specify a named DSConnections list using the following call syntax:\n setDefaultDSConnections(name='symbol name of the DSConnections list in inverted commas')", call.=FALSE)
  }
  eval(parse(text=paste0("default.connections <- ", name)), envir = env)
}
