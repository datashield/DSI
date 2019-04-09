
#' List of DataSHIELD methods
#'
#' Get the list of all the DataSHIELD methods from the different data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @return Methods details from all the servers.
#' @export
datashield.methods <- function(conns, type = "aggregate") {
  if (is.list(conns)) {
    methods <- lapply(conns, function(c) { dsListMethods(c, type) })
    ms <- NULL
    for (server in names(methods)) {
      mdf <- methods[[server]]
      mdf$server <- rep(server, length(mdf$name))
      if (is.null(ms)) {
        ms <- mdf
      } else {
        ms <- rbind(ms, mdf)
      }
    }
    ms
  } else {
    dsListMethods(conns, type)
  }
}

#' Status of the DataSHIELD methods
#'
#' Get the status of the DataSHIELD methods in the different data repositories to check if any method is missing.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @return Methods availability on each server.
#' @export
datashield.method_status <- function(conns, type = "aggregate") {
  # make a named list of connections
  cs <- .asNamedListOfConnections(conns)

  methods <- lapply(cs, function(c) { dsListMethods(c, type) })
  #unique method names
  unique_name <- unique(unlist(lapply(methods, function(x) x$name)))

  #loop over each study and look if each function (y) in unique name is present in study (x)
  status <- sapply(methods, function(x) { sapply(unique_name, function(y) { y %in% x$name }) })

  if (is.null(unique_name)) {
    data.frame()
  } else {
    data.frame(name = unique_name, type=type, status)
  }
}

#' Status of the DataSHIELD packages
#'
#' Get the status of the DataSHIELD packages in the different data repositories to check if any package is missing.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @return Packages status for each server.
#' @export
datashield.pkg_status <- function(conns) {
  # make a named list of connections
  cs <- .asNamedListOfConnections(conns)
  types = c('aggregate','assign')
  df_pkges <- data.frame(NULL)
  df_verses <- data.frame(NULL)
  pkg_checked <- NULL

  for(type in types){
    res <- lapply(cs, function(c) { dsListMethods(c, type) })
    #package names by type
    unique_pkg <- unique(unlist(lapply(res,function(x) x$package)))

    #new package to check
    pkg_tocheck <- subset(unique_pkg,!unique_pkg %in% pkg_checked)

    #pkg_tbl
    status <- sapply(res, function(x) { sapply(pkg_tocheck, function(y) { y %in% x$package }) })
    df_pkg <- data.frame(package = pkg_tocheck, status)
    df_pkges <- rbind(df_pkges, df_pkg)

    #pkg_vers
    vers <- sapply(res, function(x) {
      sapply(pkg_tocheck, function(y) {
        pkg.in.study <- as.character(x$package) #avoid problem with different factor levels (e.g: one study has less levels than pkg_tocheck)
        idx <- which(y == pkg.in.study)
        idx <- idx[1]
        return (x$version[idx])
      })
    })

    df_vers <- data.frame(package = pkg_tocheck, vers)
    df_verses <- rbind(df_verses, df_vers)

    #update already checked package
    pkg_checked <- c(pkg_checked, pkg_tocheck)
  }

  df_pkges <- subset(df_pkges, !is.na(df_pkges$package)) #take only valid packages (eg: <NA> is not package)
  df_verses <- subset(df_verses, !is.na(df_verses$package)) #take only valid packages (eg: <NA> is not package)

  list(package_status = unique(df_pkges), version_status = unique(df_verses))
}

#' Check server-side package minimum version
#'
#' Check for each of the server, accessible through provided \code{\link{DSConnection-class}} objects, whether the installed
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param name The name of the server-side package.
#' @param version The minimum package version number to be matched.
#' @param env Environment where the package status result should be cached. Try to get it from the
#' 'datashield.env' option, with default to the Global Environment.
#' @export
datashield.pkg_check <- function(conns, name, version, env=getOption("datashield.env", globalenv())) {
  status <- NULL
  if (exists(".datashield.pkg_status", envir = env)) {
    status <- get(".datashield.pkg_status", envir = env)
  }
  if (is.null(status)) {
    status <- datashield.pkg_status(conns)
    assign(".datashield.pkg_status", status, envir = env)
  }

  version_status <- status$version_status
  pkg_version_status <- version_status[version_status$package == name,]
  if (nrow(pkg_version_status) == 0) {
    stop("Package ", name, " is not installed in any of the servers. Minimum version required is ", version, call. = FALSE)
  }

  minversion <- numeric_version(version)
  for (conn in conns) {
    server <- conn@name
    v <- pkg_version_status[[server]]
    if (is.na(v)) {
      stop("Package ", name, " is not installed on server ", server, ". Minimum version required is ", version, call. = FALSE)
    }
    if (numeric_version(v) < minversion) {
      stop("Package ", name, " on server ", server, " has not the minimum version required: ", v , " < ", version, call. = FALSE)
    }
  }

}

#' Status of some tables
#'
#' Get whether some identified tables are accessible in each of the data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param table Fully qualified name of a table in the data repository (can be a vector or must be
#'   the same in each data repository); or a named list of fully qualified table names (one per server
#'   name); or a data frame with 'server' and 'table' columns (such as the one that is used in
#'   \code{\link{datashield.login}})
#' @return Table status for each server.
#' @import progress
#' @export
datashield.table_status <- function(conns, table) {
  # prepare tables as a named list
  tables <- .asNamedListOfTables(conns, table)
  # make a named list of connections
  cs <- .asNamedListOfConnections(conns)

  server <- names(cs)
  tbl <- rep(NA, length(server))
  accessible <- rep(NA, length(server))
  pb <- .newProgress(total = 1 + length(server))
  for (i in 1:length(server)) {
    name <- server[i]
    if (!is.null(tables[[name]])) {
      tbl[i] <- tables[[name]]
      accessible[i] <- dsHasTable(cs[[name]], tables[[name]])
    }
    .tickProgress(pb, tokens = list(what = paste0("Checking ", name)))
  }
  .tickProgress(pb, tokens = list(what = "All servers checked"))
  data.frame(server, table=tbl, accessible)
}
