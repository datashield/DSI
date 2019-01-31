
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
#' @param conns A list of \code{\link{DSConnection-class}}s.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @return Methods availability on each server.
#' @export
datashield.method_status <- function(conns, type = "aggregate") {
  methods <- lapply(conns, function(c) { dsListMethods(c, type) })
  #unique method names
  unique_name <- unique(unlist(lapply(methods, function(x) x$name)))

  #loop over each study and look if each function (y) in unique name is present in study (x)
  status <- sapply(methods, function(x) { sapply(unique_name, function(y) { y %in% x$name }) })

  data.frame(name = unique_name, type=type, status)
}

#' Status of the DataSHIELD packages
#'
#' Get the status of the DataSHIELD packages in the different data repositories to check if any package is missing.
#'
#' @param conns A list of \code{\link{DSConnection-class}}s.
#' @return Packages status for each server.
#' @export
datashield.pkg_status <- function(conns) {
  types = c('aggregate','assign')
  df_pkges <- data.frame(NULL)
  df_verses <- data.frame(NULL)
  pkg_checked <- NULL

  for(type in types){
    res <- lapply(conns, function(c) { dsListMethods(c, type) })
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

#' Status of some tables
#'
#' Get whether some identified tables are accessible in each of the data repositories.
#'
#' @param conns A list of \code{\link{DSConnection-class}}s.
#' @param tables A named list of fully qualified table names, per servername.
#' @return Table status for each server.
#' @export
datashield.table_status <- function(conns, tables) {
  server <- names(conns)
  table <- rep(NA, length(server))
  accessible <- rep(NA, length(server))
  for (i in 1:length(server)) {
    name <- server[i]
    if (!is.null(tables[[name]])) {
      table[i] <- tables[[name]]
      accessible[i] <- dsHasTable(conns[[name]], tables[[name]])
    }
  }
  data.frame(server, table, accessible)
}
