
#' List of DataSHIELD profiles
#'
#' Get the list of all the DataSHIELD profiles from the different data repositories: available ones
#' and currently applied to each connection.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @return Profiles details from all the servers.
#' @export
datashield.profiles <- function(conns) {
  
  as.df <- function(profiles) {
    allProfiles <- NULL
    for (std in names(profiles)) {
      allProfiles <- append(allProfiles, profiles[[std]]$available)
    }
    allProfiles <- unique(allProfiles)
    studies <- list()
    for (std in names(profiles)) {
      stdProfiles <- NULL
      for (p in allProfiles) {
        stdProfiles <- append(stdProfiles, p %in% profiles[[std]]$available)
      }
      studies[[std]] <- stdProfiles
    }
    df <- as.data.frame(studies)
    row.names(df) <- allProfiles
    rval <- list(available = df)
    df <- as.data.frame(sapply(profiles, function(s) s$current))
    colnames(df) <- "profile"
    rval$current <- df
    rval
  }
  
  onProfilesError <- function(e) {
    warning("Can't list profiles (is your DS driver library up to date?), using 'default' profile.")
    list(available = "default", current = "default")
  }
  
  if (is.list(conns)) {
    as.df(lapply(conns, function(c) { tryCatch(dsListProfiles(c), error = onProfilesError) }))
  } else { 
    lconns <- list()
    lconns[[conns@name]] <- conns
    as.df(lapply(lconns, function(c) { tryCatch(dsListProfiles(c), error = onProfilesError) }))
  }
}

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
  
  pkgs <- NULL
  
  # get packages from methods info
  for(type in types) {
    res <- lapply(cs, function(c) { dsListMethods(c, type) })
    type_pkgs <- lapply(res, function(r) { 
      if (nrow(r)>0) {
        unique(data.frame(package = r$package, version = r$version))
      } else {
        data.frame()
      } 
    })
    type_pkgs
    
    # merge package info
    if (is.null(pkgs)) {
      pkgs <- type_pkgs
    } else {
      for (n in names(type_pkgs)) {
        if (nrow(pkgs[[n]]) == 0) {
          pkgs[[n]] <- type_pkgs[[n]]
        } else if (nrow(type_pkgs[[n]])>0) {
          pkgs[[n]] <- unique(rbind(pkgs[[n]], type_pkgs[[n]]))
        }
      }
    }
  }
  
  # package names
  unique_pkgs <- unique(unlist(lapply(pkgs,function(x) x$package)))
  
  # package status
  status <- sapply(pkgs, function(x) { sapply(unique_pkgs, function(y) { y %in% x$package }) })
  status
  if (!"matrix" %in% class(status)) {
    status <- t(as.matrix(status))
    dimnames(status) <- list(unique_pkgs, names(pkgs))
  }
  status
  
  # package versions
  versions <- sapply(pkgs, function(x) { sapply(unique_pkgs, function(y) {
    pkg.in.study <- as.character(x$package) #avoid problem with different factor levels (e.g: one study has less levels than pkg_tocheck)
    idx <- which(y == pkg.in.study)
    idx <- idx[1]
    return (ifelse(is.na(idx), NA, x$version[idx]))
  })
  })
  versions
  if (!"matrix" %in% class(versions)) {
    versions <- t(as.matrix(versions))
    dimnames(versions) <- list(unique_pkgs, names(pkgs))
  }
  versions
  
  list(package_status = status, version_status = versions)
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
    stop("Package ", name, " is not installed on any of the servers. Minimum version required is ", version, call. = FALSE)
  }

  minversion <- numeric_version(version)
  connList <- conns
  if (!is.list(conns)) {
    connList <- list(conns)
  }
  for (conn in connList) {
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

#' Status of some resources
#'
#' Get whether some identified resources are accessible in each of the data repositories.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param resource Fully qualified name of a resource in the data repository (can be a vector or must be
#'   the same in each data repository); or a named list of fully qualified resource names (one per server
#'   name); or a data frame with 'server' and 'resource' columns (such as the one that is used in
#'   \code{\link{datashield.login}})
#' @return Resource status for each server.
#' @import progress
#' @export
datashield.resource_status <- function(conns, resource) {
  # prepare resources as a named list
  resources <- .asNamedListOfResources(conns, resource)
  # make a named list of connections
  cs <- .asNamedListOfConnections(conns)

  server <- names(cs)
  tbl <- rep(NA, length(server))
  accessible <- rep(NA, length(server))
  pb <- .newProgress(total = 1 + length(server))
  for (i in 1:length(server)) {
    name <- server[i]
    if (!is.null(resources[[name]])) {
      tbl[i] <- resources[[name]]
      accessible[i] <- dsHasResource(cs[[name]], resources[[name]])
    }
    .tickProgress(pb, tokens = list(what = paste0("Checking ", name)))
  }
  .tickProgress(pb, tokens = list(what = "All servers checked"))
  data.frame(server, resource=tbl, accessible)
}
