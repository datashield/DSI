#' Logs in a DataSHIELD R sessions and optionaly assigns variables to R
#'
#' This function allows for clients to login to data repository servers and (optionaly)
#' assign all the data or specific variables from the data repositories tables to R data
#' frames. The assigned dataframes (one for each data repository) are named 'D' (by default).
#'
#' @param logins A dataframe table that holds login details. This table holds five elements required
#'   to login to the servers where the data to analyse is stored. The expected column names are
#'   'driver' (the \code{\link{DSDriver-class}} name, default is "OpalDriver"),
#'   'server' (the server name), url' (the server url), 'user' (the user name or the certificate file path),
#'   'password' (the user password or the private key file path), table' (the fully qualified name of
#'   the table in the data repository), 'options' (the SSL options). An additional column 'identifiers'
#'   can be specified for identifiers mapping (if supported by data repository). See also the documentation
#'   of the examplar input table \code{logindata} for details of the login elements.
#' @param assign A boolean which tells whether or not data should be assigned from the data repository
#'   table to R after login into the server(s).
#' @param variables Specific variables to assign. If \code{assign} is set to FALSE this argument is ignored
#'   otherwise the specified variables are assigned to R. If no variables are specified (default) the whole
#'   data repository's table is assigned.
#' @param symbol A character, the name of the data frame to which the data repository's table will be
#'   assigned after login into the server(s).
#' @param username Default user name to be used in case it is not specified in the logins structure.
#' @param password Default user password to be used in case it is not specified in the logins structure.
#' @param opts Default SSL options to be used in case it is not specified in the logins structure.
#' @param restore The workspace name to restore (optional).
#' @return object(s) of class DSConnection
#' @export
#' @import progress
#' @examples
#'\dontrun{
#'
#'#### The below examples illustrate an analysises that use test/simulated data ####
#'
#'# build your data.frame
#'server <- c("study1", "study2")
#'url <- c("https://some.opal.host:8443","https://another.opal.host")
#'user <- c("user1", "datashield-certificate.pem")
#'password <- c("user1pwd", "datashield-private.pem")
#'table <- c("store.Dataset","foo.DS")
#'options <- c("","c(ssl.verifyhost=2,ssl.verifypeer=1)")
#'driver <- c("","OpalDriver")
#'logindata <- data.frame(server,url,user,password,table,options,driver)
#'
#'# or load the data.frame that contains the login details
#'data(logindata)
#'
#'# Example 1: just login (default)
#'connections <- datashield.login(logins=logindata)
#'
#'# Example 2: login and assign the whole dataset
#'connections <- datashield.login(logins=logindata,assign=TRUE)
#'
#'# Example 3: login and assign specific variable(s)
#'myvar <- list("LAB_TSC")
#'connections <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'}
#'
datashield.login <- function(logins=NULL, assign=FALSE, variables=NULL, symbol="D",
                             username=getOption("datashield.username"), password=getOption("datashield.password"),
                             opts=getOption("datashield.opts", list()), restore=NULL){

  defaultDriver <- "OpalDriver"

  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop("Provide valid login details!", call.=FALSE)
  }

  # studies names
  stdnames <- as.character(logins$server)
  # URLs
  urls <- as.character(logins$url)
  # usernames
  userids <- as.character(logins$user)
  # passwords
  pwds <- as.character(logins$password)
  # table fully qualified name
  paths <- as.character(logins$table)
  # identifiers mapping
  idmappings <- logins$identifiers
  if (is.null(idmappings)) {
    idmappings <- rep("", length(stdnames))
  }
  # DSConnection specific options
  options <- logins$options
  if (is.null(options)) {
    options <- rep("", length(stdnames))
  }
  # DSDriver class name for instanciation
  drivers <- unlist(lapply(logins$driver, function(d) {
    if (is.null(d) || length(d) == 0) {
      defaultDriver
    } else {
      d
    }
  }))
  if (is.null(drivers)) {
    drivers <- rep(defaultDriver, length(stdnames))
  }
  drivers <- as.character(drivers)

  # name of the assigned dataframe - check the user gave a character string as name
  if(!(is.character(symbol))){
    message("\nWARNING: symbol has been set to 'D' because the provided value is not a valid character!")
    symbol <- "D"
  }

  # login to the connections keeping the server names as specified in the login file
  message("\nLogging into the collaborating servers")
  connections <- vector("list", length(stdnames))
  names(connections) <- as.character(stdnames)
  pb <- .newProgress(total = 1 + length(connections))
  for(i in 1:length(connections)) {
    # connection options
    conn.opts <- append(opts, eval(parse(text=as.character(options[[i]]))))
    restoreId <- restore
    if (!is.null(restore)) {
      restoreId <- paste0(stdnames[i], ":", restore)
    }

    .tickProgress(pb, tokens = list(what = paste0("Login ", stdnames[i])))
    # instanciate the DSDriver
    drv <- new(drivers[i])
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      # pem files or username/password ?
      if (grepl("\\.pem$",userids[i])) {
        cert <- userids[i]
        private <- pwds[i]
        conn.opts <- append(conn.opts, list(sslcert=cert, sslkey=private))
        connections[[i]] <- dsConnect(drv, name=stdnames[i], url=urls[i], opts=conn.opts, restore=restoreId)
      } else {
        u <- userids[i];
        if(is.null(u) || is.na(u)) {
          u <- username;
        }
        p <- pwds[i];
        if(is.null(p) || is.na(p)) {
          p <- password;
        }
        connections[[i]] <- dsConnect(drv, name=stdnames[i], username=u, password=p, url=urls[i], opts=conn.opts, restore=restoreId)
      }
    } else {
      u <- userids[i];
      if(is.null(u) || is.na(u)) {
        u <- username;
      }
      p <- pwds[i];
      if(is.null(p) || is.na(p)) {
        p <- password;
      }
      connections[[i]] <- dsConnect(drv, name=stdnames[i], username=u, password=p, url=urls[i], opts=conn.opts, restore=restoreId)
    }
  }
  .tickProgress(pb, tokens = list(what = "Logged in all servers"))

  # sanity check: server availability and table path is valid
  excluded <- c()
  for(i in 1:length(connections)) {
    res <- try(dsHasTable(connections[[i]], paths[i]), silent=TRUE)
    excluded <- append(excluded, inherits(res, "try-error"))
    if ((is.logical(res) && !res) || inherits(res, "try-error")) {
      warning(stdnames[i], " will be excluded: ", res[1], call.=FALSE, immediate.=TRUE)
    }
  }
  rconnections <- c()
  for (i in 1:length(connections)) {
    if(!excluded[i]) {
      x <- list(connections[[i]])
      names(x) <- stdnames[[i]]
      rconnections <- append(rconnections, x)
    }
  }

  # if argument 'assign' is true assign data to the data repository server(s) you logged
  # in to. If no variables are specified the whole dataset is assigned
  # i.e. all the variables in the repository are assigned
  if(assign && length(rconnections) > 0) {
    if(is.null(variables)){
      # if the user does not specify variables (default behaviour)
      # display a message telling the user that the whole dataset
      # will be assigned since he did not specify variables
      message("\n  No variables have been specified. \n  All the variables in the table \n  (the whole dataset) will be assigned to R!")
    }

    # Assign data in parallel
    message("\nAssigning data...")
    results <- list()
    async <- unlist(lapply(connections, function(conn) { dsIsAsync(conn)$assignTable }))
    # async first

    pb <- .newProgress(total = 1 + length(connections) - length(excluded[excluded == TRUE]))
    for (i in 1:length(connections)) {
      if(!excluded[i] && async[i]) {
        results[[i]] <- dsAssignTable(connections[[i]], symbol, paths[i], variables, identifiers=idmappings[i])
      }
    }
    # not async (blocking calls)
    for (i in 1:length(connections)) {
      if(!excluded[i] && !async[i]) {
        .tickProgress(pb, tokens = list(what = paste0("Assigning ", stdnames[i], " (", paths[i], ")")))
        results[[i]] <- dsAssignTable(connections[[i]], symbol, paths[i], variables, identifiers=idmappings[i])
      }
    }
    for (i in 1:length(stdnames)) {
      res <- results[[i]]
      if (!is.null(res)) {
        if (async[i]) {
          .tickProgress(pb, tokens = list(what = paste0("Assigning ", stdnames[i], " (", paths[i], ")")))
        }
        resInfo <- dsGetInfo(res)
        if (resInfo$status == "FAILED") {
          warning("Data assignment of '", paths[i],"' failed for '", stdnames[i],"': ", res@error, call.=FALSE, immediate.=TRUE)
        }
      }
    }
    .tickProgress(pb, tokens = list(what = "Assigned all tables"))


    # Get column names in parallel
    # Ensure the colnames aggregation is available
    aggs <- datashield.method_status(rconnections, type="aggregate")
    hasColnames <- aggs[aggs$name == "colnames",]
    if (nrow(hasColnames)>0) {
      message("\nVariables assigned:")
      lapply(names(rconnections), function(n) {
        if (hasColnames[[n]]) {
          varnames <- dsFetch(dsAggregate(rconnections[[n]], paste0('colnames(', symbol,')'), async = FALSE))
          if(length(varnames[[1]]) > 0) {
            message(n," -- ",paste(unlist(varnames), collapse=", "))
          } else {
            message(n," -- No variables assigned. Please check login details for this study and verify that the variables are available!")
          }
        } else {
          message(n," -- ? (colnames() aggregation method not available)")
        }
      })
    }
  }

  # return the DSConnection objects
  rconnections
}
