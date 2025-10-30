#' Logs in a DataSHIELD R sessions and optionaly assigns variables to R
#'
#' This function allows for clients to login to data repository servers and (optionaly)
#' assign all the data or specific variables from the data repositories tables to R data
#' frames. The assigned dataframes (one for each data repository) are named 'D' (by default).
#' Different login strategies are supported: using a certificate/private key pair (2-way SSL encryption mechanism),
#' using user credentials (user name and password) or using a personal access token (could be
#' combined with a user name, depending on the data repository system).
#'
#' @param logins A dataframe table that holds login details. This table holds five elements required
#'   to login to the servers where the data to analyse is stored. The expected column names are
#'   'driver' (the \code{\link{DSDriver-class}} name, default is "OpalDriver"),
#'   'server' (the server name), url' (the server url), 'user' (the user name or the certificate PEM file path),
#'   'password' (the user password or the private key PEM file path),
#'   'token' (the personal access token, ignored if 'user' is defined), 'table' (the fully qualified name of
#'   the table in the data repository), 'resource' (the fully qualified name of
#'   the resource reference in the data repository), 'profile' (an optional DataSHIELD profile name), 'options' (the SSL options). 
#'   An additional column 'identifiers' can be specified for identifiers mapping (if supported by data repository). See also the documentation
#'   of the examplar input table \code{logindata} for details of the login elements.
#' @param assign A boolean which tells whether or not data should be assigned from the data repository
#'   table to R after login into the server(s).
#' @param variables Specific variables to assign. If \code{assign} is set to FALSE this argument is ignored
#'   otherwise the specified variables are assigned to R. If no variables are specified (default) the whole
#'   data repository's table is assigned.
#' @param missings If TRUE, missing values will be pushed from data repository to R, default is FALSE.
#' @param symbol A character, the name of the data frame to which the data repository's table will be
#'   assigned after login into the server(s).
#' @param id.name Name of the column that will contain the entity identifiers. If not specified, the identifiers
#'   will be the data frame row names. When specified this column can be used to perform joins between data frames.
#' @param opts Default SSL options to be used in case it is not specified in the logins structure.
#' @param restore The workspace name to restore (optional).
#' @param failSafe Ignores, with a warning, the servers for which the connection cannot be established. Optional, default is FALSE. 
#' @return object(s) of class DSConnection
#' @export
#' @import progress
#' @importFrom methods new
#' @examples
#'\dontrun{
#'
#'#### The below examples illustrate an analysises that use test/simulated data ####
#'
#' # build your data.frame
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url="https://opal-demo.obiba.org",
#'                table="datashield.CNSIM1", resource="datashield.CNSIM1r",
#'                user="dsuser", password="password",
#'                options="list(ssl_verifyhost=0,ssl_verifypeer=0)")
#' builder$append(server="server2", url="dslite.server",
#'                table="CNSIM2", resource="CNSIM2r", driver="DSLiteDriver")
#' builder$append(server="server3", url="https://molgenis.example.org",
#'                table="CNSIM3", resource="CNSIM3r", token="123456789", driver="MolgenisDriver")
#' builder$append(server="server4", url="dslite.server",
#'                table="CNSIM4", resource="CNSIM4r", driver="DSLiteDriver")
#' logindata <- builder$build()
#'
#' # or load the data.frame that contains the login details
#' data(logindata)
#'
#' # Example 1: just login (default)
#' connections <- datashield.login(logins=logindata)
#'
#' # Example 2: login and assign the whole dataset
#' connections <- datashield.login(logins=logindata, assign=TRUE)
#'
#' # Example 3: login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' connections <- datashield.login(logins=logindata, assign=TRUE, variables=myvar)
#'
#' # Example 4: ignore with a warning message servers for which connection cannot be established
#' connections <- datashield.login(logins=logindata, failSafe=TRUE)
#' 
#' # note that the asignment information can also be provided afterwards
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url="https://opal-demo.obiba.org",
#'                user="dsuser", password="password")
#' builder$append(server="server2", url="https://opal-test.obiba.org",
#'                token="123456789")
#' logindata <- builder$build()
#' connections <- datashield.login(logins=logindata)
#' datashield.assign.table(connections, symbol = "D",
#'                         table = list(server1 = "CNSIM.CNSIM1",
#'                                      server2 = "CNSIM.CNSIM2"))
#' datashield.assign.resource(connections, symbol = "rsrc",
#'                            resource = list(server1 = "res.CNSIM1",
#'                                         server2 = "res.CNSIM2"))
#'}
#'
datashield.login <- function(logins=NULL, assign=FALSE, variables=NULL, missings=FALSE, symbol="D", id.name=NULL,
                             opts=getOption("datashield.opts", list()), restore=NULL, failSafe=FALSE){
  .clearLastErrors()
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
  # tokens
  tokens <- as.character(logins$token)
  # profiles
  profiles <- as.character(logins$profile)
  # table fully qualified names
  tables <- as.character(logins$table)
  if (is.null(tables) || length(tables) == 0) {
    tables <- rep("", length(stdnames))
  }
  # resource fully qualified names
  resources <- as.character(logins$resource)
  if (is.null(resources) || length(resources) == 0) {
    resources <- rep("", length(stdnames))
  }
  # identifiers mapping
  idmappings <- logins$identifiers
  if (is.null(idmappings) || length(idmappings) == 0) {
    idmappings <- rep("", length(stdnames))
  }

  # DSConnection specific options
  options <- logins$options
  if (is.null(options) || length(options) == 0) {
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
  
  doConnection <- function(i) {
    # connection options
    conn.opts <- append(opts, eval(parse(text=as.character(options[[i]]))))
    # instanciate the DSDriver
    drv <- new(drivers[i])
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    conn.obj <- NULL
    if(protocol=="https") {
      # pem files or username/password or token ?
      if (grepl("\\.pem$", userids[i])) {
        cert <- userids[i]
        private <- pwds[i]
        conn.opts <- append(conn.opts, list(sslcert=cert, sslkey=private))
        conn.obj <- dsConnect(drv, name=stdnames[i], url=urls[i], opts=conn.opts, profile=profiles[i])
      } else {
        conn.obj <- dsConnect(drv, name=stdnames[i], username=userids[i], password=pwds[i], token=tokens[i], url=urls[i], profile=profiles[i], opts=conn.opts)
      }
    } else {
      conn.obj <- dsConnect(drv, name=stdnames[i], username=userids[i], password=pwds[i], token=tokens[i], url=urls[i], profile=profiles[i], opts=conn.opts)
    }
    conn.obj
  }
  
  for(i in 1:length(connections)) {
    .tickProgress(pb, tokens = list(what = paste0("Login ", stdnames[i])))
    if (failSafe) {
      res <- try(doConnection(i), silent = TRUE)
      if (inherits(res, "try-error")) {
        warning(stdnames[i], " is excluded because login connection failed", call.=FALSE, immediate.=FALSE)
        connections[[i]] <- NULL
      } else {
        connections[[i]] <- res
      }
    } else {
      connections[[i]] <- doConnection(i)
    }
  }
  .tickProgress(pb, tokens = list(what = "Logged in all servers"))

  # sanity check: server availability and table path is valid
  excluded <- c()
  for(i in 1:length(connections)) {
    if (is.null(connections[[i]])) {
      excluded <- append(excluded, TRUE)
    } else if (!is.null(tables[i]) && nchar(tables[i])>0) {
      res <- try(dsHasTable(connections[[i]], tables[i]), silent=TRUE)
      excluded <- append(excluded, inherits(res, "try-error"))
      if ((is.logical(res) && !res) || inherits(res, "try-error")) {
        warning(stdnames[i], " will be excluded because table ", tables[i]," is not accessible", call.=FALSE, immediate.=TRUE)
      }
    } else if (!is.null(resources[i]) && nchar(resources[i])>0) {
      res <- try(dsHasResource(connections[[i]], resources[i]), silent=TRUE)
      excluded <- append(excluded, inherits(res, "try-error"))
      if ((is.logical(res) && !res) || inherits(res, "try-error")) {
        warning(stdnames[i], " will be excluded because resource ", resources[i], " is not accessible", call.=FALSE, immediate.=TRUE)
      }
    } else {
      excluded <- append(excluded, FALSE)
    }
  }
  
  rconnections <- c()
  for (i in 1:length(connections)) {
    if (is.null(excluded) || !excluded[i]) {
      x <- list(connections[[i]])
      names(x) <- stdnames[[i]]
      rconnections <- append(rconnections, x)
    }
  }
  
  # if restore is not null, restore the workspaces
  if (!is.null(restore) && length(rconnections) > 0) {
    datashield.workspace_restore(rconnections, restore)
  }

  # if argument 'assign' is true assign data/resources to the data repository server(s) you logged in to.
  if(assign && length(rconnections) > 0) {
    #warning("Assignment of table/resources at login time is deprecated. Use datashield.assign functions instead.", call.=FALSE, immediate.=TRUE)
    isNotEmpty <- Vectorize(function(x) { !(is.null(x) || is.na(x) || length(x) == 0 || nchar(x) == 0) })
    if (all(isNotEmpty(resources))) {
      tryCatch({
        datashield.assign.resource(rconnections, symbol = symbol, resource = logins)
      }, error = function(e) {
        warning("Resource assignment failed", call.=FALSE, immediate.=TRUE)
      })
    }
    if (all(isNotEmpty(tables))) {
      tryCatch({
        datashield.assign.table(rconnections, symbol = symbol, table = logins, variables = variables, missings = missings, id.name = id.name)  
      }, error = function(e) {
        warning("Resource assignment failed", call.=FALSE, immediate.=TRUE)
      })
    }
  }

  .clearCache()

  # return the DSConnection objects
  rconnections
}
