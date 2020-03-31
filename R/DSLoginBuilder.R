#' Create a new DataSHIELD login details builder
#'
#' Shortcut function to create a new \link{DSLoginBuilder} instance. The data frame that is being
#' built can be used to perform \code{\link{datashield.login}}.
#'
#' @param logins A valid login details data frame to initiate the builder, optional.
#' @param .silent Do not warn user when non secure HTTP urls are encountered. Default is FALSE.
#' @export
#' @examples
#' {
#'   builder <- newDSLoginBuilder()
#'   builder$append(server="server1", url="https://opal-demo.obiba.org", table="datashield.CNSIM1",
#'     user="administrator", password="password")
#'   builder$append(server="server2", url="dslite.server", table="CNSIM2")
#'   builder$append(server="server3", url="http://molgenis.example.org", table="CNSIM3",
#'    token="123456789")
#'   builder$append(server="server4", url="dslite.server", table="CNSIM4")
#'   logindata <- builder$build()
#' }
newDSLoginBuilder <- function(logins = NULL, .silent = FALSE) {
  DSLoginBuilder$new(logins = logins, .silent = .silent)
}

#' DataSHIELD login details builder
#'
#' Helper class for creating a valid data frame that can be used to perform \code{\link{datashield.login}}.
#' See also \link{newDSLoginBuilder}.
#'
#' @docType class
#' @format A R6 object of class DSLoginBuilder
#' @import R6
#' @export
DSLoginBuilder <- R6::R6Class(
  "DSLoginBuilder",
  public = list(

    #' @description Create a new DSLoginBuilder instance.
    #' @param logins A valid login details data frame to initiate the builder, optional.
    #' @param .silent Do not warn user when non secure HTTP urls are encountered. Default is FALSE.
    #' @return A DSLoginBuilder object.
    initialize = function(logins = NULL, .silent = FALSE) {
      private$.logins <- private$.as.logins(logins)
      private$.silent <- .silent
    },

    #' @description Append login information for a specific server.
    #' @param server The server name (must be unique).
    #' @param url The url to connect to the server or a R symbol name.
    #' @param table The table path that identifies the dataset in the server.
    #' @param resource The resource path that identifies the resource reference in the server.
    #' @param driver The \code{\link{DSDriver-class}} name to build the \code{\link{DSConnection-class}}.
    #' @param user The user name in the user credentials.
    #' @param password The user password in the user credentials.
    #' @param token The personal access token (ignored when user credentials are not empty).
    #' @param options Any options (R code to be parsed) that could be relevant for the DS connection object.
    append = function(server, url, table="", resource="", driver = "OpalDriver", user = "", password = "", token = "", options = "") {
      if (private$.is.empty(server)) {
        stop("The server parameter cannot be empty", call. = FALSE)
      }
      if (private$.is.empty(url)) {
        stop("The url parameter cannot be empty", call. = FALSE)
      } else if (startsWith(url,"http") && !startsWith(url,"https") && !private$.silent) {
        warning("Secure HTTP connection is recommended: ", url, call. = FALSE)
      }
      if (private$.is.empty(table) && private$.is.empty(resource)) {
        stop("The table and resource parameters cannot be both empty", call. = FALSE)
      }

      lg  <- private$.get.logins()
      if (ncol(lg) == 0) {
        private$.logins <- data.frame(server=as.character(server),
                                      url=as.character(url),
                                      table=as.character(table),
                                      resource=as.character(resource),
                                      driver=as.character(driver),
                                      user=as.character(user),
                                      password=as.character(password),
                                      token=as.character(token),
                                      options=as.character(options),
                                      stringsAsFactors = FALSE)
      } else {
        if (server %in% lg$server) {
          stop("Duplicate server name: ", server, call. = FALSE)
        }
        private$.logins <- rbind(lg, list(server=as.character(server),
                                          url=as.character(url),
                                          table=as.character(table),
                                          resource=as.character(resource),
                                          driver=as.character(driver),
                                          user=as.character(user),
                                          password=as.character(password),
                                          token=as.character(token),
                                          options=as.character(options)))
      }
    },

    #' @description Build the DSLoginBuilder instance.
    #' @return The DataSHIELD logindata data.frame
    build = function() {
      private$.get.logins()
    }
  ),
  private = list(
    # the login data frame
    .logins = NULL,
    .silent = FALSE,
    .get.logins = function() {
      if (is.null(private$.logins)) {
        private$.logins <- data.frame()
      }
      private$.logins
    },
    .is.empty = function(value) {
      return(is.null(value) || is.na(value) || length(value) == 0 || nchar(as.character(value)) == 0)
    },
    .as.logins = function(df) {
      cnames <- colnames(df)
      if (length(cnames) > 0) {
        if (!("server" %in% cnames)) {
          stop("The provided login details is missing server column", call. = FALSE)
        }
        server <- df$server
        if (!("url" %in% cnames)) {
          stop("The provided login details is missing url column", call. = FALSE)
        }
        url <- df$url
        if (!("table" %in% cnames) && !("resource" %in% cnames)) {
          stop("The provided login details is missing both table and resource columns", call. = FALSE)
        }

        if (!("table" %in% cnames)) {
          table <- rep("", length(server))
        } else {
          table <- df$table
        }
        if (!("resource" %in% cnames)) {
          resource <- rep("", length(server))
        } else {
          resource <- df$resource
        }
        if (!("driver" %in% cnames)) {
          driver <- rep("", length(server))
        } else {
          driver <- df$driver
        }
        if (!("user" %in% cnames)) {
          user <- rep("", length(server))
        } else {
          user <- df$user
        }
        if (!("password" %in% cnames)) {
          password <- rep("", length(server))
        } else {
          password <- df$password
        }
        if (!("token" %in% cnames)) {
          token <- rep("", length(server))
        } else {
          token <- df$token
        }
        if (!("options" %in% cnames)) {
          options <- rep("", length(server))
        } else {
          options <- df$options
        }

        data.frame(server=server, url=url, table=table, resource=resource, driver=driver,
                   user=user, password=password, token=token,
                   options=options, stringsAsFactors = FALSE)
      } else {
        data.frame()
      }
    }
  )
)
