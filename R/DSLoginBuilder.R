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
#' @field logins A valid login details data frame to initiate the builder, optional.
#' @field .silent Do not warn user when non secure HTTP urls are encountered. Default is FALSE.
#'
#' @section Methods:
#' \code{$new(logins, .silent)} Create new DSLoginBuilder instance with the arguments described in the Fields section.
#'
#' \code{$append(server, url, table, driver, user, password, token, options)} Append a row in the login data frame with the specifications:
#' \code{server} is the server name, \code{url} is the url to connect to the server or a R symbol name, \code{table} is the table path
#' in that identifies the dataset in the server, \code{driver} is the \code{\link{DSDriver-class}} name to build the \code{\link{DSConnection-class}}, \code{user}
#' and \code{password} are the user credentials, \code{token} is the personal access token (ignored when user credentials are not empty),
#' \code{options} any options (R code to be parsed) that could be relevant for the DS connection object.
#'
#' \code{$build()} Get the created login details data frame.
#'
#' @docType class
#' @format A R6 object of class DSLoginBuilder
#' @import R6
#' @export
DSLoginBuilder <- R6::R6Class(
  "DSLoginBuilder",
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
        if (!("table" %in% cnames)) {
          stop("The provided login details is missing table column", call. = FALSE)
        }
        table <- df$table

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
        table <- df$table
        data.frame(server=server, url=url, table=table, driver=driver,
                   user=user, password=password, token=token,
                   options=options, stringsAsFactors = FALSE)
      } else {
        data.frame()
      }
    }
  ),
  public = list(
    initialize = function(logins = NULL, .silent = FALSE) {
      private$.logins <- private$.as.logins(logins)
      private$.silent <- .silent
    },
    append = function(server, url, table, driver = "OpalDriver", user = "", password = "", token = "", options = "") {
      if (private$.is.empty(server)) {
        stop("The server parameter cannot be empty", call. = FALSE)
      }
      if (private$.is.empty(url)) {
        stop("The url parameter cannot be empty", call. = FALSE)
      } else if (startsWith(url,"http") && !startsWith(url,"https") && !private$.silent) {
        warning("Secure HTTP connection is recommended: ", url, call. = FALSE)
      }
      if (private$.is.empty(table)) {
        stop("The table parameter cannot be empty", call. = FALSE)
      }

      lg  <- private$.get.logins()
      if (ncol(lg) == 0) {
        private$.logins <- data.frame(server=as.character(server),
                                      url=as.character(url),
                                      table=as.character(table),
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
                                          driver=as.character(driver),
                                          user=as.character(user),
                                          password=as.character(password),
                                          token=as.character(token),
                                          options=as.character(options)))
      }
    },
    build = function() {
      private$.get.logins()
    }
  )
)
