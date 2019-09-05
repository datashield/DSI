#' Builds a dataframe to login to datashield
#'
#' This function generates a valid data frame, that can be used to login to some data servers implementing
#' the \code{\link{DSConnection-class}}. Each row of the returned data frame holds the information
#' in relation to one data computer: location of the server and of the dataset, connection class, credentials.
#' See also \link{newDSLoginBuilder}.
#'
#' @param servers.name A vector of characters listing all the names of the data servers.
#' @param servers.url A vector of characters listing each data server address:  can be a HTTP address (format is
#'   http[s]://[TCPIP address or host name][:port]) or a R symbol name.
#' @param servers.table A vector of characters listing the name of the table stored in a data server.
#' @param servers.driver A vector of characters listing the name of the DS connection driver to be used, see \code{\link{DSDriver-class}}.
#'   When NULL this vector fallbacks to a vector of "OpalDriver" strings.
#' @param users.name A vector of characters listing a valid user name to log on each server.
#' @param users.password A vector of characters listing the password for each user to log in to a data server.
#' @param users.token A vector of characters listing the API access tokens for each user to log in to a data server (ignored if user name and password are specified).
#' @param .silent Do not warn user when non secure HTTP urls are encountered. Default is FALSE.
#' @return A data frame formatter in this manner: (server, url, driver, user, password, token, table). If the arguments are not correct. Then a data.frame with no rows is created.
#' @export
datashield.build.login.data.frame.o <- function (servers.name, servers.url, servers.table, servers.driver = NULL, users.name = NULL, users.password = NULL, users.token = NULL, .silent = FALSE) {
  #assign the arguments to the data frame format.
  server <- as.character(servers.name)
  url <- as.character(servers.url)
  table <- as.character(servers.table)
  driver <- as.character(servers.driver)
  if (is.null(servers.driver)) {
    driver <- rep("OpalDriver", length(server))
  }
  user <- as.character(users.name)
  if (is.null(users.name)) {
    user <- rep("", length(server))
  }
  password <- as.character(users.password)
  if (is.null(users.password)) {
    password <- rep("", length(server))
  }
  token <- as.character(users.token)
  if (is.null(users.token)) {
    token <- rep("", length(server))
  }

  #Verify the length of each vector is the same
  NO.COLUMNS = 7
  expected.elements = length(server) * NO.COLUMNS
  total.elements = length(server) + length(url) + length(driver) + length(table) + length(user) + length(password) + length(token)

  if (expected.elements != total.elements) {
    stop("The length of the vectors passed as arguments are not the same length.")
  }

  builder <- newDSLoginBuilder(.silent = .silent)
  for (i in 1:length(server)) {
    builder$append(server = server[i],
                   url = url[i],
                   table = table[i],
                   driver = driver[i],
                   user = user[i],
                   password = password[i],
                   token = token[i])
  }
  builder$build()
}
