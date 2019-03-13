#' List saved DataSHIELD R workspaces
#'
#' Get the list of R workspaces that were saved during a Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#'
#' @export
datashield.workspaces <- function(conns) {
  if (is.list(conns)) {
    res <- lapply(conns, function(c) dsListWorkspaces(c))
    server <- c()
    name <- c()
    user <- c()
    context <- c()
    lastAccessDate <- c()
    size <- c()
    for (n in names(res)) {
      wss <- res[[n]]
      if (!is.character(wss)) {
        server <- c(server, as.vector(wss$server))
        name <- c(name, as.vector(wss$name))
        user <- c(user, as.vector(wss$user))
        context <- c(context, as.vector(wss$context))
        lastAccessDate <- c(lastAccessDate, as.vector(wss$lastAccessDate))
        size <- c(size, as.vector(wss$size))
      }
    }
    if (length(server)) {
      data.frame(server=server, name=name, user=user, context=context, lastAccessDate=lastAccessDate, size=size)
    } else {
      data.frame()
    }
  } else {
    dsListWorkspaces(conns)
  }
}

#' Save DataSHIELD R session to a workspace
#'
#' Save the current state of the DataSHIELD R session in a workspace with the provided name in each data repository.
#' The workspace can be restored on the next \code{\link{datashield.login}}.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param ws The workspace name
#' @export
datashield.workspace_save <- function(conns, ws) {
  if (is.list(conns)) {
    ignore <- lapply(conns, function(c) datashield.workspace_save(c, ws))
  } else {
    name <- paste0(conns@name, ":", ws)
    ignore <- dsSaveWorkspace(conns, name)
  }
}

#' Remove a DataSHIELD workspace
#'
#' Remove in each data repository the workspace with the provided name.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#' @param ws The workspace name
#' @export
datashield.workspace_rm <- function(conns, ws) {
  if (is.list(conns)) {
    ignore <- lapply(conns, function(c) datashield.workspace_rm(c, ws))
  } else {
    name <- paste0(conns@name, ":", ws)
    ignore <- dsRmWorkspace(conns, name)
  }
}
