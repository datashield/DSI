#' List saved DataSHIELD R workspaces
#'
#' Get the list of R workspaces that were saved during a Datashield R session.
#'
#' @param conns \code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.
#'
#' @export
datashield.workspaces <- function(conns) {
  res <- lapply(conns, FUN=dsListWorkspaces)
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
  }
}
