% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.assign.R
\name{datashield.assign.expr}
\alias{datashield.assign.expr}
\title{Expression result assignment}
\usage{
datashield.assign.expr(conns, symbol, expr, async = TRUE)
}
\arguments{
\item{conns}{\code{\link{DSConnection-class}} object or a list of \code{\link{DSConnection-class}}s.}

\item{symbol}{Name of the R symbol.}

\item{expr}{R expression with allowed assign functions calls.}

\item{async}{Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
the connections, when the connection supports that feature, with an extra overhead of requests.}
}
\description{
Assign the result of the execution of an expression to a R symbol in the Datashield R session.
}
\examples{
\dontrun{
# assign an expression to G
datashield.assign.expr(conns, symbol = "G", expr = quote(as.numeric(D$GENDER)))

# assign the expressions that are defined in the provided named list. 
# Connections are filtered by the list names.
datashield.assign.expr(conns, "G",
  list(server1=quote(as.numeric(D$GENDER)), server2=quote(as.numeric(D$SEX))))
}
}
