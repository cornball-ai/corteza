% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_installed_packages}
\alias{tool_installed_packages}
\title{List installed R packages, optionally filtered by name.}
\usage{
tool_installed_packages(pattern = NULL, limit = 100L)
}
\arguments{
\item{pattern}{(character) Case-insensitive package-name filter.}

\item{limit}{(integer) Maximum number of packages to return.}
}
\value{
An MCP tool-result list.
}
\description{
List installed R packages, optionally filtered by name.
}
\keyword{internal}
