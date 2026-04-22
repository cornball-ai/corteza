% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_list_files}
\alias{tool_list_files}
\title{List files in a directory.}
\usage{
tool_list_files(path = ".", pattern = NULL, recursive = FALSE,
                all_files = FALSE, limit = 200L)
}
\arguments{
\item{path}{(character) Directory to inspect.}

\item{pattern}{(character) Regex pattern to filter file names.}

\item{recursive}{(logical) Recurse into subdirectories.}

\item{all_files}{(logical) Include hidden files.}

\item{limit}{(integer) Maximum number of entries to return.}
}
\value{
An MCP tool-result list.
}
\description{
List files in a directory.
}
\keyword{internal}
