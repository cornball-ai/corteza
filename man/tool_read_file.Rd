% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_read_file}
\alias{tool_read_file}
\title{Read file contents, optionally with line numbers.}
\usage{
tool_read_file(path, from = 1L, lines = NULL, line_numbers = TRUE)
}
\arguments{
\item{path}{(character) Path to the file.}

\item{from}{(integer) Starting line number (1-based).}

\item{lines}{(integer) Number of lines to read.}

\item{line_numbers}{(logical) Prefix each line with its line number.}
}
\value{
An MCP tool-result list.
}
\description{
Read file contents, optionally with line numbers.
}
\keyword{internal}
