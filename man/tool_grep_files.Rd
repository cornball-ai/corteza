% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_grep_files}
\alias{tool_grep_files}
\title{Search file contents with regex pattern.}
\usage{
tool_grep_files(pattern, path = ".", file_pattern = "*.R")
}
\arguments{
\item{pattern}{(character) Regex pattern to search.}

\item{path}{(character) Directory to search.}

\item{file_pattern}{(character) File glob pattern.}
}
\value{
An MCP tool-result list.
}
\description{
Search file contents with regex pattern.
}
\keyword{internal}
