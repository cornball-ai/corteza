% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_write_file}
\alias{tool_write_file}
\title{Write text to a file.}
\usage{
tool_write_file(path, content, append = FALSE, create_dirs = TRUE)
}
\arguments{
\item{path}{(character) Path to the file.}

\item{content}{(character) Text to write.}

\item{append}{(logical) Append instead of overwrite.}

\item{create_dirs}{(logical) Create parent directories if needed.}
}
\value{
An MCP tool-result list.
}
\description{
Creates parent directories by default.
}
\keyword{internal}
