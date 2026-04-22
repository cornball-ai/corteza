% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_replace_in_file}
\alias{tool_replace_in_file}
\title{Replace exact text in a file without rewriting the whole file manually.}
\usage{
tool_replace_in_file(path, old_text, new_text, all = FALSE,
                     expected_count = NULL)
}
\arguments{
\item{path}{(character) Path to the file.}

\item{old_text}{(character) Exact text to replace.}

\item{new_text}{(character) Replacement text.}

\item{all}{(logical) Replace all matches instead of exactly one.}

\item{expected_count}{(integer) Fail unless this many matches are found.}
}
\value{
An MCP tool-result list.
}
\description{
Replace exact text in a file without rewriting the whole file manually.
}
\keyword{internal}
