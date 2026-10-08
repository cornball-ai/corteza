% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_read_handle}
\alias{tool_read_handle}
\title{Read / inspect a stashed handle.}
\usage{
tool_read_handle(handle, op = "str", pattern = NULL, start = NULL, end = NULL)
}
\arguments{
\item{handle}{(character) Handle id, e.g. `.h_001`.}

\item{op}{(character; one of: str, head, summary, print, grep, lines)
Inspection operation.}

\item{pattern}{(character) For `op = "grep"`: a regular expression,
matched without regard to case. Text that is not a valid
expression is matched as written.}

\item{start}{(integer) For `op = "lines"`: the first line wanted.
Defaults to 1.}

\item{end}{(integer) For `op = "lines"`: the last line wanted. At most
40 lines come back.}
}
\value{
An MCP tool-result list.
}
\description{
The LLM's only window onto large stashed objects. Supports a few
common ops: `str` (structure), `head` (first six rows / elements),
`summary` (R's summary()), `print` (full print of the object),
`grep` (the lines matching `pattern`, numbered), and `lines` (a range
of lines, `start` to `end`). The last two are how a tool result that
was cut for length is searched and paged; each returns few enough
lines not to be cut again.
}
\keyword{internal}
