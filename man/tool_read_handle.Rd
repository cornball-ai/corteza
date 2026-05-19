% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_read_handle}
\alias{tool_read_handle}
\title{Read / inspect a stashed handle.}
\usage{
tool_read_handle(handle, op = "str")
}
\arguments{
\item{handle}{(character) Handle id, e.g. `.h_001`.}

\item{op}{(character; one of: str, head, summary, print) Inspection
operation.}
}
\value{
An MCP tool-result list.
}
\description{
The LLM's only window onto large stashed objects. Supports a few
common ops: `str` (structure), `head` (first six rows / elements),
`summary` (R's summary()), `print` (full print of the object).
}
\keyword{internal}
