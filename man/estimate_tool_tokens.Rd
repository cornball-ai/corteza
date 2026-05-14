% tinyrox says don't edit this manually, but it can't stop you!
\name{estimate_tool_tokens}
\alias{estimate_tool_tokens}
\title{Token estimate for the tool schema payload.}
\usage{
estimate_tool_tokens(tools)
}
\arguments{
\item{tools}{List of tool definitions (or NULL).}
}
\value{
Integer.
}
\description{
Serializes the tool list as JSON and counts tokens, plus a 12-
token overhead per tool for the schema framing.
}
\keyword{internal}
