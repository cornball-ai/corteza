% tinyrox says don't edit this manually, but it can't stop you!
\name{format_live_ctx}
\alias{format_live_ctx}
\title{Format a live-context display like "4.2K/200K" or "?".}
\usage{
format_live_ctx(tokens, limit)
}
\arguments{
\item{tokens}{Live context, in tokens.}

\item{limit}{The model's context window, in tokens.}
}
\value{
A character string: "tokens/limit", or "?" when either value
  is NA.
}
\description{
Used by /agents to summarize live tokens versus model limit.
}
\keyword{internal}
