% tinyrox says don't edit this manually, but it can't stop you!
\name{format_live_ctx}
\alias{format_live_ctx}
\title{Format a live-context display like "4.2K/200K" or "?".}
\usage{
format_live_ctx(tokens, limit)
}
\description{
Used by /agents to summarize live tokens versus model limit.
Returns "?" when either value is NA.
}
\keyword{internal}
