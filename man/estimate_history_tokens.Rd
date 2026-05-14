% tinyrox says don't edit this manually, but it can't stop you!
\name{estimate_history_tokens}
\alias{estimate_history_tokens}
\title{Token estimate for a list of messages (history).}
\usage{
estimate_history_tokens(messages)
}
\arguments{
\item{messages}{List of message lists, each with `$role` and
`$content`.}
}
\value{
Integer.
}
\description{
Sums text tokens for each message and adds a small framing
overhead (6 tokens / message) that the chars/4 estimate misses.
}
\keyword{internal}
