% tinyrox says don't edit this manually, but it can't stop you!
\name{estimate_text_tokens}
\alias{estimate_text_tokens}
\title{Rough token estimate from raw text.}
\usage{
estimate_text_tokens(text)
}
\arguments{
\item{text}{Character (length 1 or vector; collapsed with newlines).}
}
\value{
Integer.
}
\description{
Returns `ceil(nchar(text) / 4)`. Good enough for budget decisions
but not a substitute for the provider's real usage count.
}
\keyword{internal}
