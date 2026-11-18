% tinyrox says don't edit this manually, but it can't stop you!
\docType{data}
\name{MODEL_CONTEXT_LIMITS}
\alias{MODEL_CONTEXT_LIMITS}
\title{Model context limits in tokens.}
\format{
An object of class \code{list} of length 55.
}
\usage{
MODEL_CONTEXT_LIMITS
}
\description{
The largest input each known model takes: the provider's published
input limit where it has one, else its context window. Used by
[context_limit_for_model()]. Each group names the page its numbers
were read from and the day. A model that is not here gets the
fallback, 128,000, and compacts early, so add a row when a provider
ships a model. `test_compact_trigger.R` fails when a default model of
llm.api has no row.
}
\keyword{internal}
