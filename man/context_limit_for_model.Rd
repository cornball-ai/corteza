% tinyrox says don't edit this manually, but it can't stop you!
\name{context_limit_for_model}
\alias{context_limit_for_model}
\title{Look up the context window for a given model.}
\usage{
context_limit_for_model(model)
}
\arguments{
\item{model}{Model name (character).}
}
\value{
Context limit in tokens (integer). Returns 128000L when
  no entry matches.
}
\description{
Tries exact match, then prefix match either direction (so
`"claude-3-5-sonnet"` resolves to the dated entry, and a longer
model id with a known prefix also resolves).
}
\keyword{internal}
