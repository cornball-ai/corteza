% tinyrox says don't edit this manually, but it can't stop you!
\name{context_limit_for_model}
\alias{context_limit_for_model}
\title{Look up the context window for a given model.}
\usage{
context_limit_for_model(model, provider = NULL)
}
\arguments{
\item{model}{Model name (character).}

\item{provider}{Optional provider/product endpoint. Provider-specific
limits take precedence over the public model limit.}
}
\value{
Context limit in tokens (integer). Returns 128000L when
  no entry matches.
}
\description{
Tries the model's own row, then the longest row name the id starts
with (so a dated or suffixed id resolves to its family), then the
rows whose names start with the id (so `"claude-3-haiku"` resolves to
the dated entry). An id cut short can mean several rows, and it gets
the smallest limit among them.
}
\keyword{internal}
