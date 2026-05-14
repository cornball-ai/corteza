% tinyrox says don't edit this manually, but it can't stop you!
\name{default_provider_model}
\alias{default_provider_model}
\title{Provider-specific default model name.}
\usage{
default_provider_model(provider)
}
\arguments{
\item{provider}{Provider name.}
}
\value{
Model name (character) or NULL.
}
\description{
Resolves the actual model a subagent (or chat session) will run
with when no explicit \code{model} is set. Mirrors the defaults
the CLI script picks at startup so /agents, compaction, and the
CLI all show the same model identity. Returns NULL for unknown
providers (lets llm.api fall back to its own default).
}
\keyword{internal}
