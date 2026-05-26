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
with when no explicit \code{model} is set, so /agents, compaction,
and the CLI all show the same model identity. Delegates to
\code{llm.api::provider_default_model()} -- the canonical table --
rather than keeping a parallel one that drifts. Returns NULL for an
unknown or empty provider (llm.api errors there; we map it to NULL).
}
\keyword{internal}
