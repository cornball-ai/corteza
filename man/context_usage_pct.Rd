% tinyrox says don't edit this manually, but it can't stop you!
\name{context_usage_pct}
\alias{context_usage_pct}
\title{Percent of a model's context window used by a session.}
\usage{
context_usage_pct(session, model, system_prompt = NULL, tools = NULL)
}
\arguments{
\item{session}{Session-like object with `$messages`.}

\item{model}{Model name used to look up the context limit.}

\item{system_prompt}{Optional system prompt.}

\item{tools}{Optional tools list.}
}
\value{
Numeric percentage in `[0, +Inf)`.
}
\description{
Convenience wrapper around [estimate_live_context_tokens()] and
[context_limit_for_model()]. Returns 0 when the limit is 0 or
negative (defensive — shouldn't happen with a real model).
}
\keyword{internal}
