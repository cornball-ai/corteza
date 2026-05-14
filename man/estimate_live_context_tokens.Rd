% tinyrox says don't edit this manually, but it can't stop you!
\name{estimate_live_context_tokens}
\alias{estimate_live_context_tokens}
\title{Token estimate for an entire live model-context.}
\usage{
estimate_live_context_tokens(session, system_prompt = NULL, tools = NULL)
}
\arguments{
\item{session}{Session-like object with `$messages` list.}

\item{system_prompt}{Character or NULL.}

\item{tools}{List of tool definitions or NULL.}
}
\value{
Integer.
}
\description{
Sum of system prompt + tool schema + message history, plus
framing overheads. Used to drive auto-compaction triggers.
}
\keyword{internal}
