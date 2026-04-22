% tinyrox says don't edit this manually, but it can't stop you!
\name{register_skill_from_fn}
\alias{register_skill_from_fn}
\title{Register a skill whose schema is derived from its function.}
\usage{
register_skill_from_fn(tool_name, fn, available = NULL)
}
\arguments{
\item{tool_name}{Name the LLM sees.}

\item{fn}{The R function to introspect and execute.}

\item{available}{Optional zero-argument predicate. When it returns
`FALSE`, [schema_from_registry()] omits the tool from the LLM
payload. Used for context-aware pruning (e.g. git tools gated on
a real git repo, web tools on an API key being set). The tool
stays registered and callable regardless.}
}
\value{
Invisible tool name.
}
\description{
Register a skill whose schema is derived from its function.
}
\keyword{internal}
