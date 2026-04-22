% tinyrox says don't edit this manually, but it can't stop you!
\name{register_skill_from_fn}
\alias{register_skill_from_fn}
\title{Register a skill whose schema is derived from its function.}
\usage{
register_skill_from_fn(tool_name, fn)
}
\arguments{
\item{tool_name}{Name the LLM sees.}

\item{fn}{The R function to introspect and execute.}
}
\value{
Invisible tool name.
}
\description{
Register a skill whose schema is derived from its function.
}
\keyword{internal}
