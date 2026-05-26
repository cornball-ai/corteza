% tinyrox says don't edit this manually, but it can't stop you!
\name{ensure_skills}
\alias{ensure_skills}
\title{Ensure skills are registered.}
\usage{
ensure_skills()
}
\value{
Invisible character vector of skill names.
}
\description{
Registers built-in skills if not already registered. Exported with
`@keywords internal` so a subagent's `callr::r_session` child can
register skills in its own namespace (via `worker_init()`) before
dispatching tools.
}
\keyword{internal}
