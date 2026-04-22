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
`@keywords internal` so the CLI (which runs in its own R process,
separate from the callr worker) can register skills in its own
namespace before calling `schema_from_registry()`.
}
\keyword{internal}
