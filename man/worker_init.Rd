% tinyrox says don't edit this manually, but it can't stop you!
\name{worker_init}
\alias{worker_init}
\title{Worker-side initialization.}
\usage{
worker_init(cwd = getwd())
}
\arguments{
\item{cwd}{Working directory for the worker.}
}
\value{
Invisible TRUE on success.
}
\description{
Called once after the callr session starts. Sets up cwd, loads the
package, registers skills. Separate from worker_dispatch so session
init is explicit and inspectable.
Exported (with `@keywords internal`) for the same reason as
`worker_dispatch()`.
}
\keyword{internal}
