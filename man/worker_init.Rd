% tinyrox says don't edit this manually, but it can't stop you!
\name{worker_init}
\alias{worker_init}
\title{Subagent session initialization.}
\usage{
worker_init(cwd = getwd())
}
\arguments{
\item{cwd}{Working directory for the subagent session.}
}
\value{
Invisible TRUE on success.
}
\description{
Runs inside a subagent's private `callr::r_session` subprocess
(invoked via `corteza::worker_init()` across the callr boundary).
Sets the subprocess cwd, ensures built-in skills are registered, and
loads user/project skills so the subagent can execute tools.
}
\details{
Exported (with `@keywords internal`) because it is called as
`corteza::worker_init()` from inside the `callr::r_session` child,
where `corteza:::` would trip the R CMD check "calls to the package's
namespace" NOTE.

}
\keyword{internal}
