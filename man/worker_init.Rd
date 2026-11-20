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
Sets the subprocess cwd and ensures the built-in skills are
registered. The child's session takes its own tools when
\code{\link{subagent_turn_init}} makes it: the user's skills, the config's
package tools, and the project's skills where \code{cwd} is a trusted
directory.
}
\details{
Exported (with `@keywords internal`) because it is called as
`corteza::worker_init()` from inside the `callr::r_session` child,
where `corteza:::` would trip the R CMD check "calls to the package's
namespace" NOTE.

}
\keyword{internal}
