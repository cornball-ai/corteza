% tinyrox says don't edit this manually, but it can't stop you!
\name{add_observer}
\alias{add_observer}
\title{Add a tool-call observer to a session}
\usage{
add_observer(session, observer)
}
\arguments{
\item{session}{A session environment from \code{\link{new_session}}.}

\item{observer}{A function of one argument (the event list).}
}
\value{
The session, invisibly.
}
\description{
Observers run after every tool call (run, denied, or declined). They
receive a single \code{event} list with fields:
\itemize{
\item \code{call} — the call list passed to \code{policy()}.
\item \code{decision} — the policy decision for the call.
\item \code{outcome} — one of \code{"ran"}, \code{"deny"},
\code{"declined"}.
\item \code{result} — the string returned to the LLM.
\item \code{success} — logical; TRUE only for \code{"ran"} with no
tool error.
\item \code{elapsed_ms} — wall time including policy overhead.
\item \code{turn_number} — the session's tool-call counter.
}
Errors raised inside an observer are swallowed.
}
