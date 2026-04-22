% tinyrox says don't edit this manually, but it can't stop you!
\name{cli_worker_drain_events}
\alias{cli_worker_drain_events}
\title{Drain structured events the worker wrote to stderr while a tool
ran. When `trace` is TRUE each event is pretty-printed via
[printify::print_step()] / [printify::print_message()]; otherwise
the events are still read (to keep the stderr buffer from growing)
but not displayed.}
\usage{
cli_worker_drain_events(session, trace = FALSE)
}
\arguments{
\item{session}{A `callr::r_session`.}

\item{trace}{Pretty-print events if TRUE.}
}
\value{
Invisible NULL.
}
\description{
Drain structured events the worker wrote to stderr while a tool
ran. When `trace` is TRUE each event is pretty-printed via
[printify::print_step()] / [printify::print_message()]; otherwise
the events are still read (to keep the stderr buffer from growing)
but not displayed.
}
\keyword{internal}
