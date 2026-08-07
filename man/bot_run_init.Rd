% tinyrox says don't edit this manually, but it can't stop you!
\name{bot_run_init}
\alias{bot_run_init}
\title{Initialize the Matrix long-poll state}
\usage{
bot_run_init(system = NULL, model = NULL, provider = NULL, tools_filter = NULL)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}
}
\value{
A list holding the session registry, startup session handle,
  archive-flush signal path, and the saved poll options. Pass it to
  \code{\link{bot_run_step}}.
}
\description{
Performs everything \code{\link{bot_run}} does before its loop:
builds the per-room session registry, catches up on invites that
predate the saved sync token, and backfills recent room history into
the registry. Returns an opaque state object to drive with
\code{\link{bot_run_step}}.
}
\details{
End-to-end encryption is not set up here. When the config sets
\code{e2ee}, the chat.api Matrix adapter owns the Olm account and
Megolm sessions and both directions of encrypted traffic, so there is
no crypto state for this package to build or carry.

Use this with \code{bot_run_step()} when an external loop owns the
main process and needs to interleave the Matrix poll with other work
(a scheduler, a multiplexer, an embedding host). For a standalone bot,
call \code{\link{bot_run}}, which wraps both.

}
\examples{
\dontrun{
# Drive the loop yourself instead of calling bot_run():
state <- bot_run_init()
repeat bot_run_step(state, timeout = 30000L)
}
}
\seealso{
\code{\link{bot_run_step}}, \code{\link{bot_run}}
}
