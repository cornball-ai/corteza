% tinyrox says don't edit this manually, but it can't stop you!
\name{bot_run_init}
\alias{bot_run_init}
\title{Initialize the Matrix long-poll state}
\usage{
bot_run_init(
  system = NULL,
  model = NULL,
  provider = NULL,
  tools_filter = NULL,
  hooks = NULL
)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{hooks}{Named list of functions or NULL. Policy the loop asks
the host about; see Hooks. NULL, the default, means a bare bot:
\code{/clear} archives and resets without filing anything, and a
thread reply starts from the thread's own history.}
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
\section{Hooks}{
corteza runs a conversation. What becomes of one once it ends, and
where a thread's earlier context comes from, belong to the host that
runs the bot over time (cerebro files segments and folds them into
topic rooms). Two slots, both optional:

\describe{
  \item{\code{on_clear(chat, cfg, room_id, title, vault_ref)}}{Called
    from the \code{/clear} handler once the conversation is archived,
    when the user said something in it and the clear was not inside a
    thread. \code{chat} is the live chat.api client, \code{cfg} the
    bot config, \code{title} the segment title corteza derived from
    the transcript's opening line, \code{vault_ref} the archive's
    vault-relative path (or NULL). Returns \code{list(id, name)} for a
    room it filed the conversation into, which the acknowledgement
    names, or NULL for none. An error is reported and treated as
    none: archive-and-reset is the critical path.}
  \item{\code{thread_context(chat, room_id, thread_root)}}{Called once
    per thread session before its first reply. Returns
    \code{list(segment, vault)} naming the archived conversation the
    thread continues, or NULL when it continues nothing; \code{vault}
    is the transcript's path, vault-relative or absolute. A result
    seeds the session with the transcript's tail.}
}

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
