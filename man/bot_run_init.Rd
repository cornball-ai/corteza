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
  hooks = NULL,
  cwd = NULL,
  project_roots = NULL
)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{hooks}{Named list of functions or NULL. Policy the loop asks
the host about; see Hooks. NULL, the default, means a bare bot:
\code{/clear} resets and nothing is filed, a thread reply starts
from the thread's own history, and every message is the bot's to
answer.}

\item{cwd}{Character or NULL. The directory a room works in when no
operator has bound it to one with \code{/cd}, for this run. It must
exist: corteza does not create it, and the run stops at its start
when this directory, or the one saved by \code{\link{bot_configure}},
is missing. NULL (default) uses the saved one. With neither, such a
room works in a scratch folder under the session's temporary
directory, which is deleted when the process ends.}

\item{project_roots}{Character vector or NULL. The directories inside
which an operator may bind a room with \code{/cd} and list with
\code{/ls}, for this run. Each must exist: corteza does not create
them. NULL (default) uses the ones saved by
\code{\link{bot_configure}}; with none, \code{/cd <dir>} is refused.}
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
corteza runs a conversation. What becomes of one once it ends, where
a thread's earlier context comes from, and which rooms' messages the
host answers itself, belong to the host that runs the bot over time
(a host might file segments and fold them into topic rooms). Four
slots, all optional:

\describe{
  \item{\code{on_archive(chat, cfg, room_id, archive)}}{Called with
    each range of a room's or thread's conversation that nothing has
    filed yet: at \code{/clear}, at a flush
    (\code{\link{bot_request_flush}}), and again for every range the
    hook did not take before. \code{archive} is a list: \code{id},
    the same however often the range is offered; \code{bot}, the
    bot's Matrix id; \code{room_id}; \code{thread}, the thread's root
    event or NULL; \code{session}, the session's id;
    \code{first_event} and \code{last_event}, the Matrix events that
    bound the range; \code{entries}, oldest first, each
    \code{list(event_id, role, content)} with the text as posted; and
    \code{reason}, \code{"clear"} or \code{"flush"}. Returns
    \code{list(archived = TRUE, ref)} once the host has filed the
    range or chosen not to; \code{ref}, a string or NULL, is what
    \code{on_clear} receives. Anything else, or an error, which is
    reported, and the range waits under \code{CORTEZA_STATE_DIR} for
    the next \code{/clear} or flush, through a restart. When it
    cannot be written there either, \code{/clear} keeps the session
    that holds it, and says so in the room. A range from
    a flush takes the conversation's later entries under the same
    \code{id}, so a host that filed one before replaces it. With no
    hook nothing is filed.}
  \item{\code{on_clear(chat, cfg, room_id, title, vault_ref)}}{Called
    from the \code{/clear} handler once \code{on_archive} took the
    conversation, when the user said something in it and the clear
    was not inside a thread. \code{chat} is the live chat.api client,
    \code{cfg} the bot config, \code{title} the segment title corteza
    derived from the transcript's opening line, \code{vault_ref} the
    \code{ref} the archive hook returned (or NULL). Returns
    \code{list(id, name)} for a room it filed the conversation into,
    which the acknowledgement names, or NULL for none. An error is
    reported and treated as none: archive-and-reset is the critical
    path.}
  \item{\code{thread_context(chat, room_id, thread_root)}}{Called once
    per thread session before its first reply. Returns
    \code{list(segment, vault, kind)} naming the archived
    conversation the thread continues, or NULL when it continues
    nothing; \code{vault} is the file's path, vault-relative or
    absolute. With \code{kind = "summary"} the file is a summary, and
    the session is seeded with its start, said to be one; otherwise
    it is a transcript, and the session is seeded with its tail.}
  \item{\code{on_message(chat, cfg, room_id, message, session,
    backfill)}}{The host's first refusal on each message from someone
    else, for a host that answers some rooms itself. Called once per
    event, after remote control and delegation take theirs and before
    anything else: the read receipt, the media step, the reply rules,
    commands and the turn. \code{message} is the transport's
    \code{chat_message} as received, \code{raw} included;
    \code{session} is the room's or the thread's session, where the
    host may keep state of its own. Returns \code{list(handled = TRUE)}
    when the host took the message: corteza marks it seen and does
    nothing more with it, not even a read receipt, and from then on
    records none of the bot's own messages in that room. Anything
    else, or an error, which is reported, and the message goes on as
    if there were no hook. In a restart's backfill \code{backfill} is
    TRUE: the hook answers from the room's own state and drives
    nothing, and a message it takes stays out of the session's
    history.}
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
