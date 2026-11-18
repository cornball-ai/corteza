% tinyrox says don't edit this manually, but it can't stop you!
\name{bot_archive_all}
\alias{bot_archive_all}
\title{Hand each room's new conversation to the host}
\usage{
bot_archive_all(sessions, chat = NULL, on_archive = NULL, cfg = NULL)
}
\arguments{
\item{sessions}{A registry environment built by
\code{bot_run}/\code{bot_poll}. Keys are room IDs, values
are session environments carrying \code{$transcript}, the
Matrix-visible event ledger.}

\item{chat}{Optional chat.api client, handed to the hook.}

\item{on_archive}{The host's \code{on_archive} hook, or NULL.}

\item{cfg}{The bot config, which names the bot. NULL reads the saved
one when there is a hook.}
}
\value{
Integer count of ranges the host took, invisibly.
}
\description{
Offers what every room and thread session said since its last
archive to the host's \code{on_archive} hook (see
\code{\link{bot_run_init}}), and again whatever the hook did not take
before. A range the host took is consumed from the session's ledger
and its Matrix event ids are kept per room under
\code{CORTEZA_STATE_DIR}, so a restart's backfill is recognized
rather than offered again. A range it did not take waits there for
the next flush. With no hook nothing is filed.
}
\examples{
\dontrun{
# Requires a running Matrix session registry and a host's hook.
reg <- new.env(parent = emptyenv())
bot_archive_all(reg, on_archive = function(chat, cfg, room_id, archive) {
    list(archived = TRUE, ref = NULL)
})
}
}
