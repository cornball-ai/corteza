% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_poll}
\alias{matrix_poll}
\title{One iteration of sync-and-reply}
\usage{
matrix_poll(system = NULL, model = NULL, provider = NULL, tools_filter = NULL,
            timeout = 0L, sessions = NULL)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{timeout}{Integer. Long-poll timeout in milliseconds. 0 returns
immediately.}

\item{sessions}{Environment from \code{matrix_new_session_registry()}
keyed by room_id, or NULL to build fresh sessions each call.}
}
\value{
An integer count of messages replied to, invisibly.
}
\description{
Fetches new messages across all joined rooms and runs \code{\link{turn}}
against each. Auto-joins any pending invites the bot has received.
Replies are sent back to the originating room. On first run there is
no saved sync token, so this call establishes a baseline and returns
without processing history.
Pass \code{sessions = NULL} (the default) for a stateless one-shot —
each incoming message builds a fresh session. Pass a registry created
by \code{matrix_new_session_registry()} so a long-running
\code{matrix_run} keeps a separate history per room (conversations
in different rooms don't cross-contaminate).
}
