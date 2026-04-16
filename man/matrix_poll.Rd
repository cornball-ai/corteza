% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_poll}
\alias{matrix_poll}
\title{One iteration of sync-and-reply}
\usage{
matrix_poll(system = NULL, model = NULL, provider = NULL, timeout = 0L)
}
\arguments{
\item{system}{Character or NULL. System prompt; default derived
from the bot identity.}

\item{model}{Character or NULL. Model override; falls back to the
model stored in the saved configuration.}

\item{provider}{Character or NULL. Provider override; falls back to
the provider stored in the saved configuration.}

\item{timeout}{Integer. Long-poll timeout in milliseconds. 0 returns
immediately.}
}
\value{
An integer count of messages replied to, invisibly.
}
\description{
Fetches new messages from the configured room, passes each user
message to \code{\link[llm.api]{chat}}, posts the reply back, and
persists the sync token. Safe to call repeatedly (idempotent per
sync token).
On first run there is no saved sync token, so this call establishes
a baseline and returns without processing any history.
}
