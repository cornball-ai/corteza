% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_poll}
\alias{matrix_poll}
\title{One iteration of sync-and-reply}
\usage{
matrix_poll(system = NULL, model = NULL, provider = NULL, tools_filter = NULL,
            timeout = 0L, session = NULL)
}
\arguments{
\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}

\item{timeout}{Integer. Long-poll timeout in milliseconds. 0 returns
immediately.}

\item{session}{Session environment from \code{matrix_new_session}, or
NULL to build a fresh session each call.}
}
\value{
An integer count of messages replied to, invisibly.
}
\description{
Fetches new messages from the configured room and runs \code{\link{turn}}
against each. On first run there is no saved sync token, so this call
establishes a baseline and returns without processing any history.
Pass \code{session = NULL} (the default) for a stateless one-shot that
cron can invoke. Pass a session created by \code{matrix_new_session} to
accumulate conversation history across polls within one process.
}
