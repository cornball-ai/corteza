% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_configure}
\alias{matrix_configure}
\title{Configure the Matrix channel for this host}
\usage{
matrix_configure(server, user, password, room, model = NULL, provider = "auto")
}
\arguments{
\item{server}{Character. Homeserver base URL.}

\item{user}{Character. Bot localpart or full Matrix ID.}

\item{password}{Character. Bot password. Stored locally so the bot
can re-authenticate if its access token is invalidated.}

\item{room}{Character. Room ID or alias the bot should read and post
to. If the bot has been invited but not joined, it will be joined.}

\item{model}{Character or NULL. Default model name passed to
\code{llm.api::chat}.}

\item{provider}{Character. Default LLM provider.}
}
\value{
The saved configuration, invisibly.
}
\description{
Logs in to a Matrix homeserver as the bot account, joins (or records)
the target room, and writes credentials to ~/.llamar/matrix.json with
file mode 0600. Call once per host. Model and provider are optional
defaults the poll loop uses unless overridden at call time.
}
