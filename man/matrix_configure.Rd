% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_configure}
\alias{matrix_configure}
\title{Configure the Matrix channel for this host}
\usage{
matrix_configure(server, user, password, room, model = NULL,
                 provider = c("anthropic", "openai", "moonshot", "ollama"),
                 tools_filter = NULL, auto_approve_asks = FALSE)
}
\arguments{
\item{server}{Character. Homeserver base URL.}

\item{user}{Character. Bot localpart or full Matrix ID.}

\item{password}{Character. Bot password. Stored locally so the bot
can re-authenticate if its access token is invalidated.}

\item{room}{Character. Room ID or alias the bot should read and post
to. If the bot has been invited but not joined, it will be joined.}

\item{model}{Character or NULL. Default model name.}

\item{provider}{Character. LLM provider: "anthropic", "openai",
"moonshot", or "ollama".}

\item{tools_filter}{Character vector or NULL. Passed to
\code{get_tools()} to restrict which tools the bot can invoke.
NULL allows all registered tools.}

\item{auto_approve_asks}{Logical. When TRUE, tool calls that policy
returns \code{"ask"} for are auto-approved. Suitable for a
personal bot on a trusted tailnet. When FALSE (default) asks are
declined until the thumbs-up reaction protocol lands.}
}
\value{
The saved configuration, invisibly.
}
\description{
Logs in to a Matrix homeserver as the bot account, joins (or records)
the target room, and writes credentials to ~/.llamar/matrix.json with
file mode 0600. Call once per host. Model, provider, tools_filter,
and auto_approve_asks are defaults the poll loop uses unless
overridden at call time.
}
