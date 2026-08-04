% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_configure}
\alias{matrix_configure}
\title{Configure the Matrix channel for this host}
\usage{
matrix_configure(
  server,
  user,
  password,
  room,
  model = NULL,
  provider = "anthropic",
  tools_filter = NULL,
  auto_approve_asks = FALSE,
  bots = NULL,
  models = NULL,
  model_badge = c("never", "non_default", "always"),
  display_name = NULL
)
}
\arguments{
\item{server}{Character. Homeserver base URL.}

\item{user}{Character. Bot localpart or full Matrix ID.}

\item{password}{Character. Bot password. Stored locally so the bot
can re-authenticate if its access token is invalidated.}

\item{room}{Character. Room ID or alias the bot should read and post
to. If the bot has been invited but not joined, it will be joined.}

\item{model}{Character or NULL. Default model name.}

\item{provider}{Character. LLM provider: "anthropic", "anthropic_claude",
"openai", "openai_codex",
"moonshot", or "ollama".}

\item{tools_filter}{Character vector or NULL. Passed to
\code{get_tools()} to restrict which tools the bot can invoke.
NULL allows all registered tools.}

\item{auto_approve_asks}{Logical. When TRUE, tool calls that policy
returns \code{"ask"} for are auto-approved. Suitable for a
personal bot on a trusted tailnet. When FALSE (default) asks are
declined until the thumbs-up reaction protocol lands.}

\item{bots}{Character vector or NULL. Full Matrix IDs of other known
bot accounts. Their messages only get a reply when they mention
this bot, and they are not counted as humans when deciding whether
a room gets ungated replies (a room whose only non-bot member is
one human is answered without a mention).}

\item{models}{Character vector or NULL. Extra entries for the
\code{/model} menu, each a \code{"model provider"} pair (e.g.
\code{"claude-sonnet-4-6 anthropic_claude"}; a bare model name
uses the default provider). The menu always lists the configured
default and the live local Ollama inventory; this key adds hosted
models that can't be discovered automatically.}

\item{model_badge}{Character. When to show which model is answering:
\code{"never"} (default, current behavior), \code{"non_default"}
(only while a \code{/model} switch has moved a room session off
the configured default -- silence means the default, a badge means
you are spending something else), or \code{"always"}. When active,
replies get a lightning-bolt first line naming the model and
provider, and the bot renames itself to \code{"<name> <bolt>
<model>"} so every message wears the model in its sender line.
The display name is account-global: with sessions in several
rooms, the most recent switch wins (the per-reply badge line is
always room-accurate).}

\item{display_name}{Character or NULL. Base display name the badge
rename builds on. Defaults to the localpart of the bot's user id.}
}
\value{
The saved configuration, invisibly.
}
\description{
Logs in to a Matrix homeserver as the bot account, joins (or records)
the target room, and writes credentials to
\code{tools::R_user_dir("corteza", "config")/matrix.json} with file
mode 0600. Call once per host. Model, provider, tools_filter, and
auto_approve_asks are defaults the poll loop uses unless overridden
at call time.
}
\details{
Pre-CRAN releases stored the file at \code{~/.corteza/matrix.json};
that path is still read for backward compatibility, but the next
\code{matrix_configure()} call writes to the new location.

}
\examples{
\dontrun{
# Requires a real Matrix server and bot credentials. Configuration
# is written under tools::R_user_dir("corteza", "config").
matrix_configure(
    server = "https://matrix.example.org",
    user = "bot",
    password = "secret",
    room = "!roomid:example.org"
)
}
}
