% tinyrox says don't edit this manually, but it can't stop you!
\name{session_setup}
\alias{session_setup}
\title{Configure and construct a session for any channel}
\usage{
session_setup(channel = c("cli", "console", "matrix"), cwd = getwd(),
              provider = NULL, model = NULL, tools = NULL, system = NULL,
              approval_cb = NULL, history = NULL, load_project_context = TRUE,
              validate_api_key = TRUE, verbose = FALSE, max_turns = 10L)
}
\arguments{
\item{channel}{Character, one of \code{"cli"}, \code{"console"},
\code{"matrix"}.}

\item{cwd}{Working directory. Defaults to the current directory.}

\item{provider}{Character or NULL. LLM provider override. NULL falls
back to \code{config$provider}, then \code{"anthropic"}.}

\item{model}{Character or NULL. Model override. NULL falls back to
\code{config$model}, then the provider default.}

\item{tools}{Character vector, NULL, or the string \code{"all"}.
Tool filter passed through to \code{get_tools()}. NULL is treated
as \code{"all"}.}

\item{system}{Character or NULL. System prompt. NULL auto-builds via
\code{load_context(cwd)} when \code{load_project_context = TRUE},
otherwise left NULL (channel supplies its own).}

\item{approval_cb}{Function or NULL. Approval callback for
\code{"ask"} verdicts; see \code{\link{new_session}}.}

\item{load_project_context}{Logical. When TRUE, auto-call
\code{load_context(cwd)} to assemble the system prompt. Channels
with their own short system prompt (like matrix) pass FALSE.}

\item{validate_api_key}{Logical. When TRUE, error if the provider's
API key env var is unset or empty.}

\item{verbose}{Logical. Passed through to \code{new_session}.}

\item{max_turns}{Integer. Passed through to \code{new_session}.}
}
\value{
A session environment from \code{\link{new_session}}, with
  an extra \code{cwd} field set.
}
\description{
Performs pre-turn setup common to all channels:
\enumerate{
\item Loads project + global corteza config from \code{cwd}.
\item Resolves provider, model, and verifies the required API
environment variable is set.
\item Registers built-in skills and loads user/project skills and
skill docs from \code{~/.corteza/skills} and
\code{<cwd>/.corteza/skills}.
\item Loads skill packages declared in the config.
\item Optionally builds the system prompt via \code{load_context(cwd)}.
\item Returns a \code{new_session()} built from the above.
}
}
