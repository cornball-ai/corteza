% tinyrox says don't edit this manually, but it can't stop you!
\name{session_setup}
\alias{session_setup}
\title{Configure and construct a session for any channel}
\usage{
session_setup(
  channel = c("cli", "console", "matrix"),
  cwd = getwd(),
  provider = NULL,
  model = NULL,
  tools = NULL,
  system = NULL,
  approval_cb = NULL,
  history = NULL,
  load_project_context = TRUE,
  validate_api_key = TRUE,
  verbose = FALSE,
  max_turns = 50L,
  web_search = NULL
)
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

\item{system}{Character or NULL. System prompt. NULL auto-builds via the
saber-backed context manifest when \code{load_project_context = TRUE},
otherwise left NULL (channel supplies its own).}

\item{approval_cb}{Function or NULL. Approval callback for
\code{"ask"} verdicts; see \code{\link{new_session}}.}

\item{history}{List or NULL. Prior conversation messages to seed
the session with (each entry a list with \code{role} and
\code{content}).}

\item{load_project_context}{Logical. When TRUE, auto-call
the saber manifest and rendered system prompt. Channels that supply
their own system prompt pass FALSE.}

\item{validate_api_key}{Logical. When TRUE, error if the provider's
API key env var is unset or empty.}

\item{verbose}{Logical. Passed through to \code{new_session}.}

\item{max_turns}{Integer. Passed through to \code{new_session}.
Defaults to 50, a safety net for interactive channels where a
multi-step request (read + edit + verify several files) can easily
exceed the \code{new_session()} default of 10.}

\item{web_search}{Logical or NULL. Provider-native web search toggle
passed through to \code{\link{new_session}}. NULL falls back to
\code{config$web_search}, then on by default for supported providers.
The endpoint for the \code{"openai_compatible"} provider is read
from \code{config$base_url} (falling back to the
\code{OPENAI_COMPATIBLE_BASE_URL} environment variable); a missing
endpoint or model errors here rather than at the first request.}
}
\value{
A session environment from \code{\link{new_session}}, with an
  extra \code{cwd} field and, for an auto-built system prompt, a
  \code{context_manifest} field.
}
\description{
Performs pre-turn setup common to all channels:
}
\details{
\enumerate{
\item Loads project + global corteza config from \code{cwd}.
\item Resolves provider, model, and verifies the required API
environment variable is set.
\item Registers built-in skills and loads user/project skills and
skill docs from \code{tools::R_user_dir("corteza", "data")/skills}
and \code{<cwd>/.corteza/skills}.
\item Loads skill packages declared in the config.
\item Optionally builds the system prompt and source manifest via
\code{load_context_bundle(cwd)}.
\item Returns a \code{new_session()} built from the above.
}

}
\examples{
\dontrun{
# Requires the relevant provider API key in the environment.
s <- session_setup("cli", provider = "anthropic",
                   load_project_context = FALSE)
s$model
}
}
