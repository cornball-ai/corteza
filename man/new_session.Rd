% tinyrox says don't edit this manually, but it can't stop you!
\name{new_session}
\alias{new_session}
\title{Create a new turn session}
\usage{
new_session(channel = c("cli", "console", "matrix"), history = NULL,
            model_map = NULL, provider = "anthropic", tools_filter = NULL,
            system = NULL, approval_cb = NULL, max_turns = 10L, verbose = FALSE)
}
\arguments{
\item{channel}{Character, one of "cli", "console", "matrix".}

\item{history}{List of prior messages, or NULL.}

\item{model_map}{Named list with \code{cloud} and \code{local} model
names. Defaults to configured defaults.}

\item{provider}{LLM provider passed to \code{llm.api::agent}.}

\item{tools_filter}{Character vector passed to \code{get_tools()}.}

\item{system}{System prompt override (NULL for built-in default).}

\item{approval_cb}{Function called when policy returns \code{"ask"}.
Signature: \code{function(call, decision) -> TRUE|FALSE}. Default
denies (safe fallback).}

\item{max_turns}{Maximum LLM turns per call.}

\item{verbose}{Print tool call progress.}
}
\value{
An environment holding the session state.
}
\description{
Returns an environment with sensible defaults. Adapters set channel-
specific fields (e.g. \code{approval_cb}, \code{tools_filter}) before
calling \code{\link{turn}}.
}
