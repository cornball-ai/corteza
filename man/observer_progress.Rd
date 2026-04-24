% tinyrox says don't edit this manually, but it can't stop you!
\name{observer_progress}
\alias{observer_progress}
\title{Built-in progress observer that prints to stdout}
\usage{
observer_progress()
}
\value{
A function to pass to \code{\link{add_observer}}.
}
\description{
Prints one line per tool call suitable for an interactive REPL:
\code{"  [tool] hint (N lines)\n"}. The hint is a short summary of
the call (file path, code snippet, search pattern) computed by
\code{tool_hint()}.
}
