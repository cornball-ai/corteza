% tinyrox says don't edit this manually, but it can't stop you!
\name{turn}
\alias{turn}
\title{Run one agent turn}
\usage{
turn(prompt, session)
}
\arguments{
\item{prompt}{Character. User prompt.}

\item{session}{A session environment created by \code{\link{new_session}}.}
}
\value{
A list with \code{reply} (character) and \code{session} (the
  updated session environment; also mutated in place).
}
\description{
Sends \code{prompt} to the configured LLM with tool use enabled. Every
tool call the LLM makes is routed through \code{\link{policy}} before
being dispatched to the skill registry.
}
