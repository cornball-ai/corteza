% tinyrox says don't edit this manually, but it can't stop you!
\name{tool_harness_note}
\alias{tool_harness_note}
\title{Record a one-line lesson in the continual harness store}
\usage{
tool_harness_note(
  title,
  fact,
  evidence = NULL,
  scope = "project",
  ctx = list()
)
}
\arguments{
\item{title}{Short title for the fact; becomes the entry id (slug).}

\item{fact}{The fact itself: one exact line, at most 300 characters.}

\item{evidence}{Where this was established: a test, a run id, an
issue URL, a file. Optional but strongly encouraged.}

\item{scope}{Where to store it: "project" (default; travels with
this repo) or "global" (all projects).}

\item{ctx}{Server-injected context (session, cwd); not model-facing.}
}
\value{
Confirmation string with the entry id.
}
\description{
Stores one durable, verified fact learned during the session, as a
single exact line. It will be shown in future sessions' context.
Use it the moment a fact is verified -- including negative results
("X does NOT do Y"). Approval is asked per note by default.
}
\keyword{internal}
