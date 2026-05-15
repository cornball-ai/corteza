% tinyrox says don't edit this manually, but it can't stop you!
\name{user_deny_condition}
\alias{user_deny_condition}
\title{Build a condition object representing "user denied this tool call".}
\usage{
user_deny_condition(tool = "?")
}
\arguments{
\item{tool}{Character. Name of the denied tool (for the history
marker). Defaults to `"?"` when unavailable.}
}
\value{
A condition object with class
  `c("corteza_user_deny", "interrupt", "condition")`.
}
\description{
Raised by [chat_approval_cb()] (and the CLI's `cli_approval_cb`) when
the user picks "3. Deny". The class deliberately excludes `"error"`
so the defensive `tryCatch(error = function(e) FALSE)` wrapper around
approval_cb in [.make_tool_handler()] does not swallow it. The
`"interrupt"` class lets the existing chat()/CLI interrupt-marker
machinery fall through cleanly if a surface forgets to register a
`corteza_user_deny` handler.
}
\keyword{internal}
