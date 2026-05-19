% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_run}
\alias{matrix_run}
\title{Run the Matrix adapter as a long-poll loop}
\usage{
matrix_run(timeout = 30000L, system = NULL, model = NULL, provider = NULL,
           tools_filter = NULL)
}
\arguments{
\item{timeout}{Integer. Long-poll timeout in milliseconds.}

\item{system}{Character or NULL. System prompt override.}

\item{model}{Character or NULL. Model override.}

\item{provider}{Character or NULL. Provider override.}

\item{tools_filter}{Character vector or NULL. Tool filter override.}
}
\value{
Never returns under normal operation. Crashes on fatal error
  so systemd can restart.
}
\description{
Creates one session up front and reuses it across polls so conversation
history accumulates within the process lifetime. Intended as the entry
point for a systemd user unit.
}
