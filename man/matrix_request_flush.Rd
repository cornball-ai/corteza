% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_request_flush}
\alias{matrix_request_flush}
\title{Ask the running matrix bot to archive sessions to pensar}
\usage{
matrix_request_flush()
}
\value{
The signal file path, invisibly.
}
\description{
Drops an \code{archive.signal} file in the corteza state directory.
The next iteration of the long-poll loop in \code{\link{matrix_run}}
picks it up, runs \code{\link{matrix_archive_all}}, and removes the
file. Safe to call from any process or scheduler — systemd, Task
Scheduler, launchd, cron, or a separate R session — without needing
to know the bot's PID or share its memory.
}
