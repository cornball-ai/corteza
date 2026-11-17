% tinyrox says don't edit this manually, but it can't stop you!
\name{sessions_prune}
\alias{sessions_prune}
\title{Remove what old sessions no longer need}
\usage{
sessions_prune(dry_run = FALSE, days = NULL)
}
\arguments{
\item{dry_run}{If \code{TRUE}, remove nothing and report what would
go.}

\item{days}{Sessions unused for at least this many days lose their
journal and trace. Default: \code{retention$session_days} from the
user's config. 0 removes nothing.}
}
\value{
A data frame with one row per journal or trace, invisibly:
  \code{agent}, \code{session}, \code{kind}, \code{path},
  \code{last_use}, \code{idle_days}, \code{bytes} and \code{removed}.
}
\description{
corteza keeps its sessions under
\code{tools::R_user_dir("corteza", "data")}. For every session that
has not been used for \code{days}, this removes its journal (in
\code{<id>.journal/}: the provider-native record of the live history
and of every history a compaction replaced) and its tool trace
(\code{<id>_trace.jsonl}).
}
\details{
Transcripts, session stores, workspaces, config, audit records and
crypto state are never removed. A session whose journal is gone still
resumes from its transcript. A chat room that restores its context
from its journal starts its next conversation fresh.

A session's audit records are kept in its journal folder, under
\code{audit/}. They stay, and so does the folder that holds them:
see \code{\link{audit_payload_expire}}.

A session was last used when its journal, its trace or its transcript
last changed, whichever is newest. A symbolic link is never removed.

corteza runs this itself at most once a day: when a terminal session
starts, and while a chat bot runs. The session a terminal has open is
left alone. The number of days is \code{retention$session_days} in
the user's config file (default 90; 0 keeps everything). A project's
config cannot change it.

}
\examples{
# A data directory under tempdir(), so that nothing of yours is read
# or removed.
old <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
Sys.setenv(R_USER_DATA_DIR = file.path(tempdir(), "corteza-data"))

# A session last used 100 days ago: a journal and a transcript.
sessions <- file.path(tools::R_user_dir("corteza", "data"), "agents",
                      "main", "sessions")
journal <- file.path(sessions, "example.journal")
transcript <- file.path(sessions, "example.jsonl")
dir.create(journal, recursive = TRUE)
writeLines("{}", file.path(journal, "live.json"))
writeLines("{}", transcript)
long_ago <- Sys.time() - 100 * 24 * 60 * 60
Sys.setFileTime(c(file.path(journal, "live.json"), journal, transcript),
                long_ago)

sessions_prune(dry_run = TRUE, days = 90)
sessions_prune(days = 90)
list.files(sessions)

unlink(file.path(tempdir(), "corteza-data"), recursive = TRUE)
if (is.na(old)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old)
}
}
