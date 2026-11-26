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
journal, trace and attachments, and job records, worker folders and
remote-control sessions unchanged for as long go. Default:
\code{retention$session_days} from the user's config. Zero disables
idle-session cleanup. Separately configured payload, segment and
workspace lifetimes still apply.}
}
\value{
A data frame with one row per journal, trace, attachments
  folder, job folder, worker folder, remote-control session or unknown
  folder, or file eligible under a separate lifetime, invisibly:
  \code{agent} (\code{NA} outside a session's
  files), \code{session} (for a job, its chain's newest job),
  \code{kind} (\code{"journal"}, \code{"trace"}, \code{"attachments"},
  \code{"job"}, \code{"worker"}, \code{"checkpoint"}, \code{"remote"},
  \code{"segment"}, \code{"entry"}, \code{"payload"}, \code{"workspace snapshot"}
  or \code{"unknown"}), \code{path}, \code{last_use}, \code{idle_days},
  \code{bytes}, \code{incomplete} (a job chain kept longer for its
  resume snapshot) and \code{removed}.
}
\description{
corteza keeps its sessions under
\code{tools::R_user_dir("corteza", "data")}. For every session that
has not been used for \code{days}, this removes its journal (in
\code{<id>.journal/}: the provider-native record of the live history
and of every history a compaction replaced), its tool trace
(\code{<id>_trace.jsonl}), and the files a chat room's session kept of
what people sent it (\code{<id>.attachments/}).
}
\details{
It also removes, from corteza's state folder (\code{state/} in the
same place, or \env{CORTEZA_STATE_DIR}) and its remote-control folder
(\code{remote-control/}, or \env{CORTEZA_REMOTE_DIR}):
\itemize{
\item a delegated job's records, with every job linked to it as a
resume, a review or a delegation, once all of them have ended and
none has changed for \code{days}. While one of them is queued or
running, all stay. A job that ended incomplete and was never
resumed keeps the snapshot a resume starts from, so its chain
waits for \code{retention$incomplete_days} in the user's config
(default 180; never fewer than \code{days}; 0 keeps it);
\item a worker process's folder that holds no checkpoint, unchanged
for \code{days}: its journal, less its audit records;
\item a worker's folder with a checkpoint (the R objects and history
a delegated job's worker left), once both the folder and the
session it belongs to have gone unused for \code{days}: at the
terminal the conversation's session, in a chat room the room's or
thread's session, judged by its journal. The checkpoint goes first,
then the rest, less the journal's audit records;
\item a remote-control session unchanged for \code{days} that was
closed, or whose process on this machine has ended.
}

A worker's folder stays, whatever its age, while its worker process
is alive, while its session is open in the process doing the removing,
and while a job is open in its directory.

Transcripts, session stores, configuration, audit events and crypto state
are kept. A session whose journal is gone still
resumes from its transcript. A chat room that restores its context
from its journal starts its next conversation fresh.

A session's audit records are kept in its journal folder, under
\code{audit/}, and a job's in its own folder. Separate personal retention
settings opt into expiring unpinned payloads (\code{audit_payload_days}),
old journal segments (\code{journal_segment_days}) and legacy R workspace
snapshots beside a transcript (\code{workspace_snapshot_days}). Each
defaults to zero. The latest committed segment and live history stay.

A session was last used when its journal, its trace, its transcript or
its attachments last changed, whichever is newest. A symbolic link is
never removed, nor anything reached through one. A folder in the state
or remote-control folder whose name and record are not of the form
corteza writes stays whole; once it is \code{days} old it is listed
with kind \code{"unknown"}, so it can be looked at.

corteza runs this itself at most once a day: when a terminal session
starts, and while a chat bot runs. The session a terminal has open, and
the rooms a bot has open, are left alone. The number of days is
\code{retention$session_days} in
the user's config file (default 90; 0 keeps everything). A project's
config cannot change either setting.

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
