# Retention: what corteza removes from its own data directory, and when.
#
# CRAN lets a package keep files under tools::R_user_dir() on condition
# that they stay small and are "actively managed (including removing
# outdated material)". Up to 0.8.0 corteza removed nothing. A session's
# journal (R/journal.R) keeps every history a rewrite replaced, and it
# stayed on disk however long ago the session was last used.
#
# What goes: the journal, the tool trace and the kept attachments of a
# session nobody has used for `session_days`. What stays: every
# transcript, the session stores, workspaces, config, audit records and
# crypto state. A session whose journal is gone still opens from its
# transcript. A room that restores its context from its journal starts
# fresh.
#
# A session's audit records are kept inside its journal folder
# (`<id>.journal/audit/`, R/audit-stream.R) and have their own expiry,
# controlled separately from idle-session retention. A journal is removed file by file
# (retention_own()), and its folder goes only once empty.
#
# Job chains, worker folders and remote-control sessions are part 2, in
# R/retention-state.R. The daily run and sessions_prune() do both.
#
# The setting is read from the user's config file alone
# (retention_config()), like the supervisor's. A project's config sits
# in a directory an agent writes in, and must not decide how long the
# user's records are kept.

RETENTION_SESSION_DAYS <- 90
# A job chain that ended incomplete and was never resumed holds a resume
# snapshot, so it waits longer (R/retention-state.R).
RETENTION_INCOMPLETE_DAYS <- 180
RETENTION_EVERY_SECS <- 24 * 60 * 60
# A bot calls retention_tick() on every poll. It looks at the stamp file
# this often and no more.
RETENTION_LOOK_SECS <- 60 * 60

.retention <- new.env(parent = emptyenv())

# `retention` from the user's config. What corteza cannot read keeps
# things: a config file that does not parse turns retention off, and a
# value that is not one non-negative number is read as 0, which keeps
# everything it governs. Neither must be taken for a shorter time than
# the person meant. load_config_file() reads a file it cannot parse as
# an empty one, which here would mean the defaults, so the file is
# parsed again.
retention_config <- function() {
    defaults <- list(session_days = RETENTION_SESSION_DAYS,
                     incomplete_days = RETENTION_INCOMPLETE_DAYS,
                     audit_payload_days = 0, journal_segment_days = 0,
                     workspace_snapshot_days = 0)
    path <- corteza_config_path("config.json")
    if (!file.exists(path)) {
        return(defaults)
    }
    cfg <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE,
                                       simplifyDataFrame = FALSE)$retention,
                    error = function(e) NA)
    if (is.null(cfg)) {
        return(defaults)
    }
    out <- list()
    for (key in names(defaults)) {
        days <- NA
        if (is.list(cfg)) {
            days <- cfg[[key]] %||% defaults[[key]]
        }
        if (!is.numeric(days) || length(days) != 1L || !is.finite(days) ||
            days < 0) {
            log_event("retention_config_invalid", key = key, level = "warn")
            days <- 0
        }
        if (key == "journal_segment_days") days <- ceiling(days)
        out[[key]] <- as.numeric(days)
    }
    out
}

#' Remove what old sessions no longer need
#'
#' corteza keeps its sessions under
#' \code{tools::R_user_dir("corteza", "data")}. For every session that
#' has not been used for \code{days}, this removes its journal (in
#' \code{<id>.journal/}: the provider-native record of the live history
#' and of every history a compaction replaced), its tool trace
#' (\code{<id>_trace.jsonl}), and the files a chat room's session kept of
#' what people sent it (\code{<id>.attachments/}).
#'
#' It also removes, from corteza's state folder (\code{state/} in the
#' same place, or \env{CORTEZA_STATE_DIR}) and its remote-control folder
#' (\code{remote-control/}, or \env{CORTEZA_REMOTE_DIR}):
#' \itemize{
#'   \item a delegated job's records, with every job linked to it as a
#'     resume, a review or a delegation, once all of them have ended and
#'     none has changed for \code{days}. While one of them is queued or
#'     running, all stay. A job that ended incomplete and was never
#'     resumed keeps the snapshot a resume starts from, so its chain
#'     waits for \code{retention$incomplete_days} in the user's config
#'     (default 180; never fewer than \code{days}; 0 keeps it);
#'   \item a worker process's folder that holds no checkpoint, unchanged
#'     for \code{days}: its journal, less its audit records;
#'   \item a worker's folder with a checkpoint (the R objects and history
#'     a delegated job's worker left), once both the folder and the
#'     session it belongs to have gone unused for \code{days}: at the
#'     terminal the conversation's session, in a chat room the room's or
#'     thread's session, judged by its journal. The checkpoint goes first,
#'     then the rest, less the journal's audit records;
#'   \item a remote-control session unchanged for \code{days} that was
#'     closed, or whose process on this machine has ended.
#' }
#'
#' A worker's folder stays, whatever its age, while its worker process
#' is alive, while its session is open in the process doing the removing,
#' and while a job is open in its directory.
#'
#' Transcripts, session stores, configuration, audit events and crypto state
#' are kept. A session whose journal is gone still
#' resumes from its transcript. A chat room that restores its context
#' from its journal starts its next conversation fresh.
#'
#' A session's audit records are kept in its journal folder, under
#' \code{audit/}, and a job's in its own folder. Separate personal retention
#' settings opt into expiring unpinned payloads (\code{audit_payload_days}),
#' old journal segments (\code{journal_segment_days}) and legacy R workspace
#' snapshots beside a transcript (\code{workspace_snapshot_days}). Each
#' defaults to zero. The latest committed segment and live history stay.
#'
#' A session was last used when its journal, its trace, its transcript or
#' its attachments last changed, whichever is newest. A symbolic link is
#' never removed, nor anything reached through one. A folder in the state
#' or remote-control folder whose name and record are not of the form
#' corteza writes stays whole; once it is \code{days} old it is listed
#' with kind \code{"unknown"}, so it can be looked at.
#'
#' corteza runs this itself at most once a day: when a terminal session
#' starts, and while a chat bot runs. The session a terminal has open, and
#' the rooms a bot has open, are left alone. The number of days is
#' \code{retention$session_days} in
#' the user's config file (default 90; 0 keeps everything). A project's
#' config cannot change either setting.
#'
#' @param dry_run If \code{TRUE}, remove nothing and report what would
#'   go.
#' @param days Sessions unused for at least this many days lose their
#'   journal, trace and attachments, and job records, worker folders and
#'   remote-control sessions unchanged for as long go. Default:
#'   \code{retention$session_days} from the user's config. Zero disables
#'   idle-session cleanup. Separately configured payload, segment and
#'   workspace lifetimes still apply.
#'
#' @return A data frame with one row per journal, trace, attachments
#'   folder, job folder, worker folder, remote-control session or unknown
#'   folder, or file eligible under a separate lifetime, invisibly:
#'   \code{agent} (\code{NA} outside a session's
#'   files), \code{session} (for a job, its chain's newest job),
#'   \code{kind} (\code{"journal"}, \code{"trace"}, \code{"attachments"},
#'   \code{"job"}, \code{"worker"}, \code{"checkpoint"}, \code{"remote"},
#'   \code{"segment"}, \code{"entry"}, \code{"payload"}, \code{"workspace snapshot"}
#'   or \code{"unknown"}), \code{path}, \code{last_use}, \code{idle_days},
#'   \code{bytes}, \code{incomplete} (a job chain kept longer for its
#'   resume snapshot) and \code{removed}.
#' @export
#'
#' @examples
#' # A data directory under tempdir(), so that nothing of yours is read
#' # or removed.
#' old <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
#' Sys.setenv(R_USER_DATA_DIR = file.path(tempdir(), "corteza-data"))
#'
#' # A session last used 100 days ago: a journal and a transcript.
#' sessions <- file.path(tools::R_user_dir("corteza", "data"), "agents",
#'                       "main", "sessions")
#' journal <- file.path(sessions, "example.journal")
#' transcript <- file.path(sessions, "example.jsonl")
#' dir.create(journal, recursive = TRUE)
#' writeLines("{}", file.path(journal, "live.json"))
#' writeLines("{}", transcript)
#' long_ago <- Sys.time() - 100 * 24 * 60 * 60
#' Sys.setFileTime(c(file.path(journal, "live.json"), journal, transcript),
#'                 long_ago)
#'
#' sessions_prune(dry_run = TRUE, days = 90)
#' sessions_prune(days = 90)
#' list.files(sessions)
#'
#' unlink(file.path(tempdir(), "corteza-data"), recursive = TRUE)
#' if (is.na(old)) {
#'     Sys.unsetenv("R_USER_DATA_DIR")
#' } else {
#'     Sys.setenv(R_USER_DATA_DIR = old)
#' }
sessions_prune <- function(dry_run = FALSE, days = NULL) {
    # Anything but FALSE that is not TRUE would otherwise remove.
    if (!isTRUE(dry_run) && !isFALSE(dry_run)) {
        stop("`dry_run` must be TRUE or FALSE.", call. = FALSE)
    }
    cfg <- retention_config()
    if (is.null(days)) {
        days <- cfg$session_days
    } else if (!is.numeric(days) || length(days) != 1L || is.na(days) ||
        days < 0) {
        stop("`days` must be one non-negative number.", call. = FALSE)
    }
    found <- retention_prune(days, dry_run = dry_run, incomplete_days = cfg$incomplete_days)
    found <- rbind(found, retention_history(cfg, dry_run = dry_run, idle = found))
    message(retention_summary(found, days, dry_run, cfg))
    invisible(found)
}

# Find what `days` lets go and, unless `dry_run`, remove it. `keep`
# names sessions to leave alone whatever their age.
retention_prune <- function(days, dry_run = FALSE, keep = character(),
                            now = Sys.time(),
                            incomplete_days = RETENTION_INCOMPLETE_DAYS) {
    found <- rbind(retention_scan(days, now = now),
                   retention_state_scan(days, incomplete_days, now = now, keep = keep))
    found <- found[!found$session %in% keep,, drop = FALSE]
    found$removed <- rep(FALSE, nrow(found))
    if (dry_run) {
        return(found)
    }
    # By session: a journal and its trace go together or not at all. So
    # do the folders of a job chain.
    family <- ifelse(found$kind %in% c(RETENTION_STATE_KINDS, "unknown"),
                     found$kind, "session")
    for (rows in split(seq_len(nrow(found)),
                       paste(family, found$agent, found$session, sep = "/"))) {
        first <- rows[[1L]]
        kind <- found$kind[[first]]
        if (kind == "unknown") {
            next
        }
        if (kind %in% RETENTION_STATE_KINDS) {
            # A checkpoint's place is read again with its folder, and a
            # worker folder that a live worker, an open session or an
            # open job holds now is left.
            path <- found$path[[first]]
            folder <- list(name = basename(path), dir = path,
                           real = normalizePath(path, winslash = "/", mustWork = FALSE))
            moved <- if (kind == "checkpoint") {
                retention_checkpoint_moved(path, found$last_use[[first]], keep)
            } else {
                retention_state_moved(found$path[rows], found$last_use[[first]]) ||
                (kind == "worker" && retention_worker_held(folder, keep = keep))
            }
            if (moved) {
                next
            }
            for (i in rows) {
                found$removed[[i]] <- retention_state_remove(found$path[[i]],
                    kind)
            }
            next
        }
        if (retention_moved(dirname(found$path[[first]]),
                            found$session[[first]], found$last_use[[first]])) {
            next
        }
        for (i in rows) {
            found$removed[[i]] <- retention_remove(found$path[[i]],
                found$kind[[i]])
        }
    }
    found
}

# Another process may hold the session and may have written to it since
# the scan. Its last use is read again just before its files go, and a
# session that moved is left.
retention_moved <- function(dir, id, last_use) {
    !identical(as.numeric(retention_last_use(dir, id)), as.numeric(last_use))
}

retention_remove <- function(path, kind) {
    if (!retention_removable(path, kind)) {
        return(FALSE)
    }
    if (!kind %in% c("journal", "attachments")) {
        # expand = FALSE: a name is a name, never a pattern.
        unlink(path, expand = FALSE)
        return(!file.exists(path))
    }
    own <- retention_own(path, kind)
    unlink(own$files, expand = FALSE)
    # Deepest first, each only once empty: a folder with a link in it
    # stays, and so does every folder above it.
    for (dir in own$dirs) {
        retention_rmdir(dir)
    }
    retention_rmdir(path)
    !any(file.exists(own$files))
}

# What a room's session keeps of what people sent it (bot_keep_attachment(),
# R/media.R): each file named by a 12-character digest and a dash, and
# the partial copy one is written through. Anything else in the folder is
# someone else's.
retention_attachment_files <- function(dir) {
    names <- list.files(dir, all.files = TRUE, no.. = TRUE)
    own <- grepl("^[0-9a-f]{12}-", names)
    files <- file.path(dir, names[own])
    files[!dir.exists(files)]
}

# What the journal's writer makes in a journal folder (R/journal.R): the
# live snapshot and the segments, each with the copy a replacement parks
# and the temp file it is written through, and the folder of shared
# entries. And a room's records of its conversations, kept beside them
# for the room's monitor (R/room-record.R). Anything else there is
# someone else's, the audit records first of all.
retention_journal_files <- function(journal) {
    names <- list.files(journal, all.files = TRUE, no.. = TRUE)
    own <- grepl("^(live|segment-[0-9]+)[.]json([.]tmp|[.]prev)?$", names) |
    grepl("^conversation-[A-Za-z0-9-]+[.]jsonl$", names) |
    names == "entries"
    file.path(journal, names[own])
}

# What retention may remove from a journal or attachments folder: the
# files named above and everything in a folder named above, less each
# link and everything behind one. corteza's writers make files and
# folders, never links, so a link was put there by someone else, and it
# stays with every folder it is in. A list of `files`, and of `dirs`
# deepest first, to remove once empty.
retention_own <- function(path, kind) {
    if (!retention_within(path)) {
        return(list(files = character(), dirs = character()))
    }
    if (kind == "journal") {
        named <- retention_journal_files(path)
    } else {
        named <- retention_attachment_files(path)
    }
    real <- normalizePath(path, winslash = "/", mustWork = FALSE)
    retention_tree(path, real, basename(named))
}

# The files and folders under `dir`, whose real path is `real`, that are
# no link and lie behind none: of `names` only, or of all it holds. No
# link is followed.
retention_tree <- function(dir, real, names = NULL) {
    if (is.null(names)) {
        names <- list.files(dir, all.files = TRUE, no.. = TRUE)
    }
    out <- list(files = character(), dirs = character())
    for (name in names) {
        here <- file.path(dir, name)
        real_here <- retention_step(here, real, name)
        if (is.null(real_here)) {
            next
        }
        if (dir.exists(here)) {
            inside <- retention_tree(here, real_here)
            out$files <- c(out$files, inside$files)
            out$dirs <- c(out$dirs, inside$dirs, here)
        } else {
            out$files <- c(out$files, here)
        }
    }
    out
}

# Remove a folder that has nothing in it. file.remove() refuses one
# that is not empty, which is the point. On Windows it refuses every
# folder, so the folder is looked at first there.
retention_rmdir <- function(dir) {
    if (length(list.files(dir, all.files = TRUE, no.. = TRUE))) {
        return(FALSE)
    }
    if (.Platform$OS.type == "windows") {
        unlink(dir, recursive = TRUE, expand = FALSE)
    } else {
        suppressWarnings(file.remove(dir))
    }
    !dir.exists(dir)
}

# One line for a person: what went, or would go.
retention_summary <- function(found, days, dry_run, config = list()) {
    history <- any(unlist(config[c("audit_payload_days", "journal_segment_days",
                                   "workspace_snapshot_days")]) > 0)
    if (days <= 0 && !history && !nrow(found)) {
        return("Retention is off: nothing is removed.")
    }
    session <- found$kind %in% c("journal", "trace", "attachments")
    state <- found$kind %in% RETENTION_STATE_KINDS
    lines <- c(retention_session_line(found[session,, drop = FALSE], days,
                                      dry_run),
               retention_state_line(found[state,, drop = FALSE], days, dry_run))
    for (kind in c("segment", "entry", "payload", "workspace snapshot")) {
        count <- sum(found$kind == kind & (dry_run | found$removed))
        if (count > 0L) {
            lines <- c(lines, sprintf("%s %s.", if (dry_run) "Would expire" else "Expired",
                                      retention_count(count, kind, if (kind == "entry") "entries" else paste0(kind, "s"))))
        }
    }
    if (!length(lines) && history && any(found$kind != "unknown")) {
        lines <- "No eligible records were removed."
    } else if (!length(lines) && history) {
        lines <- "Nothing is old enough to expire."
    } else if (!length(lines)) {
        lines <- sprintf("No session has gone unused for %s.",
                         retention_days(days))
    }
    unknown <- sum(found$kind == "unknown")
    if (unknown == 1L) {
        lines <- c(lines, paste("1 folder that does not read as corteza's",
                                "stays; sessions_prune(dry_run = TRUE) lists it."))
    } else if (unknown > 1L) {
        lines <- c(lines, sprintf(paste("%d folders that do not read as",
                                        "corteza's stay; sessions_prune(dry_run",
                                        "= TRUE) lists them."), unknown))
    }
    paste(lines, collapse = " ")
}

# What went of sessions' files, or would go. NULL when nothing did.
retention_session_line <- function(found, days, dry_run) {
    if (!nrow(found)) {
        return(NULL)
    }
    gone <- found
    verb <- "Would remove"
    if (!dry_run) {
        gone <- found[found$removed,, drop = FALSE]
        verb <- "Removed"
    }
    what <- c(retention_count(sum(gone$kind == "journal"), "journal"),
              retention_count(sum(gone$kind == "trace"), "trace"))
    folders <- sum(gone$kind == "attachments")
    if (folders > 0L) {
        what <- c(what, retention_count(folders, "folder of attachments",
                                        "folders of attachments"))
    }
    line <- sprintf("%s %s of sessions unused for %s or more (%.1f MB).",
                    verb, retention_and(what), retention_days(days),
                    sum(gone$bytes) / 1024 ^ 2)
    left <- nrow(found) - nrow(gone)
    if (left > 0L) {
        line <- paste(line, sprintf("%d left in place.", left))
    }
    line
}

# What went of the state stores (R/retention-state.R), or would go,
# counting a job chain once however many folders it has, and naming each
# chain that waited for its resume snapshot. NULL when nothing did.
retention_state_line <- function(found, days, dry_run) {
    if (!nrow(found)) {
        return(NULL)
    }
    gone <- found
    verb <- "Would remove"
    if (!dry_run) {
        gone <- found[found$removed,, drop = FALSE]
        verb <- "Removed"
    }
    units <- function(x, kind) length(unique(x$session[x$kind == kind]))
    nouns <- list(job = c("job chain", "job chains"),
                  worker = c("worker folder", "worker folders"),
                  checkpoint = c("worker checkpoint", "worker checkpoints"),
                  remote = c("remote-control session", "remote-control sessions"))
    what <- character()
    left <- 0L
    for (kind in names(nouns)) {
        if (any(found$kind == kind)) {
            what <- c(what, retention_count(units(gone, kind),
                    nouns[[kind]][[1L]], nouns[[kind]][[2L]]))
            left <- left + units(found, kind) - units(gone, kind)
        }
    }
    line <- sprintf("%s %s unchanged for %s or more (%.1f MB).", verb,
                    retention_and(what), retention_days(days),
                    sum(gone$bytes) / 1024 ^ 2)
    waiting <- unique(gone$session[gone$kind == "job" & gone$incomplete])
    if (length(waiting)) {
        line <- paste(line, sprintf("Incomplete and never resumed: %s.",
                                    paste(waiting, collapse = ", ")))
    }
    if (left > 0L) {
        line <- paste(line, sprintf("%d left in place.", left))
    }
    line
}

# "a", "a and b", "a, b and c".
retention_and <- function(x) {
    if (length(x) < 2L) {
        return(x)
    }
    paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and ")
}

retention_count <- function(n, noun, plural = paste0(noun, "s")) {
    if (n == 1L) {
        return(paste(n, noun))
    }
    sprintf("%d %s", as.integer(n), plural)
}

retention_days <- function(days) {
    if (days == 1) {
        return("1 day")
    }
    paste(format(days), "days")
}

# One row of what a scan found. `incomplete` marks a job chain that
# waited for its resume snapshot (R/retention-state.R).
retention_row <- function(agent, session, kind, path, last_use, idle, bytes,
                          incomplete = FALSE) {
    data.frame(agent = agent, session = session, kind = kind, path = path,
               last_use = last_use, idle_days = idle, bytes = bytes,
               incomplete = incomplete, stringsAsFactors = FALSE)
}

# Rows bound into one data frame, which has the columns even when empty.
retention_rows <- function(rows = list()) {
    if (!length(rows)) {
        return(retention_row(character(), character(), character(),
                             character(), as.POSIXct(character()), numeric(),
                             numeric(), logical()))
    }
    do.call(rbind, rows)
}

# Every journal and trace whose session has gone unused for `days`, in
# every agent's folder. Reads the directory and removes nothing.
retention_scan <- function(days, now = Sys.time()) {
    found <- retention_rows()
    root <- corteza_data_path("agents")
    if (days <= 0 || !dir.exists(root)) {
        return(found)
    }
    for (agent in list.files(root)) {
        dir <- file.path(root, agent, "sessions")
        # A folder reached through a link is not read at all.
        if (dir.exists(dir) && retention_within(dir)) {
            found <- rbind(found, retention_scan_dir(dir, agent, days, now))
        }
    }
    found
}

# One agent's sessions folder. A session is its id: `<id>.journal/`,
# `<id>_trace.jsonl`, `<id>.attachments/` and `<id>.jsonl` stand or fall
# together, so a trace is not removed from under a session whose
# transcript was written yesterday.
retention_scan_dir <- function(dir, agent, days, now) {
    names <- list.files(dir, all.files = TRUE, no.. = TRUE)
    ids <- unique(c(retention_ids(names, "[.]journal$"),
                    retention_ids(names, "_trace[.]jsonl$"),
                    retention_ids(names, "[.]attachments$")))
    rows <- list()
    for (id in ids) {
        used <- retention_last_use(dir, id)
        if (is.na(used)) {
            next
        }
        idle <- as.numeric(difftime(now, used, units = "days"))
        if (idle < days) {
            next
        }
        paths <- retention_paths(dir, id)
        for (kind in c("journal", "trace", "attachments")) {
            path <- paths[[kind]]
            if (!retention_removable(path, kind)) {
                next
            }
            rows[[length(rows) + 1L]] <- retention_row(agent, id, kind, path,
                used, idle, retention_bytes(path, kind))
        }
    }
    retention_rows(rows)
}

# The ids behind the names that end the way `suffix` says.
retention_ids <- function(names, suffix) {
    sub(suffix, "", grep(suffix, names, value = TRUE))
}

retention_paths <- function(dir, id) {
    list(journal = file.path(dir, paste0(id, ".journal")),
         trace = file.path(dir, paste0(id, "_trace.jsonl")),
         transcript = file.path(dir, paste0(id, ".jsonl")),
         attachments = file.path(dir, paste0(id, ".attachments")))
}

# When a session was last used: the newest change among its journal
# folder and its attachments folder, what lies directly in each, its
# trace and its transcript. NA when none of them exists.
retention_last_use <- function(dir, id) {
    p <- retention_paths(dir, id)
    paths <- unlist(p, use.names = FALSE)
    for (folder in c(p$journal, p$attachments)) {
        if (dir.exists(folder)) {
            inside <- list.files(folder, all.files = TRUE, no.. = TRUE,
                                 full.names = TRUE)
            paths <- c(paths, inside)
        }
    }
    times <- file.info(paths, extra_cols = FALSE)$mtime
    times <- times[!is.na(times)]
    if (!length(times)) {
        return(as.POSIXct(NA))
    }
    max(times)
}

# Only what corteza's own writers make, where they make it: a real
# folder with journal files in it, a real file for a trace, under the
# data directory. A symbolic link is left alone, whatever it points at,
# and so is everything reached through one.
retention_removable <- function(path, kind) {
    if (!retention_within(path)) {
        return(FALSE)
    }
    if (kind %in% c("journal", "attachments")) {
        own <- retention_own(path, kind)
        return(dir.exists(path) && length(c(own$files, own$dirs)) > 0L)
    }
    file.exists(path) && !dir.exists(path)
}

# TRUE when `path` lies under corteza's data directory with no link on
# the way: neither the path nor a folder between it and the data
# directory is one, wherever the link points. The data directory itself
# may be a link.
#
# Each part is tested 2 ways, since Windows hides a link from the first.
# Sys.readlink() names a link on Unix and gives "" for every path on
# Windows. The real path tells on both: a part that is no link resolves
# to its parent's real path and its own name, and a link resolves to its
# target, inside the data directory or out of it. So does "..".
#
# On Windows one path may hold both separators (tools::R_user_dir() and
# tempfile() give backslashes, dirname() and file.path() forward ones),
# and case does not matter, so the two are compared with forward slashes
# and, there, in any case.
retention_within <- function(path, root = corteza_data_dir()) {
    path <- retention_slashes(path)
    root <- retention_slashes(root)
    prefix <- paste0(root, "/")
    if (!retention_same_path(substr(path, 1L, nchar(prefix)), prefix)) {
        return(FALSE)
    }
    here <- root
    real <- normalizePath(root, winslash = "/", mustWork = FALSE)
    for (part in strsplit(substring(path, nchar(prefix) + 1L), "/",
                          fixed = TRUE)[[1L]]) {
        here <- file.path(here, part)
        real <- retention_step(here, real, part)
        if (is.null(real)) {
            return(FALSE)
        }
    }
    TRUE
}

# One step down, from a folder whose real path is `real` to its part
# `part` at `here`: the real path of `here`, or NULL when it is a link
# or resolves anywhere else.
retention_step <- function(here, real, part) {
    here <- .windows_io_path(here)
    if (retention_is_link(here)) {
        return(NULL)
    }
    real_here <- tryCatch(normalizePath(here, winslash = "/", mustWork = TRUE),
                          error = function(e) NULL)
    if (is.null(real_here)) return(NULL)
    if (!retention_same_path(real_here, paste0(real, "/", part))) {
        return(NULL)
    }
    real_here
}

retention_is_link <- function(path) {
    link <- Sys.readlink(path)
    is.na(link) || nzchar(link)
}

# A path with forward slashes alone on Windows, and as it is elsewhere.
retention_slashes <- function(path) {
    if (.Platform$OS.type == "windows") {
        path <- chartr("\\", "/", path)
        path <- sub("^//[?]/([A-Za-z]:/)", "\\1", path)
        return(sub("^//[?]/UNC/", "//", path, ignore.case = TRUE))
    }
    path
}

# Windows file names ignore case, so a real path there may differ from
# the expected one in case alone.
retention_same_path <- function(a, b) {
    if (.Platform$OS.type == "windows") {
        return(identical(tolower(retention_slashes(a)), tolower(retention_slashes(b))))
    }
    identical(a, b)
}

retention_bytes <- function(path, kind) {
    files <- path
    if (kind %in% c("journal", "attachments")) {
        files <- retention_own(path, kind)$files
    }
    info <- file.info(files, extra_cols = FALSE)
    sum(info$size[!info$isdir], na.rm = TRUE)
}

retention_stamp_path <- function() {
    corteza_data_path("retention.stamp")
}

# The daily run. Called where a terminal session starts and from the
# bot's step. Returns the line that says what went, or NULL when
# nothing did.
#
# The age of the stamp file says whether a run is due, so every process
# that shares the data directory shares one schedule. Stamped before the
# run: two processes that start together do not both scan, and a run
# that fails waits a day like any other. Never raises, since retention
# must not cost anyone a session. It makes no directory: with no data
# directory there is nothing to remove.
#
# `keep` names sessions the caller has open.
retention_tick <- function(keep = character(), now = Sys.time()) {
    tryCatch({
        if (!retention_look_due(now)) {
            return(invisible(NULL))
        }
        cfg <- retention_config()
        days <- cfg$session_days
        if (all(unlist(cfg[names(cfg) != "incomplete_days"]) == 0) || !dir.exists(corteza_data_dir())) {
            return(invisible(NULL))
        }
        if (!retention_run_due(now)) {
            return(invisible(NULL))
        }
        writeLines(format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   retention_stamp_path())
        found <- retention_prune(days, keep = keep, now = now, incomplete_days = cfg$incomplete_days)
        found <- rbind(found, retention_history(cfg, dry_run = FALSE, now = now, idle = found))
        unknown <- found$path[found$kind == "unknown"]
        if (length(unknown)) {
            log_event("retention_unknown", paths = unknown)
        }
        if (!any(found$removed)) {
            return(invisible(NULL))
        }
        log_event("retention", removed = sum(found$removed),
                  bytes = sum(found$bytes[found$removed]))
        invisible(retention_summary(found, days, dry_run = FALSE, config = cfg))
    }, error = function(e) {
        tryCatch(log_event("retention_failed", error = conditionMessage(e),
                           level = "warn"), error = function(e) NULL)
        invisible(NULL)
    })
}

# Has this process gone long enough without looking at the stamp file?
retention_look_due <- function(now) {
    looked <- .retention$looked
    if (!is.null(looked) &&
        abs(as.numeric(difftime(now, looked, units = "secs"))) <
        RETENTION_LOOK_SECS) {
        return(FALSE)
    }
    .retention$looked <- now
    TRUE
}

# Is the stamp file missing, a day old, or dated in the future (a clock
# that was wrong once must not put retention off for good)?
retention_run_due <- function(now) {
    last <- file.info(retention_stamp_path(), extra_cols = FALSE)$mtime
    if (is.na(last)) {
        return(TRUE)
    }
    age <- as.numeric(difftime(now, last, units = "secs"))
    age < 0 || age >= RETENTION_EVERY_SECS
}

# What a person resuming an old session is told. NULL unless the session
# went unused past the limit and neither its journal nor its trace is
# there. Read before the resume writes anything.
retention_resume_note <- function(id, agent_id = DEFAULT_AGENT_ID,
                                  now = Sys.time()) {
    tryCatch({
        days <- retention_config()$session_days
        p <- retention_paths(sessions_dir(agent_id), id)
        used <- file.info(p$transcript, extra_cols = FALSE)$mtime
        if (days <= 0 || is.na(used)) {
            return(NULL)
        }
        idle <- as.numeric(difftime(now, used, units = "days"))
        if (idle < days || file.exists(p$trace)) {
            return(NULL)
        }
        if (length(retention_journal_files(p$journal))) {
            return(NULL)
        }
        sprintf("Last used %d days ago. Its journal and tool trace were kept for %s and are gone; the conversation is intact.",
                as.integer(floor(idle)), retention_days(days))
    }, error = function(e) NULL)
}
