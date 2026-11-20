# Retention: what corteza removes from its own data directory, and when.
#
# CRAN lets a package keep files under tools::R_user_dir() on condition
# that they stay small and are "actively managed (including removing
# outdated material)". Up to 0.8.0 corteza removed nothing. A session's
# journal (R/journal.R) keeps every history a rewrite replaced, and it
# stayed on disk however long ago the session was last used.
#
# What goes: the journal and the tool trace of a session nobody has used
# for `session_days`. What stays: every transcript, the session stores,
# workspaces, config, audit records and crypto state. A session whose
# journal is gone still opens from its transcript. A room that restores
# its context from its journal starts fresh.
#
# A session's audit records are kept inside its journal folder
# (`<id>.journal/audit/`, R/audit-stream.R) and have their own expiry,
# which never runs by itself. So a journal is removed file by file
# (retention_journal_files()), and its folder goes only once empty.
#
# Job records, worker checkpoints and remote-control sessions are not
# handled here.
#
# The setting is read from the user's config file alone
# (retention_config()), like the supervisor's. A project's config sits
# in a directory an agent writes in, and must not decide how long the
# user's records are kept.

RETENTION_SESSION_DAYS <- 90
RETENTION_EVERY_SECS <- 24 * 60 * 60
# A bot calls retention_tick() on every poll. It looks at the stamp file
# this often and no more.
RETENTION_LOOK_SECS <- 60 * 60

.retention <- new.env(parent = emptyenv())

# `retention` from the user's config. What corteza cannot read turns
# retention off: a config file that does not parse, or a `session_days`
# that is not one non-negative number, must not be taken for a shorter
# time than the person meant. load_config_file() reads a file it cannot
# parse as an empty one, which here would mean 90 days, so the file is
# parsed again.
retention_config <- function() {
    path <- corteza_config_path("config.json")
    if (!file.exists(path)) {
        return(list(session_days = RETENTION_SESSION_DAYS))
    }
    cfg <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE,
                                       simplifyDataFrame = FALSE)$retention,
                    error = function(e) NA)
    if (is.null(cfg)) {
        return(list(session_days = RETENTION_SESSION_DAYS))
    }
    days <- NA
    if (is.list(cfg)) {
        days <- cfg$session_days %||% RETENTION_SESSION_DAYS
    }
    if (!is.numeric(days) || length(days) != 1L || is.na(days) || days < 0) {
        log_event("retention_config_invalid", level = "warn")
        days <- 0
    }
    list(session_days = as.numeric(days))
}

#' Remove what old sessions no longer need
#'
#' corteza keeps its sessions under
#' \code{tools::R_user_dir("corteza", "data")}. For every session that
#' has not been used for \code{days}, this removes its journal (in
#' \code{<id>.journal/}: the provider-native record of the live history
#' and of every history a compaction replaced) and its tool trace
#' (\code{<id>_trace.jsonl}).
#'
#' Transcripts, session stores, workspaces, config, audit records and
#' crypto state are never removed. A session whose journal is gone still
#' resumes from its transcript. A chat room that restores its context
#' from its journal starts its next conversation fresh.
#'
#' A session's audit records are kept in its journal folder, under
#' \code{audit/}. They stay, and so does the folder that holds them:
#' see \code{\link{audit_payload_expire}}.
#'
#' A session was last used when its journal, its trace or its transcript
#' last changed, whichever is newest. A symbolic link is never removed.
#'
#' corteza runs this itself at most once a day: when a terminal session
#' starts, and while a chat bot runs. The session a terminal has open is
#' left alone. The number of days is \code{retention$session_days} in
#' the user's config file (default 90; 0 keeps everything). A project's
#' config cannot change it.
#'
#' @param dry_run If \code{TRUE}, remove nothing and report what would
#'   go.
#' @param days Sessions unused for at least this many days lose their
#'   journal and trace. Default: \code{retention$session_days} from the
#'   user's config. 0 removes nothing.
#'
#' @return A data frame with one row per journal or trace, invisibly:
#'   \code{agent}, \code{session}, \code{kind}, \code{path},
#'   \code{last_use}, \code{idle_days}, \code{bytes} and \code{removed}.
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
    if (is.null(days)) {
        days <- retention_config()$session_days
    } else if (!is.numeric(days) || length(days) != 1L || is.na(days) ||
        days < 0) {
        stop("`days` must be one non-negative number.", call. = FALSE)
    }
    found <- retention_prune(days, dry_run = dry_run)
    message(retention_summary(found, days, dry_run))
    invisible(found)
}

# Find what `days` lets go and, unless `dry_run`, remove it. `keep`
# names sessions to leave alone whatever their age.
retention_prune <- function(days, dry_run = FALSE, keep = character(),
                            now = Sys.time()) {
    found <- retention_scan(days, now = now)
    found <- found[!found$session %in% keep,, drop = FALSE]
    found$removed <- rep(FALSE, nrow(found))
    if (dry_run) {
        return(found)
    }
    # By session: a journal and its trace go together or not at all.
    for (rows in split(seq_len(nrow(found)),
                       paste(found$agent, found$session, sep = "/"))) {
        first <- rows[[1L]]
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
    files <- path
    if (kind == "journal") {
        files <- retention_journal_files(path)
    }
    # expand = FALSE: a name is a name, never a pattern.
    unlink(files, recursive = TRUE, expand = FALSE)
    if (kind == "journal") {
        retention_rmdir(path)
    }
    !any(file.exists(files))
}

# What the journal's writer makes in a journal folder (R/journal.R): the
# live snapshot and the segments, each with the copy a replacement parks
# and the temp file it is written through, and the folder of shared
# entries. Anything else there is someone else's, the audit records
# first of all.
retention_journal_files <- function(journal) {
    names <- list.files(journal, all.files = TRUE, no.. = TRUE)
    own <- grepl("^(live|segment-[0-9]+)[.]json([.]tmp|[.]prev)?$", names) |
    names == "entries"
    file.path(journal, names[own])
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
retention_summary <- function(found, days, dry_run) {
    if (days <= 0) {
        return("Retention is off: nothing is removed.")
    }
    if (!nrow(found)) {
        return(sprintf("No session has gone unused for %s.",
                       retention_days(days)))
    }
    gone <- found
    verb <- "Would remove"
    if (!dry_run) {
        gone <- found[found$removed,, drop = FALSE]
        verb <- "Removed"
    }
    line <- sprintf("%s %s and %s of sessions unused for %s or more (%.1f MB).",
                    verb,
                    retention_count(sum(gone$kind == "journal"), "journal"),
                    retention_count(sum(gone$kind == "trace"), "trace"),
                    retention_days(days), sum(gone$bytes) / 1024 ^ 2)
    left <- nrow(found) - nrow(gone)
    if (left > 0L) {
        line <- paste(line, sprintf("%d left in place.", left))
    }
    line
}

retention_count <- function(n, noun) {
    if (n == 1L) {
        return(paste(n, noun))
    }
    sprintf("%d %ss", as.integer(n), noun)
}

retention_days <- function(days) {
    if (days == 1) {
        return("1 day")
    }
    paste(format(days), "days")
}

# Every journal and trace whose session has gone unused for `days`, in
# every agent's folder. Reads the directory and removes nothing.
retention_scan <- function(days, now = Sys.time()) {
    found <- data.frame(agent = character(), session = character(),
                        kind = character(), path = character(),
                        last_use = as.POSIXct(character()),
                        idle_days = numeric(), bytes = numeric(),
                        stringsAsFactors = FALSE)
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
# `<id>_trace.jsonl` and `<id>.jsonl` stand or fall together, so a trace
# is not removed from under a session whose transcript was written
# yesterday.
retention_scan_dir <- function(dir, agent, days, now) {
    names <- list.files(dir, all.files = TRUE, no.. = TRUE)
    ids <- unique(c(retention_ids(names, "[.]journal$"),
                    retention_ids(names, "_trace[.]jsonl$")))
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
        for (kind in c("journal", "trace")) {
            path <- paths[[kind]]
            if (!retention_removable(path, kind)) {
                next
            }
            rows[[length(rows) + 1L]] <- data.frame(agent = agent,
                session = id, kind = kind, path = path, last_use = used,
                idle_days = idle, bytes = retention_bytes(path, kind),
                stringsAsFactors = FALSE)
        }
    }
    do.call(rbind, rows)
}

# The ids behind the names that end the way `suffix` says.
retention_ids <- function(names, suffix) {
    sub(suffix, "", grep(suffix, names, value = TRUE))
}

retention_paths <- function(dir, id) {
    list(journal = file.path(dir, paste0(id, ".journal")),
         trace = file.path(dir, paste0(id, "_trace.jsonl")),
         transcript = file.path(dir, paste0(id, ".jsonl")))
}

# When a session was last used: the newest change among its journal
# folder, what lies directly in it, its trace and its transcript. NA
# when none of them exists.
retention_last_use <- function(dir, id) {
    p <- retention_paths(dir, id)
    paths <- unlist(p, use.names = FALSE)
    if (dir.exists(p$journal)) {
        paths <- c(paths, list.files(p$journal, all.files = TRUE,
                                     no.. = TRUE, full.names = TRUE))
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
    if (kind == "journal") {
        return(dir.exists(path) && length(retention_journal_files(path)) > 0L)
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
retention_within <- function(path, root = corteza_data_dir()) {
    prefix <- paste0(root, "/")
    if (!startsWith(path, prefix)) {
        return(FALSE)
    }
    here <- root
    real <- normalizePath(root, winslash = "/", mustWork = FALSE)
    for (part in strsplit(substring(path, nchar(prefix) + 1L), "/",
                          fixed = TRUE)[[1L]]) {
        here <- file.path(here, part)
        if (retention_is_link(here)) {
            return(FALSE)
        }
        real_here <- normalizePath(here, winslash = "/", mustWork = FALSE)
        if (!retention_same_path(real_here, paste0(real, "/", part))) {
            return(FALSE)
        }
        real <- real_here
    }
    TRUE
}

retention_is_link <- function(path) {
    link <- Sys.readlink(path)
    is.na(link) || nzchar(link)
}

# Windows file names ignore case, so a real path there may differ from
# the expected one in case alone.
retention_same_path <- function(a, b) {
    if (.Platform$OS.type == "windows") {
        return(identical(tolower(a), tolower(b)))
    }
    identical(a, b)
}

retention_bytes <- function(path, kind) {
    files <- path
    if (kind == "journal") {
        own <- retention_journal_files(path)
        files <- c(own, list.files(own[dir.exists(own)], all.files = TRUE,
                                   no.. = TRUE, full.names = TRUE,
                                   recursive = TRUE))
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
        days <- retention_config()$session_days
        if (days <= 0 || !dir.exists(corteza_data_dir())) {
            return(invisible(NULL))
        }
        if (!retention_run_due(now)) {
            return(invisible(NULL))
        }
        writeLines(format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   retention_stamp_path())
        found <- retention_prune(days, keep = keep, now = now)
        if (!any(found$removed)) {
            return(invisible(NULL))
        }
        log_event("retention", removed = sum(found$removed),
                  bytes = sum(found$bytes[found$removed]))
        invisible(retention_summary(found, days, dry_run = FALSE))
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
