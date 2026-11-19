# Retention, part 2: what corteza removes from its state stores, and
# when. R/retention.R has part 1, a session's journal and trace, and the
# daily run that calls both.
#
# Three stores, each judged against its own root with part 1's rules for
# links: nothing reached through a link is read or removed, and a folder
# that holds a link stays.
#
# - Job chains, under job_root(). A chain is the jobs linked to one
#   another by `resume_of`, `review_of` or `parent`. It goes once every
#   job in it has ended and nothing in it has changed for `days`. A
#   chain with an incomplete job that nothing resumed holds that job's
#   resume snapshot, so it waits `incomplete_days` instead, never fewer
#   than `days`; 0 keeps it for good. An open job keeps its whole chain.
#   Audit records in a job's folder stay (a consultation keeps its own
#   there).
# - Worker folders, under <state>/workers/, with no checkpoint: a
#   monitor's, which keeps only its journal, or a doer's that never ended
#   a job. The journal goes as part 1 removes a session's, its audit
#   records staying. A folder with a checkpoint is left alone for now.
# - Remote-control sessions, under <remote>/sessions/, that were closed
#   or whose process on this host has ended.
#
# A folder is judged only when it reads as corteza's: a name of the form
# corteza gives it, and for a job or a remote session a record that
# names it. Anything else stays whole, and one as old as the limit is
# listed with kind "unknown", so a person can look.
#
# A removal takes first the record that makes a folder live: a job's
# intent, a remote session's request. One that stops partway leaves no
# job that reads as queued and no session that reads as open.
#
# Not here: checkout lock generations (R/job-lock.R rests on a number,
# once taken, staying taken) and the bot's room state.

RETENTION_STATE_KINDS <- c("job", "worker", "remote")

# What a worker writes in its folder (R/job-worker.R), and the temporary
# files those writes leave when the process ends before the rename:
# job_write_file()'s, the history's and the workspace's.
RETENTION_WORKER_FILES <- c("checkpoint.json", "workspace.RData",
                            "history.rds", "journal")
RETENTION_WORKER_TEMP <- paste0("^(checkpoint[.]json[.]|history[.])[0-9a-f]+$",
                                "|^workspace[.]RData[.]tmp-[0-9]+$")

# The state stores' rows: what `days` and `incomplete_days` let go, and
# the old folders that do not read as corteza's.
retention_state_scan <- function(days, incomplete_days, now = Sys.time()) {
    if (days <= 0) {
        return(retention_rows())
    }
    rbind(retention_job_rows(days, incomplete_days, now),
          retention_worker_rows(days, now), retention_remote_rows(days, now))
}

# The root a state row's path is judged against.
retention_state_root <- function(kind) {
    if (kind == "remote") {
        return(file.path(.remote_root(), "sessions"))
    }
    if (kind == "worker") {
        return(file.path(bot_signal_dir(), "workers"))
    }
    job_root()
}

# The folders in a store, each with its real path. A store reached
# through a link is not read; one that is a link of its own is skipped.
retention_store_dirs <- function(root, top) {
    if (!dir.exists(root) || !retention_within(root, top)) {
        return(list())
    }
    real <- normalizePath(root, winslash = "/", mustWork = FALSE)
    out <- list()
    for (name in list.files(root, all.files = TRUE, no.. = TRUE)) {
        dir <- file.path(root, name)
        dir_real <- retention_step(dir, real, name)
        if (!is.null(dir_real) && dir.exists(dir)) {
            out[[length(out) + 1L]] <- list(name = name, dir = dir,
                real = dir_real)
        }
    }
    out
}

# Does `name` have the form job_new_id() gives a job and a remote
# session?
retention_is_id <- function(name) {
    isTRUE(tryCatch(identical(job_check_id(name), name),
                    error = function(e) FALSE))
}

# The newest change in a folder: the folder itself and everything in it
# that is no link and lies behind none. NA when none can be read.
retention_newest <- function(dir, real) {
    tree <- retention_tree(dir, real)
    times <- file.info(c(dir, tree$files, tree$dirs), extra_cols = FALSE)$mtime
    times <- times[!is.na(times)]
    if (!length(times)) {
        return(as.POSIXct(NA))
    }
    max(times)
}

retention_idle <- function(now, newest) {
    as.numeric(difftime(now, newest, units = "days"))
}

retention_size <- function(files) {
    info <- file.info(files, extra_cols = FALSE)
    sum(info$size[!info$isdir], na.rm = TRUE)
}

# A folder that does not read as corteza's, listed once it is as old as
# the limit. Nothing in it is removed.
retention_unknown_row <- function(d, days, now) {
    newest <- retention_newest(d$dir, d$real)
    idle <- retention_idle(now, newest)
    if (is.na(idle) || idle < days) {
        return(NULL)
    }
    retention_row(NA_character_, d$name, "unknown", d$dir, newest, idle,
                  NA_real_)
}

# ---- Job chains ----

retention_job_rows <- function(days, incomplete_days, now) {
    jobs <- list()
    rows <- list()
    for (d in retention_store_dirs(job_root(), bot_signal_dir())) {
        job <- retention_job_read(d)
        if (is.null(job)) {
            rows <- c(rows, list(retention_unknown_row(d, days, now)))
        } else if (is.list(job)) {
            jobs[[d$name]] <- job
        }
    }
    chain <- retention_chains(names(jobs), lapply(jobs, `[[`, "links"))
    for (g in unique(chain)) {
        rows <- c(rows, retention_chain_rows(jobs[chain == g], days,
                incomplete_days, now))
    }
    retention_rows(Filter(Negate(is.null), rows))
}

# A job folder's record: the ids it names, whether and how it ended, its
# newest change, and the size of what may go. NULL for a folder that
# does not read as a job's. TRUE for one that holds nothing but the
# audit records an earlier removal left, which stay as they are.
retention_job_read <- function(d) {
    if (!retention_is_id(d$name)) {
        return(NULL)
    }
    entries <- list.files(d$dir, all.files = TRUE, no.. = TRUE)
    if (identical(entries, "audit")) {
        return(TRUE)
    }
    intent <- tryCatch(job_read_file(file.path(d$dir, "intent.json")),
                       error = function(e) NULL)
    if (!is.list(intent) || !identical(intent$id, d$name)) {
        return(NULL)
    }
    # An outcome that cannot be read counts as none: the job stays open.
    outcome <- tryCatch(job_read_file(file.path(d$dir, "outcome.json")),
                        error = function(e) NULL)
    links <- unlist(intent[c("resume_of", "review_of", "parent")],
                    use.names = FALSE)
    links <- Filter(retention_is_id, as.list(as.character(links)))
    own <- retention_tree(d$dir, d$real, setdiff(entries, "audit"))
    list(id = d$name, dir = d$dir, links = unlist(links),
         resume_of = unlist(Filter(retention_is_id, list(intent$resume_of))),
         ended = is.list(outcome),
         incomplete = is.list(outcome) &&
         identical(outcome$status, "incomplete"),
         newest = retention_newest(d$dir, d$real),
         bytes = retention_size(own$files))
}

# Group jobs into chains: each job's number is that of the first job of
# its chain, by the links among them. A link to a job not in `ids` joins
# nothing.
retention_chains <- function(ids, links) {
    group <- seq_along(ids)
    find <- function(i) {
        while (group[[i]] != i) {
            i <- group[[i]]
        }
        i
    }
    for (i in seq_along(ids)) {
        for (j in stats::na.omit(match(links[[i]], ids))) {
            a <- find(i)
            b <- find(j)
            group[[max(a, b)]] <- min(a, b)
        }
    }
    vapply(seq_along(ids), find, integer(1))
}

# One row per job folder of a chain that may go, each naming the chain
# by its newest job. None while a job of it is open, or while it is
# younger than its limit.
retention_chain_rows <- function(chain, days, incomplete_days, now) {
    if (!all(vapply(chain, `[[`, logical(1), "ended"))) {
        return(list())
    }
    resumed <- unlist(lapply(chain, `[[`, "resume_of"))
    waiting <- vapply(chain, function(j) {
        j$incomplete && !j$id %in% resumed
    }, logical(1))
    limit <- days
    if (any(waiting)) {
        if (incomplete_days <= 0) {
            return(list())
        }
        limit <- max(days, incomplete_days)
    }
    newest <- max(do.call(c, lapply(chain, `[[`, "newest")))
    idle <- retention_idle(now, newest)
    if (is.na(idle) || idle < limit) {
        return(list())
    }
    id <- max(names(chain))
    lapply(chain, function(j) {
        retention_row(NA_character_, id, "job", j$dir, newest, idle,
                      j$bytes, incomplete = any(waiting))
    })
}

# ---- Worker folders ----

retention_worker_rows <- function(days, now) {
    rows <- list()
    for (d in retention_store_dirs(retention_state_root("worker"),
                                   bot_signal_dir())) {
        entries <- list.files(d$dir, all.files = TRUE, no.. = TRUE)
        ours <- grepl("^[0-9a-f]{24}$", d$name) &&
        all(entries %in% RETENTION_WORKER_FILES |
            grepl(RETENTION_WORKER_TEMP, entries))
        if (!ours) {
            rows <- c(rows, list(retention_unknown_row(d, days, now)))
            next
        }
        if ("checkpoint.json" %in% entries) {
            next
        }
        newest <- retention_newest(d$dir, d$real)
        idle <- retention_idle(now, newest)
        own <- retention_state_own(d$dir, d$real, "worker")
        if (is.na(idle) || idle < days ||
            (length(entries) && !length(c(own$files, own$dirs)))) {
            next
        }
        rows <- c(rows, list(retention_row(NA_character_, d$name, "worker",
                    d$dir, newest, idle,
                    retention_size(own$files))))
    }
    retention_rows(Filter(Negate(is.null), rows))
}

# ---- Remote-control sessions ----

retention_remote_rows <- function(days, now) {
    rows <- list()
    for (d in retention_store_dirs(retention_state_root("remote"),
                                   .remote_root())) {
        request <- NULL
        if (retention_is_id(d$name)) {
            request <- tryCatch(job_read_file(file.path(d$dir, "request.json")),
                                error = function(e) NULL)
        }
        if (!is.list(request) || !identical(request$id, d$name)) {
            rows <- c(rows, list(retention_unknown_row(d, days, now)))
            next
        }
        newest <- retention_newest(d$dir, d$real)
        idle <- retention_idle(now, newest)
        if (is.na(idle) || idle < days || !retention_remote_ended(d$dir,
                request)) {
            next
        }
        own <- retention_state_own(d$dir, d$real, "remote")
        rows <- c(rows, list(retention_row(NA_character_, d$name, "remote",
                    d$dir, newest, idle,
                    retention_size(own$files))))
    }
    retention_rows(Filter(Negate(is.null), rows))
}

# A remote-control session is over when it was closed, or when its record
# names this host and a process id and that process is gone. Anything
# else may still be in use, however old: a record from another host, one
# with no process id, and on Windows, where the check cannot be made,
# every session that was not closed.
retention_remote_ended <- function(dir, request) {
    if (file.exists(file.path(dir, "closed.json"))) {
        return(TRUE)
    }
    pid <- request$pid
    is.numeric(pid) && length(pid) == 1L && is.finite(pid) && pid > 0 &&
    identical(request$host, Sys.info()[["nodename"]]) &&
    isTRUE(job_lock_process_gone(request))
}

# ---- Removal ----

# What may go of a state folder, as part 1's retention_tree() lists it:
# for a job or a remote session everything but a job's audit records,
# with `first` the record to take before the rest; for a worker only the
# files a worker writes and its journal's own files, with `after` the
# journal folder to remove once empty.
retention_state_own <- function(path, real, kind) {
    entries <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (kind != "worker") {
        own <- retention_tree(path, real, setdiff(entries, "audit"))
        if (kind == "job") {
            first <- "intent.json"
        } else {
            first <- "request.json"
        }
        own$first <- file.path(path, first)
        return(own)
    }
    top <- entries[entries %in% c("workspace.RData", "history.rds") |
        grepl(RETENTION_WORKER_TEMP, entries)]
    own <- retention_tree(path, real, top)
    own$first <- character()
    own$after <- character()
    journal <- file.path(path, "journal")
    journal_real <- retention_step(journal, real, "journal")
    if (dir.exists(journal) && !is.null(journal_real)) {
        inside <- retention_tree(journal, journal_real,
                                 basename(retention_journal_files(journal)))
        own$files <- c(own$files, inside$files)
        own$dirs <- c(own$dirs, inside$dirs)
        own$after <- journal
    }
    own
}

# Remove what a state row names. TRUE when nothing that was to go is
# left. A worker that has written a checkpoint since the scan is left.
retention_state_remove <- function(path, kind) {
    if (!retention_within(path, dirname(retention_state_root(kind))) ||
        (kind == "worker" && file.exists(file.path(path, "checkpoint.json")))) {
        return(FALSE)
    }
    real <- normalizePath(path, winslash = "/", mustWork = FALSE)
    own <- retention_state_own(path, real, kind)
    # A job whose intent stays while its outcome goes would read as
    # queued, and run again.
    first <- intersect(own$first, own$files)
    if (length(first) && !retention_unlink(first)) {
        return(FALSE)
    }
    retention_unlink(setdiff(own$files, first))
    for (dir in c(own$dirs, own$after)) {
        retention_rmdir(dir)
    }
    retention_rmdir(path)
    if (!length(own$files)) {
        return(!dir.exists(path))
    }
    !any(file.exists(own$files))
}

# Remove files, each a name and never a pattern. TRUE when none is left.
retention_unlink <- function(paths) {
    unlink(paths, expand = FALSE)
    !any(file.exists(paths))
}

# Has a state folder changed since the scan? The newest change of each
# of its folders (all of a chain's) is read again just before they go.
retention_state_moved <- function(paths, last_use) {
    newest <- vapply(paths, function(p) {
        as.numeric(retention_newest(p, normalizePath(p, winslash = "/",
                    mustWork = FALSE)))
    }, numeric(1))
    !identical(max(newest), as.numeric(last_use))
}
