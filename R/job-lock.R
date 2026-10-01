# One writer per checkout.
#
# Two jobs editing one working tree at once corrupt each other's work,
# and that holds within one bot as much as between two. A job whose role
# writes takes the checkout's lock when it is dispatched and gives it
# back when it ends; a second writer for the same checkout stays queued
# until then, and conversation carries on meanwhile.
#
# The checkout is the git toplevel when the workspace is inside a
# repository, so jobs in two subdirectories of one repo still exclude
# each other. Locks live in the shared state directory, so every bot on
# the machine sees the same ones.
#
# The lock is a directory: dir.create() fails when it already exists,
# which makes taking it atomic without a lock library. holder.json
# inside says who holds it.
#
# A lock is stale, and may be taken over, when the job holding it has
# ended or no longer exists, or when the process that owns it is gone.
# The job-ended check is the backstop for every path that forgets to
# release; the process check covers a bot that died and has not
# restarted to settle its jobs.
#
# Only jobs take this lock. A tmux session, an editor, or a corteza
# session editing directly does not, and nothing here can stop it.

JOB_WRITER_ROLES <- c("doer")

job_role_writes <- function(role) {
    role %in% JOB_WRITER_ROLES
}

# The checkout a workspace belongs to.
job_checkout <- function(workspace) {
    workspace <- normalizePath(workspace, mustWork = FALSE)
    top <- tryCatch(suppressWarnings(system2("git",
                c("-C", shQuote(workspace), "rev-parse", "--show-toplevel"),
                stdout = TRUE, stderr = FALSE)),
                    error = function(e) character())
    if (length(top) == 1L && nzchar(top) && is.null(attr(top, "status"))) {
        return(normalizePath(top, mustWork = FALSE))
    }
    workspace
}

job_lock_dir <- function(checkout) {
    file.path(bot_signal_dir(), "locks",
              paste0(substr(digest::digest(checkout, algo = "sha256",
                    serialize = FALSE), 1L, 16L),
                     ".lock"))
}

job_lock_holder <- function(checkout) {
    job_read_file(file.path(job_lock_dir(checkout), "holder.json"))
}

# Is the process that took the lock gone? Only answerable for a process
# on this host, and only where signal 0 can be sent; anywhere else the
# holder is presumed alive and the job-ended check decides.
job_lock_process_gone <- function(holder) {
    if (.Platform$OS.type == "windows" ||
        !identical(holder$host, Sys.info()[["nodename"]]) ||
        is.null(holder$pid)) {
        return(FALSE)
    }
    !isTRUE(tools::pskill(as.integer(holder$pid), signal = 0L))
}

job_lock_stale <- function(holder) {
    if (is.null(holder)) {
        return(TRUE)
    }
    j <- tryCatch(job_read(holder$job), error = function(e) NULL)
    is.null(j) || j$status %in% JOB_STATUSES_FINAL ||
    job_lock_process_gone(holder)
}

# Take the checkout's lock for `job_id`. Returns list(ok = TRUE) when
# held (including when this job already holds it), else
# list(ok = FALSE, holder = <who has it>).
job_lock_acquire <- function(checkout, job_id, owner = "local") {
    dir <- job_lock_dir(checkout)
    dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
    holder_new <- list(checkout = checkout, job = job_id, owner = owner,
                       pid = Sys.getpid(), host = Sys.info()[["nodename"]],
                       acquired_at = job_now())
    for (attempt in 1:2) {
        if (dir.create(dir, showWarnings = FALSE)) {
            job_write_file(file.path(dir, "holder.json"), holder_new)
            return(list(ok = TRUE))
        }
        holder <- job_lock_holder(checkout)
        if (identical(holder$job, job_id)) {
            return(list(ok = TRUE))
        }
        if (attempt == 2L || !job_lock_stale(holder)) {
            return(list(ok = FALSE, holder = holder))
        }
        # Take over a stale lock. Rename it aside first: of several
        # processes judging the same lock stale, only one rename
        # succeeds. The renamed copy is checked against what was judged
        # stale, so a fresh lock someone took in between is put back
        # rather than discarded.
        aside <- paste0(dir, ".stale-", Sys.getpid(), "-",
                        format(Sys.time(), "%H%M%OS3"))
        if (isTRUE(file.rename(dir, aside))) {
            moved <- job_read_file(file.path(aside, "holder.json"))
            if (!identical(moved$job, holder$job) ||
                !identical(moved$acquired_at, holder$acquired_at)) {
                file.rename(aside, dir)
                return(list(ok = FALSE, holder = moved))
            }
            unlink(aside, recursive = TRUE)
        }
    }
    list(ok = FALSE, holder = job_lock_holder(checkout))
}

# What a surface says about a "blocked" event.
job_blocked_text <- function(ev) {
    h <- ev$holder
    sprintf(paste("Job %s is queued: job %s (%s) is editing %s.",
                  "It starts when that job ends."),
            ev$job$id, h$job %||% "?", h$owner %||% "unknown owner",
            h$checkout %||% ev$job$workspace)
}

# Give the lock back, only if `job_id` holds it.
job_lock_release <- function(checkout, job_id) {
    holder <- job_lock_holder(checkout)
    if (!identical(holder$job, job_id)) {
        return(invisible(FALSE))
    }
    unlink(job_lock_dir(checkout), recursive = TRUE)
    invisible(TRUE)
}
