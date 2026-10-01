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
# ---- Protocol ---------------------------------------------------------
#
# A checkout's lock is a sequence of generations, gen-1, gen-2, ...,
# each a directory holding holder.json. The highest generation is the
# lock. It is free when it has a release marker (gen-<n>.released), or
# when its holder is stale: the job has ended or no longer exists, or the
# process that took it is gone.
#
# Acquiring writes holder.json into a private temp directory and renames
# that directory to gen-<n+1>, where n is the highest generation seen.
# A directory rename onto a name that already exists fails, so of any
# number of processes that saw generation n free, exactly one creates
# n+1; the rest see it held and back off. The holder is in place the
# moment the generation exists -- there is no window in which a lock
# is visible without one.
#
# Taking over a stale lock is the same operation. Nothing is deleted or
# moved aside to do it, so there is no check-then-delete for another
# process to interleave with.
#
# Releasing writes the release marker next to the releaser's own
# generation. Generation numbers are never reused, so a release that
# arrives late -- after its lock went stale and someone else took the
# next generation -- marks only its own old generation and cannot free
# the new holder's.
#
# Old generations are pruned once a newer one exists. The highest is
# never removed, which is what keeps numbers from being reused.
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

# Directory holding a checkout's generations.
job_lock_root <- function(checkout) {
    file.path(bot_signal_dir(), "locks",
              substr(digest::digest(checkout, algo = "sha256", serialize = FALSE),
                     1L, 24L))
}

job_lock_gen_dir <- function(root, n) {
    file.path(root, sprintf("gen-%d", as.integer(n)))
}

job_lock_gens <- function(root) {
    if (!dir.exists(root)) {
        return(integer())
    }
    names <- list.files(root, pattern = "^gen-[0-9]+$")
    names <- names[dir.exists(file.path(root, names))]
    sort(as.integer(sub("^gen-", "", names)))
}

# The highest generation and what it says, or NULL when there is none.
job_lock_current <- function(checkout) {
    root <- job_lock_root(checkout)
    gens <- job_lock_gens(root)
    if (!length(gens)) {
        return(NULL)
    }
    n <- max(gens)
    dir <- job_lock_gen_dir(root, n)
    list(gen = n, holder = job_read_file(file.path(dir, "holder.json")),
         released = file.exists(paste0(dir, ".released")))
}

job_lock_free <- function(current) {
    is.null(current) || isTRUE(current$released) ||
    job_lock_stale(current$holder)
}

# Who holds the checkout, or NULL when it is free.
job_lock_holder <- function(checkout) {
    cur <- job_lock_current(checkout)
    if (job_lock_free(cur)) {
        NULL
    } else {
        cur$holder
    }
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

# Try to create generation `n` with `holder`. TRUE when this call
# created it; FALSE when it already existed. The single atomic step the
# protocol rests on.
job_lock_try <- function(checkout, n, holder) {
    root <- job_lock_root(checkout)
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile("acquire-", tmpdir = root)
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    job_write_file(file.path(tmp, "holder.json"), holder)
    isTRUE(suppressWarnings(file.rename(tmp, job_lock_gen_dir(root, n))))
}

# Remove generations below `n`. The highest is kept so numbers are never
# reused.
job_lock_prune <- function(checkout, n) {
    root <- job_lock_root(checkout)
    for (k in job_lock_gens(root)) {
        if (k < n) {
            unlink(job_lock_gen_dir(root, k), recursive = TRUE)
        }
    }
    # Markers too, including any a late release left for a generation
    # already pruned.
    markers <- list.files(root, pattern = "^gen-[0-9]+\\.released$")
    old <- as.integer(sub("^gen-([0-9]+)\\.released$", "\\1", markers)) < n
    unlink(file.path(root, markers[old]))
    invisible(TRUE)
}

# Take the checkout's lock for `job_id`. Returns list(ok = TRUE) when
# held (including when this job already holds it), else
# list(ok = FALSE, holder = <who has it>).
job_lock_acquire <- function(checkout, job_id, owner = "local") {
    for (attempt in 1:5) {
        cur <- job_lock_current(checkout)
        if (!job_lock_free(cur)) {
            if (identical(cur$holder$job, job_id)) {
                return(list(ok = TRUE))
            }
            return(list(ok = FALSE, holder = cur$holder))
        }
        n <- (cur$gen %||% 0L) + 1L
        holder <- list(checkout = checkout, job = job_id, owner = owner,
                       pid = Sys.getpid(), host = Sys.info()[["nodename"]],
                       gen = n, acquired_at = job_now())
        if (job_lock_try(checkout, n, holder)) {
            job_lock_prune(checkout, n)
            return(list(ok = TRUE))
        }
        # Someone else created generation n first. Look again: it is
        # either held now, or (rarely) already free again.
    }
    list(ok = FALSE, holder = job_lock_holder(checkout))
}

# Give the lock back: mark every generation `job_id` holds as released.
# Only ever touches the releaser's own generations.
job_lock_release <- function(checkout, job_id) {
    root <- job_lock_root(checkout)
    released <- FALSE
    for (k in job_lock_gens(root)) {
        dir <- job_lock_gen_dir(root, k)
        h <- job_read_file(file.path(dir, "holder.json"))
        if (identical(h$job, job_id)) {
            file.create(paste0(dir, ".released"))
            released <- TRUE
        }
    }
    invisible(released)
}

# What a surface says about a "blocked" event.
job_blocked_text <- function(ev) {
    h <- ev$holder
    sprintf(paste("Job %s is queued: job %s (%s) is editing %s.",
                  "It starts when that job ends."),
            ev$job$id, h$job %||% "?", h$owner %||% "unknown owner",
            h$checkout %||% ev$job$workspace)
}
