# Review of a doer's work by a second worker.
#
# A job submitted with review = TRUE is followed, when it ends `done`,
# by a review job in the same session. The reviewer is its own worker
# process with its own conversation, read-only tools, and no way to run
# code. It reads the diff and the doer's report and answers with a
# verdict and findings.
#
# The checkout stays locked from the moment the doer starts until the
# review ends. The doer's lock is handed to the review job
# (job_lock_transfer()) before the doer's job is settled, so there is no
# moment in between when a queued writer -- this bot's or another's --
# could take the checkout and change what is being reviewed.
#
# That holds while the owning process lives. If it dies with a review
# queued, the lock goes stale and another job can take the checkout. A
# review is therefore checked again when it starts
# (job_review_unbroken()) and cancelled, not run, if any other job held
# the checkout since the doer.
#
# What the reviewer cannot do is run the tests. Running tests executes
# arbitrary code and writes fixtures, snapshots, and build output, so a
# worker that can run them is not read-only, and taking away its edit
# tools would only hide that. The reviewer checks the doer's claims
# against the code and says what it could not verify. Independent test
# runs need an isolated copy of the checkout, which is not built yet.
#
# The review does not start a revision on its own. Its report goes back
# to the talker and the person; what to do about it is their call. The
# lock is released when the review ends.

# Look, do not touch: no bash, no run_r, no write tools, no network.
JOB_REVIEWER_TOOLS <- c("read_file", "skill_instructions", "grep_files",
                        "list_files", "git_status", "git_diff", "git_log",
                        "r_help")

JOB_REVIEWER_SYSTEM <- paste(
                             "You are the reviewer for work a doer agent has just finished in this",
                             "checkout. Each message is one review. You are read-only: you can read",
                             "files and git history and nothing else, and you cannot run code or",
                             "tests. The checkout is locked while you review, so what you read will",
                             "not change under you.",
                             "",
                             "Judge the change against the task it was given. Check what the doer's",
                             "report claims against the code itself. Where a claim rests on a test",
                             "run you cannot repeat, say so instead of taking it as shown. Look for",
                             "wrong behavior first, then missing cases, then anything the task asked",
                             "for that is not there. Leave style alone unless it hides a defect.",
                             sep = "\n")

# Untracked files are copied into the snapshot only up to this many
# bytes in total. `jobs$review_snapshot_max_mb` overrides it.
JOB_SNAPSHOT_MAX_BYTES <- 20 * 1024 ^ 2

job_snapshot_max_bytes <- function(session) {
    mb <- suppressWarnings(as.numeric(
                                      session$config$jobs$review_snapshot_max_mb)[1L])
    if (length(mb) == 1L && !is.na(mb) && mb >= 0) {
        mb * 1024 ^ 2
    } else {
        JOB_SNAPSHOT_MAX_BYTES
    }
}

# A commit holding the checkout's files as they are right now: tracked
# files with their uncommitted changes, and untracked files that are not
# ignored. Taken when a reviewed job starts and again when it ends, so
# the reviewer's diff between the two is that job's work and nothing
# else -- including edits to files that were never committed, which a
# diff against HEAD or a stash cannot show.
#
# Built on a private copy of the index, so the working tree, the real
# index, HEAD, and every ref are untouched; the only trace is
# unreferenced objects in the object store, which git's own gc removes.
#
# Untracked files are skipped when together they exceed
# `max_untracked_bytes`, since snapshotting copies them into the object
# store. `untracked$included` records which happened, and the reviewer
# is told. Ignored files are never covered.
#
# NULL outside a git repository or before the first commit. `snapshot`
# is NULL when the commit could not be built.
job_git_snapshot <- function(checkout,
                             max_untracked_bytes = JOB_SNAPSHOT_MAX_BYTES) {
    run <- function(args, env = NULL) {
        res <- git_run(args, path = checkout, env = env)
        if (res$status != 0L) {
            return(NULL)
        }
        res$text
    }
    head <- run(c("rev-parse", "HEAD"))
    if (is.null(head) || !grepl("^[0-9a-f]{40,64}$", head)) {
        return(NULL)
    }
    listed <- run(c("-c", "core.quotePath=false", "ls-files", "--others",
                    "--exclude-standard"))
    files <- if (is.null(listed) || !nzchar(listed)) {
        character()
    } else {
        strsplit(listed, "\n", fixed = TRUE)[[1L]]
    }
    bytes <- sum(file.size(file.path(checkout, files)), na.rm = TRUE)
    include <- bytes <= max_untracked_bytes
    untracked <- list(included = include, n = length(files), bytes = bytes,
                      files = as.list(utils::head(files, 20L)))

    index <- tempfile("corteza-index-")
    on.exit(unlink(c(index, paste0(index, ".lock"))), add = TRUE)
    env <- c(GIT_INDEX_FILE = index, GIT_AUTHOR_NAME = "corteza",
             GIT_AUTHOR_EMAIL = "corteza@localhost",
             GIT_COMMITTER_NAME = "corteza",
             GIT_COMMITTER_EMAIL = "corteza@localhost")
    # Start from a copy of the real index: its cached file stats let
    # `git add` hash only what changed instead of every tracked file.
    real <- run(c("rev-parse", "--git-path", "index"))
    if (!is.null(real) && !grepl("^(/|[A-Za-z]:)", real)) {
        real <- file.path(checkout, real)
    }
    seeded <- !is.null(real) && file.exists(real) && file.copy(real, index)
    if (!seeded && is.null(run(c("read-tree", "HEAD"), env))) {
        return(list(head = head, snapshot = NULL, untracked = untracked))
    }
    commit <- NULL
    if (!is.null(run(c("add", if (include) "-A" else "-u", "--", "."), env))) {
        tree <- run("write-tree", env)
        if (!is.null(tree) && grepl("^[0-9a-f]{40,64}$", tree)) {
            commit <- run(c("commit-tree", tree, "-p", head, "-m",
                            "corteza job snapshot"), env)
        }
    }
    if (is.null(commit) || !grepl("^[0-9a-f]{40,64}$", commit)) {
        commit <- NULL
    }
    list(head = head, snapshot = commit, untracked = untracked)
}

# How the reviewer sees the change, given the snapshots taken as the job
# started (`base`) and ended (`end`). States what the diff covers and,
# where it falls short, what it does not: a reviewer told a diff is the
# job's work will attribute everything in it to the job.
job_review_how <- function(base, end) {
    if (is.null(base)) {
        return(paste("This directory is not a git repository (or has no",
                     "commits), so there is no diff. Read the files the",
                     "report names."))
    }
    log <- sprintf("- The job started at commit %s; `git_log` shows any commits it added.",
                   base$head)
    if (is.null(base$snapshot) || is.null(end$snapshot)) {
        return(paste(c(
                       paste("- The checkout's state could not be snapshotted, so the",
                             "job's changes cannot be isolated. `git_diff` shows",
                             "uncommitted changes against HEAD; some may predate this",
                             "job. Do not attribute them to it."),
                       "- `git_status` lists untracked files; read the ones that matter.",
                       log), collapse = "\n"))
    }
    covered <- isTRUE(base$untracked$included) &&
    isTRUE(end$untracked$included)
    paste(c(
            sprintf("- `git_diff` with ref = \"%s..%s\" shows what this job changed.",
                    base$snapshot, end$snapshot),
            if (covered) {
                paste("- Both ends are snapshots of the whole checkout, untracked",
                      "files included: new files appear as added, and edits to",
                      "files that were never committed appear as edits.")
            } else {
                skipped <- if (isTRUE(base$untracked$included)) {
                    end$untracked
                } else {
                    base$untracked
                }
                paste(c(
                        sprintf(paste("- Untracked files are NOT in that diff: there",
                                      "were %d of them (%.1f MB), too much to",
                                      "snapshot. The diff covers tracked files only."),
                                as.integer(skipped$n), skipped$bytes / 1024 ^ 2),
                        paste("  `git_status` lists them. What this job did to a file",
                              "that was already untracked cannot be told apart from",
                              "what was there before; do not attribute such a",
                              "file's contents to this job."),
                        if (length(base$untracked$files)) {
                            paste0("  Untracked when the job started: ",
                                   paste(unlist(base$untracked$files), collapse = ", "),
                                if (base$untracked$n > length(base$untracked$files)) {
                                    ", ..."
                                })
                        }), collapse = "\n")
            },
            "- Files matched by .gitignore are not covered either way.",
            log), collapse = "\n")
}

# The reviewer's task: what the doer was asked, what it says it did, and
# how to see the change.
job_review_task <- function(job, result, end = NULL) {
    how <- job_review_how(job$dispatch$base, end)
    paste(c(
            sprintf("Review the work done for job %s.", job$id),
            "",
            "## The task the doer was given",
            job$task,
            "",
            "## The doer's report",
            if (nzchar(result %||% "")) {
                result
            } else {
                "(the doer returned no report)"
            },
            "",
            "## How to see the change",
            how,
            "",
            "## What to answer",
            "First line, exactly one of:",
            "VERDICT: approve",
            "VERDICT: changes requested",
            "",
            paste("Then your findings, most serious first: file and line, what",
                  "is wrong, and why it matters. End with what you could not",
                  "verify. Do not restate the diff.")),
          collapse = "\n")
}

# The verdict a review opened with, or NA when it gave none in the
# asked-for form. Read from the first lines only, so a verdict quoted
# later in the findings is not mistaken for the reviewer's own.
job_review_verdict <- function(text) {
    lines <- utils::head(trimws(strsplit(text %||% "", "\n", fixed = TRUE)[[1L]]),
                         5L)
    hit <- grep("^\\**VERDICT:", lines, ignore.case = TRUE, value = TRUE)
    if (!length(hit)) {
        return(NA_character_)
    }
    v <- tolower(sub("^\\**VERDICT:\\**\\s*", "", hit[[1L]],
                     ignore.case = TRUE))
    if (grepl("^approve", v)) {
        "approve"
    } else if (grepl("^changes requested", v)) {
        "changes requested"
    } else {
        NA_character_
    }
}

# Queue the review of a doer job that is about to be settled `done`, and
# hand it the checkout lock. Returns list(id, locked): `locked` is FALSE
# when the hand-off could not be made, in which case the review takes
# the lock the ordinary way when it starts. Either way the review only
# runs if no other job held the checkout in between (see
# job_review_unbroken()). An error creating the review job propagates;
# the caller records it and settles the doer's job without a review.
job_queue_review <- function(session, job, result) {
    checkout <- job_checkout(job$workspace)
    # The end state, taken while the doer's lock is still held.
    end <- if (!is.null(job$dispatch$base)) {
        job_git_snapshot(checkout, job_snapshot_max_bytes(session))
    }
    id <- job_create(job_review_task(job, result, end), role = "reviewer",
                     workspace = job$workspace, requester = job$requester,
                     origin = job$origin, owner = job$owner %||% "local",
                     limits = job$limits,
                     permissions = list(tools = JOB_REVIEWER_TOOLS),
                     review_of = job$id)
    locked <- job_lock_transfer(checkout, job$id, id, job$owner %||% "local")
    list(id = id, locked = isTRUE(locked))
}

# May this review still run? Only if the checkout has been held by the
# reviewed job and then by the review, with no other job in between.
#
# The lock handed to a queued review does not survive everything. If the
# process holding it dies, the lock goes stale, another bot's job can
# take the checkout and change it, and the review -- still queued when
# its owner restarts -- would then inspect that job's changes as if they
# were the doer's. The lock's generations are never removed, so the
# history says exactly who held the checkout since the doer: called
# after the review has acquired the lock, this passes only when every
# generation after the doer's belongs to the review. Returns
# list(ok, by), `by` naming the jobs that got in between.
job_review_unbroken <- function(checkout, review_of, review_id) {
    root <- job_lock_root(checkout)
    after <- character()
    for (k in rev(job_lock_gens(root))) {
        holder <- job_read_file(file.path(job_lock_gen_dir(root, k),
                "holder.json"))$job %||% ""
        if (identical(holder, review_of)) {
            by <- setdiff(after, review_id)
            return(list(ok = length(after) > 0L && !length(by), by = by))
        }
        after <- c(after, holder)
    }
    # The reviewed job never held this checkout: nothing shows the tree
    # is the one it left.
    list(ok = FALSE, by = character())
}

# A short name for a job in listings and result headings. A review's
# task is its whole instruction text, so it is named by what it reviews.
job_title <- function(job, max_chars = 100L) {
    if (!is.null(job$review_of)) {
        return(sprintf("review of job %s", job$review_of))
    }
    .sanitize_inline(job$task, max_chars = max_chars)
}

# Lines a surface adds under a finished job's result: the review that
# follows a doer's job, or a review's verdict.
job_outcome_notes <- function(job) {
    o <- job$outcome
    c(if (!is.null(o$review_job)) {
            if (isFALSE(o$review_locked)) {
                # Say what is true: the lock was not handed over, so the
                # checkout is open until the review takes it.
                sprintf(paste("Review queued as job %s. The checkout lock",
                              "could not be handed to it directly; if",
                              "another job takes the checkout first, the",
                              "review is cancelled rather than run on a",
                              "changed tree."), o$review_job)
            } else {
                sprintf("Review queued as job %s. The checkout stays locked until it ends.",
                        o$review_job)
            }
        },
        if (!is.null(o$review_error)) {
            sprintf("No review was queued: %s", o$review_error)
        },
        if (!is.null(job$review_of) && identical(job$status, "done")) {
            sprintf("Verdict: %s.",
                if (is.null(o$verdict) || is.na(o$verdict)) {
                    "none given"
                } else {
                    o$verdict
                })
        })
}
