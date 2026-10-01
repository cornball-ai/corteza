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

# The checkout's git state as a job starts: HEAD, and a snapshot commit
# of any uncommitted tracked changes, so a later diff against `snapshot`
# shows only what the job did. `git stash create` builds that commit
# without touching the working tree, the index, or any ref. NULL outside
# a git repository or before the first commit.
job_git_base <- function(checkout) {
    git <- function(args) {
        out <- tryCatch(suppressWarnings(system2("git",
                    c("-C", shQuote(checkout), args), stdout = TRUE,
                    stderr = FALSE)),
                        error = function(e) structure(character(), status = 1L))
        if (!is.null(attr(out, "status"))) {
            return(NULL)
        }
        out
    }
    head <- git(c("rev-parse", "HEAD"))
    if (length(head) != 1L || !nzchar(head)) {
        return(NULL)
    }
    status <- git(c("status", "--porcelain")) %||% character()
    dirty <- any(!startsWith(status, "??"))
    snapshot <- head
    exact <- TRUE
    if (dirty) {
        snap <- git(c("stash", "create"))
        if (length(snap) == 1L && nzchar(snap)) {
            snapshot <- snap
        } else {
            # The snapshot could not be made (no git identity, say). The
            # reviewer is told the diff also holds earlier changes.
            exact <- FALSE
        }
    }
    list(head = head, snapshot = snapshot, dirty = dirty, exact = exact)
}

# The reviewer's task: what the doer was asked, what it says it did, and
# how to see the change.
job_review_task <- function(job, result) {
    base <- job$dispatch$base
    how <- if (is.null(base)) {
        paste("This directory is not a git repository (or has no commits),",
              "so there is no diff. Read the files the report names.")
    } else {
        paste(c(
                sprintf("- `git_diff` with ref = \"%s\" shows what changed since the job started.",
                        base$snapshot),
                if (isTRUE(base$dirty) && isTRUE(base$exact)) {
                    paste("- The checkout already had uncommitted changes then.",
                          "That ref is a snapshot including them, so the diff",
                          "is this job's work only.")
                } else if (isTRUE(base$dirty)) {
                    paste("- The checkout already had uncommitted changes then,",
                          "and they could not be separated: the diff also shows",
                          "them. Do not attribute them to this job.")
                },
                sprintf("- The job started at commit %s; `git_log` shows any commits it added.",
                        base$head),
                "- `git_status` lists new files. They are not in the diff; read them."),
              collapse = "\n")
    }
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
# the lock the ordinary way when it starts and the checkout was briefly
# open to other writers. An error creating the review job propagates;
# the caller records it and settles the doer's job without a review.
job_queue_review <- function(session, job, result) {
    id <- job_create(job_review_task(job, result), role = "reviewer",
                     workspace = job$workspace, requester = job$requester,
                     origin = job$origin, owner = job$owner %||% "local",
                     limits = job$limits,
                     permissions = list(tools = JOB_REVIEWER_TOOLS),
                     review_of = job$id)
    locked <- job_lock_transfer(job_checkout(job$workspace), job$id, id,
                                job$owner %||% "local")
    list(id = id, locked = isTRUE(locked))
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
            sprintf("Review queued as job %s. The checkout stays locked until it ends.",
                    o$review_job)
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
