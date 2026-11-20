# What a worker may do is its conversation's to say (WP20a, rule 7).
#
# A doer or a reviewer runs in a process of its own (R/job-worker.R),
# and its session was built when that process started. So plan mode,
# approval mode and dry run, which a person changes with /plan,
# /permissions and /dryrun, reached a warm worker only through its
# first job. Now the session that dispatched a job writes them to a
# record beside the job, `authority.json`: when the job starts, and
# whenever one of them changes while it runs (job_pump()). The worker
# reads the record at each tool call, and again just before the call
# runs, after any wait for an approval. A record that is missing,
# cannot be read, or names another job or conversation refuses the
# call: nothing falls back to what the worker started with.
#
# A job keeps the conversation that started it. Only that conversation
# writes its record. After the terminal's /clear (a new session id), or
# once a room's job passes to the session that replaces it
# (job_state_move()), the record stays as the old conversation left
# it, and /cancel stops the job. A job that conversation queued and had
# not started is held with what it allowed as it ended
# (job_conversation_hold()), and its record is written from that.

job_authority_path <- function(id) {
    file.path(job_dir(id), "authority.json")
}

# The settings a worker obeys, read from the session that dispatched
# its job. Dry run counts from either place it is kept, as turn()'s
# handler reads it.
job_authority_now <- function(session) {
    list(plan_mode = isTRUE(session$plan_mode),
         approval_mode = session$config$approval_mode %||% "ask",
         dry_run = isTRUE(session$dry_run) || isTRUE(session$config$dry_run))
}

# Write the record of job `id`. Each write takes a number above any the
# job's record had, so a worker can tell that its record changed while
# a call waited. A write that fails removes the record: one that no
# longer says what the conversation allows must not stand, and with
# none the worker refuses every call until a later write. A job held
# from a conversation that has ended (job_conversation_hold()) passes
# that conversation and what it allowed, so no later pump of another
# conversation writes the record again (job_authority_sync()).
job_authority_write <- function(session, id,
                                values = job_authority_now(session),
                                conversation = session_conversation(session)) {
    now <- values
    path <- job_authority_path(id)
    last <- session$.job_authority
    seqs <- 0L
    if (identical(last$job, id) && is.numeric(last$seq)) {
        seqs <- c(seqs, as.integer(last$seq))
    }
    if (file.exists(path)) {
        prev <- tryCatch(job_read_file(path), error = function(e) NULL)
        if (is.numeric(prev$seq) && length(prev$seq) == 1L) {
            seqs <- c(seqs, as.integer(prev$seq))
        }
    }
    seq <- max(seqs) + 1L
    written <- tryCatch(job_write_file(path, c(list(job = id,
                    session_key = job_worker_key(session), seq = seq,
                    at = job_now()), now)), error = function(e) e)
    if (inherits(written, "error")) {
        unlink(path)
        stop(conditionMessage(written), call. = FALSE)
    }
    session$.job_authority <- list(job = id, conversation = conversation,
                                   values = now, seq = seq)
    invisible(seq)
}

# Bring the running job's record up to date when a setting changed
# since the last write, and only for the conversation that started the
# job. job_pump() calls it, and every surface pumps after a command.
job_authority_sync <- function(session) {
    id <- session$.job_current
    last <- session$.job_authority
    if (is.null(id) || !identical(last$job, id) ||
        !identical(last$conversation, session_conversation(session))) {
        return(invisible(FALSE))
    }
    if (identical(last$values, job_authority_now(session))) {
        return(invisible(FALSE))
    }
    job_authority_write(session, id)
    invisible(TRUE)
}

# The worker's reading of its job's record: list(ok = TRUE, seq,
# plan_mode, approval_mode, dry_run), or list(ok = FALSE, reason).
# `session_key` is the conversation the job came from, as its own
# record says.
job_authority_read <- function(id, session_key) {
    refuse <- function(why) list(ok = FALSE, reason = why)
    if (is.null(id)) {
        return(refuse("no job is running"))
    }
    path <- job_authority_path(id)
    if (!file.exists(path)) {
        return(refuse("this job has no authority record"))
    }
    rec <- tryCatch(job_read_file(path), error = function(e) NULL)
    if (!is.list(rec)) {
        return(refuse("this job's authority record could not be read"))
    }
    if (!identical(rec$job, id)) {
        return(refuse("this job's authority record names another job"))
    }
    if (is.null(session_key) || !identical(rec$session_key, session_key)) {
        return(refuse("this job's authority record names another conversation"))
    }
    flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
    if (!is.numeric(rec$seq) || length(rec$seq) != 1L ||
        !flag(rec$plan_mode) || !flag(rec$dry_run) ||
        !is.character(rec$approval_mode) || length(rec$approval_mode) != 1L ||
        !rec$approval_mode %in% .approval_modes) {
        return(refuse("this job's authority record could not be read"))
    }
    list(ok = TRUE, seq = rec$seq, plan_mode = rec$plan_mode,
         approval_mode = rec$approval_mode, dry_run = rec$dry_run)
}

# Read a session's authority into it, before a call and again before
# the call runs (turn()'s handler). A session with no `authority`
# function, everything but a worker, has nothing to read.
#
# Review is a gate: a worker that holds one (.job_worker_child_review_gate())
# uses it while the record says "review", so a /permissions typed while
# a job runs reaches its next call. A room's worker holds none; its
# supervision comes with its job.
.authority_apply <- function(session) {
    if (!is.function(session$authority)) {
        return(list(ok = TRUE))
    }
    a <- tryCatch(session$authority(), error = function(e) {
        list(ok = FALSE, reason = paste("its authority record could not be read:",
                                        conditionMessage(e)))
    })
    if (isTRUE(a$ok)) {
        session$plan_mode <- a$plan_mode
        session$dry_run <- a$dry_run
        session$config$dry_run <- a$dry_run
        session$config$approval_mode <- a$approval_mode
        if (is.function(session$.job_review_gate)) {
            session$auto_gate <- if (identical(a$approval_mode, "review")) {
                session$.job_review_gate
            }
        }
    }
    a
}
