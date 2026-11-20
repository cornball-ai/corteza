# A conversation's lifecycle, as the job system sees it (WP20a, rules 2
# to 4). A surface gives its session a new conversation and calls
# job_conversation_clear(); it touches no worker and no checkpoint.
#
# A worker learns of a new conversation from its next job, whose spec
# names it (job_worker_spec()), and builds its session again in the same
# process: the R objects stay, the history starts empty, and the prompt
# and tools are those of a session built now (.job_worker_child_run()).
# A job in flight finishes where it started, under the conversation that
# asked for it, and a job that conversation queued runs under what it
# allowed as it ended (job_conversation_hold()).
#
# Where a surface keeps a checkpoint for each conversation (the
# terminal, by `repl:<id>`), the new conversation gets a starting
# checkpoint of its own at /clear: the R objects as they are then, and
# no history. An idle doer writes it; with no doer alive the last
# conversation's workspace file is copied; with a job in flight it is
# written when that job settles. A copy, never a reference: resuming the
# old conversation later and changing its objects leaves the new one's
# alone. A room keeps one checkpoint for each room, and a checkpoint
# records the conversation it was saved under, so a new conversation's
# doer takes the objects and leaves the history.

# The conversation a session is in: at the terminal, its session id.
# NULL where a surface has none, and then no worker is rebuilt.
session_conversation <- function(session) {
    session$conversation_id %||% session$sessionId
}

# A conversation that ends with jobs it asked for still queued keeps
# them, as it keeps a job in flight (rule 4): each is held with the spec
# and the authority the conversation gives it now, and runs under them
# when its turn comes, whichever conversation's pump starts it
# (job_dispatch()). A surface calls this just before a conversation
# ends, while the session still holds its settings and ids: the
# terminal's /clear, and a room's reset before the replacement takes
# the jobs. Returns how many jobs it held.
job_conversation_hold <- function(session) {
    if (!is.environment(session)) {
        return(invisible(0L))
    }
    conversation <- session_conversation(session)
    queued <- job_list(status = "queued",
                       origin_key = job_worker_key(session),
                       owner = job_worker_owner(session))
    queued <- Filter(function(j) {
        !j$backend %in% c("remote", "consultation")
    }, queued)
    ids <- vapply(queued, function(j) j$id, character(1))
    held <- session$.job_held %||% list()
    # A held job that is no longer queued (started, or cancelled) has
    # nothing left to hold.
    held <- held[intersect(names(held), ids)]
    n <- 0L
    for (j in queued) {
        if (is.null(conversation) ||
            !identical(j$origin$conversation, conversation) ||
            !is.null(held[[j$id]])) {
            next
        }
        held[[j$id]] <- list(conversation = conversation,
                             spec = job_worker_spec(session, j$role),
                             authority = job_authority_now(session))
        n <- n + 1L
    }
    session$.job_held <- held
    invisible(n)
}

# The session is now in `conversation`. `checkpoint_key` is the
# surface's checkpoint key for it, where the surface keeps one for each
# conversation; NULL keeps the key.
job_conversation_clear <- function(session,
                                   conversation = session_conversation(session),
                                   checkpoint_key = NULL) {
    if (!is.environment(session)) {
        return(invisible(NULL))
    }
    from <- job_worker_identity(session, "doer")
    if (!is.null(checkpoint_key)) {
        session$job_checkpoint_key <- checkpoint_key
    }
    to <- job_worker_identity(session, "doer")
    if (identical(job_identity_text(from), job_identity_text(to))) {
        return(invisible(NULL))
    }
    session$.job_starts <- c(session$.job_starts %||% list(),
                             list(list(from = from, to = to, conversation = conversation)))
    job_start_checkpoints(session)
    invisible(NULL)
}

# Write the starting checkpoints asked for, oldest first, once no job is
# in flight. Each new conversation's comes from the doer when one is
# alive, else from the checkpoint of the conversation before it, so a
# chain of clears passes the objects along.
job_start_checkpoints <- function(session) {
    while (length(session$.job_starts) && is.null(session$.job_current)) {
        start <- session$.job_starts[[1L]]
        session$.job_starts <- session$.job_starts[-1L]
        tryCatch(job_start_checkpoint(session, start), error = function(e) {
            message("jobs: could not write the new conversation's ",
                    "starting checkpoint: ", conditionMessage(e))
        })
    }
    invisible(NULL)
}

job_start_checkpoint <- function(session, start) {
    dir <- job_worker_state_dir(start$to)
    worker <- job_worker_live_for(session, "doer")
    if (!is.null(worker)) {
        return(invisible(worker$run(function(state_dir, identity,
                        conversation) {
            get(".job_worker_child_start_checkpoint",
                envir = asNamespace("corteza"), inherits = FALSE)(state_dir, identity, conversation)
        }, list(state_dir = dir, identity = start$to,
                        conversation = start$conversation))))
    }
    job_checkpoint_copy(job_worker_state_dir(start$from), dir, start$to,
                        start$conversation)
}

# The session's live worker for `role`, active or parked, when it stands
# in the session's directory. R objects do not follow a conversation
# into another directory.
job_worker_live_for <- function(session, role) {
    w <- session$.job_workers_parked[[role]]
    if (identical(session$.job_worker_role, role)) {
        w <- session$.job_worker
    }
    if (is.null(w) ||
        !isTRUE(tryCatch(w$is_alive(), error = function(e) FALSE)) ||
        !job_worker_in_place(session, role)) {
        return(NULL)
    }
    w
}

# A starting checkpoint copied from the last conversation's: its
# workspace file, under the new identity and conversation, and no
# history. Nothing when the last conversation left no checkpoint.
job_checkpoint_copy <- function(from_dir, to_dir, identity, conversation) {
    marker <- file.path(from_dir, "checkpoint.json")
    workspace <- file.path(from_dir, "workspace.RData")
    if (!file.exists(marker) || !file.exists(workspace)) {
        return(invisible(FALSE))
    }
    info <- job_read_file(marker)
    dir.create(to_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    Sys.chmod(to_dir, "0700")
    target <- file.path(to_dir, "workspace.RData")
    tmp <- paste0(target, ".tmp-", Sys.getpid())
    if (!isTRUE(file.copy(workspace, tmp, overwrite = TRUE)) ||
        !isTRUE(file.rename(tmp, target))) {
        unlink(tmp)
        stop("could not copy the workspace checkpoint", call. = FALSE)
    }
    unlink(file.path(to_dir, "history.rds"))
    job_write_file(file.path(to_dir, "checkpoint.json"),
                   list(identity = identity, job = NULL, at = job_now(),
                        objects = info$objects, packages = info$packages,
                        conversation = conversation))
    Sys.chmod(file.path(to_dir, c("workspace.RData", "checkpoint.json")),
              "0600")
    invisible(TRUE)
}
