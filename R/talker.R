# Talker mode: the session that talks stays responsive and hands work
# to a doer.
#
# With talker mode on, a conversation session runs on a fast model with
# read-only tools plus three job tools. Work that needs edits, commands,
# tests, or a long investigation goes to the session's job worker
# (R/job-worker.R) through `delegate`, and the talker keeps answering.
# The doer runs on the model the session was configured with, so turning
# talker mode on moves that model to the worker rather than dropping it.
#
# The pattern is LiveKit's talker/reasoner split
# (https://livekit.com/blog/talker-reasoner-pattern-voice-agents). The
# rule it depends on is that the talker never guesses at a delegated
# answer; the result arrives later as its own message.

# Fast model per provider. Config can name any other.
TALKER_DEFAULT_MODELS <- list(anthropic = "claude-haiku-4-5-20251001",
                              openai = "gpt-6-luna")

# What the talker can do itself: look, not touch.
TALKER_TOOLS <- c("read_file", "skill_instructions", "grep_files",
                  "list_files", "git_status", "git_diff", "git_log",
                  "r_help", "delegate", "job_status", "job_cancel")

TALKER_GUIDANCE <- paste(
                         "## Talker mode",
                         "",
                         "You are the talker. Stay responsive: answer quick questions yourself",
                         "with your read-only tools. For anything that needs edits, commands,",
                         "tests, or a long investigation, call `delegate` with a self-contained",
                         "task. The doer does not see this conversation, so the task must say",
                         "everything it needs: files, goal, constraints, how to check the work.",
                         "Pass `review = true` when the change should be checked before anyone",
                         "relies on it: a read-only reviewer then inspects the result, and its",
                         "findings arrive as a second message. Whether to act on a review is",
                         "the user's call; do not start a revision on your own.",
                         "Tell the user the job has started. Never guess or pre-empt a",
                         "delegated job's result; it arrives as its own message when the job",
                         "ends. Use `job_status` when asked how work is going and `job_cancel`",
                         "to stop a job.",
                         sep = "\n")

# Resolve talker settings from a config's `talker` entry, or NULL when
# talker mode is off.
talker_config <- function(cfg) {
    t <- cfg$talker
    if (is.null(t) || !isTRUE(t$enabled)) {
        return(NULL)
    }
    t
}

# Switch a session into talker mode. The session's current model and
# provider become the doer's; the talker model is the config's, else the
# provider's default fast model.
talker_enable <- function(session, talker = list()) {
    provider <- talker$provider %||% session$provider %||% "anthropic"
    model <- talker$model %||% TALKER_DEFAULT_MODELS[[provider]]
    if (is.null(model)) {
        stop("talker mode needs a model for provider '", provider,
             "'; set talker$model", call. = FALSE)
    }
    session$doer_model <- .resolve_model(session)
    session$doer_provider <- session$provider
    # model_map$cloud is what turn() dispatches on (.resolve_model()).
    session$model_map$cloud <- model
    session$provider <- provider
    session$tools_filter <- TALKER_TOOLS
    session$system <- paste(c(session$system, TALKER_GUIDANCE),
                            collapse = "\n\n")
    session$talker <- TRUE
    invisible(session)
}

#' Hand a task to this session's doer.
#'
#' Queues a job for the session's persistent worker and returns at once.
#' The result arrives later as its own message. The doer does not see
#' this conversation, so the task must be self-contained.
#'
#' @param task (character) Complete description of the work: files,
#'   goal, constraints, and how to check the result.
#' @param review (logical) Have a read-only reviewer check the work when
#'   the doer finishes. The checkout stays locked until the review ends,
#'   and the review arrives as a second message. Omit to use the
#'   configured default.
#' @param ctx Server-side context; supplies the session.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_delegate <- function(task, review = NULL, ctx = list()) {
    session <- ctx$session
    if (!is.environment(session)) {
        return(err("delegate needs a live session"))
    }
    # The config default (`jobs$review`) applies when the model does not
    # say; an explicit FALSE from the model is honored.
    review <- if (is.null(review)) {
        isTRUE(session$config$jobs$review)
    } else {
        isTRUE(review)
    }
    tryCatch({
        id <- job_submit(session, task,
                         requester = session$job_requester %||% "local",
                         origin = session$job_origin %||% list(),
                         review = review)
        ok(sprintf(paste0("Delegated as job %s. The result will arrive as ",
                          "its own message; do not answer the delegated ",
                          "question yourself.%s"), id,
                if (review) {
                    " A review of the work will follow it."
                } else {
                    ""
                }))
    }, error = function(e) err(paste("Delegate failed:", conditionMessage(e))))
}

#' Show this session's recent jobs.
#'
#' @param id (character) Optional job id for one job's full record.
#' @param ctx Server-side context; supplies the session.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_job_status <- function(id = NULL, ctx = list()) {
    session <- ctx$session
    if (!is.environment(session)) {
        return(err("job_status needs a live session"))
    }
    if (!is.null(id)) {
        j <- tryCatch(job_read(id), error = function(e) NULL)
        if (!job_in_session(j, session)) {
            return(err(sprintf("No job %s in this session.", id)))
        }
        return(ok(format_job(j, detail = TRUE)))
    }
    jobs <- utils::tail(job_list(origin_key = job_worker_key(session),
                                 owner = job_worker_owner(session)), 10L)
    if (!length(jobs)) {
        return(ok("No jobs in this session."))
    }
    ok(paste(vapply(jobs, format_job, ""), collapse = "\n"))
}

#' Cancel one of this session's jobs.
#'
#' @param id (character) Job id.
#' @param ctx Server-side context; supplies the session.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_job_cancel <- function(id, ctx = list()) {
    session <- ctx$session
    if (!is.environment(session)) {
        return(err("job_cancel needs a live session"))
    }
    tryCatch({
        if (job_cancel(session, id, by = session$job_requester %||% "local")) {
            ok(sprintf("Cancellation requested for job %s.", id))
        } else {
            ok(sprintf("Job %s had already ended.", id))
        }
    }, error = function(e) err(conditionMessage(e)))
}

# The job tools only mean something to a talker session: elsewhere there
# is no worker wired to a surface that would deliver the result. Drop
# them from the payload unless the session is in talker mode, the way
# exit_plan_mode is dropped outside plan mode.
.talker_filter_tools <- function(tools, is_talker) {
    if (isTRUE(is_talker)) {
        return(tools)
    }
    job_tools <- c("delegate", "job_status", "job_cancel")
    keep <- vapply(tools, function(t) !(t$name %||% "") %in% job_tools,
                   logical(1))
    tools[keep]
}

# One line per job, or the full outcome with detail = TRUE.
format_job <- function(j, detail = FALSE) {
    line <- sprintf("%s  %-13s %s", j$id, j$status,
                    job_title(j, max_chars = 70L))
    if (!detail) {
        return(line)
    }
    o <- j$outcome
    paste(c(line, if (!is.null(o$reason)) paste("reason:", o$reason),
            if (!is.null(o$error)) paste("error:", o$error),
            job_outcome_notes(j),
            if (!is.null(o$result) && nzchar(o$result)) c("", o$result)),
          collapse = "\n")
}
