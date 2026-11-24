# Talker mode: the session that talks stays responsive and hands work
# to a doer.
#
# With talker mode on, a conversation session runs with read-only tools
# plus the job tools. Work that needs edits, commands, tests, or a long
# investigation goes to the session's job worker (R/job-worker.R)
# through `delegate`, and the talker keeps answering. The doer runs on
# the model the session was configured with, and so does the talker
# unless its config names a faster one (`talker.model`).
#
# A bot's rooms are talkers unless the config says `"talker":
# {"enabled": false}` (WP20b), the bot's tools are restricted, or the
# room has no directory (bot_talker_config()). The CLI and chat() use
# direct execution unless the config enables talker mode.
#
# The pattern is LiveKit's talker/reasoner split
# (https://livekit.com/blog/talker-reasoner-pattern-voice-agents). The
# rule it depends on is that the talker never guesses at a delegated
# answer; the result arrives later as its own message.

# What the talker can do itself: look, not touch. Web search is not in
# this list and the talker has it anyway: on providers that support it,
# turn() turns on the provider's own search (.session_web_search()),
# which does not come from the skill registry. The guidance says so,
# because a model that is not told answers "I don't have access" and
# then searches.
#
# `fetch_url` is here so a question about a page the user names -- an
# issue and its comments, a document, a raw file -- is a quick question
# the talker answers by reading it, not a whole job it hands off. Its
# absence is what made a talker asked to read an upstream issue try to
# clone the repo through the doer instead.
#
# `read_handle` is here because any of these can return more than the
# model is shown: the result is cut and the rest stored under a handle.
# Without a tool to search that, a talker told "40 of 60 lines shown"
# cannot tell what is absent from what it was not shown.
TALKER_TOOLS <- c("read_file", "skill_instructions", "grep_files",
                  "list_files", "git_status", "git_diff", "git_log",
                  "r_help", "read_handle", "fetch_url", "delegate",
                  "job_status", "job_cancel", "job_resume")

TALKER_GUIDANCE <- paste(
                         "## Talker mode",
                         "",
                         "You are the talker. Stay responsive: answer quick questions yourself",
                         "with your read-only tools. When web search is available to you, a",
                         "question about current facts (a score, news, a price) is a quick",
                         "question: search and answer with what you found. Do not open by",
                         "saying you lack access to something you are about to look up.",
                         "To read a specific page the user names -- a URL, a GitHub issue",
                         "and its comments, a raw file -- fetch it with `fetch_url` and",
                         "answer from it. Do not hand off a whole job, and never try to",
                         "clone a repository, just to read something on the web.",
                         "For anything that needs edits, commands,",
                         "tests, or a long investigation, hand the work to `delegate` with a",
                         "self-contained task: the work runs without seeing this chat, so the",
                         "task must carry everything it needs -- files, goal, constraints, how",
                         "to check the work -- and any document or text the work must act on.",
                         "Put all of that in the task argument, never in your reply here.",
                         "When the work is about something the user just provided here -- an",
                         "article, a document, a log -- do not copy it into the task: call",
                         "`delegate` with `attach_context = true` and a short task that refers",
                         "to it, and their message is attached for the doer to read.",
                         "Pass `review = true` when the result should be checked before anyone",
                         "relies on it; the check arrives as a later message, and whether to",
                         "act on it is the user's call; do not start a revision on your own.",
                         "",
                         "Keep all of this invisible to the user. They asked a person for",
                         "help, not a pipeline: never mention a doer, the task you wrote, a",
                         "reviewer, or that work was 'delegated' or 'handed off', and never",
                         "explain that the work cannot see this chat. To them you are simply",
                         "taking care of it. Acknowledge in one short, natural line ('On it --",
                         "I'll get that done and let you know') and keep your reply brief:",
                         "never paste an article, file contents, or long quoted text back into",
                         "the chat. Never guess or pre-empt the result; it arrives as its own",
                         "message when the work is done. Use `job_status` when asked how it is",
                         "going and `job_cancel` to stop it. An incomplete job has stopped.",
                         "Use `job_resume` only when the user explicitly asks to continue that job; never resume automatically.",
                         "When you need to know what was discussed or decided in another",
                         "room, use `ask_room` when available. It asks that room's conversation",
                         "and returns its answer here. Wait for the answer before relying on it.",
                         "When work belongs to another room's project, pass `room` to",
                         "`delegate`: that room's doer runs it there, that room is told, and",
                         "the result is posted in both rooms. Tell the user which room took",
                         "it. A message here saying a job was accepted from another room means",
                         "this room's doer is doing that room's work; say so when asked what",
                         "it is busy with.",
                         sep = "\n")

# Resolve talker settings from a config's `talker` entry, or NULL when
# talker mode is off. `talker.enabled` decides; without it, `default`
# does: on for a bot's rooms (bot_talker_config()), off in the CLI
# and chat() (.repl_talker_default()).
talker_config <- function(cfg, default = FALSE) {
    t <- cfg$talker
    if (!is.null(t) && !is.list(t)) {
        stop("the config's `talker` entry must be an object, such as ",
             "{\"enabled\": false}", call. = FALSE)
    }
    on <- t[["enabled"]] %||% default
    if (!isTRUE(on)) {
        return(NULL)
    }
    t %||% list()
}

# A room's talker settings, or NULL when its session is no talker. A
# room's session is a talker unless its config says otherwise, with 2
# exceptions to the default:
#
# - The bot's tools are restricted (`tools_filter`, from the run or the
#   config). A doer gets the work tools whatever that list says, so a bot
#   started with fewer tools gets one only when its config asks for it.
# - The room has no directory (`unbound`, WP10). It hands no work to a
#   doer, so a talker there would be told to hand off what it cannot.
bot_talker_config <- function(cfg, tools_filter = NULL, unbound = FALSE) {
    restricted <- length(tools_filter %||% cfg$tools_filter) > 0L
    talker_config(cfg, default = !restricted && !isTRUE(unbound))
}

# Switch a session into talker mode. The session's current model and
# provider become the doer's, unless the config's `jobs` block names a
# worker model or provider. The talker runs on the config's
# `talker.model`, else on the session's own model, or on its provider's
# default model where `talker.provider` names another provider. Without
# a model or provider of its own, the talker follows the session's
# model through /model (bot_apply_model_command()). Provider switches
# move both roles, remembering the old pair (R/model-pairs.R).
#
# The doer's settings are read from the config here, once: /model and
# /effort change them, /status shows them and job_worker_spec() runs a
# job with them, so all 3 say the same.
talker_enable <- function(session, talker = list()) {
    .context_conversation_enable(session, talker$context_edit,
                                 talker$context_vault %||% session$context_archive_vault)
    own <- session$provider %||% "anthropic"
    provider <- talker$provider %||% own
    configured <- .resolve_model(session)
    jobs <- session$config$jobs %||% list()
    session$doer_model <- jobs$model %||% configured
    session$doer_provider <- jobs$provider %||% session$provider
    model <- talker$model
    if (is.null(model)) {
        if (identical(provider, own)) {
            model <- configured
        } else {
            model <- default_provider_model(provider)
            if (is.null(model)) {
                stop("talker$provider '", provider,
                     "' has no default model; set talker$model", call. = FALSE)
            }
        }
    }
    session$talker_follows <- is.null(talker$model) && is.null(talker$provider)
    # The reasoning settings were chosen for the configured model, so
    # they go to the doer with it. Left on the session they are sent
    # with the talker's model, and a fast model refuses them outright
    # ("This model does not support the effort parameter"), which fails
    # every reply. The talker runs on the provider's defaults unless the
    # talker config names its own. For the same reason a doer on the
    # `jobs` block's own model starts from the provider's defaults, and
    # takes a setting only where that block names one.
    effort <- .session_reasoning_effort(session)
    budget <- .session_thinking_budget(session)
    thinking <- .session_thinking(session)
    if (!identical(session$doer_model, configured)) {
        effort <- budget <- thinking <- NULL
    }
    session$doer_reasoning_effort <- jobs$reasoning_effort %||% effort
    # What /effort default goes back to (.effort_command()).
    session$doer_reasoning_default <- session$doer_reasoning_effort
    session$doer_thinking_budget <- jobs$thinking_budget_tokens %||% budget
    # `[[`: `$thinking` on a list also matches `thinking_budget_tokens`.
    session$doer_thinking <- jobs[["thinking"]] %||% thinking
    session$reasoning_effort <- .check_reasoning_effort(
        talker$reasoning_effort, "talker$reasoning_effort")
    session$thinking_budget_tokens <- .check_max_tokens(
        talker$thinking_budget_tokens, "talker$thinking_budget_tokens",
        what = "thinking_budget_tokens")
    # `talker$thinking` is the talker model's own thinking type: on
    # Claude Sonnet 5.5, "between_tools" answers without thinking first.
    session$thinking <- .check_thinking(talker[["thinking"]], "talker$thinking")
    # model_map$cloud is what turn() dispatches on (.resolve_model()).
    session$model_map$cloud <- model
    session$provider <- provider
    session$tools_filter <- TALKER_TOOLS
    session$system <- paste(c(session$system, TALKER_GUIDANCE),
                            collapse = "\n\n")
    # And a source of the session's context, ordered after every other,
    # so a rebuild of the context (the terminal's /clear, the archival
    # runtime), which passes these sources back, renders it where it is
    # now. A worker builds its prompt from sources of its own
    # (job_worker_context()), so it never reaches a doer.
    guidance <- context_text_source("talker_guidance", "runtime",
                                    TALKER_GUIDANCE, 1000,
                                    "corteza::talker_enable", "session")
    session$context_prefix_sources <- c(Filter(function(x) {
        !identical(x$id, "talker_guidance")
    }, session$context_prefix_sources %||% list()), list(guidance))
    session$talker <- TRUE
    if (!is.null(session$config$model_pairs[[session$doer_provider]])) {
        .apply_model_pair(session, .model_pair(session, session$doer_provider))
    }
    .remember_model_pair(session)
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
#' @param room (character) Hand the task to another room's doer instead
#'   of this one's: the room's name, its id, or its project directory.
#'   The work then runs in that room's project, that room is told, and
#'   the result is posted in both rooms. Omit for this room. Only where
#'   the bot serves several rooms.
#' @param attach_context (logical) Attach the message this turn is
#'   answering to the task, so the doer can read it without the task
#'   repeating it. Use it when the work is about something the user just
#'   provided here (an article, a document, a log): the content is
#'   written beside the job and the task points the doer at it, instead
#'   of being copied into the task text and through the model's output
#'   budget. This session's doer only; not for a `room` hand-off.
#' @param ctx Server-side context; supplies the session.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_delegate <- function(task, review = NULL, room = NULL,
                          attach_context = NULL, ctx = list()) {
    session <- ctx$session
    if (!is.environment(session)) {
        return(err("delegate needs a live session"))
    }
    # A room with no directory hands no work on, to its own doer or
    # another room's (WP10). policy() refuses `delegate` there first;
    # this holds for any other way in.
    if (isTRUE(session$unbound)) {
        return(err(paste("This room has no directory, so it hands no work",
                         "to a doer.", bot_unbound_how())))
    }
    if (!is.null(room) && nzchar(trimws(as.character(room)[[1L]]))) {
        # Another room's session, its directory, and its doer belong to
        # the surface that holds the rooms. The surface supplies the
        # hand-off for the turn in progress (bot_poll()); a session with
        # none has no other rooms to hand to.
        handoff <- session$job_handoff
        if (!is.function(handoff)) {
            return(err(paste("This session has no other rooms to hand work",
                             "to. Call delegate without `room`.")))
        }
        return(tryCatch(handoff(room = as.character(room)[[1L]], task = task,
                                review = review),
                        error = function(e) {
            err(paste("Delegate failed:", conditionMessage(e)))
        }))
    }
    # The config default (`jobs$review`) applies when the model does not
    # say; an explicit FALSE from the model is honored.
    review <- if (is.null(review)) {
        isTRUE(session$config$jobs$review)
    } else {
        isTRUE(review)
    }
    context <- if (isTRUE(attach_context)) {
        p <- message_text(list(content = session$current_prompt))
        if (inherits(session$current_prompt, "llm_content")) {
            p <- paste(p, "Attachment image bytes are not included in this delegated text.", sep = "\n\n")
        }
        if (is.character(p) && length(p) == 1L && nzchar(trimws(p))) {
            p
        }
    }
    tryCatch({
        id <- job_submit(session, task,
                         requester = session$job_requester %||% "local",
                         origin = session$job_origin %||% list(),
                         review = review, context = context)
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
        if (!job_visible_to(j, session)) {
            return(err(sprintf("No job %s in this session.", id)))
        }
        return(ok(bot_sidebar_job_status(j, session, detail = TRUE)))
    }
    # This session's own jobs, and the ones it handed to another room:
    # the user who asked for those asks about them here.
    jobs <- Filter(function(j) job_visible_to(j, session),
                   job_list(owner = job_worker_owner(session)))
    jobs <- utils::tail(jobs, 10L)
    if (!length(jobs)) {
        return(ok("No jobs in this session."))
    }
    ok(paste(vapply(jobs, function(j) bot_sidebar_job_status(j, session), ""), collapse = "\n"))
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
    job_tools <- c("delegate", "job_status", "job_cancel", "job_resume")
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
            if (!is.null(j$resume_of)) paste("resumes:", j$resume_of),
            if (length(o$cumulative_usage)) paste("cumulative usage:",
                as.character(jsonlite::toJSON(o$cumulative_usage, auto_unbox = TRUE))),
            if (!is.null(o$error)) paste("error:", o$error),
            job_outcome_notes(j),
            if (nzchar(job_result_body(j))) c("", job_result_body(j))),
          collapse = "\n")
}
