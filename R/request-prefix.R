# What a session sends ahead of its conversation (WP14, decision S1).
#
# The system text and the tool list are composed once and stay fixed
# for the session, so a provider's cache and a reply's thinking blocks
# hold from one request to the next: a change to either makes the
# provider read the whole history again, and a model that binds its
# thinking to the request refuses a replay after one. So:
#
# - A change of mode or of the task list reaches the model as a note at
#   the head of the next prompt (.prefix_notes()), and notes stay in the
#   history.
# - Permission is checked when a tool is called, never by editing the
#   list: plan mode and an /auto run refuse at the call (turn()'s
#   handler, policy()), and so does a job's tool limit.
# - The availability tests (git tools in a repository, a search key)
#   run when the list is composed, not on every request.
#
# The prefix is composed again when what it is composed from changes
# (.prefix_inputs()): a terminal /clear, a job whose tools differ, the
# archival runtime's new system text. Each is logged. A job that may not
# search sends the provider's tools without search, on purpose
# (.prefix_search_reset()). A context-editing session edits what it
# sends on every turn and is the stated exception.

# The parts a prefix is composed from. Data only, so a digest of it says
# whether the cached prefix still holds.
.prefix_inputs <- function(session, tools = NULL) {
    table <- session$tool_table
    list(system = session$system %||% "",
         tools_filter = session$tools_filter,
         tools = tools,
         table = if (is.environment(table)) format(table),
         channel = session$channel %||% "",
         task_list = .task_list_on(session),
         talker = isTRUE(session$talker),
         plan_capable = .prefix_plan_capable(session),
         plan_locked = isTRUE(session$plan_mode_locked),
         delegate_to = if (is.function(session$delegate_to)) {
            as.character(session$delegation_names %||% character())
        },
         ask_room = is.function(session$ask_room),
         send_room_file = is.function(session$send_room_file))
}

# Can this session be in plan mode? The terminal and chat(), with their
# whole tool set, turn it on with /plan; a subagent takes its parent's
# at spawn. A room never is, nor is a monitor, which reads with a tool
# list of its own, and a job's worker may not leave it
# (`plan_mode_locked`): none is offered exit_plan_mode. Once a session
# has been in plan mode it stays offered, so the list does not change
# when plan mode ends; one with a narrowed list that turns plan mode on
# is offered it from then, a reset the log records.
.prefix_plan_capable <- function(session) {
    if (isTRUE(session$plan_mode_locked)) {
        return(FALSE)
    }
    if (isTRUE(session$plan_mode)) {
        session$.plan_capable <- TRUE
    }
    isTRUE(session$.plan_capable) ||
    ((session$channel %||% "") %in% c("cli", "console") &&
        is.null(session$tools_filter))
}

# The session's tools as its table gives them, the availability tests
# run (git tools in a repository, a search key): once for a table and a
# filter, so a `git init` during the session, or a new system text,
# changes nothing. A table built again (a terminal /clear) tests again.
.prefix_base_tools <- function(session) {
    table <- session$tool_table
    key <- digest::digest(list(session$tools_filter,
            if (is.environment(table)) format(table)),
                          algo = "sha256")
    cached <- session$.prefix_base
    if (is.list(cached) && identical(cached$key, key)) {
        return(cached$tools)
    }
    ensure_skills()
    tools <- skills_as_api_tools(session$tools_filter, table)
    session$.prefix_base <- list(key = key, tools = tools)
    tools
}

# Compose the prefix: the session's tools (or the caller's `tools`) and
# its system text, by what the session is, never by its mode.
.prefix_compose <- function(session, tools = NULL) {
    if (is.null(tools)) {
        tools <- .prefix_base_tools(session)
    }
    tools <- .plan_mode_filter_tools(tools, .prefix_plan_capable(session),
                                     offer_exit = !isTRUE(session$plan_mode_locked))
    tools <- .task_filter_tools(tools, .task_list_on(session))
    tools <- .talker_filter_tools(tools, isTRUE(session$talker))
    tools <- .delegation_tools(tools, session)
    tools <- .room_consult_tools(tools, session)
    tools <- .room_media_tools(tools, session)
    system <- task_compose_system(session$system, list(),
                                  on = .task_list_on(session))
    list(system = system, tools = tools)
}

#' The system text and tools a session sends
#'
#' Composed once and cached on the session; composed again, and logged,
#' only when what it is made from changes.
#'
#' @param session A session environment.
#' @param tools NULL for the session's own tools, or a list the caller
#'   gives \code{turn()}.
#' @return A list with \code{system} and \code{tools}.
#' @noRd
session_request_prefix <- function(session, tools = NULL) {
    inputs <- .prefix_inputs(session, tools)
    key <- digest::digest(inputs, algo = "sha256")
    cached <- session$.request_prefix
    if (is.list(cached) && identical(cached$key, key)) {
        return(cached)
    }
    prefix <- .prefix_compose(session, tools)
    prefix$key <- key
    prefix$inputs <- inputs
    if (is.list(cached) && is.list(cached$inputs)) {
        changed <- Filter(function(n) {
            !identical(inputs[[n]], cached$inputs[[n]])
        }, names(inputs))
        log_event("request_prefix_reset", changed = as.list(changed))
    }
    session$.request_prefix <- prefix
    prefix
}

# Does the session's next request use the provider's own search? On
# when the session asks for it (default on), the provider supports it,
# and the llm.api build accepts the argument.
.session_ws_active <- function(session, ws = .session_web_search(session)) {
    !identical(ws, FALSE) &&
    .web_search_supported(session$provider) &&
    "web_search" %in% names(formals(llm.api::agent))
}

# The system text and tools the session's next request carries, as
# turn() sends them: for /context and the meter, and for a compaction's
# request, which keeps the provider's cache by sending the same.
session_request_parts <- function(session) {
    prefix <- session_request_prefix(session)
    tools <- .context_edit_tools(prefix$tools, session)
    list(system = .context_edit_system(prefix$system, session) %||% "",
         tools = .drop_redundant_web_search(tools, .session_ws_active(session)))
}

# The tool definition the session sends under `name`, or NULL.
.prefix_tool <- function(session, name) {
    tools <- session$.request_prefix$tools %||% list()
    for (tool in tools) {
        if (identical(tool$name, name)) {
            return(tool)
        }
    }
    NULL
}

# ---- Notes ----------------------------------------------------------------

# What the model was last told of the session's state. The system text
# describes a session with plan mode off, no /auto run and no tasks, so
# that is where a session starts.
.prefix_seen_default <- function() {
    list(plan_mode = FALSE, auto = FALSE, tasks = list())
}

.prefix_state <- function(session) {
    tasks <- if (.task_list_on(session)) {
        session$tasks %||% list()
    } else {
        list()
    }
    list(plan_mode = isTRUE(session$plan_mode),
         auto = !is.null(session$auto_run_id), tasks = tasks)
}

# The notes the next prompt opens with: each part of the state that
# differs from what the model was last told. Returns the notes and the
# state they bring the model to.
.prefix_notes <- function(session) {
    seen <- session$.prefix_seen %||% .prefix_seen_default()
    now <- .prefix_state(session)
    notes <- character()
    if (!identical(now$plan_mode, isTRUE(seen$plan_mode))) {
        notes <- c(notes, .plan_mode_note(now$plan_mode,
                locked = isTRUE(session$plan_mode_locked)))
    }
    if (!identical(now$auto, isTRUE(seen$auto))) {
        notes <- c(notes, .auto_note(now$auto))
    }
    if (!identical(now$tasks, seen$tasks %||% list())) {
        notes <- c(notes, .task_list_note(now$tasks))
    }
    list(notes = notes, seen = now)
}

# The prompt with its notes ahead of it. A prompt in parts, as one with
# an image is (llm.api's llm_content, R/media.R), gets the notes as a
# text part of its own in front, and its parts go on as they were.
.prefix_prompt <- function(prompt, notes) {
    if (!length(notes) || is.null(prompt)) {
        return(prompt)
    }
    text <- paste(notes, collapse = "\n\n")
    if (inherits(prompt, "llm_content")) {
        part <- structure(list(text = text), class = "llm_text")
        return(structure(c(list(part), unclass(prompt)), class = class(prompt)))
    }
    if (is.list(prompt)) {
        return(c(list(list(type = "text", text = text)), prompt))
    }
    paste(c(text, prompt), collapse = "\n\n")
}

# Record that the model has been told `field`, as a tool's result told
# it (exit_plan_mode), so the next prompt does not say it again.
.prefix_seen_set <- function(session, field, value) {
    seen <- session$.prefix_seen %||% .prefix_seen_default()
    seen[[field]] <- value
    session$.prefix_seen <- seen
    invisible(seen)
}

# Rewrites of the history that keep every message, so every note the
# model was given is still there: a thinking strip, a call closed with
# a placeholder, a seed put in front, a strip for a job's request.
# Any other rewrite (a compaction, a context edit, a restore) may have
# taken notes with it, and the next prompt states the session again.
.prefix_keeps_notes <- c("thinking_refused", "open_calls", "turn_error",
                         "rehydrate", "request_prefix")

.prefix_history_rewritten <- function(session, reason) {
    if (is.environment(session) && !reason %in% .prefix_keeps_notes) {
        session$.prefix_seen <- NULL
    }
    invisible(NULL)
}

# A request whose tools differ from the session's on purpose: a job that
# may not search sends the provider's tools without its search tool,
# and the request after the job sends them with it again. Each is a new
# prefix, so the history's thinking blocks, made under the other one,
# are removed first (history_replace(), which journals the strip), and
# the reset is logged.
.prefix_search_reset <- function(session, why) {
    history <- session$history %||% list()
    if (history_normalize(session, history)$removed > 0L) {
        history_replace(session, history, "request_prefix")
    }
    log_event("request_prefix_reset", changed = list("web_search"),
              reason = why)
    invisible(NULL)
}
