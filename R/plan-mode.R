# Plan mode.
#
# Plan mode is a session-scoped flag (set on the turn session env). When
# on:
#   1. `policy()` denies write/exec tool calls via `check_plan_mode()`,
#      except for `exit_plan_mode` itself.
#   2. `turn()` appends a plan-mode addendum to the system prompt so the
#      LLM knows to research and propose rather than act.
#   3. `turn()` adds `exit_plan_mode` to the tool list, hidden the rest
#      of the time so the LLM doesn't see it when it can't use it.
#   4. A successful `exit_plan_mode` call (handled in
#      `.make_tool_handler`) flips `session$plan_mode` back to FALSE.
#
# Subagents inherit plan_mode from their parent via `subagent_spawn()`.
# A job's worker reads it from its conversation at each call
# (R/job-authority.R) and cannot end it (`session$plan_mode_locked`):
# it is not offered `exit_plan_mode`, the handler refuses the call by
# name, and its addendum says the person ends plan mode.

#' Submit a plan and exit plan mode.
#'
#' Called by the LLM after it has finished research in plan mode. The
#' plan text is shown to the user; on approval the session leaves plan
#' mode and the LLM proceeds with the work. On decline the session
#' stays in plan mode and the LLM iterates.
#'
#' This tool is only exposed when the session is in plan mode. Outside
#' plan mode it is hidden from the LLM's tool list.
#'
#' @param plan (character) Markdown-formatted implementation plan. State
#'   what will change, in which files, and why. Be concrete.
#' @return An MCP tool-result list.
#' @keywords internal
#' @export
tool_exit_plan_mode <- function(plan) {
    if (!is.character(plan) || length(plan) != 1L || !nzchar(plan)) {
        return(err("plan must be a non-empty string"))
    }
    ok("Plan approved. Proceeding with implementation.")
}

# Add exit_plan_mode to (or strip it from) a tool list before it is
# shipped to the LLM. Hidden the rest of the time so the LLM only sees
# the escape hatch when it can actually use it, and always from a
# session that may not leave plan mode (`offer_exit = FALSE`).
.plan_mode_filter_tools <- function(tools, in_plan_mode, offer_exit = TRUE) {
    has_exit <- vapply(tools, function(t) {
        identical(t$name %||% "", "exit_plan_mode")
    }, logical(1))

    if (isTRUE(in_plan_mode) && isTRUE(offer_exit)) {
        if (any(has_exit)) {
            return(tools)
        }
        skill <- tryCatch(get_skill("exit_plan_mode"), error = function(e) NULL)
        if (is.null(skill)) {
            return(tools)
        }
        tools <- c(tools, list(list(name = sanitize_tool_name(skill$name),
                                    description = skill$description,
                                    input_schema = skill$inputSchema)))
        return(tools)
    }

    if (any(has_exit)) {
        return(tools[!has_exit])
    }
    tools
}

# System-prompt addendum the LLM sees while plan mode is on. Kept
# short: the policy engine enforces the constraint; the LLM just needs
# to know what to produce.
.plan_mode_system_addendum <- function() {
    paste(
          "",
          "# Plan mode",
          "",
          paste("You are in plan mode. Research with read-only tools",
                "(read_file, list_files, grep_files, git_log, git_diff,",
                "git_status, r_help, web_search, fetch_url). Do not edit",
                "files, run shell commands, or execute R code -- those",
                "tools will be denied. When you have a plan, call",
                "`exit_plan_mode` with a concrete markdown plan describing",
                "what will change, in which files, and why. The user",
                "approves the plan, then plan mode lifts and you proceed.",
                sep = " "),
          sep = "\n")
}

# The addendum for a job's worker, which cannot leave plan mode: the
# conversation that gave it the job is in plan mode, and the person
# ends it.
.plan_mode_worker_addendum <- function() {
    paste(
          "",
          "# Plan mode",
          "",
          paste("The conversation that gave you this job is in plan mode.",
                "Research with read-only tools. Editing files, running",
                "shell commands and executing R code will be denied.",
                "Report what you would change, in which files, and why.",
                "Only the person can end plan mode.", sep = " "),
          sep = "\n")
}

# Compose the system prompt for this turn. Outside plan mode this is
# the session's stored system prompt verbatim. Inside plan mode we
# append the addendum so the LLM knows the constraints; a session that
# may not leave plan mode (`locked`) gets the worker's.
.plan_mode_compose_system <- function(base_system, in_plan_mode,
                                      locked = FALSE) {
    if (!isTRUE(in_plan_mode)) {
        return(base_system)
    }
    addendum <- if (isTRUE(locked)) {
        .plan_mode_worker_addendum()
    } else {
        .plan_mode_system_addendum()
    }
    if (is.null(base_system) || !nzchar(base_system)) {
        return(addendum)
    }
    paste(base_system, addendum, sep = "\n")
}
