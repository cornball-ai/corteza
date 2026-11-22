# Plan mode.
#
# Plan mode is a session-scoped flag (set on the turn session env). When
# on:
#   1. `policy()` denies write/exec tool calls via `check_plan_mode()`,
#      except for `exit_plan_mode` itself.
#   2. The next prompt opens with a note saying so (.plan_mode_note(),
#      R/request-prefix.R), and another when it ends. The system text
#      and the tool list stay as they are (WP14).
#   3. `exit_plan_mode` is in the tool list of every session that can be
#      in plan mode, and refused at the call outside it.
#   4. `policy()` asks a person before `exit_plan_mode` runs
#      (`check_plan_exit()`, rule "plan_exit"): the approval shows the
#      plan, and no saved "always", user policy or config answers it. A
#      no keeps plan mode, and the model is told so.
#   5. A successful `exit_plan_mode` call (handled in
#      `.make_tool_handler`) flips `session$plan_mode` back to FALSE.
#
# Subagents inherit plan_mode from their parent via `subagent_spawn()`.
# A job's worker reads it from its conversation at each call
# (R/job-authority.R) and cannot end it (`session$plan_mode_locked`):
# it is not offered `exit_plan_mode`, the handler refuses the call by
# name, and its note says the person ends plan mode.

#' Submit a plan and exit plan mode.
#'
#' Called by the LLM after it has finished research in plan mode. The
#' plan text is shown to the user; on approval the session leaves plan
#' mode and the LLM proceeds with the work. On decline the session
#' stays in plan mode and the LLM iterates.
#'
#' Every session that can be in plan mode is offered this tool, so its
#' tool list stays the same when plan mode starts or ends. Outside plan
#' mode the call is refused.
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

# Add exit_plan_mode to (or strip it from) a tool list. The session's
# prefix (R/request-prefix.R) calls it once with `in_plan_mode` saying
# whether the session can be in plan mode at all; a session that may
# not leave plan mode (`offer_exit = FALSE`) is never offered it.
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

# The note the next prompt opens with when plan mode starts or ends
# (R/request-prefix.R). Kept short: the policy engine enforces the
# constraint; the model just needs to know what to produce. A job's
# worker cannot leave plan mode: the conversation that gave it the job
# is in plan mode, and the person ends it.
.plan_mode_note <- function(on, locked = FALSE) {
    if (!isTRUE(on)) {
        return(paste("[Plan mode is off. Editing files, running shell",
                     "commands and executing R code are allowed again, as",
                     "the approval rules say.]"))
    }
    if (isTRUE(locked)) {
        return(paste("[Plan mode is on: the conversation that gave you this",
                     "job is in plan mode. Research with read-only tools.",
                     "Editing files, running shell commands and executing R",
                     "code will be denied. Report what you would change, in",
                     "which files, and why. Only the person can end plan",
                     "mode.]"))
    }
    paste("[Plan mode is on. Research with read-only tools (read_file,",
          "list_files, grep_files, git_log, git_diff, git_status, r_help,",
          "web_search, fetch_url). Do not edit files, run shell commands, or",
          "execute R code -- those tools will be denied. When you have a",
          "plan, call `exit_plan_mode` with a concrete markdown plan",
          "describing what will change, in which files, and why. The user",
          "approves the plan, then plan mode lifts and you proceed.]")
}
