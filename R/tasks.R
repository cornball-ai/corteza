# Persistent task list the LLM maintains across turns for multi-step
# work. Distinct from R/plan-mode.R, which is a one-shot
# research-and-propose flow; this one tracks ongoing progress.
#
# Architecture note. The task tools (task_create, task_update) are
# *not* dispatched through the normal skill executor. chat() runs
# skill handlers with ctx = list() in the main process; inst/bin/corteza
# dispatches via a callr worker subprocess. Either path would leave a
# normal handler unable to mutate the live session. The fix: intercept
# task_create / task_update inside .make_tool_handler() before any
# executor call, mutating the main-process session env directly.
# Skill registration is still required so the API tool list advertises
# them to the LLM, but the registered handler is a stub the intercept
# bypasses.

# Allowed status values for a task.
.TASK_STATUSES <- c("pending", "in_progress", "completed", "cancelled")

# Status -> single-character ASCII marker for the displayed list.
# ASCII (not Unicode boxes) so the rendering is /copy- and
# pipe-to-file-safe in any terminal.
.TASK_MARKER <- c(pending = " ", in_progress = ">", completed = "x",
                  cancelled = "-")

# Validate / normalize a status string. Errors on unknown.
# @noRd
.task_status_canonical <- function(status) {
    if (length(status) != 1L || !is.character(status)) {
        stop("status must be a single string", call. = FALSE)
    }
    if (!(status %in% .TASK_STATUSES)) {
        stop(sprintf("status must be one of: %s (got %s)",
                     paste(.TASK_STATUSES, collapse = ", "), status),
             call. = FALSE)
    }
    status
}

# 1-based index validity check.
# @noRd
.task_index_valid <- function(index, tasks) {
    if (length(tasks) == 0L) {
        return(FALSE)
    }
    if (length(index) != 1L || !is.numeric(index)) {
        return(FALSE)
    }
    idx <- as.integer(index)
    idx >= 1L && idx <= length(tasks)
}

#' Apply task_create to a session. Replaces any existing task list.
#' Each new task starts as `pending`.
#'
#' Mutates `session$tasks` in place (session is an environment).
#' Returns the new list-of-list for the caller's logging.
#' @noRd
task_create_apply <- function(session, tasks) {
    if (is.list(tasks)) {
        tasks <- unlist(tasks, use.names = FALSE)
    }
    if (!is.character(tasks) || length(tasks) == 0L) {
        stop("tasks must be a non-empty character vector", call. = FALSE)
    }
    new_list <- lapply(tasks, function(t) {
        list(text = as.character(t), status = "pending")
    })
    session$tasks <- new_list
    session$tasks_dirty <- TRUE
    new_list
}

#' Apply task_update. When promoting a task to `in_progress`, any
#' other `in_progress` task is auto-demoted to `pending` so the
#' "exactly one active" invariant holds without rejecting valid
#' transitions.
#' @noRd
task_update_apply <- function(session, index, status) {
    tasks <- session$tasks %||% list()
    if (!.task_index_valid(index, tasks)) {
        stop(sprintf("index %s is out of range (have %d task%s)", index,
                     length(tasks), if (length(tasks) == 1L) "" else "s"),
             call. = FALSE)
    }
    status <- .task_status_canonical(status)
    idx <- as.integer(index)
    if (identical(status, "in_progress")) {
        for (j in seq_along(tasks)) {
            if (j != idx && identical(tasks[[j]]$status, "in_progress")) {
                tasks[[j]]$status <- "pending"
            }
        }
    }
    tasks[[idx]]$status <- status
    session$tasks <- tasks
    session$tasks_dirty <- TRUE
    tasks
}

#' Format the task list for the system-prompt addendum the LLM sees
#' each turn. Plain text; the LLM doesn't see ANSI. Empty list ->
#' empty string.
#' @noRd
format_task_list_prompt <- function(tasks) {
    if (length(tasks) == 0L) {
        return("")
    }
    lines <- vapply(seq_along(tasks), function(i) {
        t <- tasks[[i]]
        marker <- .TASK_MARKER[[t$status]]
        sprintf("%d. [%s] %s", i, marker, t$text)
    }, character(1))
    paste(c(
            "",
            "# Active tasks",
            "",
            lines,
            "",
            paste("Maintain this list. Mark a task in_progress when",
                  "you start it, completed when done. Keep at most",
                  "one task in_progress at a time. When the user",
                  "gives you a multi-step request (3+ steps), call",
                  "task_create at the start to track it.")
        ),
          collapse = "\n"
    )
}

#' Compose the system prompt for this turn, including any task list.
#' Returns `base_system` unchanged if there are no tasks.
#' @noRd
task_compose_system <- function(base_system, tasks) {
    if (length(tasks) == 0L) {
        return(base_system)
    }
    addendum <- format_task_list_prompt(tasks)
    if (is.null(base_system) || !nzchar(base_system)) {
        return(addendum)
    }
    paste(base_system, addendum, sep = "\n")
}

#' Render a task list for terminal display. Empty list -> NULL so the
#' caller can skip printing.
#' @noRd
format_task_list_display <- function(tasks, palette = ansi_colors()) {
    if (length(tasks) == 0L) {
        return(NULL)
    }
    color_for <- function(status) {
        switch(status, pending = palette$dim,
               in_progress = palette$bright_yellow, completed = palette$green,
               cancelled = palette$dim, palette$reset)
    }
    lines <- vapply(seq_along(tasks), function(i) {
        t <- tasks[[i]]
        marker <- .TASK_MARKER[[t$status]]
        col <- color_for(t$status)
        sprintf("  %s%d. [%s] %s%s", col, i, marker, t$text, palette$reset)
    }, character(1))
    paste(c(sprintf("%sTasks:%s", palette$dim, palette$reset), lines),
          collapse = "\n")
}

#' Create a list of tasks to track for the current request.
#'
#' Call this at the start of any multi-step user request (3+ steps)
#' to commit to a visible plan. The list persists across turns and
#' replaces any prior list. After creating, update each task's
#' status with `task_update` as you work: mark a task `in_progress`
#' when you start it, `completed` when done, `cancelled` if the
#' user redirects.
#'
#' @param tasks (character vector) One task description per element.
#'   Order matters; task_update references tasks by 1-based index.
#' @return Confirmation string.
#' @keywords internal
#' @export
tool_task_create <- function(tasks) {
    # The real implementation runs in .make_tool_handler() via
    # task_tool_intercept(); reaching this body means the intercept
    # was bypassed, which is a corteza bug worth surfacing rather
    # than silently mutating a worker-process session.
    err("task_create reached its skill handler instead of the in-process intercept")
}

#' Update one task's status in the current session task list.
#'
#' @param index (integer) 1-based position of the task to update.
#' @param status (character) New status. One of `pending`,
#'   `in_progress`, `completed`, `cancelled`. Promoting a task to
#'   `in_progress` automatically demotes any other `in_progress`
#'   task to `pending`, so there is at most one active task at a
#'   time.
#' @return Confirmation string.
#' @keywords internal
#' @export
tool_task_update <- function(index, status) {
    err("task_update reached its skill handler instead of the in-process intercept")
}

#' Try to handle `name` as a task tool by mutating `session` directly.
#' Returns a character result on hit, NULL when the tool isn't a task
#' tool (caller falls through to normal dispatch).
#' @noRd
task_tool_intercept <- function(session, name, args) {
    if (!(name %in% c("task_create", "task_update"))) {
        return(NULL)
    }
    tryCatch({
        if (identical(name, "task_create")) {
            new <- task_create_apply(session, args$tasks)
            sprintf("Created %d task%s.", length(new),
                if (length(new) == 1L) "" else "s")
        } else {
            new <- task_update_apply(session, args$index, args$status)
            sprintf("Task %s -> %s.", args$index, args$status)
        }
    },
             error = function(e) sprintf("[task error: %s]", conditionMessage(e))
    )
}
