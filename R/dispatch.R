# Runtime init for subagent callr sessions plus trace-event rendering.
#
# worker_init() runs inside a subagent's private callr::r_session to set
# cwd and register skills. .cli_render_event()/.plain_trace() render
# structured trace events for the CLI's in-process --trace observer.
#
# The CLI worker transport (a callr-backed tool dispatch channel) was
# removed once the CLI moved to in-process tool execution; only the
# subagent-side init and the trace renderer remain here.

#' Subagent session initialization.
#'
#' Runs inside a subagent's private `callr::r_session` subprocess
#' (invoked via `corteza::worker_init()` across the callr boundary).
#' Sets the subprocess cwd, ensures built-in skills are registered, and
#' loads user/project skills so the subagent can execute tools.
#'
#' Exported (with `@keywords internal`) because it is called as
#' `corteza::worker_init()` from inside the `callr::r_session` child,
#' where `corteza:::` would trip the R CMD check "calls to the package's
#' namespace" NOTE.
#'
#' @param cwd Working directory for the subagent session.
#' @return Invisible TRUE on success.
#' @keywords internal
#' @export
worker_init <- function(cwd = getwd()) {
    # Runs in a private callr::r_session subprocess, never in the
    # user's main R session: the whole point of worker_init is to set
    # the subprocess cwd for its lifetime so every tool the subagent
    # runs inherits it. on.exit(setwd(oldwd)) would undo that
    # immediately. Justified in cran-comments.md.
    setwd(cwd)
    ensure_skills()
    load_skills(corteza_data_path("skills"))
    load_skills(file.path(cwd, ".corteza", "skills"))
    invisible(TRUE)
}

.cli_render_event <- function(event, pretty = FALSE) {
    summary <- cli_event_summary(event, width = 88L)
    detail <- paste(summary$detail_lines, collapse = " | ")

    if (identical(summary$kind, "start")) {
        if (pretty) {
            printify::print_step("minor", summary$title)
            for (line in summary$detail_lines) {
                printify::print_message("neutral", paste(" ", line))
            }
        } else {
            .plain_trace(summary$title, color = 90L)
            for (line in summary$detail_lines) {
                .plain_trace(paste(" ", line), color = 90L)
            }
        }
    } else if (identical(summary$kind, "ok")) {
        label <- paste(summary$title, detail)
        if (pretty) {
            printify::print_message("note", label)
        } else {
            .plain_trace(label, color = 32L)
        }
    } else if (identical(summary$kind, "error")) {
        label <- paste(summary$title, detail)
        if (pretty) {
            printify::print_message("error", label)
        } else {
            .plain_trace(label, color = 31L)
        }
    } else if (identical(summary$kind, "warn") ||
        identical(summary$kind, "error")) {
        label <- paste(summary$title, detail)
        if (pretty) {
            printify::print_message(
                if (identical(summary$kind, "warn")) "warning" else "error",
                                    label
            )
        } else {
            .plain_trace(label, color = 33L)
        }
    }
}

.plain_trace <- function(text, color = 90L) {
    if (.corteza_verbose()) {
        cat(sprintf("\033[%dm  %s\033[0m\n", color, text), file = stderr())
    }
}

