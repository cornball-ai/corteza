# Worker-side dispatch for the CLI transport.
#
# The CLI uses callr::r_session to run an isolated R process. Tool
# invocations cross the boundary by calling worker_dispatch() inside the
# session via session$call(). This file is the single worker-side entry
# point: everything the CLI needs to do to a tool goes through here.
#
# serve() (public MCP) uses its own JSON-RPC handler in R/mcp-handler.R
# and is not affected by this file.

#' Worker-side tool dispatch.
#'
#' Called from the CLI over `callr::r_session$run()`. Looks up the skill
#' in the registry, runs it, and normalizes any dispatch-level failures
#' as a `corteza_tool_error` condition. Tool-body failures that are
#' already caught by `skill_run()` remain as `err()` envelopes.
#'
#' Exported (with `@keywords internal`) because it runs inside a
#' `callr::r_session` child process, where `corteza:::` would otherwise
#' trip the R CMD check "calls to the package's namespace" NOTE.
#'
#' @param name Tool name.
#' @param args Named list of arguments.
#' @param ctx Optional context (cwd, session metadata).
#' @param timeout Timeout in seconds.
#' @param dry_run If TRUE, preview only.
#' @return MCP-shaped tool result list (content, isError).
#' @keywords internal
#' @export
worker_dispatch <- function(name, args, ctx = list(), timeout = 30L,
                            dry_run = FALSE) {
    if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        stop(make_tool_error("<unknown>", args,
                             "tool name must be a single non-empty string"))
    }
    if (is.null(get_skill(name))) {
        stop(make_tool_error(name, args,
                             sprintf("unknown tool: %s", name)))
    }
    tryCatch(
        call_tool(name, args, ctx = ctx, timeout = timeout, dry_run = dry_run),
        error = function(e) {
            if (inherits(e, "corteza_tool_error")) stop(e)
            stop(make_tool_error(name, args, conditionMessage(e), e))
        }
    )
}

#' Worker-side tool listing.
#'
#' Returns the full tool definition list the CLI needs to build its LLM
#' API `tools` payload. Ensures built-in skills and user skills are
#' loaded before listing.
#'
#' Exported (with `@keywords internal`) for the same reason as
#' `worker_dispatch()`.
#'
#' @param filter Optional tool-name or category filter; see get_tools().
#' @param cwd Project root for project-local skill discovery.
#' @return List of tool definitions.
#' @keywords internal
#' @export
worker_tool_list <- function(filter = NULL, cwd = getwd()) {
    ensure_skills()
    load_skills(corteza_data_path("skills"))
    load_skills(file.path(cwd, ".corteza", "skills"))
    get_tools(filter)
}

#' Worker-side initialization.
#'
#' Called once after the callr session starts. Sets up cwd, loads the
#' package, registers skills. Separate from worker_dispatch so session
#' init is explicit and inspectable.
#'
#' Exported (with `@keywords internal`) for the same reason as
#' `worker_dispatch()`.
#'
#' @param cwd Working directory for the worker.
#' @return Invisible TRUE on success.
#' @keywords internal
#' @export
worker_init <- function(cwd = getwd()) {
    setwd(cwd)
    ensure_skills()
    load_skills(corteza_data_path("skills"))
    load_skills(file.path(cwd, ".corteza", "skills"))
    invisible(TRUE)
}

#' Drain structured events the worker wrote to stderr while a tool
#' ran. When `trace` is TRUE each event is pretty-printed via
#' [printify::print_step()] / [printify::print_message()]; otherwise
#' the events are still read (to keep the stderr buffer from growing)
#' but not displayed.
#'
#' @param session A `callr::r_session`.
#' @param trace Pretty-print events if TRUE.
#' @return Invisible NULL.
#' @importFrom printify print_step print_message
#' @keywords internal
#' @export
cli_worker_drain_events <- function(session, trace = FALSE) {
    lines <- tryCatch(session$read_error_lines(),
                      error = function(e) character())
    if (length(lines) == 0L) return(invisible(NULL))
    if (!isTRUE(trace)) return(invisible(NULL))
    pretty <- requireNamespace("printify", quietly = TRUE) &&
        isTRUE(tryCatch(isatty(stdout()), error = function(e) FALSE))
    for (line in lines) {
        event <- tryCatch(
            jsonlite::fromJSON(line, simplifyVector = TRUE),
            error = function(e) NULL
        )
        if (is.null(event) || is.null(event$event)) next
        .cli_render_event(event, pretty = pretty)
    }
    invisible(NULL)
}

.cli_render_event <- function(event, pretty = FALSE) {
    tool <- event$tool %||% ""
    elapsed <- event$elapsed_ms
    detail <- if (!is.null(elapsed)) {
        sprintf("%s (%dms)", tool, elapsed)
    } else {
        tool
    }
    if (identical(event$event, "tool_call")) {
        label <- sprintf("tool_call: %s", tool)
        if (pretty) printify::print_step(label)
        else .plain_trace(label, color = 90L)
    } else if (identical(event$event, "tool_result")) {
        ok <- isTRUE(event$success)
        label <- if (ok) sprintf("tool_result: %s", detail)
                 else sprintf("tool_error: %s", detail)
        if (pretty) {
            if (ok) printify::print_step(label)
            else printify::print_message(label)
        } else {
            .plain_trace(label, color = if (ok) 32L else 31L)
        }
    } else if (!is.null(event$level) &&
               event$level %in% c("warn", "error")) {
        msg <- event$message %||% event$event
        label <- sprintf("%s: %s", event$level, msg)
        if (pretty) printify::print_message(label)
        else .plain_trace(label, color = 33L)
    }
}

.plain_trace <- function(text, color = 90L) {
    cat(sprintf("\033[%dm  %s\033[0m\n", color, text), file = stderr())
}

#' Spawn and initialize a CLI worker session.
#'
#' Starts a fresh `callr::r_session`, loads corteza inside it, and runs
#' `worker_init()` so skills are registered in the session. Returns an
#' opaque handle the CLI uses for tool dispatch.
#'
#' Schema production is CLI-side. The worker registers skills only so
#' it can execute them; the CLI builds the LLM `tools` payload from its
#' own registry via `schema_from_registry()`. Nothing about schema
#' shape travels over the worker pipe.
#'
#' @param cwd Working directory for the worker.
#' @return A list with `session` (the `callr::r_session` instance) and
#'   `cwd`, with class `corteza_cli_worker`.
#' @importFrom callr r_session
#' @keywords internal
#' @export
cli_worker_spawn <- function(cwd = getwd()) {
    session <- callr::r_session$new(wait = TRUE)
    session$run(
        function(cwd) {
            library(corteza)
            corteza::worker_init(cwd = cwd)
        },
        list(cwd = cwd)
    )
    structure(list(session = session, cwd = cwd),
              class = "corteza_cli_worker")
}
