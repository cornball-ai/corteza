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

#' Spawn and initialize a CLI worker session.
#'
#' Starts a fresh `callr::r_session`, loads corteza inside it, runs
#' `worker_init()` so skills are registered, and returns a handle the
#' CLI treats opaquely. The CLI (and any other caller that drives tools
#' through a private subprocess) should use this rather than invoking
#' `callr::r_session$new()` directly.
#'
#' @param cwd Working directory for the worker.
#' @param tools_filter Optional filter for `worker_tool_list()`.
#' @return A list with `session` (the `callr::r_session` instance),
#'   `tools` (tool list ready for LLM API conversion), and `cwd`.
#' @importFrom callr r_session
#' @keywords internal
#' @export
cli_worker_spawn <- function(cwd = getwd(), tools_filter = NULL) {
    session <- callr::r_session$new(wait = TRUE)
    session$run(
        function(cwd) {
            library(corteza)
            corteza::worker_init(cwd = cwd)
        },
        list(cwd = cwd)
    )
    tools <- session$run(
        function(filter, cwd) corteza::worker_tool_list(filter, cwd = cwd),
        list(tools_filter, cwd)
    )
    structure(list(session = session, tools = tools, cwd = cwd),
              class = "corteza_cli_worker")
}
