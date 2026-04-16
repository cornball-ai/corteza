# Shared agent turn.
#
# turn(prompt, session) is the single entry point used by all three
# channel adapters (cli, console, matrix). It runs the llm.api agent
# loop and applies the policy engine to every tool call the LLM makes.
#
# Session is an environment (mutable across tool calls within a turn):
#   channel        one of "cli", "console", "matrix"
#   history        list of prior messages (may be NULL)
#   model_map      list(cloud = "...", local = "...") or NULL
#   provider       "anthropic" | "openai" | "moonshot" | "ollama"
#   tools_filter   character vector of tool names/categories, or NULL
#   system         character, system prompt override (or NULL for default)
#   approval_cb    function(call, decision) -> TRUE|FALSE
#   recent_classes character, sticky data classes from earlier tool calls
#   max_turns      integer, max LLM turns per call
#   verbose        logical

# ---- Session construction ----

#' Create a new turn session
#'
#' Returns an environment with sensible defaults. Adapters set channel-
#' specific fields (e.g. \code{approval_cb}, \code{tools_filter}) before
#' calling \code{\link{turn}}.
#'
#' @param channel Character, one of "cli", "console", "matrix".
#' @param history List of prior messages, or NULL.
#' @param model_map Named list with \code{cloud} and \code{local} model
#'   names. Defaults to configured defaults.
#' @param provider LLM provider passed to \code{llm.api::agent}.
#' @param tools_filter Character vector passed to \code{get_tools()}.
#' @param system System prompt override (NULL for built-in default).
#' @param approval_cb Function called when policy returns \code{"ask"}.
#'   Signature: \code{function(call, decision) -> TRUE|FALSE}. Default
#'   denies (safe fallback).
#' @param max_turns Maximum LLM turns per call.
#' @param verbose Print tool call progress.
#'
#' @return An environment holding the session state.
#' @export
new_session <- function(channel = c("cli", "console", "matrix"),
                        history = NULL, model_map = NULL,
                        provider = "anthropic", tools_filter = NULL,
                        system = NULL, approval_cb = NULL, max_turns = 10L,
                        verbose = FALSE) {
    channel <- match.arg(channel)
    if (is.null(model_map)) {
        model_map <- getOption(
                               "llamaR.model_map",
                               list(cloud = NULL, local = NULL)
        )
    }
    if (is.null(approval_cb)) {
        approval_cb <- function(call, decision) FALSE
    }

    s <- new.env(parent = emptyenv())
    s$channel <- channel
    s$history <- history
    s$model_map <- model_map
    s$provider <- provider
    s$tools_filter <- tools_filter
    s$system <- system
    s$approval_cb <- approval_cb
    s$max_turns <- as.integer(max_turns)
    s$verbose <- isTRUE(verbose)
    s$recent_classes <- character()
    s
}

# ---- Internal helpers ----

# Convert an MCP-format skill result (list with $content) to a plain string
# that llm.api::agent expects from tool_handler.
.flatten_mcp_result <- function(result) {
    if (is.character(result) && length(result) == 1L) {
        return(result)
    }
    if (!is.list(result)) {
        return(as.character(result))
    }

    content <- result$content
    if (is.null(content)) {
        return(as.character(result))
    }

    parts <- vapply(content, function(block) {
        if (!is.null(block$text)) {
            as.character(block$text)
        } else {
            ""
        }
    }, character(1))
    text <- paste(parts, collapse = "\n")
    if (isTRUE(result$isError)) {
        paste0("Error: ", text)
    } else {
        text
    }
}

# Build the closure passed as tool_handler to llm.api::agent. Closes over
# the session so sticky classifications persist across tool calls.
.make_tool_handler <- function(session) {
    ensure_skills()
    function(name, args) {
        internal_name <- unsanitize_tool_name(name)
        call <- list(
                     tool = internal_name,
                     args = as.list(args),
                     channel = session$channel,
                     context = list(recent_classes = session$recent_classes)
        )
        # Resolve once up front so policy() and the sticky classifier
        # below see the same paths/urls.
        call$paths <- resolve_paths(call)
        call$urls <- resolve_urls(call)
        decision <- policy(call)

        # Sticky: record the class regardless of the decision outcome.
        # Even a denied tool call means the LLM is trying to touch that
        # data class, so downstream calls should inherit it.
        klass <- classify_data(call,
                               list(recent_classes = session$recent_classes))
        session$recent_classes <- unique(c(session$recent_classes, klass))

        if (identical(decision$approval, "deny")) {
            return(sprintf("[llamaR policy denied: %s]", decision$reason))
        }
        if (identical(decision$approval, "ask")) {
            approved <- tryCatch(
                                 session$approval_cb(call, decision),
                                 error = function(e) FALSE
            )
            if (!isTRUE(approved)) {
                return(sprintf("[user declined: %s]", decision$reason))
            }
        }

        result <- tryCatch(
                           call_skill(internal_name, as.list(args)),
                           error = function(e) err(paste("Tool error:", conditionMessage(e)))
        )
        .flatten_mcp_result(result)
    }
}

# Resolve the LLM model for the turn. Policy's per-call model routing
# decision is advisory at the turn level; we just pick the session's
# cloud (or local) default. A future PR can switch mid-turn.
.resolve_model <- function(session) {
    session$model_map$cloud %||% getOption("llamaR.model", NULL)
}

# ---- Public entry point ----

#' Run one agent turn
#'
#' Sends \code{prompt} to the configured LLM with tool use enabled. Every
#' tool call the LLM makes is routed through \code{\link{policy}} before
#' being dispatched to the skill registry.
#'
#' @param prompt Character. User prompt.
#' @param session A session environment created by \code{\link{new_session}}.
#'
#' @return A list with \code{reply} (character) and \code{session} (the
#'   updated session environment; also mutated in place).
#' @export
turn <- function(prompt, session) {
    stopifnot(is.environment(session))

    ensure_skills()
    tools <- skills_as_api_tools(session$tools_filter)
    tool_handler <- .make_tool_handler(session)

    response <- llm.api::agent(
                               prompt = prompt,
                               tools = tools,
                               tool_handler = tool_handler,
                               system = session$system,
                               model = .resolve_model(session),
                               provider = session$provider,
                               max_turns = session$max_turns,
                               verbose = session$verbose,
                               history = session$history
    )

    if (!is.null(response$history)) {
        session$history <- response$history
    }

    list(reply = response$content, session = session)
}

