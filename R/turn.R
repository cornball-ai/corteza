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

# ---- Local model detection ----

# Cache the detected local model for the process lifetime so new_session
# doesn't hit the Ollama API on every call.
.local_model_cache <- new.env(parent = emptyenv())

#' Detect the preferred local Ollama model
#'
#' Walks \code{getOption("corteza.local_models")} (default
#' \code{c("gpt-oss:120b", "gpt-oss:20b")}) and returns the first one that
#' is currently installed in the local Ollama server. Returns NULL if
#' Ollama is unreachable or none of the candidates are installed.
#' Cached per R process.
#'
#' @return Character scalar model name, or NULL.
#' @export
default_local_model <- function() {
    if (isTRUE(.local_model_cache$initialized)) {
        return(.local_model_cache$value)
    }
    candidates <- getOption(
                            "corteza.local_models",
                            c("gpt-oss:120b", "gpt-oss:20b")
    )
    available <- tryCatch(
                          llm.api::list_ollama_models()$name,
                          error = function(e) character()
    )
    pick <- NULL
    for (m in candidates) {
        if (m %in% available) {
            pick <- m
            break
        }
    }
    .local_model_cache$value <- pick
    .local_model_cache$initialized <- TRUE
    pick
}

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
                               "corteza.model_map",
                               list(cloud = NULL, local = default_local_model())
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
    s$on_tool <- list()
    s$turn_number <- 0L
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
.fire_observers <- function(session, event) {
    observers <- session$on_tool %||% list()
    for (obs in observers) {
        tryCatch(obs(event), error = function(e) NULL)
    }
    invisible(NULL)
}

.make_tool_handler <- function(session, tool_executor = NULL) {
    if (is.null(tool_executor)) {
        ensure_skills()
        tool_executor <- function(name, args) call_skill(name, as.list(args))
    }
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
        session$turn_number <- (session$turn_number %||% 0L) + 1L

        start <- Sys.time()

        outcome_text <- function(kind, text, success) {
            event <- list(
                          call = call,
                          decision = decision,
                          outcome = kind,
                          result = text,
                          success = isTRUE(success),
                          elapsed_ms = as.numeric(
                    difftime(Sys.time(), start, units = "secs")
                ) * 1000,
                          turn_number = session$turn_number
            )
            .fire_observers(session, event)
            text
        }

        if (identical(decision$approval, "deny")) {
            return(outcome_text(
                                "deny",
                                sprintf("[corteza policy denied: %s]", decision$reason),
                                FALSE
                ))
        }
        if (identical(decision$approval, "ask")) {
            approved <- tryCatch(
                                 session$approval_cb(call, decision),
                                 error = function(e) FALSE
            )
            if (!isTRUE(approved)) {
                return(outcome_text(
                                    "declined",
                                    sprintf("[user declined: %s]", decision$reason),
                                    FALSE
                    ))
            }
        }

        raw <- tryCatch(
                        tool_executor(internal_name, as.list(args)),
                        error = function(e) err(paste("Tool error:", conditionMessage(e)))
        )
        success <- !isTRUE(raw$isError)
        outcome_text("ran", .flatten_mcp_result(raw), success)
    }
}

# Resolve the LLM model for the turn. Policy's per-call model routing
# decision is advisory at the turn level; we just pick the session's
# cloud (or local) default. A future PR can switch mid-turn.
#
# When the session's cloud model is unset AND no corteza.model option
# is set, we fill in a provider-specific default that's newer than
# llm.api's built-in defaults. (llm.api 0.1.1's moonshot default is
# 'kimi-k2', which Moonshot has renamed to 'kimi-k2.6'.) The override
# only fires when the caller hasn't picked a model themselves.
.resolve_model <- function(session) {
    explicit <- session$model_map$cloud %||% getOption("corteza.model", NULL)
    if (!is.null(explicit) && nzchar(explicit)) return(explicit)
    switch(session$provider %||% "anthropic",
           moonshot = "kimi-k2.6",
           NULL)
}

# ---- Public entry point ----

#' Build a tool executor that routes through an MCP connection
#'
#' Returns a closure suitable for the \code{tool_executor} argument of
#' \code{\link{turn}}. Each tool call is forwarded to the connected MCP
#' server via \code{llm.api::mcp_call}.
#'
#' @param conn An open MCP connection (from \code{llm.api::mcp_connect}).
#' @return A function with signature \code{function(name, args)} that
#'   returns an MCP-format result list.
#' @export
mcp_tool_executor <- function(conn) {
    force(conn)
    function(name, args) {
        llm.api::mcp_call(conn, name, args)
    }
}

#' Add a tool-call observer to a session
#'
#' Observers run after every tool call (run, denied, or declined). They
#' receive a single \code{event} list with fields:
#'
#' \itemize{
#'   \item \code{call} — the call list passed to \code{policy()}.
#'   \item \code{decision} — the policy decision for the call.
#'   \item \code{outcome} — one of \code{"ran"}, \code{"deny"},
#'     \code{"declined"}.
#'   \item \code{result} — the string returned to the LLM.
#'   \item \code{success} — logical; TRUE only for \code{"ran"} with no
#'     tool error.
#'   \item \code{elapsed_ms} — wall time including policy overhead.
#'   \item \code{turn_number} — the session's tool-call counter.
#' }
#'
#' Errors raised inside an observer are swallowed.
#'
#' @param session A session environment from \code{\link{new_session}}.
#' @param observer A function of one argument (the event list).
#'
#' @return The session, invisibly.
#' @export
add_observer <- function(session, observer) {
    stopifnot(is.environment(session), is.function(observer))
    session$on_tool <- c(session$on_tool, list(observer))
    invisible(session)
}

#' Built-in progress observer that prints to stdout
#'
#' Prints one line per tool call suitable for an interactive REPL:
#' \code{"  [tool] hint (N lines)\n"}. The hint is a short summary of
#' the call (file path, code snippet, search pattern) computed by
#' \code{tool_hint()}.
#'
#' @return A function to pass to \code{\link{add_observer}}.
#' @export
observer_progress <- function() {
    function(event) {
        hint <- tool_hint(event$call$tool, event$call$args)
        if (!identical(event$outcome, "ran")) {
            cat(sprintf("  [%s]%s %s\n",
                        event$call$tool,
                        hint,
                        sub("^\\[", "", sub("\\]$", "", event$result))))
            return(invisible())
        }
        result <- event$result %||% ""
        lines <- if (nchar(result) > 0L) {
            length(strsplit(result, "\n", fixed = TRUE)[[1L]])
        } else {
            0L
        }
        if (isTRUE(event$success)) {
            status <- ""
        } else {
            status <- " (error)"
        }
        cat(sprintf("  [%s]%s (%d lines)%s\n",
                    event$call$tool, hint, lines, status))
    }
}

#' Run one agent turn
#'
#' Sends \code{prompt} to the configured LLM with tool use enabled. Every
#' tool call the LLM makes is routed through \code{\link{policy}} before
#' being dispatched.
#'
#' Tool dispatch is pluggable via \code{tool_executor}. The default is an
#' in-process dispatcher that calls the local skill registry — suitable
#' for \code{chat()} and matrix adapters running in the same R process as
#' their skills. Pass \code{\link{mcp_tool_executor}} (or any
#' \code{function(name, args) -> MCP-format result}) to run tools in a
#' separate process, which is how the CLI talks to \code{serve()}.
#'
#' @param prompt Character. User prompt.
#' @param session A session environment created by \code{\link{new_session}}.
#' @param tool_executor Function or NULL. Dispatcher with signature
#'   \code{function(name, args) -> list}. NULL uses the in-process
#'   \code{call_skill} path.
#' @param tools List or NULL. Tool schemas to pass the LLM. NULL uses
#'   the in-process skill registry (filtered by \code{session$tools_filter}).
#'   Pass explicit schemas when running against a remote skill source.
#'
#' @return A list with \code{reply} (character) and \code{session} (the
#'   updated session environment; also mutated in place).
#' @export
turn <- function(prompt, session, tool_executor = NULL, tools = NULL) {
    stopifnot(is.environment(session))

    if (is.null(tools)) {
        ensure_skills()
        tools <- skills_as_api_tools(session$tools_filter)
    }
    tool_handler <- .make_tool_handler(session, tool_executor = tool_executor)

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

    list(
         reply = response$content,
         session = session,
         usage = response$usage,
         raw = response
    )
}

