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
#   provider       "anthropic" | "openai" | "moonshot" | "openai_codex" | "ollama"
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
#' @examples
#' # NULL when Ollama isn't running locally; a model name otherwise.
#' model <- default_local_model()
#' is.null(model) || is.character(model)
#' @export
default_local_model <- function() {
    if (isTRUE(.local_model_cache$initialized)) {
        return(.local_model_cache$value)
    }
    candidates <- getOption("corteza.local_models",
                            c("gpt-oss:120b", "gpt-oss:20b"))
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
#' @param plan_mode Logical. When TRUE, the session is in plan mode:
#'   the LLM is told to research and propose without acting, the policy
#'   engine denies write/exec tool calls (except \code{exit_plan_mode}),
#'   and \code{exit_plan_mode} is added to the tool list. A successful
#'   \code{exit_plan_mode} call flips this back to FALSE.
#'
#' @return An environment holding the session state.
#' @examples
#' # Build a stateless session for the CLI channel without making any
#' # network calls. The returned environment carries history, the
#' # active provider/model, and the approval callback.
#' s <- new_session(channel = "cli", provider = "anthropic")
#' is.environment(s)
#' identical(s$provider, "anthropic")
#' @export
new_session <- function(channel = c("cli", "console", "matrix"),
                        history = NULL, model_map = NULL,
                        provider = "anthropic", tools_filter = NULL,
                        system = NULL, approval_cb = NULL, max_turns = 10L,
                        verbose = FALSE, plan_mode = FALSE) {
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
    s$plan_mode <- isTRUE(plan_mode)
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
    # Either session$dry_run (set by inst/bin/corteza per REPL
    # iteration) or session$config$dry_run (chat()'s /dryrun toggle)
    # counts. We read at call time so toggles between turns take
    # effect immediately.
    session_dry_run <- function() {
        isTRUE(session$dry_run) || isTRUE(session$config$dry_run)
    }
    if (is.null(tool_executor)) {
        ensure_skills()
        # Default in-process executor: thread dry_run through to
        # call_skill / skill_run so the short-circuit below is safe
        # for chat() and embedded sessions that don't supply a
        # custom executor. Without this, the CLI's custom executor
        # honored dry-run via opts$dry_run but every other surface
        # would silently execute the tool during a "preview".
        tool_executor <- function(name, args) {
            call_skill(name, as.list(args), ctx = list(session = session),
                       dry_run = session_dry_run())
        }
    }
    function(name, args, context = NULL) {
        internal_name <- unsanitize_tool_name(name)
        call <- list(
                     tool = internal_name,
                     args = as.list(args),
                     channel = session$channel,
                     context = list(recent_classes = session$recent_classes,
                                    plan_mode = isTRUE(session$plan_mode)),
                     # Read-only per-call snapshot from llm.api::agent (NULL
                     # with a two-arg call or older llm.api): assistant_text,
                     # agent_turn, call_index, call_count, provider. Drives the
                     # approval rationale and the silent-streak narration guard.
                     model_context = context
        )

        # Silent-streak narration guard: bookkeep once per model turn
        # (the first dispatched call of the batch, call_index == 1). A
        # turn that made tool calls with no assistant narration extends
        # the streak; any narration resets it. No-op without the llm.api
        # context snapshot.
        .update_silent_streak(session, call$model_context)

        # Single finalizer for the narration nudge: route every model-visible
        # result (executed, denied, declined, dry-run, task intercept) through
        # this, so a silent batch is nudged and the streak reset no matter
        # which outcome its final call takes -- not only the executed path.
        nudge <- function(text) {
            .maybe_append_narration_nudge(text, session, call$model_context)
        }

        # Task-tracker intercept. task_create / task_update mutate
        # session metadata (the task list) rather than doing real
        # work. They run in-process here so the mutation lands on the
        # live `session` environment, not a detached copy. Bypass
        # dry-run, policy, approval, and the normal observer chain;
        # fire a `task_event` for displays that want it.
        task_result <- task_tool_intercept(session, internal_name,
            as.list(args))
        if (!is.null(task_result)) {
            task_result <- nudge(task_result)
            .fire_observers(session, list(
                    call = call,
                    outcome = "task",
                    result = task_result,
                    success = TRUE,
                    elapsed_ms = 0,
                    turn_number = session$turn_number %||% 0L
                ))
            return(task_result)
        }

        # Dry-run mode: short-circuit before policy/approval. A dry
        # run is a "show me what would happen" preview; prompting the
        # user to approve a *preview* would be incoherent, and a
        # config-driven "deny" on the tool would silently swallow the
        # preview the user is trying to see. The tool_executor must
        # already be dry-run-safe (either the CLI's custom executor
        # that checks opts$dry_run, or the default executor above
        # which passes dry_run = TRUE down to skill_run).
        if (session_dry_run()) {
            raw <- tryCatch(
                            tool_executor(internal_name, as.list(args)),
                            error = function(e) err(paste("Tool error:",
                        conditionMessage(e)))
            )
            return(nudge(admit_tool_result(.flatten_mcp_result(raw),
                                           tool = internal_name)))
        }

        # Resolve once up front so policy() and the sticky classifier
        # below see the same paths/urls.
        call$paths <- resolve_paths(call)
        call$urls <- resolve_urls(call)
        # Pass session$config so the /permissions contract (configured
        # approval_mode + dangerous_tools + per-tool permissions) is
        # enforced regardless of how the data class falls out. Without
        # this, a CLAUDE.md edit could classify as `random` and skip
        # the prompt in chat() even though `replace_in_file` is in the
        # default dangerous_tools list.
        decision <- policy(call, config = session$config)
        # decision$reason can embed a model-controlled path or tool name
        # (policy.R); it is rendered into the approval prompt and the
        # model-visible deny/decline results, so sanitize it once here.
        decision$reason <- .sanitize_inline(decision$reason %||% "")

        # Sticky: record the class regardless of the decision outcome.
        # Even a denied tool call means the LLM is trying to touch that
        # data class, so downstream calls should inherit it.
        klass <- classify_data(call,
                               list(recent_classes = session$recent_classes))
        session$recent_classes <- unique(c(session$recent_classes, klass))
        session$turn_number <- (session$turn_number %||% 0L) + 1L

        start <- Sys.time()

        outcome_text <- function(kind, text, success, diff = NULL) {
            event <- list(
                          call = call,
                          decision = decision,
                          outcome = kind,
                          result = text,
                          success = isTRUE(success),
                          elapsed_ms = as.numeric(
                    difftime(Sys.time(), start, units = "secs")
                ) * 1000,
                          turn_number = session$turn_number,
                          diff = diff
            )
            .fire_observers(session, event)
            text
        }

        if (identical(decision$approval, "deny")) {
            return(outcome_text(
                                "deny",
                                nudge(sprintf("[corteza policy denied: %s]",
                                              decision$reason)),
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
                                    nudge(sprintf("[user declined: %s]",
                                                  decision$reason)),
                                    FALSE
                    ))
            }
        }

        .fire_observers(session, list(
                                      call = call,
                                      decision = decision,
                                      outcome = "start",
                                      result = NULL,
                                      success = NA,
                                      elapsed_ms = 0,
                                      turn_number = session$turn_number
            ))

        raw <- tryCatch(
                        tool_executor(internal_name, as.list(args)),
                        error = function(e) err(paste("Tool error:", conditionMessage(e)))
        )
        success <- !isTRUE(raw$isError)
        if (identical(internal_name, "exit_plan_mode") && isTRUE(success)) {
            session$plan_mode <- FALSE
        }
        result_text <- nudge(
            admit_tool_result(.flatten_mcp_result(raw), tool = internal_name))
        outcome_text("ran", result_text, success, diff = raw$diff)
    }
}

# Silent-streak narration guard -------------------------------------
# corteza-owned and session-scoped (never the package-global .heartbeat,
# which can't serve concurrent console/Matrix/subagent sessions). Uses
# the llm.api per-call context snapshot (call$model_context): a model
# turn is "silent" when it made tool calls with no assistant narration.
# Keyed on call_index == 1 so a multi-call batch counts once; the nudge
# rides only on the final result (call_index == call_count) of a silent
# batch.

#' Update session$silent_streak once per model turn (call_index == 1).
#' @noRd
.update_silent_streak <- function(session, mc) {
    if (is.null(mc) || !identical(mc$call_index, 1L)) {
        return(invisible(NULL))
    }
    if (nzchar(trimws(mc$assistant_text %||% ""))) {
        session$silent_streak <- 0L
    } else {
        session$silent_streak <- (session$silent_streak %||% 0L) + 1L
    }
    invisible(NULL)
}

#' Append a one-time narration reminder to the final result of a silent
#' batch once the streak reaches corteza.narration_streak (default 3).
#' Resets the streak so it fires once per breach. The policy reason is
#' untouched -- this rides on the tool-result text the model reads next.
#' @noRd
.maybe_append_narration_nudge <- function(text, session, mc) {
    threshold <- getOption("corteza.narration_streak", 3L)
    if (is.null(mc) || !is.numeric(threshold) || !is.finite(threshold) ||
        threshold < 1L) {
        return(text)
    }
    if (!identical(mc$call_index, mc$call_count) ||
        (session$silent_streak %||% 0L) < threshold) {
        return(text)
    }
    streak <- session$silent_streak
    session$silent_streak <- 0L
    paste0(text,
           "\n\n[corteza] You've made tool calls across ", streak,
           " turns without telling the user what you're doing. Before your",
           " next tool call, say in one line what you're doing and why.")
}

# Resolve the LLM model for the turn. Policy's per-call model routing
# decision is advisory at the turn level; we just pick the session's
# cloud (or local) default. A future PR can switch mid-turn.
#
# When the session's cloud model is unset AND no corteza.model option
# is set, fall back to the provider default from llm.api's canonical
# table (via default_provider_model()), so corteza tracks llm.api
# rather than carrying its own model picks.
.resolve_model <- function(session) {
    explicit <- session$model_map$cloud %||% getOption("corteza.model", NULL)
    if (!is.null(explicit) && nzchar(explicit)) {
        return(explicit)
    }
    default_provider_model(session$provider %||% "anthropic")
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
#' @examples
#' \dontrun{
#' # Needs an open MCP connection to a running corteza::serve().
#' conn <- llm.api::mcp_connect("tcp://localhost:7850")
#' executor <- mcp_tool_executor(conn)
#' s <- new_session(provider = "anthropic")
#' turn("Hello", s, tool_executor = executor)
#' }
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
#' @examples
#' s <- new_session(provider = "anthropic")
#' add_observer(s, function(event) {
#'     # An observer is just a function of one argument; record the
#'     # outcome for inspection.
#'     message(event$outcome)
#' })
#' length(s$on_tool)
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
#' @examples
#' obs <- observer_progress()
#' s <- new_session(provider = "anthropic")
#' add_observer(s, obs)
#' @export
observer_progress <- function() {
    function(event) {
        # Observer's purpose is to print tool-call traces; gate behind
        # the corteza.verbose option so non-interactive scripts are
        # silent by default.
        if (!.corteza_verbose()) {
            return(invisible())
        }

        if (identical(event$outcome, "start")) {
            summary <- cli_event_summary(event, width = 84L)
            cat(sprintf("\u25cf %s\n", summary$title))
            if (length(summary$detail_lines) > 0L) {
                for (line in summary$detail_lines) {
                    cat(sprintf("  %s\n", line))
                }
            }
            cat("  Running...\n")
            return(invisible())
        }

        if (identical(event$outcome, "deny") ||
            identical(event$outcome, "declined")) {
            summary <- cli_event_summary(event, width = 84L)
            cat(sprintf("\u25cf %s\n", summary$title))
            cat(sprintf("  \u23bf %s\n",
                        sub("^\\[", "", sub("\\]$", "", event$result %||% ""))))
            return(invisible())
        }

        if (!identical(event$outcome, "ran")) {
            return(invisible())
        }

        # File-edit tools attach a diff payload to their result. When
        # present, render the colored hunks in place of the usual
        # one-line "N lines" summary so the user can see what changed.
        if (!is.null(event$diff)) {
            render_tool_diff(event$diff)
            return(invisible())
        }

        summary <- cli_event_summary(event, width = 84L)
        detail <- if (length(summary$detail_lines) > 0L) {
            summary$detail_lines[1]
        } else {
            ""
        }
        if (!isTRUE(event$success)) {
            detail <- paste0(detail, " (error)")
        }
        cat(sprintf("  \u23bf %s\n", detail))
    }
}

#' Run one agent turn
#'
#' Sends \code{prompt} to the configured LLM with tool use enabled. Every
#' tool call the LLM makes is routed through \code{\link{policy}} before
#' being dispatched.
#'
#' Tool dispatch is pluggable via \code{tool_executor}, but the CLI and
#' \code{chat()} both leave it NULL: tools run in-process through the
#' default \code{call_skill} dispatcher against the local skill registry.
#' \code{serve()} is a separate MCP server for external clients only; it
#' is not part of the CLI's tool path. Pass an explicit
#' \code{function(name, args) -> list} executor only when dispatching
#' tools somewhere other than the in-process registry.
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
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY (or the configured provider's key) and
#' # a network connection to the LLM.
#' s <- new_session(provider = "anthropic")
#' out <- turn("Say hello", s)
#' out$reply
#' }
#' @export
turn <- function(prompt, session, tool_executor = NULL, tools = NULL) {
    stopifnot(is.environment(session))

    # Each user request is a fresh agent run: clear the narration streak so a
    # silent tail of the previous run can't bleed into this one. Reset at the
    # run boundary (here, before agent()) rather than after, so an interrupt
    # mid-run can't leave a stale streak behind.
    session$silent_streak <- 0L

    if (is.null(tools)) {
        ensure_skills()
        tools <- skills_as_api_tools(session$tools_filter)
    }
    tools <- .plan_mode_filter_tools(tools, isTRUE(session$plan_mode))
    tools <- .task_filter_tools(tools, session$channel)
    system <- .plan_mode_compose_system(session$system,
                                        isTRUE(session$plan_mode))
    system <- task_compose_system(system, session$tasks %||% list(),
                                  channel = session$channel)
    tool_handler <- .make_tool_handler(session, tool_executor = tool_executor)

    # Pass a history_callback to llm.api so session$history mirrors
    # intermediate state continuously: after each assistant message
    # and after each tool_result lands, the callback overwrites
    # session$history with the in-progress snapshot. session is an
    # environment (see new_session()), so the mutation is visible to
    # the caller (chat() / CLI) even if llm.api::agent() throws an
    # interrupt mid-flight. Without this, an interrupt would lose
    # every tool call completed in the current batch.
    #
    # history_callback arrived in llm.api 0.1.4 (now the Imports
    # minimum). The formals() check below is cheap defense for the
    # rare case of running against an older build.
    agent_args <- list(
                       prompt = prompt,
                       tools = tools,
                       tool_handler = tool_handler,
                       system = system,
                       model = .resolve_model(session),
                       provider = session$provider,
                       max_turns = session$max_turns,
                       verbose = session$verbose,
                       history = session$history
    )
    if ("history_callback" %in% names(formals(llm.api::agent))) {
        agent_args$history_callback <- function(history) {
            session$history <- history
        }
    }
    response <- do.call(llm.api::agent, agent_args)

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

