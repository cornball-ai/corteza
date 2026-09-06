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
#' @param web_search Logical or NULL. When TRUE and the provider
#'   supports it, \code{\link{turn}} enables \code{llm.api::agent}'s
#'   provider-native (server-side) web search for the session: the
#'   model searches inside its own turn, no Tavily key required. NULL
#'   defers to \code{turn}'s default (on for supported providers).
#' @param base_url Character or NULL. Endpoint for the
#'   \code{"openai_compatible"} provider (OpenRouter, DeepSeek, a
#'   corporate gateway). \code{\link{turn}} applies it via
#'   \code{llm.api::llm_base()} for the duration of the agent call.
#'   NULL falls back to \code{config$base_url}, then llm.api's own
#'   \code{OPENAI_COMPATIBLE_BASE_URL} environment variable. Ignored
#'   for every other provider.
#' @param max_tokens Integer or NULL. Per-response output-token budget,
#'   forwarded to \code{llm.api::agent}. NULL falls back to
#'   \code{config$max_tokens}, then llm.api's provider default (4096
#'   on Anthropic). Agent turns that write long tool calls -- big
#'   \code{run_r} bodies especially -- need more than that default:
#'   a response the budget cuts off ends the whole turn with
#'   \code{[Output truncated: max_tokens]} instead of the model's
#'   answer.
#' @param reasoning_effort Character or NULL. How hard a reasoning
#'   model should think before answering, e.g. \code{"low"},
#'   \code{"medium"}, \code{"high"}. One setting, routed to whichever
#'   field the provider's wire uses: \code{reasoning_effort} on
#'   \code{"openai"} / \code{"openai_codex"}, \code{output_config$effort}
#'   on \code{"anthropic"} / \code{"anthropic_claude"}. Dropped for
#'   providers with no effort control. NULL falls back to
#'   \code{config$reasoning_effort}, then the provider default. Not
#'   validated against a fixed set, because the scales differ: Anthropic
#'   accepts \code{"low"}, \code{"medium"}, \code{"high"},
#'   \code{"xhigh"} and \code{"max"}, and an unknown value is refused by
#'   the API with a message naming the alternatives.
#' @param thinking_budget_tokens Integer or NULL. Extended-thinking
#'   budget for Anthropic models, forwarded to \code{llm.api::agent}
#'   for the \code{"anthropic"} and \code{"anthropic_claude"}
#'   providers only. Must be at least 1024 and below
#'   \code{max_tokens}, both enforced by llm.api. NULL falls back to
#'   \code{config$thinking_budget_tokens}, then the provider default
#'   (thinking off).
#' @param cache Character or NULL. Anthropic prompt-cache TTL:
#'   \code{"none"}, \code{"5m"} or \code{"1h"}. Anthropic-only, and
#'   dropped for other providers so they never see a field their wire
#'   rejects. NULL falls back to \code{config$cache}, then llm.api's
#'   default (\code{"none"}). Caching is a billing setting, not a
#'   behavioural one: the model receives byte-identical input either
#'   way, so turning it on cannot change a result. llm.api (>= 0.1.9.6,
#'   the Imports floor) marks both the system prompt and the tail of the
#'   message history, so each request in an agent loop reads the
#'   previous request's context from cache and pays fresh input only
#'   for what was appended since; on a long tool loop that is most of
#'   the input bill. The floor is there because an older llm.api marks
#'   the system prompt alone and would silently deliver a fraction of
#'   that with this set. \code{turn()$usage} reports what was read and
#'   written.
#' @param compaction_prompt Character or NULL. Optional domain-specific
#'   instructions for summarizing older history during context compaction.
#'   NULL uses corteza's general coding-agent summary brief. The prompt
#'   affects only the summarizer; the main agent system prompt is unchanged.
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
                        verbose = FALSE, plan_mode = FALSE,
                        web_search = NULL, base_url = NULL,
                        max_tokens = NULL, reasoning_effort = NULL,
                        thinking_budget_tokens = NULL, cache = NULL,
                        compaction_prompt = NULL) {
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
    s$web_search <- web_search
    s$base_url <- base_url
    s$max_tokens <- .check_max_tokens(max_tokens, "new_session(max_tokens=)")
    s$reasoning_effort <- .check_reasoning_effort(reasoning_effort,
        "new_session(reasoning_effort=)")
    s$thinking_budget_tokens <-
    .check_max_tokens(thinking_budget_tokens,
                      "new_session(thinking_budget_tokens=)",
                      what = "thinking_budget_tokens")
    s$cache <- .check_cache(cache, "new_session(cache=)")
    if (!is.null(compaction_prompt) &&
        (!is.character(compaction_prompt) || length(compaction_prompt) != 1L ||
            is.na(compaction_prompt) || !nzchar(compaction_prompt))) {
        stop("compaction_prompt must be NULL or a single non-empty string",
             call. = FALSE)
    }
    s$compaction_prompt <- compaction_prompt
    s
}

# Validate a prompt-cache TTL. NULL passes through so a later
# resolution step can fall back to config. Unlike reasoning_effort this
# IS a closed set: llm.api runs match.arg over exactly these three, so
# anything else is a caller error we can name here rather than a
# partial-match surprise ("5" silently becoming "5m") further down.
.cache_values <- c("none", "5m", "1h")

.check_cache <- function(x, where) {
    if (is.null(x)) {
        return(NULL)
    }
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !x %in% .cache_values) {
        stop(where, ": cache must be one of ",
             paste(sQuote(.cache_values), collapse = ", "), call. = FALSE)
    }
    x
}

# Validate an output-token budget at a resolution boundary. NULL passes
# through (means "defer"); anything else must be a single positive whole
# number that survives as.integer() intact -- as.integer alone silently
# truncates fractions and turns Inf/overflow into NA, and an NA or
# nonsense budget must not reach the provider request. No upper bound
# here: model output ceilings differ per provider, and an oversized
# value fails loudly at the API with a clear message.
.check_max_tokens <- function(x, where, what = "max_tokens") {
    if (is.null(x)) {
        return(NULL)
    }
    ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x >= 1 && x == trunc(x) && x <= .Machine$integer.max
    if (!isTRUE(ok)) {
        stop(what, " must be a single positive whole number; got ",
             deparse(x), " via ", where, call. = FALSE)
    }
    as.integer(x)
}

# Providers whose wire format llm.api wires provider-native (server-side)
# web search into. Mirrors llm.api's supported set; we gate on it so
# providers without native search (e.g. ollama) don't take a per-turn
# "ignored" warning. Local models fall back to the Tavily web_search tool.
.web_search_providers <- c("anthropic", "anthropic_claude", "openai",
                           "openai_codex", "moonshot")

.web_search_supported <- function(provider) {
    isTRUE(provider %in% .web_search_providers)
}

# Resolve the session's provider-native web-search setting to a concrete
# value. Explicit session$web_search wins; then a config web_search key;
# then on by default (the model only searches when it decides to, so the
# cost is per actual search, not per turn).
.session_web_search <- function(session) {
    session$web_search %||% session$config$web_search %||% TRUE
}

# Validate a reasoning-effort setting at a resolution boundary. NULL
# passes through (means "provider default").
.check_reasoning_effort <- function(x, where) {
    if (is.null(x)) {
        return(NULL)
    }
    ok <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    if (!isTRUE(ok)) {
        stop("reasoning_effort must be a single non-empty string ",
             "(e.g. \"low\", \"medium\", \"high\"); got ", deparse(x),
             " via ", where, call. = FALSE)
    }
    # Deliberately not an enum: the provider owns the vocabulary and
    # adds to it, so an unknown value is refused loudly by the API
    # rather than silently dropped here.
    x
}

# Providers whose wire spells reasoning effort as `reasoning_effort`.
# llm.api maps it onto the Responses `reasoning$effort` field for the
# codex wire.
.reasoning_effort_providers <- c("openai", "openai_codex")

.anthropic_providers <- c("anthropic", "anthropic_claude")

# Route reasoning args to the spelling the provider's wire uses, and
# drop what it cannot carry at all.
#
# Effort is one idea with two spellings. On the openai/codex wire it is
# the top-level `reasoning_effort`; on the Anthropic Messages wire it is
# `output_config.effort`, whose scale the API states itself:
#
#   API error (400): output_config.effort: Input should be 'low',
#   'medium', 'high', 'xhigh' or 'max'
#
# So a session sets `reasoning_effort` once and it means the same thing
# on either family, rather than the caller keeping track of which
# provider spells it which way. The value is still not validated here --
# the scales differ per provider and the API refuses an unknown one with
# a message naming the alternatives, as above.
#
# Anthropic rejects unknown top-level body fields outright ("Extra
# inputs are not permitted"), and llm.api splices every extra into the
# body verbatim (R/agent.R:600), so routing is not cosmetic: sending the
# wrong spelling is a 400. Applied by turn() for the session's own
# provider and again by .agent_with_fallback() for each candidate,
# because a fallback rewrites provider under args routed for the
# primary. That failure defeats the fallback itself -- a 400 is not a
# limit error, so the turn stops instead of trying the next provider.
#
# thinking_budget_tokens is a separate Anthropic knob (extended
# thinking) and is left where it is; the two are independent, and
# Agno's ARC harness runs effort "max" with thinking off.
.gate_reasoning_args <- function(args, provider) {
    effort <- args$reasoning_effort %||% args$output_config$effort
    args$reasoning_effort <- NULL
    args$output_config <- NULL
    if (!is.null(effort)) {
        if (provider %in% .reasoning_effort_providers) {
            args$reasoning_effort <- effort
        } else if (provider %in% .anthropic_providers) {
            args$output_config <- list(effort = effort)
        }
    }
    if (!is.null(args$thinking_budget_tokens) &&
        !provider %in% .anthropic_providers) {
        args$thinking_budget_tokens <- NULL
    }
    # Prompt caching is Anthropic-only. llm.api does warn and degrade to
    # "none" itself, but that warning would then fire on every turn of
    # every non-Anthropic session -- including each fallback candidate --
    # for a setting the caller set once at the top. Drop it here so the
    # warning stays a signal about a genuinely misdirected call.
    if (!is.null(args$cache) && !provider %in% .anthropic_providers) {
        args$cache <- NULL
    }
    args
}

# Resolve the reasoning-effort setting: explicit session field wins,
# then config. NULL means "provider default".
.session_reasoning_effort <- function(session) {
    .check_reasoning_effort(session$reasoning_effort %||%
                            session$config$reasoning_effort,
                            "session/config reasoning_effort")
}

# Resolve the prompt-cache TTL: explicit session field wins, then
# config. NULL means "llm.api's default", which is "none".
.session_cache <- function(session) {
    .check_cache(session$cache %||% session$config$cache,
                 "session/config cache")
}

# Resolve the Anthropic extended-thinking budget. llm.api enforces the
# provider's own floor (1024) and the max_tokens ceiling, so this only
# has to agree that it is a positive whole number.
.session_thinking_budget <- function(session) {
    .check_max_tokens(session$thinking_budget_tokens %||%
                      session$config$thinking_budget_tokens,
                      "session/config thinking_budget_tokens",
                      what = "thinking_budget_tokens")
}

# Resolve the per-response output-token budget. Explicit
# session$max_tokens wins, then config$max_tokens. NULL means "not set
# on the corteza side"; llm.api then applies its own provider default.
.session_max_tokens <- function(session) {
    # Validated even though new_session() already checks its own arg:
    # config$max_tokens arrives here unchecked, and a session field can
    # be assigned directly.
    .check_max_tokens(session$max_tokens %||% session$config$max_tokens,
                      "session/config max_tokens")
}

# Resolve the endpoint for the openai_compatible provider. Explicit
# session$base_url wins, then config$base_url. NULL means "not set on
# the corteza side"; llm.api then falls back to its own
# OPENAI_COMPATIBLE_BASE_URL env var, or errors with setup guidance.
.session_base_url <- function(session) {
    bu <- session$base_url %||% session$config$base_url
    if (is.null(bu) || !nzchar(bu)) {
        return(NULL)
    }
    bu
}

# When provider-native search is active, llm.api injects the provider's own
# web_search tool. Drop corteza's same-named (Tavily) tool so the request
# doesn't carry two tools called "web_search" (Anthropic 400s on duplicate
# tool names). Native search supersedes Tavily for supported providers.
.drop_redundant_web_search <- function(tools, ws_active) {
    if (!isTRUE(ws_active)) {
        return(tools)
    }
    keep <- !vapply(tools, function(t) identical(t$name, "web_search"),
                    logical(1))
    tools[keep]
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
            # cwd travels with the session, not the process: a tool
            # that resolves a project path (harness_note's store, say)
            # must land in the session's project, and a bot or
            # embedded session rarely shares the process working
            # directory. Falls back to getwd() when the session
            # doesn't carry one.
            call_skill(name, as.list(args),
                       ctx = list(session = session, cwd = session$cwd %||% getwd()),
                       dry_run = session_dry_run())
        }
    }
    # A custom executor may need the provider's read-only call metadata to
    # enforce semantics that span a batch of tool calls. Preserve the
    # historical two-argument contract while opting executors that declare
    # `context` (or `...`) into that metadata.
    executor_formals <- tryCatch(names(formals(tool_executor)),
                                 error = function(e) character())
    executor_wants_context <- "context" %in% executor_formals ||
    "..." %in% executor_formals
    execute <- function(name, args, context) {
        if (executor_wants_context) {
            tool_executor(name, args, context = context)
        } else {
            tool_executor(name, args)
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
                            execute(internal_name, as.list(args), context),
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

        # Policy denial, attended path. In auto mode the same denial
        # routes through the gate below instead -- not so the monitor
        # can weigh in (it cannot; the gate's deny branch is mechanical
        # and first) but so the denial leaves the same structured,
        # call_id-carrying record as every other decision. The
        # model-visible message is identical on both paths.
        if (identical(decision$approval, "deny") &&
            !is.function(session$auto_gate)) {
            return(outcome_text(
                                "deny",
                                nudge(sprintf("[corteza policy denied: %s]",
                            decision$reason)),
                                FALSE
                ))
        }

        # Auto-authority gate (unattended runs only; NULL otherwise).
        #
        # This cannot live in approval_cb, which only ever sees "ask".
        # The default tensor (R/policy.R) resolves the `random` data
        # class to "allow" for write and exec on every channel, and
        # classify_data() only returns "code" for paths under
        # get_code_paths() -- ~/projects, ~/src by default. So a repo
        # anywhere else classifies `random`, its writes decide "allow",
        # and an approval_cb-based supervisor is never consulted for a
        # single edit. Config can widen the same way: a per-tool
        # "allow" permission downgrades ask to allow before we get
        # here. A supervisor wired to approval_cb would be decorative
        # in exactly the repos people run this in.
        #
        # So the gate sees every call that survived policy, whatever
        # the verdict, and it is the thing that decides. It returns
        # "proceed", "refuse" (tell the model no, keep the turn alive),
        # or "escalate" (abort the whole turn for a human).
        gate_approved <- FALSE
        if (is.function(session$auto_gate)) {
            # One id per gated call, minted before anything decides.
            # Every artifact this call produces -- the gate record, the
            # observer event (and through it the trace row), the
            # monitor's approval request -- carries it, so two calls to
            # the same tool with the same args (one refused, one run)
            # reconstruct unambiguously from disk. turn_number cannot do
            # this job: it also counts denials and intercepts.
            session$auto_call_seq <- (session$auto_call_seq %||% 0L) + 1L
            call$call_id <- sprintf("c%d", session$auto_call_seq)
            gate <- tryCatch(
                             session$auto_gate(call, decision),
                             # A gate that errored did not approve anything.
                             # Fail closed: absence of a verdict is never a yes.
                             error = function(e) {
                list(action = "escalate",
                     reason = paste("auto gate failed:", conditionMessage(e)))
            }
            )
            action <- gate$action %||% "escalate"
            if (identical(action, "deny")) {
                # Policy's verdict, surfaced through the gate purely so
                # it was recorded; same message as the attended branch.
                return(outcome_text(
                                    "deny",
                                    nudge(sprintf("[corteza policy denied: %s]",
                                decision$reason)),
                                    FALSE
                    ))
            }
            if (identical(action, "escalate")) {
                stop(auto_escalate_condition(gate$reason %||% "unspecified",
                        call$tool %||% "?"))
            }
            if (!identical(action, "proceed")) {
                return(outcome_text(
                                    "declined",
                                    nudge(sprintf("[monitor refused: %s]",
                                .sanitize_inline(gate$reason %||% ""))),
                                    FALSE
                    ))
            }
            # The gate brokered this call, so it stands in for the
            # human at the approval prompt below.
            gate_approved <- TRUE
        }

        if (identical(decision$approval, "ask") && !gate_approved) {
            approved <- tryCatch(
                                 session$approval_cb(call, decision),
                                 error = function(e) FALSE
            )
            if (!isTRUE(approved)) {
                return(outcome_text(
                                    "declined",
                                    nudge(sprintf("[user declined: %s]", decision$reason)),
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
                        execute(internal_name, as.list(args), context),
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
    paste0(text, "\n\n[corteza] You've made tool calls across ", streak,
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

# Add the two llm.api usage trees produced when an over-window request is
# compacted and retried. Unknown numeric accounting stays unknown rather than
# being silently converted to zero; this matters most for cost.
.turn_add_usage <- function(left, right) {
    left <- left %||% list()
    right <- right %||% list()
    out <- list()
    for (name in union(names(left), names(right))) {
        x <- left[[name]]
        y <- right[[name]]
        if (is.list(x) || is.list(y)) {
            out[[name]] <- .turn_add_usage(x, y)
        } else if (is.numeric(x) || is.numeric(y)) {
            values <- c(as.numeric(x %||% 0), as.numeric(y %||% 0))
            if (anyNA(values)) {
                out[[name]] <- NA_real_
            } else {
                out[[name]] <- sum(values)
            }
        } else {
            out[[name]] <- y %||% x
        }
    }
    out
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
#' When the provider refuses the request with a limit error (HTTP 429,
#' 503, or 529, or a body naming a rate, usage, or quota limit) and the
#' session has a fallback chain, the turn is retried on the next
#' \code{"model provider"} entry in that chain. The chain comes from
#' \code{session$fallback} (the Matrix config's \code{fallback} key, see
#' \code{\link{bot_configure}}) or the cwd config's \code{fallback}
#' key. A provider that hit a limit is skipped process-wide for
#' \code{fallback_cooldown_minutes} (default 30); a configured
#' \code{fallback_primary_retry_at} can hold the primary until a weekly
#' reset boundary. Provider-native histories are bridged when the wire
#' changes, and a partially completed tool run resumes from its captured
#' results so no completed tool call runs twice. Fallback replies identify
#' their actual model/provider; API-key fallbacks carry a prominent billable
#' usage warning. Errors that are not limits are rethrown untouched.
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
#' @return A list with \code{reply} (character), \code{session} (the
#'   updated session environment; also mutated in place), and \code{route}
#'   (the actual model/provider and fallback credential mode).
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

    # Provider-native (server-side) web search: enable it when the session
    # asks for it (default on), the provider supports it, and the llm.api
    # build accepts the arg. When active, drop corteza's own web_search tool
    # (Tavily) so its name doesn't collide with the provider's native
    # web_search tool, which llm.api injects into the request. Native search
    # supersedes Tavily for supported providers; local models (ollama) keep
    # the Tavily tool as their only web search.
    ws <- .session_web_search(session)
    ws_active <- !identical(ws, FALSE) &&
    .web_search_supported(session$provider) &&
    "web_search" %in% names(formals(llm.api::agent))
    tools <- .drop_redundant_web_search(tools, ws_active)
    system <- .plan_mode_compose_system(session$system,
                                        isTRUE(session$plan_mode))
    system <- task_compose_system(system, session$tasks %||% list(),
                                  channel = session$channel)
    tool_handler <- .make_tool_handler(session, tool_executor = tool_executor)

    # Compaction belongs to the shared agent lifecycle, not to a particular
    # REPL surface. Resolve the session's configuration once and use the same
    # existing compactor both before the run and at llm.api's quiescent
    # between-turn checkpoint. Direct callers (ARC included) therefore cannot
    # bypass context management by calling turn() without run_repl_loop().
    compact_config <- session$config %||%
    load_config(session$cwd %||% getwd())
    compact_session_history <- function(history, force = FALSE,
                                        reason = "threshold") {
        session$history <- history %||% list()
        args <- list(
                     session = session,
                     config = compact_config,
                     kind = session$kind %||% NULL,
                     tools = tools,
                     system = system,
                     reason = reason
        )
        if (isTRUE(force)) {
            args$threshold <- 0
            args$min_messages <- 2L
        }
        isTRUE(do.call(maybe_compact_turn_session, args))
    }
    compact_session_history(session$history %||% list(), reason = "preflight")

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
    if ("checkpoint_callback" %in% names(formals(llm.api::agent))) {
        agent_args$checkpoint_callback <- function(history, context) {
            session$last_checkpoint_context <- context
            if (compact_session_history(history, reason = "checkpoint")) {
                return(list(history = session$history))
            }
            NULL
        }
    }

    # A caller that wants the reply as it is generated (the voice
    # surface synthesises speech from these) sets session$on_delta.
    # Same formals() defense as history_callback: on_delta arrived in
    # llm.api later than the Imports minimum guarantees.
    if (is.function(session$on_delta) &&
        "on_delta" %in% names(formals(llm.api::agent))) {
        agent_args$on_delta <- session$on_delta
    }

    if (ws_active) {
        agent_args$web_search <- ws
    }

    # Output-token budget rides llm.api::agent's `...` into the request
    # body. Only sent when set: NULL leaves llm.api's provider default
    # in place rather than pinning it from here.
    mt <- .session_max_tokens(session)
    if (!is.null(mt)) {
        agent_args$max_tokens <- mt
    }

    # Reasoning depth. Both settings mean "how hard to think", but they
    # reach the provider differently: reasoning_effort rides `...` into
    # the request body under whichever name that wire uses (see
    # .gate_reasoning_args), thinking_budget_tokens is a named llm.api
    # arg that warns and ignores off Anthropic. Only sent when set, so
    # the provider default stands otherwise.
    re <- .session_reasoning_effort(session)
    if (!is.null(re)) {
        agent_args$reasoning_effort <- re
    }
    tb <- .session_thinking_budget(session)
    if (!is.null(tb)) {
        agent_args$thinking_budget_tokens <- tb
    }
    ch <- .session_cache(session)
    if (!is.null(ch)) {
        agent_args$cache <- ch
    }
    agent_args <- .gate_reasoning_args(agent_args, session$provider)

    # For openai_compatible the endpoint is corteza-configured, not built
    # into llm.api. Apply it through llm.api's own setter (so we don't
    # depend on the option's spelling) for the duration of this call, and
    # restore the previous value after. Scoped to the call so a mid-session
    # /provider switch or a subagent on another provider can't inherit it.
    if (identical(session$provider, "openai_compatible")) {
        base_url <- .session_base_url(session)
        if (!is.null(base_url)) {
            old_base <- llm.api::llm_base(base_url)
            on.exit(llm.api::llm_base(old_base), add = TRUE)
        }
    }

    response <- .agent_with_fallback(agent_args, session)

    # Pi pairs proactive threshold compaction with one compact-and-retry on an
    # actual context overflow. llm.api deliberately excludes the incomplete
    # assistant response from returned history, so it is safe to compact that
    # authoritative history and continue it without re-appending the prompt.
    context_overflow <- isTRUE(response$truncated) &&
    identical(response$truncation_reason, "model_context_window_exceeded")
    if (context_overflow) {
        overflow_response <- response
        session$history <- response$history %||% session$history
        if (compact_session_history(session$history, force = TRUE,
                                    reason = "context_overflow")) {
            retry_args <- agent_args
            # `$<- NULL` removes a list element in R. Preserve the named NULL:
            # llm.api distinguishes an explicit continuation from a missing
            # required argument.
            retry_args["prompt"] <- list(NULL)
            retry_args$history <- session$history
            response <- .agent_with_fallback(retry_args, session)
            response$usage <- .turn_add_usage(overflow_response$usage,
                response$usage)
            response$turns <- as.integer(overflow_response$turns %||% 0L) +
            as.integer(response$turns %||% 0L)
            response$citations <- c(overflow_response$citations %||% list(),
                                    response$citations %||% list())
            response$searches <- c(overflow_response$searches %||% list(),
                                   response$searches %||% list())
            response$corteza_overflow_retry <- TRUE
            response$corteza_overflow_recovered <- !(
                isTRUE(response$truncated) &&
                identical(response$truncation_reason,
                          "model_context_window_exceeded")
            )
        }
    }

    if (!is.null(response$history)) {
        session$history <- response$history
    }

    list(
         reply = .fallback_reply(response),
         session = session,
         usage = response$usage,
         route = response$corteza_route,
         raw = response
    )
}
