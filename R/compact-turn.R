# Compaction for turn_session history.
#
# Long-running subagents (and the parent chat) can build up multi-
# tens-of-thousands of tokens in `session$history`. Compaction asks
# the LLM to summarize the older slice and replaces it with a single
# assistant message holding the summary — keeping the most recent
# turn(s) verbatim so in-flight reasoning isn't truncated.
#
# Two principles:
#   - Disk space is cheap; context is expensive. The on-disk
#     transcript is durable (see subagent_spawn / subagent_turn_prompt
#     persistence). Compaction only mutates the live in-memory
#     history sent to the model.
#   - Never compact mid-turn or when there's an unfinished
#     tool_use → tool_result pair, because the LLM would see a
#     dangling tool_use and refuse.

#' Resolve the effective compaction threshold for a subagent.
#'
#' Returns a numeric percent. NULL means "compaction off for this
#' child" — caller skips entirely.
#' @param config Full corteza config (post-defaults).
#' @return Numeric percent in (0, 100], or NULL.
#' @keywords internal
subagent_compact_threshold <- function(config) {
    cc <- config$subagents$context_compaction %||% list()
    mode <- cc$mode %||% "inherit_strict"
    if (identical(mode, "off")) {
        return(NULL)
    }
    parent_pct <- as.numeric(config$context_compact_pct %||% 90L)
    child_pct <- as.numeric(cc$compact_pct %||% 75L)
    if (identical(mode, "inherit")) {
        return(parent_pct)
    }
    # inherit_strict (default): child threshold can only be
    # equal-or-lower than parent's. Async work shouldn't die because
    # a quietly-growing child filled its window past the parent's
    # tolerance.
    min(parent_pct, child_pct)
}

#' Find the largest cut point in `history` that doesn't split a
#' tool_use / tool_result pair.
#'
#' Returns the number of entries that can safely be summarized
#' (entries `1..cut`). Entries `cut+1..end` are preserved verbatim.
#' Returns 0 when no safe cut is available.
#'
#' Strategy: start from the maximum cut that leaves `keep_recent_turns`
#' user-prompt boundaries intact, then walk back as needed so the cut
#' doesn't land between a tool_use and the tool_result that satisfies
#' it.
#' @param history Live in-memory history list.
#' @param keep_recent_turns Number of recent user→assistant turns to
#'   keep verbatim (a turn starts at a user message).
#' @param keep_recent_tokens Approximate recent-history tokens to retain when
#'   one user prompt has grown into a long tool trajectory. The cut still lands
#'   only at a complete tool-call/result boundary.
#' @keywords internal
compact_find_cut <- function(history, keep_recent_turns = 1L,
                             keep_recent_tokens = 20000L) {
    n <- length(history)
    if (n == 0L) {
        return(0L)
    }
    # Walk from the end; find the start index of the (keep_recent +
    # 1)th-from-last user turn. Everything before that is summarizable.
    #
    # Anthropic-style tool_result messages also have role == "user",
    # but they're the second half of a tool_use round-trip — not a
    # new user turn. Filter those out so the boundary lands on real
    # human prompts.
    user_starts <- integer(0)
    for (i in seq_len(n)) {
        role <- history[[i]]$role %||% ""
        if (identical(role, "user") &&
            !compact_entry_is_tool_result_only(history[[i]])) {
            user_starts <- c(user_starts, i)
        }
    }
    if (length(user_starts) > as.integer(keep_recent_turns)) {
        # Cut just before the start of the (keep_recent + 1)th-from-last
        # user turn (i.e., the boundary is the first kept user turn).
        keep <- as.integer(keep_recent_turns)
        boundary <- user_starts[length(user_starts) - keep + 1L]
        cut <- boundary - 1L
    } else {
        # A single autonomous request can contain hundreds of assistant/tool
        # rounds. There is no second user boundary to cut at, so retain an
        # approximate token tail and then move the cut backward to a complete
        # round. This is Pi's "split turn" case in R-native form.
        retained <- 0L
        boundary <- n + 1L
        target <- max(1L, as.integer(keep_recent_tokens))
        for (i in rev(seq_len(n))) {
            retained <- retained + .estimate_history_entry_tokens(history[[i]])
            if (retained >= target) {
                boundary <- i
                break
            }
        }
        if (boundary > n) {
            return(0L)
        }
        cut <- boundary - 1L
    }
    if (cut <= 0L) {
        return(0L)
    }
    # Don't split any tool_use / tool_result pair. Walk the cut back
    # until every tool_use in the prefix `history[1..cut]` has its
    # matching tool_result also in that prefix — i.e., no dangling
    # tool_use whose tool_result lives in the kept tail.
    while (cut > 0L &&
        (compact_prefix_has_unmatched_tool_use(history, cut) ||
            compact_tail_starts_with_tool_result(history, cut))) {
        cut <- cut - 1L
    }
    as.integer(cut)
}

# A retained tail cannot begin with an orphaned result. Walking the cut back
# makes the corresponding assistant tool call part of the retained tail too.
compact_tail_starts_with_tool_result <- function(history, cut) {
    if (cut < 0L || cut >= length(history)) {
        return(FALSE)
    }
    entry <- history[[cut + 1L]]
    if (compact_entry_is_tool_result_only(entry) ||
        identical(entry$role %||% "", "tool") ||
        identical(entry$type %||% "", "function_call_output")) {
        return(TRUE)
    }
    if (.compact_is_codex_output(entry)) {
        output <- entry$output %||% list()
        return(length(output) > 0L && all(vapply(
                    output,
                    function(item) identical(item$type %||% "",
                        "function_call_output"),
                    logical(1L)
                )))
    }
    FALSE
}

# llm.api stores a Codex Responses turn as a sentinel `type` plus the exact
# provider output array. Accept the short-lived development representation too
# so an interrupted session created by that build remains recoverable.
.compact_is_codex_output <- function(entry) {
    identical(entry$type %||% "", ".openai_codex_output") ||
    isTRUE(entry$.openai_codex_output)
}

#' Does a user-role entry contain only tool_result blocks?
#'
#' Anthropic-style chat history puts tool_result blocks inside a
#' user message; this helps `compact_find_cut` avoid treating them
#' as user-turn boundaries.
#' @noRd
compact_entry_is_tool_result_only <- function(entry) {
    cnt <- entry$content
    if (!is.list(cnt) || length(cnt) == 0L) {
        return(FALSE)
    }
    for (block in cnt) {
        bt <- block$type %||% ""
        if (!identical(bt, "tool_result")) {
            return(FALSE)
        }
    }
    TRUE
}

#' Does any tool_use in `history[1..cut]` have its matching
#' tool_result in `history[(cut+1):n]`?
#' @noRd
compact_prefix_has_unmatched_tool_use <- function(history, cut) {
    n <- length(history)
    if (cut <= 0L || cut >= n) {
        return(FALSE)
    }
    # Collect tool-use ids in the prefix across Anthropic blocks, OpenAI chat
    # tool_calls, and OpenAI Responses/Codex output items.
    prefix_uses <- character(0)
    for (i in seq_len(cut)) {
        entry <- history[[i]]
        c2 <- entry$content
        if (is.list(c2)) {
            for (block in c2) {
                if (is.list(block) &&
                    identical(block$type %||% "", "tool_use")) {
                    tid <- block$id %||% ""
                    if (nzchar(tid)) {
                        prefix_uses <- c(prefix_uses, tid)
                    }
                }
            }
        }
        for (call in entry$tool_calls %||% list()) {
            tid <- call$id %||% ""
            if (nzchar(tid)) {
                prefix_uses <- c(prefix_uses, tid)
            }
        }
        if (.compact_is_codex_output(entry)) {
            for (item in entry$output %||% list()) {
                if (identical(item$type %||% "", "function_call")) {
                    tid <- item$call_id %||% item$id %||% ""
                    if (nzchar(tid)) {
                        prefix_uses <- c(prefix_uses, tid)
                    }
                }
            }
        }
    }
    if (length(prefix_uses) == 0L) {
        return(FALSE)
    }
    # Collect matching result ids in the same prefix.
    prefix_results <- character(0)
    for (i in seq_len(cut)) {
        entry <- history[[i]]
        c2 <- entry$content
        if (is.list(c2)) {
            for (block in c2) {
                if (is.list(block) &&
                    identical(block$type %||% "", "tool_result")) {
                    tid <- block$tool_use_id %||% ""
                    if (nzchar(tid)) {
                        prefix_results <- c(prefix_results, tid)
                    }
                }
            }
        }
        if (identical(entry$role %||% "", "tool")) {
            tid <- entry$tool_call_id %||% entry$id %||% ""
            if (nzchar(tid)) {
                prefix_results <- c(prefix_results, tid)
            }
        }
        if (identical(entry$type, "function_call_output")) {
            tid <- entry$call_id %||% ""
            if (nzchar(tid)) {
                prefix_results <- c(prefix_results, tid)
            }
        }
    }
    open <- setdiff(prefix_uses, prefix_results)
    length(open) > 0L
}

# Stripped-down summarization prompt — same shape the CLI uses.
.compact_summary_prompt <- paste(
                                 "Summarize this conversation concisely, preserving:",
                                 "1. What was accomplished (completed tasks, files modified)",
                                 "2. Current work in progress",
                                 "3. Key decisions and constraints",
                                 "4. Pending tasks or next steps",
                                 "5. Any errors encountered and their resolution",
                                 "",
                                 "Be specific about file names, function names, and technical details.",
                                 "Format as a structured summary the assistant can use to continue the work.",
                                 sep = "\n"
)

#' Summarize the prefix of a history slice via the LLM.
#'
#' Returns the summary text on success or NULL on any error
#' (including timeout). Caller leaves history intact on NULL.
#' @param slice List of history entries to summarize (the part being
#'   compacted; the recent tail is excluded).
#' @param provider Provider name.
#' @param model Model name.
#' @param timeout_seconds Hard wall on the summarizer call.
#' @param summary_prompt Optional caller-supplied summarization instructions.
#'   NULL uses corteza's general coding-agent brief.
#' @keywords internal
compact_summarize_slice <- function(slice, provider = "anthropic",
                                    model = NULL, timeout_seconds = 60L,
                                    summary_prompt = NULL) {
    if (length(slice) == 0L) {
        return(NULL)
    }
    rendered <- vapply(slice, .compact_render_entry, character(1L))
    conv_text <- paste(.compact_trim_total(rendered), collapse = "\n\n")
    instructions <- summary_prompt %||% .compact_summary_prompt
    prompt <- sprintf("%s\n\n---\nConversation to summarize:\n%s",
                      instructions, conv_text)
    setTimeLimit(elapsed = timeout_seconds, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
    result <- tryCatch(
                       llm.api::chat(
                                     prompt = prompt,
                                     provider = provider,
                                     model = model,
                                     system = paste("You are a helpful assistant that creates",
                "concise conversation summaries."),
                                     temperature = 0.3
        ),
                       error = function(e) {
        log_event("subagent_compact_failed",
                  reason = "summarizer_error",
                  error = conditionMessage(e), level = "warn")
        NULL
    }
    )
    if (is.null(result)) {
        return(NULL)
    }
    summary <- as.character(result$content %||% "")
    attr(summary, "usage") <- result$usage %||% NULL
    summary
}

#' Replace the compacted prefix of a session's history with a
#' single assistant summary message.
#'
#' Pure function: returns the new history list, doesn't mutate
#' anything. The summary is prefixed with a `[compacted history]`
#' tag (followed by a blank line) so it's visually distinct in the
#' transcript.
#' @keywords internal
compact_rewrite_history <- function(history, cut, summary) {
    if (cut <= 0L || cut >= length(history)) {
        return(history)
    }
    kept <- history[(cut + 1L):length(history)]
    first_kept <- kept[[1L]]
    first_role <- first_kept$role %||% ""
    kept_starts_with_assistant <- identical(first_role, "assistant") ||
    .compact_is_codex_output(first_kept)
    # Between complete user turns, an assistant summary naturally precedes the
    # next kept user prompt. When splitting one long tool trajectory, the kept
    # tail starts with an assistant tool call, so the summary must be a user
    # message to preserve provider alternation and give that tail its context.
    summary_entry <- list(
                          role = if (kept_starts_with_assistant) "user" else "assistant",
                          content = sprintf("[compacted history]\n\n%s", summary)
    )
    c(list(summary_entry), kept)
}

#' Maybe compact a turn_session's in-memory history.
#'
#' Decision points:
#'   - Compaction mode off → return invisibly without checking.
#'   - History shorter than `min_messages` → skip (nothing to gain).
#'   - Live token usage below threshold → skip.
#'   - No safe cut available (e.g. open tool_use) → skip.
#'   - Summarizer fails → log and leave history intact.
#'
#' On success, mutates `session$history` in place. Returns invisibly
#' TRUE if compaction ran successfully, FALSE otherwise.
#'
#' @param session A turn_session (`new_session()`).
#' @param config Full corteza config (post-defaults).
#' @param kind Optional marker. "archive_holder" skips compaction
#'   entirely so seeded transcript history is preserved.
#' @param tools Optional exact model-facing tool schema. Direct `turn()`
#'   callers with custom tools should pass it; otherwise tools are resolved
#'   from `session$tools_filter` as before.
#' @param system Optional exact model-facing system prompt.
#' @param threshold Optional explicit percentage. When omitted, parent/direct
#'   sessions use `config$context_compact_pct` and subagents use their stricter
#'   inherited policy. Explicit NULL disables compaction.
#' @param min_messages Optional minimum history entries. The ordinary policy
#'   uses its configured value; forced overflow recovery may lower this because
#'   a few individual entries can themselves exceed the context window.
#' @param reason Short lifecycle marker recorded in the compaction event.
#' @keywords internal
maybe_compact_turn_session <- function(session, config, kind = NULL,
                                       tools = NULL, system = NULL,
                                       threshold, min_messages,
                                       reason = "threshold") {
    if (identical(kind, "archive_holder")) {
        return(invisible(FALSE))
    }
    cc <- config$subagents$context_compaction %||% list()
    if (missing(threshold)) {
        threshold <- if (isTRUE(session$is_subagent)) {
            subagent_compact_threshold(config)
        } else {
            as.numeric(config$context_compact_pct %||% 90L)
        }
    }
    if (is.null(threshold)) {
        return(invisible(FALSE))
    }
    history <- session$history %||% list()
    if (missing(min_messages)) {
        min_messages <- as.integer(cc$min_messages %||% 6L)
    } else {
        min_messages <- as.integer(min_messages)
    }
    if (length(history) < min_messages) {
        return(invisible(FALSE))
    }
    # Resolve the same model turn() will run with; mirrors
    # subagent_live_token_count() so /agents, compaction, and the
    # next API call all reason about the same model identity.
    model <- session$model_map$cloud %||%
    default_provider_model(session$provider)
    # Estimate against the same tools turn() will send. turn()
    # resolves tools from session$tools_filter when tools is NULL,
    # so passing NULL here would undercount the live context for any
    # subagent with an active tool filter.
    tools_for_estimate <- tools %||% tryCatch(
        skills_as_api_tools(session$tools_filter),
        error = function(e) NULL
    )
    system_for_estimate <- system %||% session$system
    used <- estimate_live_context_tokens(list(history = history),
        system_prompt = system_for_estimate, tools = tools_for_estimate)
    limit <- session$context_window %||% config$context_window %||%
    context_limit_for_model(model, provider = session$provider)
    if (is.null(limit) || limit <= 0L) {
        pct <- 0
    } else {
        pct <- 100 * used / limit
    }
    if (pct < threshold) {
        return(invisible(FALSE))
    }
    cut <- compact_find_cut(history,
                            keep_recent_turns = cc$keep_recent_turns %||% 1L,
                            keep_recent_tokens = cc$keep_recent_tokens %||%
                            20000L)
    if (cut <= 0L) {
        log_event("subagent_compact_skipped",
                  reason = "no_safe_cut", history_len = length(history))
        return(invisible(FALSE))
    }
    slice <- history[seq_len(cut)]
    summary <- compact_summarize_slice(
                                       slice, provider = session$provider %||% "anthropic",
                                       model = model,
                                       timeout_seconds = as.integer(cc$timeout_seconds %||% 60L),
                                       summary_prompt = session$compaction_prompt %||% NULL)
    if (is.null(summary) || !nzchar(summary)) {
        return(invisible(FALSE))
    }
    summary_usage <- attr(summary, "usage", exact = TRUE)
    summary <- as.character(summary)
    rewritten <- compact_rewrite_history(history, cut, summary)
    event <- list(
                  summary = summary,
                  history_before = history,
                  history_after = rewritten,
                  cut = cut,
                  tokens_before = used,
                  threshold_pct = threshold,
                  context_pct = pct,
                  context_window = limit,
                  provider = session$provider,
                  model = model,
                  usage = summary_usage,
                  reason = reason,
                  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    # Lifecycle hooks run before the destructive in-memory rewrite. A durable
    # host such as the ARC driver can persist the exact provider-native prefix;
    # if that checkpoint fails, its error prevents history from being lost.
    if (is.function(session$on_compaction)) {
        session$on_compaction(event)
    }
    session$history <- rewritten
    session$last_compaction <- event
    session$compaction_count <- as.integer(session$compaction_count %||% 0L) +
    1L
    log_event("subagent_compact_applied",
              before_len = length(history),
              after_len = length(session$history),
              threshold_pct = threshold,
              pre_pct = pct,
              kind = kind %||% if (isTRUE(session$is_subagent)) {
            "subagent"
        } else {
            "turn"
        })
    invisible(TRUE)
}
