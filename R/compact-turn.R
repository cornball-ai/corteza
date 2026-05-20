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
#' @keywords internal
compact_find_cut <- function(history, keep_recent_turns = 1L) {
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
    if (length(user_starts) <= as.integer(keep_recent_turns)) {
        return(0L)
    }
    # Cut just before the start of the (keep_recent + 1)th-from-last
    # user turn (i.e., the boundary is the first kept user turn).
    keep <- as.integer(keep_recent_turns)
    boundary <- user_starts[length(user_starts) - keep + 1L]
    cut <- boundary - 1L
    if (cut <= 0L) {
        return(0L)
    }
    # Don't split any tool_use / tool_result pair. Walk the cut back
    # until every tool_use in the prefix `history[1..cut]` has its
    # matching tool_result also in that prefix — i.e., no dangling
    # tool_use whose tool_result lives in the kept tail.
    while (cut > 0L && compact_prefix_has_unmatched_tool_use(history, cut)) {
        cut <- cut - 1L
    }
    as.integer(cut)
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
    # Collect tool_use ids in prefix.
    prefix_uses <- character(0)
    for (i in seq_len(cut)) {
        c2 <- history[[i]]$content
        if (!is.list(c2)) {
            next
        }
        for (block in c2) {
            if (identical(block$type %||% "", "tool_use")) {
                tid <- block$id %||% ""
                if (nzchar(tid)) {
                    prefix_uses <- c(prefix_uses, tid)
                }
            }
        }
    }
    if (length(prefix_uses) == 0L) {
        return(FALSE)
    }
    # Collect tool_result ids in prefix to remove already-matched ones.
    prefix_results <- character(0)
    for (i in seq_len(cut)) {
        c2 <- history[[i]]$content
        if (!is.list(c2)) {
            next
        }
        for (block in c2) {
            if (identical(block$type %||% "", "tool_result")) {
                tid <- block$tool_use_id %||% ""
                if (nzchar(tid)) {
                    prefix_results <- c(prefix_results, tid)
                }
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
#' @keywords internal
compact_summarize_slice <- function(slice, provider = "anthropic",
                                    model = NULL, timeout_seconds = 60L) {
    if (length(slice) == 0L) {
        return(NULL)
    }
    conv_text <- vapply(slice, function(entry) {
        sprintf("[%s]: %s", entry$role %||% "?",
                archival_history_entry_to_text(entry))
    }, character(1))
    conv_text <- paste(conv_text, collapse = "\n\n")
    prompt <- sprintf("%s\n\n---\nConversation to summarize:\n%s",
                      .compact_summary_prompt, conv_text)
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
    as.character(result$content %||% "")
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
    summary_entry <- list(
                          role = "assistant",
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
#' @keywords internal
maybe_compact_turn_session <- function(session, config, kind = NULL) {
    if (identical(kind, "archive_holder")) {
        return(invisible(FALSE))
    }
    cc <- config$subagents$context_compaction %||% list()
    threshold <- subagent_compact_threshold(config)
    if (is.null(threshold)) {
        return(invisible(FALSE))
    }
    history <- session$history %||% list()
    min_messages <- as.integer(cc$min_messages %||% 6L)
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
    tools_for_estimate <- tryCatch(skills_as_api_tools(session$tools_filter),
                                   error = function(e) NULL)
    pct <- context_usage_pct(list(history = history), model = model,
                             system_prompt = session$system,
                             tools = tools_for_estimate)
    if (pct < threshold) {
        return(invisible(FALSE))
    }
    cut <- compact_find_cut(history,
                            keep_recent_turns = cc$keep_recent_turns %||% 1L)
    if (cut <= 0L) {
        log_event("subagent_compact_skipped",
                  reason = "no_safe_cut", history_len = length(history))
        return(invisible(FALSE))
    }
    slice <- history[seq_len(cut)]
    summary <- compact_summarize_slice(
                                       slice, provider = session$provider %||% "anthropic",
                                       model = model,
                                       timeout_seconds = as.integer(cc$timeout_seconds %||% 60L))
    if (is.null(summary) || !nzchar(summary)) {
        return(invisible(FALSE))
    }
    session$history <- compact_rewrite_history(history, cut, summary)
    log_event("subagent_compact_applied",
              before_len = length(history),
              after_len = length(session$history),
              threshold_pct = threshold,
              pre_pct = pct)
    invisible(TRUE)
}

