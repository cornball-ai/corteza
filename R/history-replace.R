# The one way a session's history is replaced.
#
# A Claude model that thinks by default signs each thinking block against
# the conversation that produced it: the system text, the set of tools
# and every message before the block. Where the provider enforces that,
# a request replaying a block after any of the three changed comes back
#
#   API error (400): messages.N.content.M: Invalid `signature` in
#   `thinking` block. The block is bound to a different conversation.
#
# and so does every retry of it.
#
# Appending to a history keeps its blocks valid. Replacing it does not:
# a compaction keeps a tail whose blocks were signed with the summarized
# turns in front of them, a context edit changes text they follow, and a
# restore puts a history into a process that built its system text anew.
# The provider accepts a history with every thinking block removed, so
# that is what a replacement holds, and the session keeps that history,
# so the next request sends the same thing.
#
# history_replace() is the only function that replaces session$history.
# inst/tinytest/test_history_replace.R reads every function of the
# namespace and fails on an assignment to a history that its table does
# not list.
#
# One kind of session keeps its blocks: one that thinks on a budget it
# names (`thinking_budget_tokens`). See .thinks_on_budget().

THINKING_BLOCK_TYPES <- c("thinking", "redacted_thinking")

# What stands in for an assistant entry that held nothing but thinking,
# so the entries keep their count and their order.
THINKING_PLACEHOLDER <- "[reasoning removed]"

.is_thinking_block <- function(block) {
    is.list(block) && is.character(block$type) && length(block$type) == 1L &&
    block$type %in% THINKING_BLOCK_TYPES
}

#' Remove the thinking blocks from a history.
#'
#' Only an assistant entry whose content is a list of typed blocks can
#' hold one, which is the Anthropic shape. Text and tool-call blocks
#' stay. A history in another provider's shape has no such block and
#' comes back as it was, and so does an entry that holds none.
#'
#' @param history A provider-native history list, or NULL.
#' @return `list(history, removed)`: the history without thinking
#'   blocks, and how many were removed.
#' @noRd
history_strip_thinking <- function(history) {
    history <- history %||% list()
    removed <- 0L
    for (i in seq_along(history)) {
        entry <- history[[i]]
        if (!is.list(entry) || !identical(entry$role, "assistant")) {
            next
        }
        content <- entry$content
        if (!is.list(content)) {
            next
        }
        thinking <- vapply(content, .is_thinking_block, logical(1))
        if (!any(thinking)) {
            next
        }
        removed <- removed + sum(thinking)
        kept <- content[!thinking]
        if (!length(kept)) {
            kept <- list(list(type = "text", text = THINKING_PLACEHOLDER))
        }
        history[[i]]$content <- kept
    }
    list(history = history, removed = removed)
}

# Does this session think on a budget it names? Two facts about the
# models that take one, both from the provider. None of them binds a
# thinking block to its conversation: no model that runs that check
# accepts a budget. And they want the assistant entry of a tool round
# to keep the thinking it opened with for as long as its results are
# pending, which is the state a rewrite inside the agent loop leaves
# the history in. So a history replaced for such a session keeps its
# blocks: removing them there prevents nothing and can cost the turn.
.thinks_on_budget <- function(session) {
    !is.null(tryCatch(.session_thinking_budget(session),
                      error = function(e) NULL))
}

#' The history a session may hold in place of the one it has.
#'
#' history_strip_thinking(), except for a session that thinks on a
#' budget (.thinks_on_budget()), whose history comes back as it was.
#' A caller that has to describe a replacement before making it (a
#' compaction's event) calls this; history_replace() calls it for
#' everyone, and a second pass changes nothing.
#'
#' @return `list(history, removed)`.
#' @noRd
history_normalize <- function(session, history) {
    if (.thinks_on_budget(session)) {
        return(list(history = history %||% list(), removed = 0L))
    }
    history_strip_thinking(history)
}

#' Replace a session's history.
#'
#' Three steps, in this order. The replacement loses its thinking
#' blocks (history_normalize()). The session's journal, where it has
#' one, records the history being replaced and what replaces it. Then
#' the session gets the replacement. An error from the journal leaves
#' the session's history as it was, so a failed write costs the rewrite
#' and never the record.
#'
#' @param session A session environment from new_session().
#' @param after The replacement history.
#' @param reason Short marker for the journal segment and the log.
#' @param before The history being replaced, as the journal should
#'   record it. NULL takes the session's own.
#' @param summary,edits Passed on to journal_rewrite().
#' @return The history the session now holds, invisibly.
#' @noRd
history_replace <- function(session, after, reason, before = NULL,
                            summary = NULL, edits = NULL) {
    stripped <- history_normalize(session, after)
    after <- stripped$history
    if (is.null(before)) {
        before <- session$history %||% list()
    }
    if (isTRUE(session$.protect_review_inputs) && !identical(reason, "scope")) {
        original <- .review_inputs(before)
        inputs <- .review_inputs(after)
        if (!.review_inputs_intact(original, inputs)) {
            stop("The reviewer's recorded instructions would change; a person must decide.",
                 call. = FALSE)
        }
    }
    # Responses reasoning belongs to the input that produced it. An edit can
    # keep the unchanged prefix, including its reasoning and tool-call ids.
    indexed <- list()
    if (reason %in% c("context_index", "context_edit", "context_edit_rejected")) {
        changed <- FALSE
        for (i in seq_along(after)) {
            entry <- after[[i]]
            changed <- changed || i > length(before) || !identical(entry, before[[i]])
            if (!changed || !identical(entry$type, ".openai_codex_output")) next
            after[[i]]$output <- Filter(function(item) {
                !identical(item$type, "reasoning")
            }, entry$output)
            if (!identical(entry, after[[i]]) && !is.null(.responses_text_message(entry))) {
                label <- .context_edit_label(.compact_entry_body(entry), "m")
                if (!is.null(label) && identical(session$.context_edit$messages[[label]], entry)) {
                    indexed[[label]] <- after[[i]]
                }
            }
        }
    }
    journal_rewrite(session, before, after, reason,
                    summary = summary, edits = edits)
    for (label in names(indexed)) session$.context_edit$messages[[label]] <- indexed[[label]]
    session$history <- after
    # A rewrite that may have taken the notes of mode and task changes
    # with it: the next prompt states the session again
    # (R/request-prefix.R).
    .prefix_history_rewritten(session, reason)
    if (stripped$removed > 0L) {
        log_event("thinking_removed", reason = reason,
                  blocks = stripped$removed)
    }
    invisible(after)
}

# Is this the provider refusing a replayed thinking block? llm.api
# raises the provider's text behind "API error (400): ". The sentences
# after "bound to a different conversation" vary and may be missing.
# The same opening clause without that sentence is a signature the
# provider could not read at all, and the same remedy answers it.
.is_bound_thinking_error <- function(e) {
    msg <- conditionMessage(e)
    grepl("API error (400)", msg, fixed = TRUE) &&
    (grepl("bound to a different conversation", msg, fixed = TRUE) ||
        grepl("Invalid `signature` in `(redacted_)?thinking` block", msg))
}

# Answer that refusal: remove every thinking block from the session's
# history, through history_replace(), so the strip is recorded and the
# blocks stay out of every later request. The provider's reference says
# a strip made for one retry alone fails again on the next turn. The
# blocks go whatever the session's thinking settings say: the provider
# has just refused them.
#
# Returns the number of blocks removed. 0 means the caller raises the
# provider's error: the history held no thinking block, so a second
# request would be the first one again, or the journal could not record
# the strip.
.bound_thinking_recover <- function(session, error) {
    stripped <- history_strip_thinking(session$history)
    blocks <- stripped$removed
    if (blocks == 0L) {
        return(0L)
    }
    recorded <- tryCatch({
        history_replace(session, stripped$history, "thinking_refused")
        TRUE
    }, error = function(e) {
        log_event("thinking_recovery_failed", error = conditionMessage(e),
                  level = "warn")
        FALSE
    })
    if (!recorded) {
        return(0L)
    }
    # The provider's text names the block it refused and, often, what
    # changed in front of it. That is the lead for whoever reads this.
    refusal <- conditionMessage(error)
    log_event("thinking_refused", blocks = blocks,
              error = substr(refusal, 1L, 500L), level = "warn")
    message(sprintf(paste("turn: the provider refused replayed thinking",
                          "(%s); removed %d %s and went on"),
                    substr(refusal, 1L, 200L), blocks,
                    ngettext(blocks, "block", "blocks")))
    blocks
}
