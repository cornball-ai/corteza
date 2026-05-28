# Universal cap on tool-result text before it reaches model context.
#
# Every tool result - shell, run_r, read_handle, grep_files, fetch_url,
# a custom skill, anything - funnels through .flatten_mcp_result() in
# .make_tool_handler() (R/turn.R) on its way to llm.api. A single
# unbounded result (e.g. a bash command that prints 57k lines) lands in
# session$history via the history_callback and wedges the next model
# call against the provider token limit. admit_tool_result() is the
# guard at that chokepoint: it caps the model-facing text and stashes
# the full output as a handle so nothing is lost.
#
# compact_message_text() is the sibling for /compact: do_compact()
# pastes message bodies into one summarization prompt, so a giant result
# already in history would blow that prompt too. Eliding oversized
# bodies lets /compact recover an already-wedged session.

# Caps. Constants for now; promote to config if tuning is needed.
# Preview is intentionally a bit under the cap so the marker header
# (~6 lines / ~250 chars) plus the preview body stays under both caps.
.tool_output_max_chars <- 5000L
.tool_output_max_lines <- 50L
.tool_output_preview_lines <- 40L
.tool_output_preview_chars <- 4000L

.compact_max_chars_per_message <- 16000L
.compact_max_total_chars <- 120000L

#' Cap a flattened tool-result string before it reaches model context.
#'
#' Under both limits the text passes through unchanged. Over either
#' limit the full text is stashed as a line-vector handle (retrievable
#' with `read_handle` / via the `/last` buffer) and a compact marker is
#' returned in its place. The marker names the tool, the original
#' line/char counts, what's shown, and the handle to recover the rest.
#'
#' @param text Flattened tool-result string (length-1 character). Other
#'   shapes pass through untouched.
#' @param tool Tool name, for the marker.
#' @param max_chars,max_lines Caps that trigger truncation.
#' @param preview_lines,preview_chars Size of the preview head kept in
#'   the marker.
#' @return A length-1 character string: either `text` unchanged or a
#'   truncation marker.
#' @noRd
admit_tool_result <- function(text, tool = "tool",
                              max_chars = .tool_output_max_chars,
                              max_lines = .tool_output_max_lines,
                              preview_lines = .tool_output_preview_lines,
                              preview_chars = .tool_output_preview_chars) {
    # Only guard plain strings. A non-string here would be a flatten
    # bug; pass it through rather than mangle it.
    if (!is.character(text) || length(text) != 1L) {
        return(text)
    }
    n_chars <- nchar(text)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    n_lines <- length(lines)
    if (n_chars <= max_chars && n_lines <= max_lines) {
        return(text)
    }

    # Stash the full output as a vector of lines so read_handle's
    # head / str ops are useful on it later.
    stash <- with_handle(lines, summary_fn = function(x) {
        sprintf("captured %s output: %d lines, %d chars", tool, n_lines,
                n_chars)
    })

    preview <- .tool_output_preview(lines, preview_lines, preview_chars)
    sprintf(paste0(
                   "[tool output truncated]\n",
                   "tool: %s\n",
                   "original: %d lines, %d chars\n",
                   "showing: first %d lines / %d chars\n",
                   "full output stored as: %s\n",
                   "Inspect with read_handle(\"%s\", op = \"head\") or /last.\n\n%s"
        ),
            tool, n_lines, n_chars,
            min(preview_lines, n_lines), min(preview_chars, n_chars),
            stash$handle, stash$handle, preview)
}

# Preview head: first preview_lines lines, then hard-capped to
# preview_chars so a single pathologically long line can't blow the cap.
.tool_output_preview <- function(lines, preview_lines, preview_chars) {
    preview <- paste(utils::head(lines, preview_lines), collapse = "\n")
    if (nchar(preview) > preview_chars) {
        preview <- substr(preview, 1L, preview_chars)
    }
    preview
}

#' Render a message body for the compaction prompt, eliding huge ones.
#'
#' Short bodies render as-is. Bodies over `max_chars` are replaced with
#' an elision marker (line/char counts) plus a preview head, so a giant
#' tool result already in history can't blow the compaction prompt past
#' the model limit.
#'
#' @param text Message body (coerced to a length-1 string).
#' @param max_chars Bodies longer than this are elided.
#' @return A length-1 character string.
#' @noRd
compact_message_text <- function(text,
                                 max_chars = .compact_max_chars_per_message) {
    if (!is.character(text) || length(text) != 1L) {
        text <- paste(as.character(text), collapse = "\n")
    }
    n_chars <- nchar(text)
    if (n_chars <= max_chars) {
        return(text)
    }
    n_lines <- length(strsplit(text, "\n", fixed = TRUE)[[1L]])
    sprintf("[large message elided: %d lines, %d chars, first %d chars shown]\n%s",
            n_lines, n_chars, max_chars, substr(text, 1L, max_chars))
}

# Drop oldest rendered messages until the joined text fits the total
# budget, prepending a note about what was dropped. Per-message elision
# (compact_message_text) handles one giant result; this handles the
# case where many large messages still overflow in aggregate. Keeps the
# most recent messages, which matter most for continuing the work.
.compact_trim_total <- function(rendered,
                                max_total = .compact_max_total_chars) {
    n <- length(rendered)
    if (n == 0L) {
        return(rendered)
    }
    sep <- 2L # "\n\n" between messages
    if (sum(nchar(rendered)) + sep * (n - 1L) <= max_total) {
        return(rendered)
    }
    keep <- logical(n)
    running <- 0L
    for (i in rev(seq_len(n))) {
        running <- running + nchar(rendered[i]) + sep
        if (running > max_total) {
            break
        }
        keep[i] <- TRUE
    }
    dropped <- sum(!keep)
    kept <- rendered[keep]
    if (dropped > 0L) {
        kept <- c(sprintf(
                          "[%d earlier message(s) elided to fit the compaction budget]",
                          dropped),
                  kept)
    }
    kept
}
