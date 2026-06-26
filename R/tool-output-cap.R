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

# Deliberate content-read tools (read_file, git_diff): the agent asked
# to see the file, so give them a far larger budget than chatty tools --
# otherwise a whole-file read gets sliced into 50-line re-reads. Still
# bounded: a pathological multi-megabyte file stashes to a handle rather
# than wedging the model.
.tool_output_read_tools <- c("read_file", "git_diff")
.tool_output_read_max_chars <- 100000L
.tool_output_read_max_lines <- 2000L

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
    # Deliberate content reads (read_file, git_diff) get a far larger
    # budget so a whole-file read isn't sliced into 50-line re-reads --
    # but only when the caller didn't pin explicit caps.
    if (missing(max_chars) && missing(max_lines) &&
        tool %in% .tool_output_read_tools) {
        max_chars <- .tool_output_read_max_chars
        max_lines <- .tool_output_read_max_lines
    }
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

#' Render one history entry as "[label]: body" for the compaction
#' prompt.
#'
#' History entries are provider-native, so shapes vary: Anthropic and
#' OpenAI chat messages carry a `role`; OpenAI Responses (codex)
#' entries carry only a `type` (".openai_codex_output" holding the raw
#' output items, "function_call_output" for tool results) and no role
#' at all. sprintf() with a NULL argument returns character(0), so a
#' role-less entry used to kill do_compact()'s vapply -- exactly when
#' /compact was needed to recover a wedged session. This renderer
#' guarantees a length-1 string for any shape.
#'
#' @param m One history entry (any provider-native shape).
#' @return A length-1 character string.
#' @noRd
.compact_render_entry <- function(m) {
    if (!is.list(m)) {
        return(sprintf("[unknown]: %s",
                       compact_message_text(paste(as.character(m), collapse = "\n"))))
    }
    label <- .compact_scalar(m$role) %||% .compact_scalar(m$type) %||%
    "unknown"
    sprintf("[%s]: %s", label, compact_message_text(.compact_entry_body(m)))
}

# x if it is a length-1 non-NA character, else NULL (for %||% chains).
.compact_scalar <- function(x) {
    if (is.character(x) && length(x) == 1L && !is.na(x)) {
        x
    } else {
        NULL
    }
}

# Extract a text body from one provider-native history entry.
.compact_entry_body <- function(m) {
    # Codex assistant turn: raw Responses output items, no role/content.
    if (identical(m$type, ".openai_codex_output")) {
        return(.compact_codex_output_text(m$output))
    }
    # Codex tool result: output is the result string.
    if (identical(m$type, "function_call_output")) {
        return(paste(as.character(m$output %||% ""), collapse = "\n"))
    }
    body <- if (is.list(m$content)) {
        parts <- vapply(m$content, .compact_block_text, character(1L))
        paste(parts[nzchar(parts)], collapse = "\n")
    } else {
        paste(as.character(m$content %||% ""), collapse = "\n")
    }
    # OpenAI chat-completions assistant turns put tool invocations in
    # tool_calls beside (often empty) content; name them so the
    # summary sees what ran.
    calls <- vapply(m$tool_calls %||% list(), function(tc) {
        tc$name %||% tc$`function`$name %||% "?"
    }, character(1L))
    if (length(calls) > 0L) {
        body <- paste(c(body[nzchar(body)], sprintf("<tool call: %s>", calls)),
                      collapse = "\n")
    }
    body
}

# Text of one Anthropic-style content block (text, tool_use,
# tool_result, or anything else).
.compact_block_text <- function(block) {
    if (!is.list(block)) {
        return(paste(as.character(block), collapse = "\n"))
    }
    if (!is.null(block$text)) {
        return(paste(as.character(block$text), collapse = "\n"))
    }
    type <- block$type %||% ""
    if (identical(type, "tool_use")) {
        return(sprintf("<tool call: %s>", block$name %||% "?"))
    }
    if (identical(type, "tool_result")) {
        inner <- block$content
        if (is.list(inner)) {
            return(paste(vapply(inner, .compact_block_text, character(1L)),
                         collapse = "\n"))
        }
        return(paste(as.character(inner %||% ""), collapse = "\n"))
    }
    ""
}

# Text of a Codex Responses output-item list: message items contribute
# their text, function_call items their name. Reasoning items are
# skipped (encrypted payloads, useless in a summary).
.compact_codex_output_text <- function(output) {
    parts <- character()
    for (item in output %||% list()) {
        type <- item$type %||% ""
        if (identical(type, "message")) {
            for (content in item$content %||% list()) {
                if (!is.null(content$text)) {
                    parts <- c(parts, content$text)
                }
            }
        } else if (identical(type, "function_call")) {
            parts <- c(parts, sprintf("<tool call: %s>", item$name %||% "?"))
        }
    }
    paste(parts, collapse = "\n")
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
