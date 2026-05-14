# Compute a unified diff between two text scalars for display in the
# CLI / chat output. Shells out to `diff -u` because writing a correct
# unified-diff algorithm in pure R is significantly more code than the
# rest of this feature combined; if `diff` isn't on PATH we degrade to
# a one-line fallback rather than fail the tool call. The output is
# uncolored — coloring happens at render time via colorize_diff() so
# the same payload can be re-rendered when ANSI is unavailable.

#' Locate the system `diff` binary.
#'
#' Returns "" when no binary is found. Cached for the process lifetime
#' since PATH doesn't change during a corteza session.
#' @noRd
.diff_binary_cache <- new.env(parent = emptyenv())
.diff_binary <- function() {
    if (!is.null(.diff_binary_cache$value)) {
        return(.diff_binary_cache$value)
    }
    bin <- Sys.which("diff")
    .diff_binary_cache$value <- if (is.na(bin)) "" else unname(bin)
    .diff_binary_cache$value
}

#' Count added and removed lines in a unified-diff body.
#'
#' Ignores file headers (`+++ `, `--- `) and hunk headers (`@@`).
#' @noRd
.diff_summary_counts <- function(lines) {
    added <- 0L
    removed <- 0L
    for (ln in lines) {
        if (startsWith(ln, "+++ ") || startsWith(ln, "--- ") ||
            startsWith(ln, "@@") ||
            startsWith(ln, "diff --git") ||
            startsWith(ln, "index ")) {
            next
        }
        if (startsWith(ln, "+")) {
            added <- added + 1L
        } else if (startsWith(ln, "-")) {
            removed <- removed + 1L
        }
    }
    list(added = added, removed = removed)
}

#' Build a one-line summary like "Added 3 lines, removed 1 line".
#' @noRd
.diff_summary_line <- function(added, removed) {
    pl <- function(n, w) sprintf("%d %s%s", n, w, if (n == 1L) "" else "s")
    if (added == 0L && removed == 0L) {
        "No textual change"
    } else if (added == 0L) {
        sprintf("Removed %s", pl(removed, "line"))
    } else if (removed == 0L) {
        sprintf("Added %s", pl(added, "line"))
    } else {
        sprintf("Added %s, removed %s",
                pl(added, "line"), pl(removed, "line"))
    }
}

#' Compute a unified diff for terminal display.
#'
#' Returns NULL when the two inputs are byte-identical (signal to the
#' caller that no diff display is warranted). When `diff` isn't on PATH,
#' returns a fallback payload describing the size of the change without
#' the per-line content. Large diffs are truncated to keep the payload
#' bounded — both for serialization across the callr worker boundary
#' and for chat scrollback hygiene — but the `summary` counts always
#' reflect the full diff.
#'
#' @param old_text Character scalar, prior file contents. Empty string
#'   means "new file".
#' @param new_text Character scalar, new file contents.
#' @param path Character scalar, the file path the diff describes; used
#'   for the `+++` / `---` header labels.
#' @param max_lines Cap on the number of diff lines retained for
#'   display. Beyond this, a `[diff truncated: N more lines]` marker is
#'   appended in place of the rest. Set to `Inf` to disable.
#' @param max_chars Cap on total characters across retained lines.
#'   Tripped if a small number of long lines blow past the budget even
#'   though `max_lines` hasn't.
#' @return NULL if identical, else a list with:
#'   \itemize{
#'     \item \code{path}: input path
#'     \item \code{summary}: one-line summary string (always reflects
#'       the full diff, not the truncated lines)
#'     \item \code{lines}: character vector of uncolored diff lines
#'       (header + hunks). May be empty when only the fallback
#'       summary is available, or truncated for large diffs.
#'     \item \code{fallback}: logical TRUE when `diff` was unavailable
#'       and the payload is summary-only.
#'     \item \code{truncated}: logical TRUE when `lines` was clipped.
#'   }
#' @noRd
compute_unified_diff <- function(old_text, new_text, path,
                                 max_lines = 200L,
                                 max_chars = 20000L) {
    old_text <- old_text %||% ""
    new_text <- new_text %||% ""
    path <- path %||% "(unnamed)"

    if (identical(old_text, new_text)) {
        return(NULL)
    }

    bin <- .diff_binary()
    if (!nzchar(bin)) {
        # Fallback: approximate added/removed by line count delta. Not
        # accurate for arbitrary edits, but it's only used when the
        # user has no `diff` available, so we communicate the size of
        # the change rather than nothing.
        old_n <- if (nzchar(old_text)) {
            length(strsplit(old_text, "\n", fixed = TRUE)[[1]])
        } else 0L
        new_n <- if (nzchar(new_text)) {
            length(strsplit(new_text, "\n", fixed = TRUE)[[1]])
        } else 0L
        delta <- new_n - old_n
        summary <- if (delta == 0L) {
            sprintf("Content changed (%d lines, diff binary unavailable)",
                    new_n)
        } else if (delta > 0L) {
            sprintf("Net +%d line(s), diff binary unavailable", delta)
        } else {
            sprintf("Net %d line(s), diff binary unavailable", delta)
        }
        return(list(path = path, summary = summary, lines = character(),
                    fallback = TRUE))
    }

    old_file <- tempfile("corteza-old-")
    new_file <- tempfile("corteza-new-")
    on.exit({
        unlink(old_file, force = TRUE)
        unlink(new_file, force = TRUE)
    }, add = TRUE)

    # writeBin avoids platform line-ending translation; we want the
    # bytes diff sees to match the bytes that were written.
    writeBin(charToRaw(old_text), old_file)
    writeBin(charToRaw(new_text), new_file)

    res <- suppressWarnings(system2(
                                    bin,
                                    args = c("-u",
                                             "--label", shQuote(path),
                                             "--label", shQuote(path),
                                             shQuote(old_file),
                                             shQuote(new_file)),
                                    stdout = TRUE, stderr = TRUE
        ))
    # diff exits 0 (identical, handled above), 1 (differ), or 2 (error).
    status <- attr(res, "status") %||% 0L
    if (!identical(status, 0L) && !identical(status, 1L)) {
        return(list(path = path,
                    summary = sprintf("diff failed (status %d)", status),
                    lines = character(),
                    fallback = TRUE))
    }

    counts <- .diff_summary_counts(res)
    full_lines <- as.character(res)
    clipped <- .clip_diff_lines(full_lines, max_lines, max_chars)
    list(path = path,
         summary = .diff_summary_line(counts$added, counts$removed),
         lines = clipped$lines,
         fallback = FALSE,
         truncated = clipped$truncated)
}

#' Clip a diff-line vector to the configured budgets.
#'
#' Returns a list with the (possibly clipped) `lines` and a `truncated`
#' flag. When the budget is busted we keep the first N lines and append
#' a `[diff truncated: N more lines]` marker so the reader knows there
#' was more.
#' @noRd
.clip_diff_lines <- function(lines, max_lines, max_chars) {
    total <- length(lines)
    if (total == 0L) {
        return(list(lines = lines, truncated = FALSE))
    }

    keep <- min(total, as.integer(max_lines))
    head <- lines[seq_len(keep)]

    # Character budget: walk the kept lines until we'd exceed max_chars,
    # then drop the rest. Counts newlines so the budget matches what
    # the user actually sees.
    if (is.finite(max_chars)) {
        widths <- nchar(head, type = "bytes") + 1L
        running <- cumsum(widths)
        within <- which(running <= as.integer(max_chars))
        keep_chars <- if (length(within) == 0L) 0L else max(within)
        if (keep_chars < length(head)) {
            head <- head[seq_len(keep_chars)]
        }
    }

    truncated <- length(head) < total
    if (truncated) {
        dropped <- total - length(head)
        head <- c(head,
                  sprintf("[diff truncated: %d more line%s]",
                          dropped, if (dropped == 1L) "" else "s"))
    }
    list(lines = head, truncated = truncated)
}

#' Render a diff payload to the terminal.
#'
#' Used by both the CLI tool_handler in `inst/bin/corteza` and the
#' `observer_progress()` printer in `R/turn.R` so file-edit tool calls
#' look the same regardless of which entry point the user launched.
#' Skips quietly when the payload is NULL (i.e., the underlying texts
#' were identical and \code{compute_unified_diff()} returned nothing).
#'
#' @param diff Payload from \code{compute_unified_diff()}, or NULL.
#' @param palette Optional ANSI palette; tests force a specific palette.
#' @param indent Leading indent string for each printed line; matches
#'   the surrounding tool-call output.
#' @return Invisibly TRUE if anything was printed, FALSE otherwise.
#' @noRd
render_tool_diff <- function(diff, palette = ansi_colors(), indent = "  ") {
    if (is.null(diff)) {
        return(invisible(FALSE))
    }
    summary <- diff$summary %||% ""
    if (nzchar(summary)) {
        cat(sprintf("%s%s⎿ %s%s\n",
                    indent, palette$dim %||% "", summary, palette$reset %||% ""))
    }
    if (isTRUE(diff$fallback) || length(diff$lines) == 0L) {
        return(invisible(TRUE))
    }
    body <- colorize_diff(paste(diff$lines, collapse = "\n"), palette)
    body_lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
    for (ln in body_lines) {
        cat(sprintf("%s%s\n", indent, ln))
    }
    invisible(TRUE)
}
