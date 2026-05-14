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
#' the per-line content.
#'
#' @param old_text Character scalar, prior file contents. Empty string
#'   means "new file".
#' @param new_text Character scalar, new file contents.
#' @param path Character scalar, the file path the diff describes; used
#'   for the `+++` / `---` header labels.
#' @return NULL if identical, else a list with:
#'   \itemize{
#'     \item \code{path}: input path
#'     \item \code{summary}: one-line summary string
#'     \item \code{lines}: character vector of uncolored diff lines
#'       (header + hunks). May be empty when only the fallback
#'       summary is available.
#'     \item \code{fallback}: logical TRUE when `diff` was unavailable
#'       and the payload is summary-only.
#'   }
#' @noRd
compute_unified_diff <- function(old_text, new_text, path) {
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
    list(path = path,
         summary = .diff_summary_line(counts$added, counts$removed),
         lines = as.character(res),
         fallback = FALSE)
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
