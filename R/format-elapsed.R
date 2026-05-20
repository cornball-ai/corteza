# Duration formatting for the per-turn footer ("\u2500 Worked for 3m 18s \u2500").
#
# Pure formatter -- takes start and end times and returns a single
# character scalar. The CLI and chat() surfaces wrap the result in
# dim color + dash padding at their own call sites; the package
# version stays terminal-agnostic and testable without a clock.

#' Format the elapsed time between two POSIXct values as
#' "Worked for <human-readable duration>".
#'
#' Rules:
#' \itemize{
#'   \item < 1 second -> "Worked for <1s".
#'   \item < 1 minute -> "Worked for Ns" (integer seconds).
#'   \item < 1 hour -> "Worked for Mm Ss".
#'   \item >= 1 hour -> "Worked for Hh Mm Ss".
#' }
#'
#' Defaults are designed so that calling
#' \code{format_worked_for(start)} after a turn just works.
#'
#' @param start POSIXct-coerceable timestamp the turn started.
#' @param end Optional end timestamp; defaults to \code{Sys.time()}.
#' @return Character scalar.
#' @noRd
format_worked_for <- function(start, end = Sys.time()) {
    secs <- as.numeric(difftime(end, start, units = "secs"))
    if (!is.finite(secs) || secs < 0) {
        return("Worked for <1s")
    }
    if (secs < 1) {
        return("Worked for <1s")
    }
    total <- as.integer(round(secs))
    hours <- total %/% 3600L
    mins <- (total %% 3600L) %/% 60L
    s <- total %% 60L
    parts <- character(0L)
    if (hours > 0L) {
        parts <- c(parts, sprintf("%dh", hours))
    }
    if (mins > 0L || hours > 0L) {
        parts <- c(parts, sprintf("%dm", mins))
    }
    parts <- c(parts, sprintf("%ds", s))
    sprintf("Worked for %s", paste(parts, collapse = " "))
}

#' Detect the current terminal width.
#'
#' Reads `COLUMNS` (set by most shells when stdin is a tty) first, then
#' falls back to `getOption("width")` (R's own knowledge of its line
#' length, set by terminal emulators and `options(width = ...)`).
#' Returns NULL if neither is available so callers can use a static
#' default.
#' @noRd
detect_terminal_width <- function() {
    cols <- suppressWarnings(as.integer(Sys.getenv("COLUMNS", "")))
    if (!is.na(cols) && cols > 0L) {
        return(cols)
    }
    opt <- suppressWarnings(as.integer(getOption("width", 0L)))
    if (!is.na(opt) && opt > 0L) {
        return(opt)
    }
    NULL
}

#' Build the full "\u2500 Worked for X \u2500\u2500\u2500\u2500" footer line a chat()/CLI surface
#' prints at the end of a turn. Caller supplies the palette so the
#' dim color wrap matches the rest of its output. The line pads to
#' \code{width} characters with the horizontal-rule glyph so the
#' footer reads as a turn separator.
#'
#' @param start Turn start time.
#' @param end Optional turn end time; defaults to \code{Sys.time()}.
#' @param palette Optional palette from \code{ansi_colors()}.
#' @param width Target visible width. When NULL (default), the
#'   terminal width is detected via COLUMNS / options("width") so the
#'   footer spans the row. Set explicitly for tests or when a fixed
#'   width is preferred.
#' @return Character scalar (no trailing newline).
#' @noRd
turn_footer_line <- function(start, end = Sys.time(),
                             palette = ansi_colors(), width = NULL) {
    if (is.null(width)) {
        width <- detect_terminal_width() %||% 60L
    }
    body <- sprintf("\u2500 %s ", format_worked_for(start, end))
    pad_n <- max(width - nchar(body), 1L)
    line <- paste0(body, strrep("\u2500", pad_n))
    sprintf("%s%s%s", palette$dim %||% "", line, palette$reset %||% "")
}

