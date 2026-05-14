# ANSI color helpers shared between inst/bin/corteza and
# corteza::chat(). Both surfaces print to a terminal; both should use
# the same palette so output looks consistent regardless of how the
# user launched the agent.

#' Detect whether the current stdout supports ANSI escape sequences.
#'
#' On Unix, `isatty(stdout())` is the right check. On Windows, modern
#' terminals (Windows Terminal, ConEmu, VS Code's integrated terminal)
#' set environment variables we can sniff; legacy `cmd.exe` doesn't
#' interpret VT sequences and returns FALSE.
#' @return Single logical.
#' @noRd
ansi_supported <- function() {
    # NO_COLOR / FORCE_COLOR are the conventional overrides; they win
    # over auto-detection so users can force either side. RStudio's R
    # console pane is not a tty (isatty(stdout()) is FALSE) but it
    # does render ANSI escape sequences — without the RSTUDIO check,
    # corteza::chat() would emit plain text in RStudio while the
    # terminal CLI gets colored output.
    if (nzchar(Sys.getenv("NO_COLOR"))) {
        return(FALSE)
    }
    if (nzchar(Sys.getenv("FORCE_COLOR"))) {
        return(TRUE)
    }
    if (identical(Sys.getenv("RSTUDIO"), "1")) {
        return(TRUE)
    }
    if (.Platform$OS.type == "windows") {
        return(any(nzchar(Sys.getenv(c("WT_SESSION", "ConEmuANSI",
                                       "TERM_PROGRAM")))))
    }
    isatty(stdout())
}

#' ANSI color palette as a named list.
#'
#' When `ansi_supported()` is FALSE every entry is the empty string,
#' so `cat(sprintf("%sfoo%s", color$bold, color$reset))` degrades
#' cleanly to `cat("foo")`. Every consumer should read this once at
#' setup and reuse the result.
#' @return A list with entries: reset, bold, dim, red, green, yellow,
#'   blue, magenta, cyan, white, bright_red, bright_green,
#'   bright_yellow, bright_blue, bright_magenta, bright_cyan.
#' @noRd
ansi_colors <- function() {
    keys <- c("reset", "bold", "dim", "red", "green", "yellow", "blue",
              "magenta", "cyan", "white", "bright_red", "bright_green",
              "bright_yellow", "bright_blue", "bright_magenta",
              "bright_cyan")
    if (!ansi_supported()) {
        return(stats::setNames(as.list(rep("", length(keys))), keys))
    }
    list(reset = "\033[0m", bold = "\033[1m", dim = "\033[2m",
         red = "\033[31m", green = "\033[32m", yellow = "\033[33m",
         blue = "\033[34m", magenta = "\033[35m", cyan = "\033[36m",
         white = "\033[37m",
         bright_red = "\033[91m", bright_green = "\033[92m",
         bright_yellow = "\033[93m", bright_blue = "\033[94m",
         bright_magenta = "\033[95m", bright_cyan = "\033[96m")
}

#' Colorize a unified-diff string for terminal display.
#'
#' Matches `git diff` / `git --color=always`'s palette: green additions,
#' red deletions, cyan hunk headers, bold file headers, dim metadata
#' lines. The `+++ ` / `--- ` file-header check has to run before the
#' bare `+` / `-` check or the file-header lines would be colored as
#' add/delete rows.
#'
#' Returns `text` unchanged when the terminal doesn't support ANSI.
#' @param text Character scalar, raw diff output.
#' @param palette Optional palette from \code{ansi_colors()}; tests pass
#'   a forced palette to assert escape sequences are emitted.
#' @return Character scalar with ANSI escapes interleaved.
#' @noRd
colorize_diff <- function(text, palette = ansi_colors()) {
    if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
        return(text)
    }
    if (!nzchar(palette$reset)) {
        return(text)
    }
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    paint <- function(ln) {
        if (startsWith(ln, "diff --git ") || startsWith(ln, "index ") ||
            startsWith(ln, "similarity ") || startsWith(ln, "rename ") ||
            startsWith(ln, "new file") || startsWith(ln, "deleted file")) {
            paste0(palette$dim, ln, palette$reset)
        } else if (startsWith(ln, "+++ ") || startsWith(ln, "--- ")) {
            paste0(palette$bold, ln, palette$reset)
        } else if (startsWith(ln, "@@")) {
            paste0(palette$cyan, ln, palette$reset)
        } else if (startsWith(ln, "+")) {
            paste0(palette$green, ln, palette$reset)
        } else if (startsWith(ln, "-")) {
            paste0(palette$red, ln, palette$reset)
        } else {
            ln
        }
    }
    paste(vapply(lines, paint, character(1L), USE.NAMES = FALSE),
          collapse = "\n")
}

