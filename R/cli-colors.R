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

