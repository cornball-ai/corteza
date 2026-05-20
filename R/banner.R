# Startup banner for `corteza::chat()` and the `~/bin/corteza` CLI.
# Renders the brain-corn silhouette using the yellow-square emoji
# (U+1F7E8) as a corn kernel. No ANSI escapes -- the emoji is
# colorful on its own and renders identically across iTerm2,
# gnome-terminal, kitty, alacritty, xterm.js / RStudio Server, and
# Windows Terminal. The version, model, provider, tool count, and
# /help / /quit hints sit between kernels as plain text.

#' The yellow-square emoji used as a corn kernel. Source is the
#' Unicode escape so the file stays ASCII-only (CRAN requires R
#' source code to be ASCII; non-ASCII in comments is also flagged
#' by R CMD check on some platforms, hence the escape here too).
#' @noRd
.KERNEL <- "\U0001F7E8"

#' Banner template, transcribed from the user's emoji silhouette
#' mockup. Each `Y` is one corn kernel; at render time it's
#' replaced with the yellow-square emoji. Each `${name}` slot is
#' substituted with dynamic text; literal spaces around text
#' placeholders give a clean kernel-to-text gap.
#'
#' The width per row is irregular -- that's the brain silhouette,
#' wider in the middle and tapered at top and bottom. Substituted
#' text shifts the right boundary slightly when its length differs
#' from the template's, so we cap each slot at the width it
#' occupies in the mockup to keep the shape recognizable.
#' @noRd
.BANNER_TEMPLATE <- c("                 YYYYYYYYYY",
                      "            YYYYYYYYYYYYYYY",
                      "       YYYY corteza Y ${version}YYYYYY",
                      "    YYYYYYYYYYYYYYYYYYYYY",
                      " YYYYYY ${model} Y ${provider}YYYYYYYY",
                      "  YYYYYYYYYYYYYYYYYYYYYYYYYY",
                      "       YYYYY /help Y /quit YYYYYYYYY",
                      "        YYYYYYYYYYYYYYYY", "               YYYYYYYY")

#' Truncate a slot string to its template width so the silhouette
#' doesn't distort too much for long values. ASCII-only ellipsis.
#' @noRd
.banner_truncate <- function(s, max_w) {
    s <- as.character(s)
    if (nchar(s) <= max_w) {
        return(s)
    }
    paste0(substr(s, 1L, max_w - 3L), "...")
}

#' Drop the 4th-component dev marker from a version string so the
#' banner reads `v0.6.6` not `v0.6.6.16`. Keeps just the first three
#' dot-separated components.
#' @noRd
.banner_short_version <- function(v) {
    parts <- strsplit(as.character(v), ".", fixed = TRUE)[[1]]
    paste(utils::head(parts, 3L), collapse = ".")
}

#' Substitute `${name}` placeholders in a banner template line.
#' @noRd
.banner_substitute <- function(line, vars) {
    for (nm in names(vars)) {
        pat <- sprintf("\\$\\{%s\\}", nm)
        line <- sub(pat, vars[[nm]], line, perl = TRUE)
    }
    line
}

#' Replace each `Y` placeholder with the yellow-square emoji.
#' @noRd
.banner_kernels <- function(line) {
    gsub("Y", .KERNEL, line, fixed = TRUE)
}

#' Render the corteza startup banner. Each slot in the template
#' is truncated to its mockup width so a long model / provider
#' name doesn't blow out the silhouette's right edge.
#'
#' @param version Corteza version string, e.g. `"0.6.6.16"`. The
#'   4th-component dev marker is dropped for display.
#' @param model Display model name (already resolved by caller).
#' @param provider Provider name.
#' @param ... Currently unused; accepts and ignores extra args
#'   (e.g. legacy `tools_count`) so callers can be updated
#'   incrementally.
#' @return Character scalar with embedded newlines, ready to `cat()`.
#' @noRd
corteza_startup_banner <- function(version, model, provider, ...) {
    # Pad model right-aligned to 9 chars, provider left-aligned to
    # 8 chars so the row-5 width stays constant across name
    # lengths. Without this, swapping kimi-k2.6 (9 chars) for
    # gpt-4o (6 chars) would shift row 5's right edge by 3 cells
    # and break the brick offset against rows 4 and 6.
    .pad_right <- function(s, w) sprintf("%*s", w, .banner_truncate(s, w))
    .pad_left <- function(s, w) sprintf("%-*s", w, .banner_truncate(s, w))
    vars <- list(version = .banner_truncate(
            paste0("v", .banner_short_version(version)), 9L
        ),
                 model = .pad_right(model, 9L),
                 provider = .pad_left(provider, 8L))
    lines <- vapply(.BANNER_TEMPLATE, function(row) {
        .banner_kernels(.banner_substitute(row, vars))
    },
                    character(1),
                    USE.NAMES = FALSE
    )
    paste(lines, collapse = "\n")
}

