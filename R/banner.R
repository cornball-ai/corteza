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

#' Column width of a rendered banner string: each kernel emoji is two
#' terminal columns, every other character is one.
#' @noRd
.banner_cols <- function(s) {
    kernels <- nchar(s, type = "chars") -
    nchar(gsub(.KERNEL, "", s, fixed = TRUE), type = "chars")
    nchar(s, type = "chars") + kernels
}

#' One half of the model / provider row: the name flanked by single
#' spaces, padded with kernels on its outer edge to a fixed `half`-column
#' width. `outer` is the side the kernels sit on -- "left" for the
#' provider (front kernels), "right" for the model (trailing kernels). A
#' lone space absorbs an odd remainder so the kernels keep the two-column
#' grid. Both halves share one width, so the centre kernel between them
#' never moves: a longer name only eats kernels on its own side.
#' @noRd
.banner_half <- function(name, half, outer) {
    name <- as.character(name %||% "")
    # A space precedes the name; a space follows it only when the name is
    # even-length. That keeps each half on the two-column grid without
    # adding a space that would shove the centre kernel off-centre -- an
    # odd-length name sits flush against what follows it (the centre kernel
    # for the provider, the trailing kernels for the model).
    if (nchar(name) %% 2L == 0L) {
        trailing <- " "
    } else {
        trailing <- ""
    }
    inner <- paste0(" ", name, trailing)
    pad <- (half - .banner_cols(inner)) %/% 2L
    if (pad < 0L) {
        return(inner)
    }
    kernels <- strrep(.KERNEL, pad)
    if (identical(outer, "left")) {
        paste0(kernels, inner)
    } else {
        paste0(inner, kernels)
    }
}

#' Render the model / provider row with the centre kernel fixed. The
#' provider sits left of centre and grows by eating front kernels; the
#' model sits right of centre and grows by eating trailing kernels. Each
#' half is a fixed 24 columns, so the centre square stays put however long
#' the names get -- and lands at the same column as the corteza|version
#' and /help|/quit dividers (rows 3 and 7), so all three stack vertically.
#' The row carries no leading indent for that alignment, so its left edge
#' juts one column past the rest of the silhouette by design.
#' @noRd
.banner_name_row <- function(model, provider) {
    paste0(.banner_half(provider, 24L, "left"), .KERNEL,
           .banner_half(model, 24L, "right"))
}

#' Render the corteza startup banner. The version slot is capped to its
#' mockup width; the model / provider row is rebuilt on the two-column
#' grid so full names show without bending the silhouette.
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
    vars <- list(version = .banner_truncate(
            paste0("v", .banner_short_version(version)), 9L
        ))
    lines <- vapply(.BANNER_TEMPLATE, function(row) {
        .banner_kernels(.banner_substitute(row, vars))
    },
                    character(1),
                    USE.NAMES = FALSE
    )
    # Row 5 carries the model and provider; render it on the two-column grid
    # so full names never truncate or bulge the silhouette.
    lines[[5L]] <- .banner_name_row(model, provider)
    paste(lines, collapse = "\n")
}
