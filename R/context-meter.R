# /context display: one horizontal meter shared by CLI and chat() so
# both surfaces answer the two questions a user actually has — how
# full is context, and what's using it — without redundant prose or
# layout drift.
#
# The renderer is a pure formatter (no side effects, no Sys.time())
# so it tests cleanly. Callers compute the numbers and hand them in.

#' Map a percentage to a palette entry using the same four-band
#' breakdown the rest of the CLI uses (normal / warn / high / crit).
#' Returns the ANSI start sequence; pair with `palette$reset`.
#' @noRd
.context_pct_color <- function(pct, palette,
                               warn_pct = 75, high_pct = 90, crit_pct = 95) {
    if (pct >= crit_pct) {
        palette$bright_red %||% ""
    } else if (pct >= high_pct) {
        palette$bright_yellow %||% ""
    } else if (pct >= warn_pct) {
        palette$yellow %||% ""
    } else {
        palette$green %||% ""
    }
}

#' Build the bar portion of the /context display.
#'
#' The filled cells take the threshold-tinted color; empty cells are
#' dim dots; the auto-compact tick is a dim vertical bar at its
#' fractional position, kept visually quieter than the actual usage
#' fill per the design brief.
#'
#' @param pct Current usage as a percent of `limit` (0..100+).
#' @param compact_pct Threshold at which auto-compact would fire,
#'   for the subtle tick mark.
#' @param width Total cell count. Default 50.
#' @param palette ANSI palette.
#' @return Character scalar (one line, including the surrounding
#'   `[ ]` brackets).
#' @noRd
.context_meter_bar <- function(pct, compact_pct = 90,
                               width = 50L,
                               palette = ansi_colors(),
                               warn_pct = 75, high_pct = 90,
                               crit_pct = 95) {
    width <- as.integer(width)
    pct_clamped <- max(0, min(100, pct))
    used_cells <- as.integer(round(pct_clamped / 100 * width))
    used_cells <- max(0L, min(width, used_cells))
    compact_cell <- as.integer(round(compact_pct / 100 * width))
    compact_cell <- max(1L, min(width, compact_cell))

    fill_color <- .context_pct_color(pct, palette,
                                     warn_pct, high_pct, crit_pct)
    cells <- vapply(seq_len(width), function(i) {
        if (i <= used_cells) {
            sprintf("%s█%s", fill_color, palette$reset %||% "")
        } else if (i == compact_cell) {
            sprintf("%s│%s", palette$dim %||% "", palette$reset %||% "")
        } else {
            sprintf("%s.%s", palette$dim %||% "", palette$reset %||% "")
        }
    }, character(1L))
    paste0("[", paste(cells, collapse = ""), "]")
}

#' Format one breakdown row: `  <label>  <tokens>  <pct>%`.
#'
#' Label is left-padded to 8 chars; token count right-padded to 6;
#' percent omitted for rows under 1% of `used` so a noise row like
#' "history 56" doesn't show "0%".
#' @noRd
.context_breakdown_row <- function(label, tokens, used,
                                   palette = ansi_colors()) {
    tok_str <- format_tokens(tokens)
    pct <- if (used > 0L) tokens / used * 100 else 0
    pct_str <- if (pct >= 1) {
        sprintf("%d%%", as.integer(round(pct)))
    } else {
        ""
    }
    sprintf("  %-8s %6s  %s%s%s",
            label, tok_str,
            palette$dim %||% "", pct_str, palette$reset %||% "")
}

#' Render the full /context block.
#'
#' @param used Live token estimate.
#' @param limit Context window for the active model.
#' @param breakdown Named list of \code{label = tokens} entries (e.g.
#'   \code{list(system = 22000L, tools = 2700L, history = 56L)}).
#'   Order is preserved.
#' @param compact_pct Auto-compact threshold (default 90).
#' @param warn_pct, high_pct, crit_pct Color-band thresholds.
#' @param files Character vector of additional context files; empty
#'   means render the "No context files loaded." short note.
#' @param palette ANSI palette.
#' @param bar_width Bar width in cells (default 50).
#' @return Character scalar (multi-line, no trailing newline).
#' @noRd
format_context_block <- function(used, limit, breakdown,
                                 compact_pct = 90,
                                 warn_pct = 75, high_pct = 90,
                                 crit_pct = 95,
                                 files = character(0L),
                                 palette = ansi_colors(),
                                 bar_width = 50L) {
    used <- as.integer(round(used %||% 0))
    limit <- as.integer(round(limit %||% 0))
    pct <- if (limit > 0L) used / limit * 100 else 0

    # Right-align the "compact N%" tick so it lines up with the right
    # edge of the bar and stays visually distinct from the usage
    # numbers on the left.
    left_plain <- sprintf("Context  %s / %s  %d%%",
                          format_tokens(used), format_tokens(limit),
                          as.integer(round(pct)))
    right_plain <- sprintf("compact %d%%", as.integer(compact_pct))
    total_width <- bar_width + 2L  # match the bar's visible width incl. [ ]
    pad <- max(1L, total_width - nchar(left_plain) - nchar(right_plain))
    header <- paste0(
                     palette$bold %||% "", left_plain, palette$reset %||% "",
                     strrep(" ", pad),
                     palette$dim %||% "", right_plain, palette$reset %||% ""
    )
    bar <- .context_meter_bar(pct, compact_pct, bar_width, palette,
                              warn_pct, high_pct, crit_pct)

    rows <- character(0L)
    if (!is.null(breakdown) && length(breakdown) > 0L) {
        labels <- names(breakdown)
        if (is.null(labels)) {
            labels <- vapply(seq_along(breakdown), function(i) {
                breakdown[[i]]$label %||% sprintf("part%d", i)
            }, character(1L))
        }
        tokens <- if (is.null(names(breakdown))) {
            vapply(breakdown, function(b) as.integer(b$tokens %||% 0L),
                   integer(1L))
        } else {
            vapply(breakdown, function(v) as.integer(v %||% 0L),
                   integer(1L))
        }
        for (i in seq_along(breakdown)) {
            rows <- c(rows,
                      .context_breakdown_row(labels[i], tokens[i], used,
                                             palette = palette))
        }
    }

    files_block <- if (length(files) > 0L) {
        c(sprintf("%sContext files (%d):%s",
                  palette$bold %||% "", length(files),
                  palette$reset %||% ""),
          vapply(files, function(f) sprintf("  %s", f), character(1L),
                 USE.NAMES = FALSE))
    } else {
        sprintf("%sNo context files loaded.%s",
                palette$dim %||% "", palette$reset %||% "")
    }

    paste(c(header, bar, rows, "", files_block), collapse = "\n")
}
