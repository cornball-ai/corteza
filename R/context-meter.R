# /context display: one horizontal meter shared by CLI and chat() so
# both surfaces answer the two questions a user actually has -- how
# full is context, and what's using it -- without redundant prose or
# layout drift.
#
# The renderer is a pure formatter (no side effects, no Sys.time())
# so it tests cleanly. Callers compute the numbers and hand them in.

#' Map a percentage to a palette entry using the same four-band
#' breakdown the rest of the CLI uses (normal / warn / high / crit).
#' Returns the ANSI start sequence; pair with `palette$reset`.
#' @noRd
.context_pct_color <- function(pct, palette, warn_pct = 75, high_pct = 90,
                               crit_pct = 95) {
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

#' Per-segment color for the context bar.
#'
#' Distinct hues for each named component so the visual fill maps
#' back to the breakdown rows below. Anything not listed falls back
#' to white so adding a new breakdown label doesn't break the
#' renderer.
#' @noRd
.context_segment_color <- function(label, palette) {
    switch(label %||% "", system = palette$bright_blue %||% "",
           tools = palette$bright_magenta %||% "",
           history = palette$cyan %||% "", messages = palette$cyan %||% "",
           memory = palette$yellow %||% "", skills = palette$green %||% "",
           palette$white %||% "")
}

#' Build the bar portion of the /context display.
#'
#' Each component of `breakdown` claims a proportional run of cells
#' in its own color; the auto-compact tick is a dim vertical bar at
#' its fractional position, kept visually quieter than the actual
#' usage fill. When the total used percent crosses the warn/high/
#' crit thresholds the empty-region dots inherit the threshold color
#' too so the bar reads "hot" at a glance even before any single
#' segment dominates.
#'
#' @param breakdown Named numeric (e.g. `c(system = 22000)`) or list
#'   of `list(label, tokens)` entries describing each component's
#'   token cost. Order matters; rendered left-to-right.
#' @param limit Total context window in tokens.
#' @param compact_pct Threshold at which auto-compact would fire,
#'   for the subtle tick mark.
#' @param width Total cell count. Default 50.
#' @param palette ANSI palette.
#' @return Character scalar (one line, including the surrounding
#'   `[ ]` brackets).
#' @noRd
.context_meter_bar <- function(breakdown, limit, compact_pct = 90,
                               width = 50L, palette = ansi_colors(),
                               warn_pct = 75, high_pct = 90, crit_pct = 95) {
    width <- as.integer(width)
    if (is.null(limit) || limit <= 0L) {
        limit <- 1L
    }
    if (is.null(names(breakdown))) {
        labels <- vapply(seq_along(breakdown), function(i) {
            breakdown[[i]]$label %||% sprintf("part%d", i)
        }, character(1L))
        tokens <- vapply(breakdown, function(b) {
            as.integer(b$tokens %||% 0L)
        }, integer(1L))
    } else {
        labels <- names(breakdown)
        tokens <- vapply(breakdown, function(v) {
            as.integer(v %||% 0L)
        }, integer(1L))
    }
    total_used <- sum(tokens)
    if (total_used > 0L) {
        pct <- total_used / limit * 100
    } else {
        pct <- 0
    }
    compact_cell <- as.integer(round(compact_pct / 100 * width))
    compact_cell <- max(1L, min(width, compact_cell))

    # Distribute cells proportional to each segment's share of the
    # full context window. Floor + remainder rebalancing so the
    # total filled count matches the rounded usage percent --
    # otherwise rounding fights the header number.
    raw <- pmax(0, tokens) / limit * width
    seg_cells <- as.integer(floor(raw))
    target_used <- as.integer(min(width, round(total_used / limit * width)))
    leftover <- target_used - sum(seg_cells)
    if (leftover > 0L && length(seg_cells) > 0L) {
        frac <- raw - seg_cells
        order_idx <- order(-frac)
        bump <- order_idx[seq_len(min(leftover, length(order_idx)))]
        seg_cells[bump] <- seg_cells[bump] + 1L
    }

    # Empty cells take the threshold color when usage crosses the
    # warn line so saturation reads at a glance.
    empty_color <- if (pct >= warn_pct) {
        .context_pct_color(pct, palette, warn_pct, high_pct, crit_pct)
    } else {
        palette$dim %||% ""
    }

    cells <- character(width)
    pos <- 1L
    for (i in seq_along(seg_cells)) {
        n <- seg_cells[i]
        if (n <= 0L) {
            next
        }
        col <- .context_segment_color(labels[i], palette)
        end <- min(width, pos + n - 1L)
        for (k in seq.int(pos, end)) {
            cells[k] <- sprintf("%s\u2588%s", col, palette$reset %||% "")
        }
        pos <- end + 1L
        if (pos > width) {
            break
        }
    }
    if (pos <= width) {
        for (k in seq.int(pos, width)) {
            cells[k] <- if (k == compact_cell) {
                sprintf("%s\u2502%s", palette$dim %||% "",
                        palette$reset %||% "")
            } else {
                sprintf("%s.%s", empty_color, palette$reset %||% "")
            }
        }
    }
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
    if (used > 0L) {
        pct <- tokens / used * 100
    } else {
        pct <- 0
    }
    pct_str <- if (pct >= 1) {
        sprintf("%d%%", as.integer(round(pct)))
    } else {
        ""
    }
    sprintf("  %-8s %6s  %s%s%s", label, tok_str, palette$dim %||% "",
            pct_str, palette$reset %||% "")
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
format_context_block <- function(used, limit, breakdown, compact_pct = 90,
                                 warn_pct = 75, high_pct = 90, crit_pct = 95,
                                 files = character(0L),
                                 palette = ansi_colors(), bar_width = 50L,
                                 status_info = NULL) {
    used <- as.integer(round(used %||% 0))
    limit <- as.integer(round(limit %||% 0))
    if (limit > 0L) {
        pct <- used / limit * 100
    } else {
        pct <- 0
    }

    # Optional Codex-style status header above the bar: corteza
    # version, model, directory, session id. Each field a `label:
    # value` row, label dim-colored, value bold. Skipped when
    # status_info is NULL so callers that just want the meter (e.g.
    # an embedded snippet) aren't forced to construct one.
    status_lines <- character(0L)
    if (!is.null(status_info) && length(status_info) > 0L) {
        # Include the trailing colon in the width calc so all values
        # line up at the same column regardless of label length.
        label_width <- max(nchar(paste0(names(status_info), ":")))
        for (lbl in names(status_info)) {
            val <- status_info[[lbl]]
            if (is.null(val) || identical(val, "")) {
                val <- "(unset)"
            }
            status_lines <- c(
                              status_lines,
                              sprintf("%s%-*s%s  %s%s%s", palette$dim %||% "", label_width,
                                      paste0(lbl, ":"), palette$reset %||% "",
                                      palette$bold %||% "", val, palette$reset %||% "")
            )
        }
        status_lines <- c(status_lines, "")
    }

    # Right-align the "compact N%" tick so it lines up with the right
    # edge of the bar and stays visually distinct from the usage
    # numbers on the left.
    left_plain <- sprintf("Context  %s / %s  %d%%",
                          format_tokens(used), format_tokens(limit),
                          as.integer(round(pct)))
    right_plain <- sprintf("compact %d%%", as.integer(compact_pct))
    total_width <- bar_width + 2L # match the bar's visible width incl. [ ]
    pad <- max(1L, total_width - nchar(left_plain) - nchar(right_plain))
    header <- paste0(
                     palette$bold %||% "", left_plain, palette$reset %||% "",
                     strrep(" ", pad),
                     palette$dim %||% "", right_plain, palette$reset %||% ""
    )
    bar <- .context_meter_bar(breakdown, limit, compact_pct, bar_width,
                              palette, warn_pct, high_pct, crit_pct)

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
            vapply(breakdown, function(v) as.integer(v %||% 0L), integer(1L))
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

    paste(c(status_lines, header, bar, rows, "", files_block), collapse = "\n")
}

