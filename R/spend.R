# Session spend tracking and the /spent command.
#
# turn() does not accumulate; the two REPL loops do, because chat()
# reuses one session env across turns while the CLI rebuilds a per-turn
# session and carries state on a persistent session list. Each loop
# calls session_accumulate_spend() on its persistent object after a
# turn.
#
# Spend is reported per process run and is never discarded. A /clear
# does not zero the tally; it closes the current conversation segment
# and opens a new one, so /spent itemizes each conversation between
# clears and shows a grand total. Subagent spend is a separate
# process-level line sourced from subagent_spend_total(): a subagent's
# cost is process-level (killed agents are retired into the run total
# rather than attributed to one conversation), so it is reported as one
# line rather than split across segments. Resumed prior-run spend is
# not loaded from disk.

#' Add an integer usage field, treating NULL/NA as zero.
#' @noRd
.spend_add_int <- function(prev, new) {
    if (is.null(new) || is.na(new)) {
        prev
    } else {
        prev + as.integer(new)
    }
}

#' TRUE when a usage list reports any nonzero token count.
#'
#' Gates the `cost_missing` floor flag: only a query that actually
#' consumed tokens but came back without a price makes the total a
#' floor. A zero-token turn (a no-op, an errored turn) with no cost
#' should not flip the flag.
#' @noRd
.spend_usage_has_tokens <- function(usage) {
    v <- c(usage$input_tokens, usage$output_tokens, usage$total_tokens)
    any(!is.na(v) & v > 0)
}

#' An empty per-conversation main-agent segment tally.
#' @param id Optional session id stamped on the segment for display.
#' @noRd
.spend_empty_segment <- function(id = NULL) {
    list(id = id, cost = 0, input_tokens = 0L, output_tokens = 0L,
         total_tokens = 0L, turns = 0L, cost_missing = FALSE)
}

#' Accumulate one turn's usage into the current spend segment.
#'
#' Works on either a session environment (chat(), mutated in place) or a
#' session list (CLI, returned for reassignment). The tally is a list of
#' conversation segments; this adds to the last (open) one, opening a
#' fresh segment first when one is pending (after a /clear) or none
#' exists. Costs are summed only when present; a missing or NA cost on a
#' query that consumed tokens (a model absent from llm.api's price
#' snapshot) flips `cost_missing` so the reported total reads as a floor.
#'
#' @param session Session environment or list.
#' @param usage Usage list from a turn: `input_tokens`, `output_tokens`,
#'   `total_tokens`, `cost` (USD scalar, possibly NA).
#' @return The session, invisibly (mutated in place for an env).
#' @noRd
session_accumulate_spend <- function(session, usage) {
    if (is.null(usage)) {
        return(invisible(session))
    }
    sp <- session$spend %||% list(segments = list())
    # Open a new segment lazily: on the first turn, or on the first turn
    # after a /clear marked one pending. Deferring to here means a /clear
    # with no following turn leaves no empty conversation in the report.
    if (isTRUE(sp$pending_new) || length(sp$segments) == 0L) {
        sp$segments <- c(sp$segments,
                         list(.spend_empty_segment(id = session$sessionId)))
        sp$pending_new <- FALSE
    }
    i <- length(sp$segments)
    seg <- sp$segments[[i]]
    seg$input_tokens <- .spend_add_int(seg$input_tokens, usage$input_tokens)
    seg$output_tokens <- .spend_add_int(seg$output_tokens, usage$output_tokens)
    seg$total_tokens <- .spend_add_int(seg$total_tokens, usage$total_tokens)
    if (is.null(usage$cost) || is.na(usage$cost)) {
        if (.spend_usage_has_tokens(usage)) {
            seg$cost_missing <- TRUE
        }
    } else {
        seg$cost <- seg$cost + as.numeric(usage$cost)
    }
    seg$turns <- seg$turns + 1L
    if (is.null(seg$id)) {
        seg$id <- session$sessionId
    }
    sp$segments[[i]] <- seg
    session$spend <- sp
    invisible(session)
}

#' Close the current conversation segment and open a fresh one.
#'
#' Called on /clear. Process-lifetime: prior segments are kept (they
#' remain visible as /spent line items). The new segment is not created
#' here -- it is marked pending and opened by the next turn that spends,
#' so a /clear with no following turn (or repeated /clear) adds no empty
#' conversation to the report.
#'
#' @param session Session environment or list.
#' @return The session, invisibly.
#' @noRd
spend_open_segment <- function(session) {
    sp <- session$spend %||% list(segments = list())
    sp$pending_new <- TRUE
    session$spend <- sp
    invisible(session)
}

#' Render the /spent report (process run; per-conversation segments).
#'
#' Lists each conversation segment (the spans between /clear) with its
#' turns, tokens, and cost; adds a process-level subagent line when any
#' subagent has spent; and closes with a grand total. A single segment
#' with no subagent spend renders in the compact two-line form.
#'
#' @param session Session environment or list.
#' @param palette Optional ANSI color list (`dim`, `reset`, `bold`).
#' @return Character block, no trailing newline.
#' @noRd
format_spend <- function(session, palette = NULL) {
    c_dim <- palette$dim %||% ""
    c_rst <- palette$reset %||% ""
    c_bold <- palette$bold %||% ""
    sp <- session$spend %||% list(segments = list())
    segs <- sp$segments
    if (length(segs) == 0L) {
        segs <- list(.spend_empty_segment())
    }
    # With a pending new segment (after a /clear, before the next turn)
    # the last existing segment is closed, not the live conversation, so
    # nothing is marked current.
    pending <- isTRUE(sp$pending_new)
    sub <- subagent_spend_total()
    has_sub <- (sub$n_agents %||% 0L) > 0L || (sub$total_tokens %||% 0L) > 0L
    tk <- function(n) format_tokens(as.integer(n %||% 0L))
    plural <- function(n, one, many = paste0(one, "s")) {
        sprintf("%d %s", as.integer(n),
            if (identical(as.integer(n), 1L)) {
                one
            } else {
                many
            })
    }

    seg_cost <- vapply(segs, function(s) s$cost %||% 0, numeric(1))
    total_cost <- sum(seg_cost) + (sub$cost %||% 0)
    any_missing <- any(vapply(segs, function(s) isTRUE(s$cost_missing),
                              logical(1))) || isTRUE(sub$cost_missing)
    floor_note <- if (any_missing) {
        paste0(c_dim, "  (floor; some model prices unknown)", c_rst)
    } else {
        ""
    }

    # Compact form: one conversation, no subagent spend.
    if (length(segs) == 1L && !has_sub) {
        s <- segs[[1]]
        return(paste(c(
                       sprintf("%sSession spend (this run)%s  ~$%.4f%s", c_bold, c_rst,
                               s$cost %||% 0, floor_note),
                       sprintf("  %s%s   %s tok (%s in / %s out)%s",
                               c_dim, plural(s$turns %||% 0L, "turn"),
                               tk(s$total_tokens), tk(s$input_tokens),
                               tk(s$output_tokens), c_rst)
                ), collapse = "\n"))
    }

    lines <- sprintf("%sSession spend (this run)%s", c_bold, c_rst)
    for (k in seq_along(segs)) {
        s <- segs[[k]]
        if (!is.null(s$id)) {
            id_short <- substr(s$id, 1L, 8L)
        } else {
            id_short <- "?"
        }
        current <- if (k == length(segs) && !pending) {
            paste0(c_dim, "  (current)", c_rst)
        } else {
            ""
        }
        lines <- c(lines, sprintf(
                                  "  [%d] %s  %s, %s tok  ~$%.4f%s",
                                  k, id_short, plural(s$turns %||% 0L, "turn"),
                                  tk(s$total_tokens), s$cost %||% 0, current))
    }
    if (has_sub) {
        cost_str <- if (isTRUE(sub$cost_missing) && (sub$cost %||% 0) == 0) {
            "~$?"
        } else {
            sprintf("~$%.4f", sub$cost %||% 0)
        }
        lines <- c(lines, sprintf(
                                  "  %ssubagents: %s, %s, %s tok  %s%s",
                                  c_dim, plural(sub$n_agents %||% 0L, "agent"),
                                  plural(sub$query_count %||% 0L, "query", "queries"),
                                  tk(sub$total_tokens), cost_str, c_rst))
    }
    lines <- c(lines, sprintf("  %stotal%s  ~$%.4f%s", c_bold, c_rst,
                              total_cost, floor_note))
    paste(lines, collapse = "\n")
}

