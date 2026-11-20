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

#' Token count for a usage list, normalized across providers.
#'
#' Must agree with [.spend_usage_has_tokens()] about what counts as
#' usage, or the two disagree silently: that predicate accepts a nonzero
#' `input_tokens` or `output_tokens`, so reading only `total_tokens`
#' means a provider that omits the total sets the "price unknown" flag
#' while the unpriced-token counter stays at zero -- and anything
#' differencing that counter concludes the spend was priced.
#'
#' Reported total when present and positive, otherwise input + output.
#' @noRd
.spend_normalized_tokens <- function(usage) {
    num <- function(x) {
        v <- suppressWarnings(as.numeric(x %||% 0))
        if (length(v) != 1L || is.na(v)) {
            0
        } else {
            v
        }
    }
    tot <- num(usage$total_tokens)
    if (tot > 0) {
        return(tot)
    }
    num(usage$input_tokens) + num(usage$output_tokens)
}

#' Input tokens a provider counts outside `input_tokens`.
#'
#' Anthropic reports the input of a request as 3 counts: what it read
#' from the prompt cache (`cache_read_input_tokens`), what it wrote to
#' it (`cache_creation_input_tokens`), and `input_tokens`, which holds
#' only the rest. Every other provider corteza talks to counts cached
#' input inside its input count, so the answer for those is 0.
#' @noRd
.usage_cached_input <- function(usage, provider) {
    if (!is.list(usage) || !isTRUE(provider %in% .anthropic_providers)) {
        return(0)
    }
    count <- function(name) {
        value <- usage[[name]]
        if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
            value > 0) {
            value
        } else {
            0
        }
    }
    count("cache_read_input_tokens") + count("cache_creation_input_tokens")
}

#' A usage list whose `total_tokens` counts every token.
#'
#' llm.api's total, and the one .audit_usage() computes, is input plus
#' output. With a prompt cache on, that leaves out nearly all the input
#' of a request to an Anthropic provider, and a token cap that reads
#' the total (`/auto`, `subagents.mcp_spend_cap_tokens`) would count a
#' small part of what a session sent. A cached token counts as 1, as
#' it did before there was a cache: a cap in tokens bounds the work,
#' and the price is the cost cap's to weigh.
#'
#' Each usage passes through here once, where its provider is known
#' and before it is added to another's: one model call's in
#' .agent_with_fallback(), one request's in .audit_usage(). A usage
#' with nothing cached comes back as it was.
#' @noRd
.usage_count_cached <- function(usage, provider) {
    cached <- .usage_cached_input(usage, provider)
    if (cached <= 0) {
        return(usage)
    }
    input <- usage[["input_tokens"]]
    output <- usage[["output_tokens"]]
    known <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
    if (known(input) && known(output)) {
        usage$total_tokens <- input + output + cached
    }
    usage
}

#' Input tokens of a usage list, cached or not.
#'
#' turn() returns a total that counts every token
#' (.usage_count_cached()). Where that total is larger than input plus
#' output, the rest is input a provider read from its prompt cache or
#' wrote to it, and left out of `input_tokens`.
#' @noRd
.spend_input_tokens <- function(usage) {
    input <- usage[["input_tokens"]]
    output <- usage[["output_tokens"]]
    total <- usage[["total_tokens"]]
    known <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
    if (known(input) && known(output) && known(total) &&
        total - output > input) {
        return(total - output)
    }
    input
}

#' An empty per-conversation main-agent segment tally.
#' @param id Optional session id stamped on the segment for display.
#' @noRd
.spend_empty_segment <- function(id = NULL) {
    list(id = id, cost = 0, input_tokens = 0L, output_tokens = 0L,
         total_tokens = 0L, turns = 0L, cost_missing = FALSE,
         # Tokens consumed by queries that came back without a price.
         # cost_missing is sticky, which is right for a report ("this
         # total is a floor") and useless for anything that needs a
         # delta: once true it stays true, so a later window can never
         # tell whether *its* spend was priced. This accumulates, so it
         # can be differenced. See auto_spend_since() in R/auto.R.
         missing_tokens = 0)
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
    seg <- .spend_segment_add(sp$segments[[i]], usage)
    if (is.null(seg$id)) {
        seg$id <- session$sessionId
    }
    sp$segments[[i]] <- seg
    session$spend <- sp
    invisible(session)
}

#' Add one turn's or one job's usage to a segment.
#' @noRd
.spend_segment_add <- function(seg, usage) {
    seg$input_tokens <- .spend_add_int(seg$input_tokens,
                                       .spend_input_tokens(usage))
    seg$output_tokens <- .spend_add_int(seg$output_tokens, usage$output_tokens)
    # Normalized, not the raw field: a provider that reports input and
    # output without a total would otherwise record a priced query as
    # zero tokens, and any token cap reading this would never trip.
    seg$total_tokens <- .spend_add_int(seg$total_tokens,
                                       .spend_normalized_tokens(usage))
    if (is.null(usage$cost) || is.na(usage$cost)) {
        if (.spend_usage_has_tokens(usage)) {
            seg$cost_missing <- TRUE
            seg$missing_tokens <- (seg$missing_tokens %||% 0) +
            .spend_normalized_tokens(usage)
        }
    } else {
        seg$cost <- seg$cost + as.numeric(usage$cost)
    }
    seg$turns <- seg$turns + 1L
    seg
}

#' Accumulate usage spent for an earlier conversation of this run.
#'
#' A job asked for before a /clear can settle after it. What it spent
#' belongs to the conversation that asked for it: the last segment with
#' that session id. A conversation with no segment yet gets one, placed
#' before the open conversation's, so the open one stays last and the
#' next turn's spend still reaches it.
#'
#' @param session Session environment or list.
#' @param usage Usage list, as for `session_accumulate_spend()`.
#' @param id The session id of the conversation that spent it.
#' @return The session, invisibly (mutated in place for an env).
#' @noRd
spend_accumulate_for <- function(session, usage, id) {
    if (is.null(usage)) {
        return(invisible(session))
    }
    sp <- session$spend %||% list(segments = list())
    ids <- vapply(sp$segments, function(s) {
        s$id %||% NA_character_
    }, character(1))
    at <- which(ids %in% id)
    if (length(at)) {
        i <- max(at)
        sp$segments[[i]] <- .spend_segment_add(sp$segments[[i]], usage)
    } else {
        seg <- .spend_segment_add(.spend_empty_segment(id = id), usage)
        n <- length(sp$segments)
        # With no segment pending, the last one is the open conversation's.
        after <- if (n > 0L && !isTRUE(sp$pending_new)) {
            n - 1L
        } else {
            n
        }
        sp$segments <- append(sp$segments, list(seg), after = after)
    }
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
