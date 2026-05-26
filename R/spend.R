# Session spend tracking and the /spent command.
#
# turn() does not accumulate; the two REPL loops do, because chat()
# reuses one session env across turns while the CLI rebuilds a per-turn
# session and carries state on a persistent session list. Each loop
# calls session_accumulate_spend() on its persistent object after a
# turn.
#
# Scope: /spent reports the current process run's MAIN-agent turns.
# Subagent spend is intentionally out of scope: in CLI mode subagents
# run in a separate worker process with their own state (and today a
# separate registry from the slash-command path), so aggregating their
# cost needs the worker backend unified first. Spend is also per-run,
# not persisted across a resumed session.

#' Add an integer usage field, treating NULL/NA as zero.
#' @noRd
.spend_add_int <- function(prev, new) {
    if (is.null(new) || is.na(new)) {
        prev
    } else {
        prev + as.integer(new)
    }
}

#' Accumulate one turn's usage into the session spend tally.
#'
#' Works on either a session environment (chat(), mutated in place) or a
#' session list (CLI, returned for reassignment). Costs are summed only
#' when present; a missing or NA cost (a model absent from llm.api's
#' price snapshot) flips `cost_missing` so the reported total reads as a
#' floor rather than a precise figure.
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
    sp <- session$spend %||% list(cost = 0, input_tokens = 0L,
                                  output_tokens = 0L, total_tokens = 0L,
                                  turns = 0L, cost_missing = FALSE)
    sp$input_tokens <- .spend_add_int(sp$input_tokens, usage$input_tokens)
    sp$output_tokens <- .spend_add_int(sp$output_tokens, usage$output_tokens)
    sp$total_tokens <- .spend_add_int(sp$total_tokens, usage$total_tokens)
    if (is.null(usage$cost) || is.na(usage$cost)) {
        sp$cost_missing <- TRUE
    } else {
        sp$cost <- sp$cost + as.numeric(usage$cost)
    }
    sp$turns <- sp$turns + 1L
    session$spend <- sp
    invisible(session)
}

#' Reset the session spend tally for a fresh conversation.
#'
#' @param session Session environment or list.
#' @return The session, invisibly.
#' @noRd
reset_session_spend <- function(session) {
    session$spend <- NULL
    invisible(session)
}

#' Render the /spent report (current run, main-agent turns).
#'
#' @param session Session environment or list.
#' @param palette Optional ANSI color list (`dim`, `reset`, `bold`).
#' @return Character block, no trailing newline.
#' @noRd
format_spend <- function(session, palette = NULL) {
    c_dim <- palette$dim %||% ""
    c_rst <- palette$reset %||% ""
    c_bold <- palette$bold %||% ""
    sp <- session$spend %||% list(cost = 0, input_tokens = 0L,
                                  output_tokens = 0L, total_tokens = 0L,
                                  turns = 0L, cost_missing = FALSE)
    tk <- function(n) format_tokens(as.integer(n %||% 0L))
    floor_note <- if (isTRUE(sp$cost_missing)) {
        paste0(c_dim, "  (floor; some model prices unknown)", c_rst)
    } else {
        ""
    }
    lines <- c(
               sprintf("%sSession spend (this run)%s  ~$%.4f%s", c_bold, c_rst,
                       sp$cost %||% 0, floor_note),
               sprintf("  %s%d turns   %s tok (%s in / %s out)%s",
                       c_dim, sp$turns %||% 0L, tk(sp$total_tokens),
                       tk(sp$input_tokens), tk(sp$output_tokens), c_rst)
    )
    paste(lines, collapse = "\n")
}

