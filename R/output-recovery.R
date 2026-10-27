# Retry an unfinished response from the last complete provider history. Never
# append or execute a partial tool call, and never replay completed tools.
.output_cutoff <- function(response) {
    isTRUE(response$truncated) && isTRUE(response$truncation_reason %in%
        c("max_tokens", "length", "max_output_tokens"))
}

.output_recovery_key <- function(args) {
    paste(args$provider, args$model, sep = "/")
}

.output_recovery_args <- function(args, session) {
    learned <- session$output_recovery
    if (is.null(args$max_tokens) &&
        identical(learned$key, .output_recovery_key(args))) {
        args$max_tokens <- learned$max_tokens
    }
    args
}

.recover_output <- function(response, args, session, call_agent) {
    config <- session$config %||% list()
    retries <- config$output_retries %||% 3L
    if (!is.numeric(retries) || length(retries) != 1L || is.na(retries) ||
        !is.finite(retries) || retries < 0 || retries > 10 ||
        retries != trunc(retries)) {
        stop("output_retries must be a whole number from 0 to 10",
             call. = FALSE)
    }
    explicit <- .session_max_tokens(session)
    ceiling <- .check_max_tokens(config$output_retry_max_tokens %||%
                                 explicit %||% 32768L, "output_retry_max_tokens")
    budget <- args$max_tokens
    if (is.null(budget) &&
        args$provider %in% c("anthropic", "anthropic_claude")) {
        budget <- 4096L
    }
    usage <- list()
    turns <- 0L
    attempts <- 0L
    citations <- searches <- list()
    while (.output_cutoff(response) &&
        !identical(args$provider, "openai_codex") &&
        !is.function(session$on_delta) && attempts < retries &&
        !is.null(budget) && budget < ceiling &&
        identical(response$provider %||% args$provider, args$provider) &&
        identical(response$model %||% args$model, args$model)) {
        used_turns <- as.integer(response$turns %||% 1L)
        if (turns + used_turns >= args$max_turns) {
            break
        }
        history <- response$history
        if (is.null(history)) {
            break
        }
        usage <- .turn_add_usage(usage, response$usage)
        turns <- turns + used_turns
        citations <- c(citations, response$citations %||% list())
        searches <- c(searches, response$searches %||% list())
        budget <- as.integer(min(ceiling, as.double(budget) * 2))
        attempts <- attempts + 1L
        session$history <- history
        journal_snapshot(session)
        retry <- args
        retry["prompt"] <- list(NULL)
        retry$history <- history
        retry$max_tokens <- budget
        retry$max_turns <- args$max_turns - turns
        message(sprintf("Output limit reached; retrying unfinished response with %d output tokens (%d/%d).",
                        budget, attempts, retries))
        log_event("output_retry", attempt = attempts, max_tokens = budget)
        response <- call_agent(retry)
    }
    if (attempts > 0L) {
        response$usage <- .turn_add_usage(usage, response$usage)
        response$turns <- turns + as.integer(response$turns %||% 0L)
        response$citations <- c(citations, response$citations %||% list())
        response$searches <- c(searches, response$searches %||% list())
        response$corteza_output_retries <- attempts
        response$corteza_output_recovered <- identical(.turn_termination(response)$status, "done")
        if (isTRUE(response$corteza_output_recovered) && is.null(explicit) &&
            identical(response$provider %||% args$provider, args$provider) &&
            identical(response$model %||% args$model, args$model)) {
            session$output_recovery <- list(key = .output_recovery_key(args),
                max_tokens = budget)
        }
    }
    if (.output_cutoff(response)) {
        response$content <- paste0(response$content %||% "", "\n\n",
                                   "Stopped at the response output limit. Completed tools are saved; ",
                                   "tool calls in the unfinished response were not executed. Increase max_tokens ",
                                   "or output_retry_max_tokens in .corteza/config.json before continuing.",
                                   if (is.null(.job_worker_state$job_id) &&
                                       !identical(session$channel, "matrix")) {
                                       " In this CLI session, /max-tokens 32768 sets the next response budget."
                                   })
    }
    response
}

.output_budget_command <- function(session, text) {
    text <- trimws(text)
    if (!nzchar(text) || identical(text, "status")) {
        args <- list(provider = session$provider,
                     model = .resolve_model(session),
                     max_tokens = .session_max_tokens(session))
        value <- .output_recovery_args(args, session)$max_tokens
        return(paste("Output budget:", value %||% "provider default"))
    }
    if (identical(text, "default")) {
        session$max_tokens <- NULL
        session$output_recovery <- NULL
    } else {
        value <- suppressWarnings(as.numeric(text))
        session$max_tokens <- .check_max_tokens(value, "/max-tokens")
    }
    .output_budget_command(session, "status")
}

# llm.api intentionally omits an incomplete assistant message from history.
# Once retries end, the text returned to the user must still be available on
# the next turn. Keep only the rendered reply, never unfinished tool blocks.
.output_keep_reply <- function(session, response, reply) {
    if (!.output_cutoff(response) || !nzchar(reply)) {
        return(FALSE)
    }
    history <- session$history %||% list()
    last <- tail(history, 1L)
    if (length(last) && identical(last[[1L]]$role, "assistant") &&
        identical(.compact_entry_body(last[[1L]]), reply)) {
        return(FALSE)
    }
    session$history <- c(history,
                         list(list(role = "assistant", content = reply)))
    journal_snapshot(session)
    TRUE
}
