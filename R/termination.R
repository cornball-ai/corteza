# llm.api's tool-loop limit is a warning rather than a return field. Capture
# only the warning emitted directly by this invocation of the real agent.
# A tool or a nested agent can emit the same text, even with an identical call.
.agent_call <- function(args) {
    agent <- llm.api::agent
    invocation <- environment()
    max_turns <- FALSE
    response <- withCallingHandlers(do.call(agent, args), warning = function(w) {
        if (!grepl("^Reached max_turns \\([0-9]+\\)$", conditionMessage(w))) {
            return(invisible(NULL))
        }
        frames <- sys.frames()
        parents <- sys.parents()
        warnings <- which(vapply(seq_along(frames), function(i) {
            identical(sys.function(i), base::warning)
        }, logical(1)))
        if (!length(warnings)) return(invisible(NULL))
        emitter <- parents[[tail(warnings, 1L)]]
        if (emitter > 0L && identical(sys.function(emitter), agent) &&
            parents[[emitter]] > 0L &&
            identical(frames[[parents[[emitter]]]], invocation)) {
            max_turns <<- TRUE
            invokeRestart("muffleWarning")
        }
    })
    if (max_turns) {
        response$corteza_termination <- list(status = "incomplete",
            reason = "max_turns")
    }
    response
}

# Called after retries and fallback, so an earlier stopped attempt cannot
# contaminate the final response. Usage remains accumulated by those callers.
.turn_termination <- function(response) {
    if (isTRUE(response$cancelled)) {
        return(list(status = "cancelled", reason = "cancelled"))
    }
    if (isTRUE(response$truncated)) {
        return(list(status = "incomplete",
                    reason = response$truncation_reason %||% "truncated"))
    }
    response$corteza_termination %||% list(status = "done", reason = NULL)
}

.termination_text <- function(termination) {
    switch(termination$reason %||% "",
           max_turns = "Stopped at the tool-turn limit; the task is incomplete.",
           max_tokens =, length =, max_output_tokens =
           "Stopped at the response output limit; the task is incomplete.",
           model_context_window_exceeded =
           "Stopped at the context-window limit; the task is incomplete.",
           cancelled = "The response was cancelled before completion.",
           sprintf("The response stopped before completion (%s).",
                   termination$reason %||% "unknown reason"))
}

job_result_body <- function(job) {
    o <- job$outcome
    switch(job$status,
           done = o$result %||% "",
           failed = o$error %||% o$reason %||% "",
           incomplete = paste(Filter(nzchar, c(o$result %||% "", o$reason %||% "",
                    if (isTRUE(o$resume$ready)) {
                        paste0("No automatic continuation was started. Use /job resume ", job$id,
                            " to request a new attempt. Tool handles and any listed unrestorable objects will be unavailable.")
                    } else paste("Resume unavailable:", o$resume$error %||% "no per-job snapshot was saved."))),
                              collapse = "\n\n"),
           o$reason %||% "")
}
