.audit_usage <- function(raw, provider, model) {
    if (!length(raw)) return(NULL)
    input <- raw[["input_tokens"]] %||% raw[["prompt_tokens"]]
    output <- raw[["output_tokens"]] %||% raw[["completion_tokens"]]
    list(input_tokens = input, output_tokens = output,
         total_tokens = if (!is.null(input) && !is.null(output)) {
            input + output
        } else {
            raw[["total_tokens"]]
        },
         cache_read_input_tokens = raw$cache_read_input_tokens %||%
         raw$prompt_tokens_details$cached_tokens %||% raw$input_tokens_details$cached_tokens,
         cache_creation_input_tokens = raw$cache_creation_input_tokens,
         cache_creation = raw$cache_creation,
         cost = tryCatch(llm.api::usage_cost(model, provider, raw),
                         error = function(e) NA_real_))
}

.audit_request_manifest <- function(audit, text) {
    request <- .audit_payload_put(audit, text, "request", "text")
    body <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    part <- function(x) .audit_payload_put(audit, .history_json(x),
        "context", "json")
    messages <- body$messages %||% body$input %||% list()
    system <- body$system %||% body$instructions
    if (is.null(system) && length(body$messages)) {
        instructions <- Filter(function(x) x$role %in% c("system", "developer"), body$messages)
        if (length(instructions)) system <- instructions
    }
    list(request = request, system = part(system),
         tools = part(body$tools), messages = lapply(messages, part),
         settings = part(body[setdiff(names(body), c("system", "instructions", "tools", "messages", "input"))]))
}

.audit_request_callback <- function(session, audit) {
    if (is.null(audit)) return(NULL)
    function(event) {
        audit$request_seen <- TRUE
        recorded <- .audit_safe({
            phase <- event$phase
            # Attribution comes from the adapter, including fallback providers.
            # The body is stored once by hash, never copied into the event stream.
            text <- event$serialized_request
            event$serialized_request <- NULL
            event$request_ts <- event$ts
            if (identical(phase, "start")) {
                event$manifest <- .audit_request_manifest(audit, text)
                event$journal <- .audit_journal_position(session)
            } else {
                usage <- .audit_usage(event$raw_usage, event$provider,
                                      event$reported_model %||% event$requested_model)
                event$usage <- usage
                if (is.null(usage$input_tokens) ||
                                is.null(usage$output_tokens) ||
                                !identical(event$status, "complete")) audit$request_partial <- TRUE
            }
            written <- do.call(.audit_append, c(list(state = audit$state,
                        type = paste0("request_", phase), turn_id = audit$id,
                        job_id = audit$origin$job_id,
                        usage_source = "breakdown"),
                    event[setdiff(names(event), c("type", "version", "ts"))]))
            if (isTRUE(written)) {
                audit$request_ids <- c(audit$request_ids %||% character(), event$request_id)
                if (identical(phase, "start")) {
                    audit$request_starts <- c(audit$request_starts %||% character(), event$attempt_id)
                } else {
                    audit$request_ends <- c(audit$request_ends %||% character(), event$attempt_id)
                    audit$request_usage <- .turn_add_usage(audit$request_usage, usage)
                }
            }
            written
        })
        if (!isTRUE(recorded)) audit$request_partial <- TRUE
        invisible(NULL)
    }
}

.audit_request_coverage <- function(audit) {
    if (!isTRUE(audit$request_supported)) return("unavailable")
    if (isTRUE(audit$request_partial)) return("partial")
    if (!length(audit$request_starts)) return("unobserved")
    if (!setequal(audit$request_starts, audit$request_ends)) return("partial")
    "complete"
}

.audit_tool_payload <- function(audit, full, admitted, tool, stage = "cap") {
    if (is.null(audit) || !is.character(full) ||
        length(full) != 1L) return(invisible(NULL))
    .audit_append(audit$state, "tool_payload", turn_id = audit$id,
                  call_id = audit$current_call, turn = audit$ref, tool = tool, stage = stage,
                  full = .audit_payload_put(audit, full, "tool_full"),
                  admitted = .audit_payload_put(audit, admitted, "tool_preview"),
                  truncated = !identical(full, admitted))
    invisible(NULL)
}
