# Observe the transport boundary. Recording must never turn a successful send
# into a retry, and errors keep their original condition class and behavior.
.audit_first_ref <- function(refs) {
    if (length(refs)) refs[[1L]] else NULL
}

.audit_delivery <- function(room, text, expr, session = NULL, thread = NULL,
                            ref = NULL, operation = "send", event_id = NULL,
                            generated = text, rich = NULL, attachment = NULL) {
    record <- .audit_safe({
        active <- if (is.environment(session)) session$.audit_turn else .audit_runtime$active
        if (is.null(session) && is.null(active)) {
            if (is.null(.audit_runtime$delivery)) {
                .audit_runtime$delivery <- new.env(parent = emptyenv())
            }
            session <- .audit_runtime$delivery
        }
        state <- if (is.environment(session)) .audit_state(session) else active$state
        ref <- ref %||% active$ref
        if (is.null(state)) NULL else list(state = state,
            fields = list(delivery_id = paste0(state$id, ":d:", .audit_id()),
                turn = ref, room = room, thread = thread, operation = operation,
                replaces = event_id,
                text_hash = digest::digest(enc2utf8(text), "sha256", serialize = FALSE),
                generated_hash = digest::digest(enc2utf8(generated), "sha256", serialize = FALSE),
                rich_hash = if (!is.null(rich)) digest::digest(enc2utf8(rich), "sha256", serialize = FALSE),
                attachment = attachment))
    })
    write <- function(phase, ...) .audit_safe({
        if (!is.null(record)) do.call(.audit_append,
            c(list(state = record$state, type = paste0("delivery_", phase)), record$fields, list(...)))
    })
    write("start")
    started <- proc.time()[["elapsed"]]
    result <- withCallingHandlers(force(expr), error = function(e) {
        write("end", status = "error", condition_classes = as.list(class(e)),
            condition_message = conditionMessage(e),
            elapsed_ms = (proc.time()[["elapsed"]] - started) * 1000)
    }, interrupt = function(e) {
        write("end", status = "interrupted", condition_classes = as.list(class(e)))
    })
    ids <- if (is.character(result)) result[!is.na(result) & nzchar(result)] else character()
    # Some edit backends acknowledge with TRUE without returning a new event.
    acknowledged <- length(ids) > 0L || (identical(operation, "edit") && isTRUE(result))
    write("end", status = if (acknowledged) "confirmed" else "unconfirmed",
        event_ids = as.list(ids), acknowledged = acknowledged,
        elapsed_ms = (proc.time()[["elapsed"]] - started) * 1000)
    result
}
