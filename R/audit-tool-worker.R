.audit_tool_worker_context <- function(session) {
    audit <- session$.audit_turn
    if (is.null(audit)) return(NULL)
    limits <- audit$limits
    limits$turn_bytes <- max(0,
                             limits$turn_bytes - (audit$payload_bytes %||% 0))
    list(root = audit$state$root, parent = audit$ref, call_id = audit$current_call,
         limits = limits, job_id = audit$origin$job_id)
}

# The evaluator writes its own stream. It never appends to the parent's file or
# receives credentials, a live session environment or a transport client.
.audit_tool_worker <- function(context, expr) {
    if (is.null(context)) return(force(expr))
    s <- .run_r_worker_state$audit_session
    if (is.null(s)) {
        s <- new.env(parent = emptyenv())
        .run_r_worker_state$audit_session <- s
    }
    s$audit_dir <- file.path(context$root, "tool-workers")
    audit <- .audit_safe({
        a <- new.env(parent = emptyenv())
        a$state <- .audit_state(s)
        a$id <- context$parent$turn_id
        a$ref <- context$parent
        a$current_call <- context$call_id
        a$limits <- context$limits
        a
    })
    if (is.null(audit)) return(force(expr))
    previous <- .audit_runtime$active
    .audit_runtime$active <- audit
    status <- "unknown"
    on.exit({
        .audit_safe(.audit_append(audit$state, "tool_capture_end",
                                  turn = context$parent,
                                  call_id = context$call_id,
                                  job_id = context$job_id, status = status))
        .audit_runtime$active <- previous
    }, add = TRUE)
    .audit_safe(.audit_append(audit$state, "tool_capture_start",
                              turn = context$parent, call_id = context$call_id, job_id = context$job_id))
    result <- withCallingHandlers(force(expr), error = function(e) status <<- "error",
                                  interrupt = function(e) status <<- "interrupted")
    status <- if (isTRUE(result$interrupted)) "interrupted" else "complete"
    result$audit_capture <- list(stream = audit$state$path, turn = context$parent,
                                 call_id = context$call_id, bytes = audit$payload_bytes %||% 0)
    result
}
