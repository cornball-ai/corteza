# One canonical turn for a non-interactive model call. The caller's turn, if
# present, links this child rather than adding its usage to its own total.
.audit_aux_context <- function(session = NULL, role, share_writer = FALSE) {
    active <- .audit_runtime$active
    list(root = if (is.environment(session)) {
            .audit_root(session)
        } else {
            active$state$root
        },
         config = if (is.environment(session)) {
            list(audit = session$config$audit)
        } else {
            active$config
        },
         enabled = if (is.environment(session)) session$audit_enabled else NULL,
         writer = if (isTRUE(share_writer)) .audit_safe({
            if (is.environment(session)) .audit_state(session) else active$state
        }),
         identity = if (is.environment(session)) {
            list(owner = session$job_owner,
                 session_key = job_worker_key(session),
                 session_id = session$sessionId)
        } else {
            active$identity
        },
         origin = list(kind = role, role = role,
                       parent_turn = active$ref %||% if (is.environment(session)) session$last_turn_audit))
}

.audit_model_call <- function(fn, args, role, session = NULL, origin = NULL,
                              root = NULL, context = NULL) {
    parent <- .audit_runtime$active
    s <- new.env(parent = emptyenv())
    s$provider <- args$provider
    s$model_map <- list(cloud = args$model)
    s$channel <- "auxiliary"
    context <- context %||% .audit_aux_context(session, role)
    s$config <- context$config
    s$audit_enabled <- context$enabled
    s$audit_dir <- root %||% context$root
    s$audit_origin <- origin %||% context$origin
    s$job_owner <- context$identity$owner
    s$job_key <- context$identity$session_key
    s$sessionId <- context$identity$session_id
    .audit_safe({
        writer <- if (is.environment(session)) .audit_state(session) else parent$state
        if (!is.null(writer) && identical(writer$pid, Sys.getpid()) &&
                is.null(root)) {
            s$audit_dir <- writer$root
            s$.audit_stream <- writer
        }
    })
    audit <- .audit_safe(.audit_turn_begin(s))
    s$.audit_turn <- audit
    .audit_runtime$active <- audit
    on.exit({
        .audit_safe(.audit_turn_end(s, audit))
        .audit_runtime$active <- parent
    }, add = TRUE)
    supported <- "request_callback" %in% names(formals(fn))
    if (!is.null(audit)) audit$request_supported <- supported
    if (supported) args$request_callback <- .audit_request_callback(s, audit)
    .audit_safe({
        if (!is.null(parent) &&
                !is.null(audit)) .audit_append(parent$state, "auxiliary_turn",
            turn_id = parent$id, child = audit$ref, role = role)
    })
    result <- withCallingHandlers(do.call(fn, args),
                                  error = function(e) .audit_safe(.audit_turn_end(s, audit,
                condition = e)),
                                  interrupt = function(e) .audit_safe(.audit_turn_end(s, audit, condition = e)))
    usage <- result$usage
    if (!is.null(usage) && is.null(usage$cost) &&
        any(c("input_tokens", "prompt_tokens", "output_tokens", "completion_tokens") %in% names(usage))) {
        usage <- .audit_usage(usage, args$provider, args$model)
    }
    result$usage <- usage
    incomplete <- isTRUE(result$finish_reason %in% c("length", "max_tokens")) ||
    isTRUE(result$max_turns_reached)
    .audit_safe(.audit_turn_end(s, audit, list(usage = usage,
                termination = list(status = if (incomplete) {
                        "incomplete"
                    } else {
                        "done"
                    },
                                   reason = result$finish_reason %||% "reply"),
                route = list(provider = args$provider, model = args$model))))
    if (!is.null(audit)) {
        result$usage_coverage <- .audit_request_coverage(audit)
        if (identical(result$usage_coverage, "complete")) result$usage <- audit$request_usage
        result$audit <- audit$ref
        result$usage_source <- "derived"
    }
    result
}
