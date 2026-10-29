.audit_journal_position <- function(session) {
    state <- session$.journal
    list(dir = session$journal_dir %||% journal_dir(session),
         seq = if (is.environment(state)) state$seq else NULL,
         length = length(session$history %||% list()))
}

.audit_turn_begin <- function(session) {
    state <- .audit_state(session)
    if (is.null(state)) {
        return(NULL)
    }
    state$turns <- state$turns + 1
    audit <- new.env(parent = emptyenv())
    audit$state <- state
    audit$id <- paste0(state$id, ":t", state$turns)
    audit$ref <- list(stream = state$path, turn_id = audit$id)
    audit$calls <- 0L
    audit$started <- proc.time()[["elapsed"]]
    audit$ended <- FALSE
    audit$origin <- session$audit_origin %||% list(kind = "unknown")
    audit$role <- audit$origin$role
    if (is.null(audit$role)) {
        if (isTRUE(session$talker)) {
            audit$role <- "talker"
        } else {
            audit$role <- "direct"
        }
    }
    .audit_append(state, "turn_start", turn_id = audit$id, origin = audit$origin,
                  role = audit$role, session_id = session$sessionId,
                  owner = session$job_owner, session_key = job_worker_key(session),
                  job_id = audit$origin$job_id, auto_run_id = session$auto_run_id,
                  channel = session$channel, provider = session$provider,
                  model = session$model_map$cloud, journal = .audit_journal_position(session),
                  corteza_version = as.character(utils::packageVersion("corteza")),
                  llm_api_version = as.character(utils::packageVersion("llm.api")),
                  settings = list(max_turns = session$max_turns, max_tokens = session$max_tokens,
                                  reasoning_effort = session$reasoning_effort, cache = session$cache,
                                  context_edit = session$context_edit %||% session$config$context_edit),
                  request_coverage = "unavailable")
    audit
}

.audit_turn_end <- function(session, audit, result = NULL, condition = NULL) {
    if (is.null(audit) || isTRUE(audit$ended)) {
        return(invisible(NULL))
    }
    audit$ended <- TRUE
    termination <- if (!is.null(condition)) {
        list(status = if (inherits(condition, "interrupt")) {
                "interrupted"
            } else {
                "error"
            },
             reason = class(condition)[[1L]])
    } else result$termination %||% list(status = "unknown",
                                        reason = "unexpected_exit")
    usage <- result$usage %||% audit$checkpoint_usage
    if (!length(usage)) {
        usage <- NULL
    }
    .audit_append(audit$state, "turn_end", turn_id = audit$id,
                  job_id = audit$origin$job_id, role = audit$role,
                  termination = termination,
                  condition_classes = if (!is.null(condition)) {
            as.list(class(condition))
        } else {
            NULL
        },
                  condition_message = if (!is.null(condition)) {
            conditionMessage(condition)
        } else {
            NULL
        },
                  usage = usage, usage_source = "canonical",
                  usage_coverage = if (is.null(usage)) "unknown" else if (is.null(result))
                  "checkpoints_partial" else "reported_turn_total",
                  route = result$route, request_coverage = "unavailable",
                  journal = .audit_journal_position(session),
                  elapsed_ms = (proc.time()[["elapsed"]] - audit$started) * 1000)
    session$last_turn_audit <- audit$ref
    invisible(audit$ref)
}

.audit_tool_call <- function(session, tool) {
    audit <- session$.audit_turn
    if (is.null(audit)) {
        return(list())
    }
    audit$calls <- audit$calls + 1L
    ids <- list(turn_id = audit$id,
                call_id = paste0(audit$id, ":c", audit$calls),
                audit = audit$ref)
    .audit_append(audit$state, "tool_call", turn_id = ids$turn_id,
                  call_id = ids$call_id, job_id = audit$origin$job_id, tool = tool)
    ids
}

.audit_tool_event <- function(session, event) {
    audit <- session$.audit_turn
    if (is.null(audit)) {
        return(invisible(NULL))
    }
    .audit_append(audit$state, "tool_event", turn_id = audit$id,
                  call_id = event$call$call_id, job_id = audit$origin$job_id,
                  tool = event$call$tool, outcome = event$outcome,
                  decision = event$decision, success = event$success,
                  elapsed_ms = event$elapsed_ms)
}

.audit_policy <- function(session, call, decision) {
    audit <- session$.audit_turn
    if (!is.null(audit)) .audit_append(audit$state, "policy",
                                       turn_id = audit$id,
                                       call_id = call$call_id,
                                       job_id = audit$origin$job_id,
                                       tool = call$tool, decision = decision)
    invisible(NULL)
}

.audit_derived <- function(ref) {
    list(usage_source = "derived", usage_turns = if (is.null(ref)) {
            list()
        } else {
            list(ref)
        })
}
