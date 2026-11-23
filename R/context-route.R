# Preflight the route that will actually run. A quota fallback may have
# a different window, byte limit, and history-editing capability.
.with_context_route <- function(session, callback,
                                route = .context_edit_request_for(session)) {
    if (is.null(route)) {
        return(callback())
    }
    provider <- session$provider
    model_map <- session$model_map
    on.exit({
        session$provider <- provider
        session$model_map <- model_map
    }, add = TRUE)
    session$provider <- route$provider
    session$model_map$cloud <- route$model
    callback()
}

.prepare_route_context <- function(args, session, config) {
    compacted <- .with_context_route(session, function() {
        maybe_compact_turn_session(session, config, kind = session$kind,
                                   tools = args$tools, system = args$system,
                                   reason = "preflight")
    }, route = args[c("provider", "model")])
    if (isTRUE(compacted)) {
        args$history <- session$history
    }
    if (isTRUE(session$.protect_review_inputs)) {
        limit <- context_limit_for_model(args$model, args$provider)
        session$.review_limit <- min(limit, .window_value(session$context_window) %||% limit,
                                     .window_value(config$context_window) %||% limit)
        session$.review_prefix <- args[c("system", "tools", "max_tokens")]
        .review_check_size(session, args$history, args$prompt)
    }
    args
}

.review_check_size <- function(session, history, prompt = NULL) {
    # Use encoded bytes as a conservative bound for text and tool JSON.
    # Leave space for framing and the reviewer's answer.
    payload <- c(session$.review_prefix, list(history = history, prompt = prompt))
    bytes <- nchar(.history_json(payload), type = "bytes")
    reserve <- max(4096, session$.review_prefix$max_tokens %||% 0)
    if (bytes + reserve > session$.review_limit) {
        stop("The review exceeds the reviewer model's input budget; a person must decide.",
             call. = FALSE)
    }
}
