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
    args
}
