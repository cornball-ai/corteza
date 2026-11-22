# A provider switch moves both roles. Remember the pair when switching
# away; model_pairs in the config can supply pairs not used yet.
.model_provider <- function(provider) {
    provider <- tolower(trimws(provider))
    ensure_llm_api_provider(provider)
    provider
}

.remember_model_pair <- function(session) {
    if (!isTRUE(session$talker) ||
        !identical(session$provider, session$doer_provider)) {
        return(invisible(NULL))
    }
    role <- function(model, effort, budget, thinking) {
        list(model = model, reasoning_effort = effort,
             thinking_budget_tokens = budget, thinking = thinking)
    }
    pair <- list(provider = session$provider, follows = isTRUE(session$talker_follows),
                 talker = role(session$model_map$cloud, session$reasoning_effort,
                               session$thinking_budget_tokens, session$thinking),
                 doer = role(session$doer_model, session$doer_reasoning_effort,
                             session$doer_thinking_budget, session$doer_thinking))
    pair$doer["reasoning_default"] <- list(session$doer_reasoning_default)
    session$.model_pairs[[session$provider]] <- pair
    invisible(pair)
}

.model_pair_role <- function(value, model) {
    if (is.character(value) && length(value) == 1L) {
        value <- list(model = value)
    }
    if (is.null(value)) {
        value <- list()
    }
    if (!is.list(value)) {
        stop("Each model_pairs role must be a model name or object",
             call. = FALSE)
    }
    value$model <- value$model %||% model
    if (!is.character(value$model) || length(value$model) != 1L ||
        is.na(value$model) || !nzchar(trimws(value$model))) {
        stop("Each model pair needs a non-empty model for both roles", call. = FALSE)
    }
    value$reasoning_effort <- .check_reasoning_effort(value$reasoning_effort, "model_pairs")
    value$thinking_budget_tokens <- .check_max_tokens(value$thinking_budget_tokens,
        "model_pairs", what = "thinking_budget_tokens")
    value$thinking <- .check_thinking(value[["thinking"]], "model_pairs")
    value
}

.model_pair <- function(session, provider, model = NULL) {
    provider <- tolower(trimws(provider))
    pair <- session$.model_pairs[[provider]] %||% session$config$model_pairs[[provider]]
    if (is.null(pair)) {
        selected <- model %||% default_provider_model(provider)
        pair <- list(follows = TRUE,
            talker = list(model = selected, reasoning_effort = session$reasoning_effort,
                          thinking_budget_tokens = session$thinking_budget_tokens),
            doer = list(model = selected, reasoning_effort = session$doer_reasoning_effort,
                        thinking_budget_tokens = session$doer_thinking_budget))
        pair$doer["reasoning_default"] <- list(session$doer_reasoning_default)
    }
    if (!is.list(pair)) {
        stop("model_pairs entries must be objects", call. = FALSE)
    }
    follows <- pair$follows %||% is.null(pair$talker)
    work <- .model_pair_role(pair$doer,
                             model %||% default_provider_model(provider))
    if (!is.null(model) && !identical(model, work$model)) {
        work$model <- model
        work$thinking <- NULL
    }
    previous_talker <- .model_pair_role(pair$talker, work$model)
    if (isTRUE(follows) && !identical(work$model, previous_talker$model)) {
        previous_talker$model <- work$model
        previous_talker$thinking <- NULL
    }
    list(provider = provider, follows = isTRUE(follows), talker = previous_talker, doer = work)
}

.apply_model_pair <- function(session, pair, replies = TRUE) {
    session$doer_provider <- pair$provider
    session$doer_model <- pair$doer$model
    session$doer_reasoning_effort <- pair$doer$reasoning_effort
    session$doer_reasoning_default <- if ("reasoning_default" %in% names(pair$doer)) {
        pair$doer$reasoning_default
    } else pair$doer$reasoning_effort
    session$doer_thinking_budget <- pair$doer$thinking_budget_tokens
    session$doer_thinking <- pair$doer[["thinking"]]
    if (isTRUE(replies)) {
        session$provider <- pair$provider
        bot_set_session_model(session, pair$talker$model)
        session$reasoning_effort <- pair$talker$reasoning_effort
        session$thinking_budget_tokens <- pair$talker$thinking_budget_tokens
        session$thinking <- pair$talker[["thinking"]]
        session$talker_follows <- pair$follows
        session$context_active_request <- NULL
    }
    invisible(session)
}
