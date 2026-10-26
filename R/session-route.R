# Configuration supplies a new session's defaults. On resume, restore the
# saved provider and model together unless the caller explicitly overrides them.
.session_model_pair <- function(provider = NULL, model = NULL,
                                config = list(), saved = NULL) {
    if (!is.null(saved$provider)) {
        base <- saved
    } else {
        base <- config
    }
    selected <- provider %||% base$provider %||% "anthropic"
    if (is.null(model)) {
        if (identical(selected, base$provider %||% "anthropic")) {
            model <- base$model
        } else if (identical(selected, config$provider)) {
            model <- config$model
        }
    }
    list(provider = selected, model = resolve_provider_model(selected, model))
}
