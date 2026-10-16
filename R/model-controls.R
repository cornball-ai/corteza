# Model and effort controls shared by the REPL and Matrix rooms.
.model_menu_entries <- function(session, cfg = NULL) {
    cfg <- cfg %||% session$config %||% list()
    provider <- session$provider %||% cfg$provider %||% "anthropic"
    model <- session$model_map$cloud %||% cfg$model %||%
    default_provider_model(provider)
    entries <- list()
    add <- function(id, p) {
        if (is.null(id) || !nzchar(id)) {
            return(invisible(NULL))
        }
        duplicate <- any(vapply(entries, function(x) {
            identical(x$model, id) && identical(x$provider, p)
        }, logical(1)))
        if (!duplicate) {
            entries[[length(entries) + 1L]] <<- list(model = id, provider = p)
        }
        invisible(NULL)
    }
    add(model, provider)
    for (entry in bot_available_models(cfg, ollama_models = character())) {
        add(entry$model, entry$provider)
    }
    note <- NULL
    catalog <- tryCatch(.model_discover(provider,
                                        base_url = session$base_url %||% cfg$base_url), error = function(e) {
        note <<- paste("Catalog unavailable; showing current/configured models.",
                       .sanitize_inline(conditionMessage(e), max_chars = 180L))
        character()
    })
    for (id in catalog) {
        add(id, provider)
    }
    if (identical(attr(catalog, "source"), "Codex cache")) {
        note <- "Codex entries come from its local model cache; account availability may have changed."
    }
    attr(entries, "notice") <- note
    entries
}

.model_discover <- function(provider, base_url = NULL) {
    if (!"list_models" %in% getNamespaceExports("llm.api")) {
        stop("Live catalogs require an updated llm.api.", call. = FALSE)
    }
    getExportedValue("llm.api", "list_models")(provider, base_url = base_url)
}

.model_menu_text <- function(entries, session) {
    paste(c(bot_render_model_menu(entries, session), attr(entries, "notice")),
          collapse = "\n")
}

.repl_model_command <- function(ctx, text) {
    cmd <- bot_parse_model_command(text)
    if (is.null(cmd)) {
        return("Usage: /model [name [provider] | number]")
    }
    ack <- bot_apply_model_command(ctx$session, cmd, cfg = ctx$config)
    ctx$model <- ctx$session$model_map$cloud
    ctx$provider <- ctx$session$provider
    if (!is.null(ctx$disk_session)) {
        ctx$disk_session$session$model <- ctx$model
        ctx$disk_session$session$provider <- ctx$provider
    }
    ack
}

.effort_parse_command <- function(text) {
    text <- trimws(bot_command_text(text))
    if (!grepl("^/effort(?:\\s|$)", text, ignore.case = TRUE, perl = TRUE)) {
        return(NULL)
    }
    parts <- strsplit(text, "\\s+")[[1]]
    list(value = if (length(parts) == 2L) tolower(parts[2]) else NULL,
         invalid = length(parts) > 2L)
}

.effort_command <- function(session, cmd, cfg = NULL) {
    usage <- "Usage: /effort [level | default]"
    if (isTRUE(cmd$invalid)) {
        return(usage)
    }
    provider <- session$provider %||% ""
    supported <- provider %in% c(.reasoning_effort_providers,
                                 .anthropic_providers)
    value <- cmd$value
    if (!is.null(value)) {
        if (identical(value, "default")) {
            if (isTRUE(session$talker)) {
                configured <- cfg$talker$reasoning_effort
            } else {
                configured <- cfg$reasoning_effort
            }
            session$reasoning_effort <- .check_reasoning_effort(configured, "/effort default")
            session$effort_command_override <- FALSE
        } else {
            if (!supported) {
                return(paste("Reasoning effort is not supported by provider", provider))
            }
            if (!grepl("^[a-z][a-z0-9_-]*$", value)) {
                return(usage)
            }
            # The provider owns the vocabulary; new levels need no client update.
            session$reasoning_effort <- .check_reasoning_effort(value, "/effort")
            session$effort_command_override <- TRUE
        }
    }
    effort <- .session_reasoning_effort(session)
    source <- if (isTRUE(session$effort_command_override)) "session override" else
    if (!is.null(session$reasoning_effort) && is.null(cfg)) {
        "session setting"
    } else
    if (!is.null(effort)) {
        "configuration"
    } else {
        "provider default"
    }
    lines <- c(sprintf("Reasoning effort: %s (%s)", effort %||% "default", source),
               usage)
    if (supported) {
        lines <- c(lines, "Levels depend on the model; common values: low, medium, high, xhigh, max.",
                   "Changes apply to the next reply. default restores the configured/provider default.")
    } else {
        lines <- c(lines, "This provider does not receive a reasoning-effort setting.")
    }
    paste(lines, collapse = "\n")
}
