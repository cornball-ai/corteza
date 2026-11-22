# Model and effort controls shared by the REPL and Matrix rooms.
#
# /model and /effort act on the session's own model, the one that does
# the work. In a talker session that is the doer's (talker_enable()).
# Selecting a provider moves both roles to that provider's pair.

# The model /model shows and sets.
.session_work_model <- function(session) {
    if (isTRUE(session$talker)) {
        return(list(model = session$doer_model,
                    provider = session$doer_provider))
    }
    list(model = session$model_map$cloud, provider = session$provider)
}

# Set it, and the provider when one is given.
.set_work_model <- function(session, model, provider = NULL) {
    provider <- provider %||% .session_work_model(session)$provider
    if (!is.null(provider)) provider <- .model_provider(provider)
    if (!isTRUE(session$talker)) {
        bot_set_session_model(session, model %||% default_provider_model(provider))
        session$provider <- provider
        session$context_active_request <- NULL
        return(invisible(session))
    }
    .remember_model_pair(session)
    pair <- .model_pair(session, provider, model)
    .apply_model_pair(session, pair)
    .remember_model_pair(session)
    invisible(session)
}

# What /model says once it has set one.
.work_model_ack <- function(session) {
    work <- .session_work_model(session)
    set <- sprintf("Model set: %s (provider: %s).",
                   .sanitize_inline(work$model %||% "", max_chars = 80L),
                   .sanitize_inline(work$provider %||% "(unchanged)", max_chars = 40L))
    if (!isTRUE(session$talker)) {
        return(paste(c(paste(set, "Effective on the next reply."),
                       .context_management_notice(session)), collapse = "\n"))
    }
    if (isTRUE(session$talker_follows)) {
        return(paste(c(paste(set, "Effective on the next reply and the next job."),
                       .context_management_notice(session)), collapse = "\n"))
    }
    paste(c(set, sprintf("Replies: %s. Next job: %s. Both use %s.",
        .sanitize_inline(bot_session_model(session) %||% "", max_chars = 80L),
        .sanitize_inline(work$model %||% "", max_chars = 80L), work$provider),
        .context_management_notice(session)), collapse = "\n")
}

.model_menu_entries <- function(session, cfg = NULL) {
    cfg <- cfg %||% session$config %||% list()
    work <- .session_work_model(session)
    provider <- work$provider %||% cfg$provider %||% "anthropic"
    model <- work$model %||% cfg$model %||%
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
    for (p in names(cfg$model_pairs)) {
        pair <- tryCatch(.model_pair(session, p), error = function(e) NULL)
        if (!is.null(pair)) add(pair$doer$model, pair$provider)
    }
    note <- NULL
    catalog <- tryCatch(.model_discover(provider,
                                        base_url = if (identical(provider, "openai_compatible")) session$base_url %||% cfg$base_url), error = function(e) {
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

# /provider at the terminal sets the work's provider, as /model sets
# its model. A talker session restores that provider's pair; an unseen
# provider starts from its default model.
.repl_provider_command <- function(ctx, parts) {
    s <- ctx$session
    work <- .session_work_model(s)
    if (length(parts) < 2L) {
        return(sprintf("Current provider: %s\nUsage: /provider <name>\n",
                       work$provider %||% "(default)"))
    }
    failure <- tryCatch({.set_work_model(s, NULL, parts[2]); NULL},
                        error = function(e) conditionMessage(e))
    if (!is.null(failure)) return(paste("Provider unchanged:", failure, "\n"))
    work <- .session_work_model(s)
    provider <- work$provider
    # The session's own provider, which --resume starts from.
    ctx$provider <- provider
    ctx$model <- work$model
    if (!is.null(ctx$disk_session)) {
        ctx$disk_session$session$provider <- provider
        ctx$disk_session$session$model <- work$model
    }
    paste0(.work_model_ack(s), "\n")
}

# The model and provider /review, /doctor and /config use and show: the
# work's. In a talker session that is the doer's (.session_work_model()),
# which the talker's own provider may not serve.
.repl_work_route <- function(ctx) {
    s <- ctx$session
    if (isTRUE(s$talker)) {
        work <- .session_work_model(s)
        return(list(model = work$model %||% ctx$model,
                    provider = work$provider %||% s$provider %||% ctx$provider))
    }
    list(model = ctx$model, provider = s$provider %||% ctx$provider)
}

.repl_model_command <- function(ctx, text) {
    cmd <- bot_parse_model_command(text)
    if (is.null(cmd)) {
        return("Usage: /model [name [provider] | number]")
    }
    ack <- bot_apply_model_command(ctx$session, cmd, cfg = ctx$config)
    # The session's own model, which --resume starts from.
    work <- .session_work_model(ctx$session)
    ctx$model <- work$model
    ctx$provider <- work$provider
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
    # A talker session's setting is the doer's (see the top of the file).
    talker <- isTRUE(session$talker)
    provider <- .session_work_model(session)$provider %||% ""
    supported <- provider %in% c(.reasoning_effort_providers,
                                 .anthropic_providers)
    value <- cmd$value
    if (!is.null(value)) {
        if (identical(value, "default")) {
            if (talker) {
                session$doer_reasoning_effort <- session$doer_reasoning_default
            } else {
                session$reasoning_effort <- .check_reasoning_effort(
                    cfg$reasoning_effort, "/effort default")
            }
            session$effort_command_override <- FALSE
        } else {
            if (!supported) {
                return(paste("Reasoning effort is not supported by provider", provider))
            }
            if (!grepl("^[a-z][a-z0-9_-]*$", value)) {
                return(usage)
            }
            # The provider owns the vocabulary; new levels need no client update.
            effort <- .check_reasoning_effort(value, "/effort")
            if (talker) {
                session$doer_reasoning_effort <- effort
            } else {
                session$reasoning_effort <- effort
            }
            session$effort_command_override <- TRUE
        }
    }
    if (talker) {
        effort <- session$doer_reasoning_effort
    } else {
        effort <- .session_reasoning_effort(session)
    }
    source <- if (isTRUE(session$effort_command_override)) "session override" else
    if (!talker && !is.null(session$reasoning_effort) && is.null(cfg)) {
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
