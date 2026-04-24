# Interactive chat inside an R session

# Detect providers supported by the currently loaded llm.api namespace.
# @noRd
llm_api_supported_providers <- function() {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        return(character())
    }

    providers <- tryCatch(
                          eval(formals(llm.api::agent)$provider),
                          error = function(e) character()
    )

    unique(as.character(providers %||% character()))
}

# Reload llm.api from disk so chat() can pick up newly installed providers
# without requiring a full R restart.
# @noRd
reload_llm_api_namespace <- function() {
    if ("package:llm.api" %in% search()) {
        try(detach("package:llm.api", unload = TRUE, character.only = TRUE),
            silent = TRUE)
    }

    if ("llm.api" %in% loadedNamespaces()) {
        try(unloadNamespace("llm.api"), silent = TRUE)
    }

    requireNamespace("llm.api", quietly = TRUE)
}

# Ensure the active llm.api namespace supports the requested provider.
# @noRd
ensure_llm_api_provider <- function(provider) {
    supported <- llm_api_supported_providers()
    if (provider %in% supported) {
        return(invisible(supported))
    }

    reload_llm_api_namespace()
    supported <- llm_api_supported_providers()
    if (provider %in% supported) {
        return(invisible(supported))
    }

    supported_text <- if (length(supported) > 0) {
        paste(supported, collapse = ", ")
    } else {
        "unknown"
    }

    stop(sprintf(
                 "Current llm.api namespace does not support provider '%s'. Restart R after reinstalling llm.api. Supported providers: %s",
                 provider, supported_text
        ), call. = FALSE)
}

# Validate model availability before starting the chat loop
# @noRd
validate_model <- function(provider, model) {
    if (provider == "ollama") {
        # Check ollama is running and model exists
        models <- tryCatch({
            url <- paste0(Sys.getenv("OLLAMA_HOST", "http://localhost:11434"),
                          "/api/tags")
            resp <- jsonlite::fromJSON(url, simplifyVector = FALSE)
            vapply(resp$models %||% list(), function(m) {
                m$name %||% m$model %||% ""
            }, character(1))
        }, error = function(e) {
            stop("Can't connect to ollama. Is it running?", call. = FALSE)
        })
        if (!is.null(model)) {
            # ollama models can be "qwen2.5-coder" or "qwen2.5-coder:latest"
            matched <- model %in% models || paste0(model, ":latest") %in% models
            if (!matched) {
                available <- paste(models, collapse = ", ")
                stop(sprintf("Model '%s' not found in ollama. Available: %s\nPull with: ollama pull %s",
                             model, available, model), call. = FALSE)
            }
        }
    }
    invisible(TRUE)
}

# Brief context hint for tool calls shown in REPL
# @noRd
tool_hint <- function(name, args) {
    hint <- if (name %in% c("base::readLines", "read_file")) {
        args$con %||% args$path %||% args$file
    } else if (name %in% c("base::writeLines", "write_file")) {
        args$con %||% args$path %||% args$file
    } else if (name == "replace_in_file") {
        args$path %||% args$file
    } else if (name == "list_files") {
        args$path %||% "."
    } else if (name == "base::list.files") {
        args$path %||% "."
    } else if (name == "bash") {
        cmd <- args$command %||% ""
        if (nchar(cmd) > 60) {
            paste0(substr(cmd, 1, 57), "...")
        } else {
            cmd
        }
    } else if (name == "grep_files") {
        paste0("/", args$pattern %||% "", "/")
    } else if (name == "run_r") {
        code <- args$code %||% ""
        if (nchar(code) > 60) {
            paste0(substr(code, 1, 57), "...")
        } else {
            code
        }
    } else if (name == "run_r_script") {
        args$path %||% args$file %||% ""
    } else if (name == "r_help") {
        args$topic %||% ""
    } else if (name == "web_search") {
        args$query %||% ""
    } else if (name == "fetch_url") {
        args$url %||% ""
    } else if (name == "git_status") {
        args$path %||% "status"
    } else if (name == "git_diff") {
        args$file_path %||% args$ref %||% args$path %||% ""
    } else if (name == "git_log") {
        args$path %||% as.character(args$n %||% 10L)
    } else if (name == "installed_packages") {
        args$pattern %||% ""
    } else {
        NULL
    }
    if (is.null(hint) || nchar(hint) == 0) {
        ""
    } else {
        paste0(" ", hint)
    }
}

#' Start Interactive Chat
#'
#' Run a conversational agent inside your R session. Tools execute as direct
#' function calls, no MCP server needed.
#'
#' @param provider LLM provider: "anthropic", "openai", "moonshot", or
#'   "ollama".
#'   Defaults to config value or "anthropic".
#' @param model Model name. Defaults to config value or provider default.
#' @param tools Character vector of tool names or categories to enable.
#'   Categories: file, code, r, data, web, git, chat, memory.
#'   Use "core" for file+code+git, "all" for everything (default).
#' @param session Session resume control. NULL (default) starts fresh,
#'   TRUE resumes the latest session, or a character session key to
#'   resume a specific session.
#' @param max_turns Integer or NULL. Maximum LLM turns per user prompt
#'   before the loop stops with \code{[Max turns reached]}. NULL (default)
#'   reads \code{getOption("corteza.max_turns")}, then falls back to the
#'   \code{\link{session_setup}} default (50).
#'
#' @return The session object (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Start chatting with defaults from config
#' chat()
#'
#' # Use a specific provider/model
#' chat(provider = "ollama", model = "llama3.2")
#'
#' # Minimal tools for focused work
#' chat(tools = "core")
#' }
chat <- function(provider = NULL, model = NULL, tools = NULL, session = NULL,
                 max_turns = NULL) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        stop("llm.api package required for chat(). ",
             "Install with: install.packages('llm.api')",
             call. = FALSE)
    }
    if (!interactive()) {
        stop("chat() requires an interactive R session", call. = FALSE)
    }

    max_turns <- as.integer(
        max_turns %||% getOption("corteza.max_turns") %||% 50L
    )

    cwd <- getwd()

    # Resume / create the on-disk session record so we can persist the
    # transcript and workspace between R sessions.
    session_arg <- session
    disk_session <- resolve_disk_session(session_arg, provider, model, cwd)
    history <- disk_session$history %||% list()
    resumed_count <- length(history)

    # Shared pre-session setup: config, provider, API key, skills,
    # system prompt.
    turn_session <- session_setup(
                                  channel = "console",
                                  cwd = cwd,
                                  provider = provider,
                                  model = model,
                                  tools = tools,
                                  history = history,
                                  load_project_context = TRUE,
                                  validate_api_key = TRUE,
                                  approval_cb = chat_approval_cb,
                                  max_turns = max_turns
    )
    config <- turn_session$config
    provider <- turn_session$provider
    model <- turn_session$model_map$cloud

    validate_model(provider, model)

    # Attach on-disk session metadata so observers can trace.
    turn_session$sessionId <- disk_session$sessionId
    turn_session$disk_session <- disk_session$session

    # Workspace setup (session-scoped, resumed from disk when appropriate)
    ws_enabled <- isTRUE(config$workspace$enabled %||% TRUE)
    chat_workspace_init(disk_session, ws_enabled, config)

    # Register observers: progress printer + trace row per tool call.
    add_observer(turn_session, observer_progress())
    add_observer(turn_session, chat_trace_observer(turn_session))

    # Optional experimental layers — off by default; opt in via options.
    if (isTRUE(getOption("corteza.experimental_ce", FALSE))) {
        ce_init(cwd, config)
        for (i in seq_along(history)) {
            ce_index_turn(i, history[[i]]$role, history[[i]]$content %||% "")
        }
        on.exit(ce_shutdown(), add = TRUE)
    }
    if (isTRUE(getOption("corteza.experimental_heartbeat", FALSE))) {
        hb_init(config)
    }

    set_log_enabled(FALSE)
    on.exit(set_log_enabled(TRUE), add = TRUE)

    n_tools <- length(skills_as_api_tools(tools))
    display_model <- model %||% "(provider default)"
    cat(sprintf(
                "corteza chat | %s @ %s | %d tools | /quit to exit%s\n\n",
                display_model, provider, n_tools,
            if (resumed_count > 0L) {
                sprintf(" | resumed (%d msgs)", resumed_count)
            } else {
                ""
            }
        ))

    # /r evals are buffered here and prepended to the next real user
    # message, so the LLM sees what the user evaluated locally.
    pending_r_context <- character(0)

    while (TRUE) {
        prompt <- readline("> ")
        if (nchar(trimws(prompt)) == 0) {
            next
        }
        if (trimws(prompt) %in% c("/quit", "/exit", "/q")) {
            if (ws_enabled) {
                ws_prune()
                tryCatch(ws_save(disk_session$sessionId),
                         error = function(e) NULL)
            }
            cat("Bye.\n")
            break
        }
        if (startsWith(trimws(prompt), "/r ")) {
            code <- sub("^/r\\s+", "", trimws(prompt))
            r_env <- new.env(parent = emptyenv())
            result_lines <- tryCatch(
                capture.output({
                    r_env$r <- withVisible(eval(parse(text = code),
                                                envir = .GlobalEnv))
                    if (r_env$r$visible) print(r_env$r$value)
                }),
                error = function(e) {
                    r_env$r <- NULL
                    paste("Error:", e$message)
                }
            )
            result_text <- paste(result_lines, collapse = "\n")
            # Show the output immediately so the user can react.
            if (nchar(result_text) > 0) cat(result_text, "\n", sep = "")
            # Stage for the next send so the LLM has the same context —
            # but a printed data frame or big vector can easily be tens
            # of thousands of tokens, so cap the staged text and fall
            # back to str() for the oversize case.
            staged <- if (nchar(result_text) > 4000L && !is.null(r_env$r)) {
                str_lines <- tryCatch(
                    capture.output(utils::str(r_env$r$value)),
                    error = function(e) paste("Error:", e$message)
                )
                sprintf(
                    "(%d chars of output truncated; showing str())\n%s",
                    nchar(result_text),
                    paste(str_lines, collapse = "\n")
                )
            } else {
                result_text
            }
            pending_r_context <- c(
                pending_r_context,
                sprintf("[/r] %s\n%s", code, staged)
            )
            next
        }

        if (length(pending_r_context) > 0) {
            prompt <- paste(c(pending_r_context, prompt), collapse = "\n\n")
            pending_r_context <- character(0)
        }
        transcript_append(disk_session$session, "user", prompt)

        result <- tryCatch(
                           turn(prompt, turn_session),
                           error = function(e) {
            message("Error: ", e$message)
            NULL
        }
        )
        if (is.null(result)) {
            next
        }

        reply <- result$reply %||% ""
        if (nchar(reply) == 0) {
            cat("[No response text]\n\n")
        } else {
            cat(reply, "\n\n")
        }
        transcript_append(disk_session$session, "assistant", reply)
    }

    invisible(disk_session$session)
}

# --- Chat-specific helpers ---

# Default console approval callback: prompt the user via readline.
chat_approval_cb <- function(call, decision) {
    cat(sprintf("\n[approval] %s on %s (%s)\n",
                decision$approval, call$tool, decision$reason))
    ans <- readline(">>> approve? [y/N] ")
    tolower(trimws(ans)) %in% c("y", "yes")
}

# Resolve the on-disk session, returning list(session, sessionId, history).
resolve_disk_session <- function(session_arg, provider, model, cwd) {
    if (is.character(session_arg)) {
        resumed <- session_load(session_arg)
        if (!is.null(resumed)) {
            return(list(
                        session = resumed,
                        sessionId = resumed$sessionId,
                        history = disk_messages_to_history(resumed$messages),
                        resumed = TRUE
                ))
        }
        fresh <- session_new(provider, model, cwd, session_key = session_arg)
        return(list(session = fresh, sessionId = fresh$sessionId,
                    history = list(), resumed = FALSE))
    }
    if (isTRUE(session_arg)) {
        latest <- session_latest()
        if (!is.null(latest)) {
            return(list(
                        session = latest,
                        sessionId = latest$sessionId,
                        history = disk_messages_to_history(latest$messages),
                        resumed = TRUE
                ))
        }
    }
    fresh <- session_new(provider, model, cwd)
    list(session = fresh, sessionId = fresh$sessionId,
         history = list(), resumed = FALSE)
}

# Flatten on-disk message blocks into simple {role, content} pairs.
disk_messages_to_history <- function(messages) {
    lapply(messages %||% list(), function(m) {
        text <- if (is.list(m$content) && length(m$content) > 0L &&
            !is.null(m$content[[1]]$text)) {
            m$content[[1]]$text
        } else {
            as.character(m$content)
        }
        list(role = m$role, content = text)
    })
}

# Load or clear the workspace to match the (possibly resumed) disk session.
chat_workspace_init <- function(disk_session, ws_enabled, config) {
    if (isTRUE(disk_session$resumed)) {
        ws_load(disk_session$sessionId)
    } else {
        ws_clear()
        if (ws_enabled && isTRUE(config$workspace$scan_globalenv %||% TRUE)) {
            scan_limit <- config$workspace$scan_max_bytes %||% 50e6
            registered <- ws_scan_globalenv(max_bytes = scan_limit)
            if (length(registered) > 0L) {
                cat(sprintf("Workspace: registered %d objects from R session\n",
                            length(registered)))
            }
        }
    }
}

# Build a trace observer that records each tool call against the on-disk
# session. Swallows errors so trace failures don't break tool dispatch.
chat_trace_observer <- function(session) {
    function(event) {
        tryCatch(
                 trace_add(
                           session$sessionId,
                           event$call$tool,
                           event$call$args,
                           event$result,
                           success = event$success,
                           elapsed_ms = round(event$elapsed_ms),
                           turn = event$turn_number
            ),
                 error = function(e) NULL
        )
    }
}

