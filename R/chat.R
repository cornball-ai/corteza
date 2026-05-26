# Interactive chat inside an R session

# Detect providers supported by the currently loaded llm.api namespace.
# @noRd
llm_api_supported_providers <- function() {
    providers <- tryCatch(eval(formals(llm.api::agent)$provider),
                          error = function(e) character())

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
#' if (interactive()) {
#'     # Start chatting with defaults from config
#'     chat()
#'
#'     # Use a specific provider/model
#'     chat(provider = "ollama", model = "llama3.2")
#'
#'     # Minimal tools for focused work
#'     chat(tools = "core")
#' }
chat <- function(provider = NULL, model = NULL, tools = NULL, session = NULL,
                 max_turns = NULL) {
    if (!interactive()) {
        stop("chat() requires an interactive R session", call. = FALSE)
    }

    # Signal that chat() is the active console REPL so the RStudio
    # addin (corteza_execute_in_chat()) knows to auto-prefix /r or !
    # when sending lines from a script editor. Cleared on exit.
    options(corteza.chat_active = TRUE)
    on.exit(options(corteza.chat_active = NULL), add = TRUE)

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
    turn_session <- session_setup(channel = "console", cwd = cwd,
                                  provider = provider, model = model,
                                  tools = tools, history = history,
                                  load_project_context = TRUE,
                                  validate_api_key = TRUE,
                                  approval_cb = chat_approval_cb(cwd),
                                  max_turns = max_turns)
    config <- turn_session$config
    provider <- turn_session$provider
    model <- turn_session$model_map$cloud

    validate_model(provider, model)

    # Attach on-disk session metadata so observers can trace.
    turn_session$sessionId <- disk_session$sessionId
    turn_session$disk_session <- disk_session$session
    # Carry any persisted task list from the resumed session into the
    # live turn_session env so the next turn's prompt addendum sees it.
    turn_session$tasks <- disk_session$session$tasks %||% list()
    # task_create routes the approval prompt through this cb so we
    # use the R-console's blocking readline() and an empty Enter
    # means "yes" (matching the rest of corteza's prompt UX).
    turn_session$task_approval_cb <- function() {
        ans <- readline("Approve this plan? [y/n] ")
        tolower(trimws(ans)) %in% c("", "y", "yes")
    }

    # Workspace setup (session-scoped, resumed from disk when appropriate)
    ws_enabled <- isTRUE(config$workspace$enabled %||% TRUE)
    chat_workspace_init(disk_session, ws_enabled, config)

    # Register observers: progress printer + trace row per tool call.
    add_observer(turn_session, observer_progress())
    add_observer(turn_session, chat_trace_observer(turn_session))
    # Capture successful tool outputs into the per-session buffer so
    # /last and /outputs can replay them. Keyed by sessionId in the
    # package; this observer just relays. The "kind" attr lets /clear
    # find and replace this specific observer when the session resets.
    tool_buf_obs <- tool_buffer_observer(disk_session$session)
    attr(tool_buf_obs, "kind") <- "tool_buffer"
    add_observer(turn_session, tool_buf_obs)

    # Optional experimental layers -- off by default; opt in via options.
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
    # Resolve a placeholder NULL model to the provider's default name
    # (from llm.api's table) so the banner shows the actual model the
    # agent is about to call, matching the CLI's display.
    display_model <- resolve_provider_model(provider, model) %||%
    "(provider default)"
    color <- ansi_colors()
    session_line <- if (resumed_count > 0L) {
        sprintf("  %ssession %s  \u00b7  resumed (%d msgs)%s\n",
                color$dim, disk_session$sessionId, resumed_count,
                color$reset)
    } else {
        sprintf("  %ssession %s%s\n",
                color$dim, disk_session$sessionId, color$reset)
    }
    cat(corteza_startup_banner(
                               version = as.character(utils::packageVersion("corteza")),
                               model = display_model,
                               provider = provider
        ),
        "\n\n",
        session_line,
        "\n",
        sep = "")

    # /r evals are buffered here and prepended to the next real user
    # message, so the LLM sees what the user evaluated locally.
    pending_r_context <- character(0)

    # Most recent assistant reply text, exposed via /copy.
    last_assistant_response <- ""

    # Build the shared REPL context. The loop lives in run_repl_loop()
    # (R/repl.R); chat() supplies console-flavored hooks. ctx is an env
    # so the loop's reassignments to mutable state persist here.
    ctx <- new.env(parent = emptyenv())
    ctx$session <- turn_session
    ctx$disk_session <- disk_session
    ctx$config <- config
    ctx$cwd <- cwd
    ctx$ws_enabled <- ws_enabled
    ctx$provider <- provider
    ctx$model <- model
    ctx$pending_r_context <- pending_r_context
    ctx$last_assistant_response <- last_assistant_response
    ctx$read_input <- function(p) read_prompt_input(p)
    ctx$palette <- color
    ctx$render_reply <- function(txt) cat(render_md_ansi(txt, palette = color), "\n\n")
    ctx$help_text <- chat_help_text
    ctx$new_session_fn <- function() {
        # Read provider/model from ctx, not the original locals: /model
        # and /provider mutate ctx, so a later /clear must spin up the
        # fresh session with the current model/provider.
        fresh <- session_new(ctx$provider, ctx$model, cwd)
        list(session = fresh, sessionId = fresh$sessionId, resumed = FALSE)
    }
    ctx$handle_copy <- chat_handle_copy
    ctx$format_tools <- chat_format_tools_list
    ctx$turn_fn <- turn
    run_repl_loop(ctx)

    # ctx$disk_session, not the original local: /clear reassigns it.
    invisible(ctx$disk_session$session)
}

# --- Chat-specific helpers ---

# Default console approval callback: structured prompt with session-local
# "allow always" support.
chat_approval_cb <- function(cwd = getwd()) {
    approved <- new.env(parent = emptyenv())
    color <- ansi_colors()

    function(call, decision) {
        key <- call$tool %||% ""
        if (isTRUE(approved[[key]])) {
            return(TRUE)
        }

        persistent_label <- "Allow always for this session"
        lines <- cli_approval_lines(call, decision, cwd = cwd,
                                    persistent_label = persistent_label,
                                    deny_label = .console_deny_label())
        cat(paste(lines, collapse = "\n"), "\n")

        ans <- read_prompt_input("Choice: ")
        if (length(ans) == 0L) {
            ans <- ""
        }
        ans <- trimws(ans)
        if (ans == "") {
            ans <- "1"
        }
        if (ans == "2") {
            approved[[key]] <- TRUE
        }
        # RStudio's R console doesn't honor cursor-position escapes, so
        # we leave the approval block in scrollback and just append the
        # one-line summary below it.
        summary <- cli_user_replied_line(call, ans,
            persistent_label = persistent_label,
            cwd = cwd)
        cat(sprintf("%s\u25CF%s User replied:\n  %s\u23BF  %s%s\n\n",
                    color$cyan, color$reset,
                    color$dim, summary, color$reset))

        if (ans == "3") {
            stop(user_deny_condition(call$tool %||% ""))
        }
        ans %in% c("1", "2")
    }
}

# Resolve the on-disk session, returning list(session, sessionId, history).
resolve_disk_session <- function(session_arg, provider, model, cwd) {
    if (is.character(session_arg)) {
        resumed <- session_load(session_arg)
        if (!is.null(resumed)) {
            return(list(session = resumed, sessionId = resumed$sessionId,
                        history = disk_messages_to_history(resumed$messages),
                        resumed = TRUE))
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
        if (!identical(event$outcome, "ran") &&
            !identical(event$outcome, "deny") &&
            !identical(event$outcome, "declined")) {
            return(invisible(NULL))
        }
        tryCatch(
                 trace_add(session$sessionId, event$call$tool, event$call$args,
                           event$result, success = event$success,
                           elapsed_ms = round(event$elapsed_ms),
                           turn = event$turn_number),
                 error = function(e) NULL
        )
    }
}

