# Interactive chat inside an R session

# Detect providers supported by the currently loaded llm.api namespace.
# @noRd
llm_api_supported_providers <- function() {
    providers <- tryCatch(eval(formals(llm.api::agent)$provider),
                          error = function(e) character())

    unique(as.character(providers %||% character()))
}

# Ensure the loaded llm.api supports the requested provider. An llm.api
# installed during the session cannot replace it: corteza imports
# llm.api, so R will not unload it while corteza is loaded. The message
# says to restart R.
# @noRd
ensure_llm_api_provider <- function(provider) {
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
#' A project's own files (its \code{.corteza/} folder, \code{AGENTS.md} or
#' \code{CLAUDE.md}, and the briefing built from it) load only from a
#' directory you have trusted. Where the working directory is not trusted
#' and holds such files, \code{chat()} lists them and asks once before it
#' starts; \code{/trust} shows the same later. See "Trusted projects" in
#' \code{vignette("configuration")}.
#'
#' @param provider LLM provider: "anthropic", "openai", "moonshot", or
#'   "ollama".
#'   Defaults to the saved provider when resuming, otherwise the config value
#'   or "anthropic".
#' @param model Model name. Defaults to the saved model when resuming with the
#'   same provider, otherwise the matching config value or provider default.
#' @param tools Character vector of tool names or categories to enable.
#'   Categories: file, code, r, data, web, git, chat, memory.
#'   Use "core" for file+code+git, "all" for everything (default).
#' @param session Session resume control. NULL (default) starts fresh,
#'   TRUE resumes the latest session started in the current directory,
#'   or a character session key to resume a specific session from any
#'   directory.
#' @param max_turns Integer or NULL. Maximum LLM turns per user prompt
#'   before the loop stops with \code{[Max turns reached]}. NULL (default)
#'   reads \code{getOption("corteza.max_turns")}, then falls back to the
#'   \code{\link{session_setup}} default (50).
#' @param web_search Logical or NULL. Enable provider-native
#'   (server-side) web search for the conversation. NULL uses the config
#'   default (on for supported providers); the model searches inside its
#'   own turn with no Tavily key required.
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
                 max_turns = NULL, web_search = NULL) {
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

    # Trust is decided before anything of the project's is read. chat()
    # runs only where a person is at the console, so it asks, once,
    # where the directory holds something a session would load.
    trust_at_start(cwd, ask = readline)

    # Resume / create the on-disk session record so we can persist the
    # transcript and workspace between R sessions.
    session_arg <- session
    config <- load_config(cwd)
    route <- .session_model_pair(provider, model, config)
    disk_session <- resolve_disk_session(session_arg, route$provider,
        route$model, cwd)
    resume_note <- NULL
    if (isTRUE(disk_session$resumed)) {
        route <- .session_model_pair(provider, model, config,
                                     saved = disk_session$session)
        # Read before the resume writes to the session (R/retention.R).
        resume_note <- retention_resume_note(disk_session$sessionId)
    }
    history <- disk_session$history %||% list()
    resumed_count <- length(history)

    # Shared pre-session setup: config, provider, API key, skills,
    # system prompt.
    turn_session <- session_setup(channel = "console", cwd = cwd,
                                  provider = route$provider, model = route$model,
                                  tools = tools, history = history,
                                  load_project_context = TRUE,
                                  validate_api_key = TRUE,
                                  approval_cb = chat_approval_cb(cwd),
                                  max_turns = max_turns,
                                  web_search = web_search)
    config <- turn_session$config
    provider <- turn_session$provider
    model <- turn_session$model_map$cloud

    validate_model(provider, model)

    disk_session$session$provider <- provider
    disk_session$session$model <- model

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

    set_log_enabled(FALSE)
    on.exit(set_log_enabled(TRUE), add = TRUE)

    # The tools the session sends, composed now, once (R/request-prefix.R).
    session_request_parts(turn_session)
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
    retention_line <- NULL
    if (!is.null(resume_note)) {
        retention_line <- sprintf("  %s%s%s\n", color$dim, resume_note,
                                  color$reset)
    }
    # Why nothing was resumed, or where what was resumed comes from.
    directory_line <- ""
    if (!is.null(disk_session$note)) {
        directory_line <- sprintf("  %s%s%s\n", color$dim, disk_session$note,
                                  color$reset)
    }
    cat(corteza_startup_banner(
                               version = as.character(utils::packageVersion("corteza")),
                               model = display_model,
                               provider = provider
        ),
        "\n\n",
        session_line,
        retention_line,
        directory_line,
        "  /guide introduces the settings.\n",
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
        fresh <- session_new(ctx$provider, ctx$model, cwd, surface = "console")
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
    # Has the person answered this call for good, "allow always" for its
    # tool in this session? The credential rule's ask, and leaving plan
    # mode, are answered each time: an "always" given for the tool does
    # not answer them, and their prompt offers none. The reviewer
    # (R/review.R) asks this first, so it never rules on an answer the
    # person gave.
    answered <- function(call, decision) {
        !.person_rule(decision) &&
        isTRUE(approved[[call$tool %||% ""]])
    }

    cb <- function(call, decision) {
        key <- call$tool %||% ""
        once_only <- .person_rule(decision)
        if (answered(call, decision)) {
            return(TRUE)
        }

        if (once_only) {
            persistent_label <- NULL
        } else {
            persistent_label <- "Allow always for this session"
        }
        lines <- cli_approval_lines(call, decision,
                                    gate_reason = decision$gate_reason,
                                    cwd = cwd,
                                    persistent_label = persistent_label,
                                    deny_label = .console_deny_label())
        cat(paste(lines, collapse = "\n"), "\n")

        ans <- read_prompt_input("Choice: ")
        if (length(ans) == 0L) {
            ans <- ""
        }
        ans <- trimws(ans)
        if (ans == "" || (once_only && ans == "2")) {
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
    attr(cb, "answered") <- answered
    # A job's prompt keeps "always" the same way, for this session
    # (R/repl-jobs.R), and prints what this returns.
    attr(cb, "always") <- function(call) {
        name <- call$tool %||% ""
        approved[[name]] <- TRUE
        sprintf("%s is allowed always in this session.", name)
    }
    cb
}

# Resolve the on-disk session, returning list(session, sessionId, history,
# resumed, note). `session = TRUE` resumes the latest session started in
# `cwd`; a key resumes that session wherever it was started. `note` is a
# line for the person, or NULL: that this directory has no session, or
# that the one resumed was started somewhere else.
resolve_disk_session <- function(session_arg, provider, model, cwd) {
    if (is.character(session_arg)) {
        resumed <- session_load(session_arg)
        if (!is.null(resumed)) {
            return(list(session = resumed, sessionId = resumed$sessionId,
                        history = disk_messages_to_history(resumed$messages),
                        resumed = TRUE,
                        note = session_elsewhere_note(resumed, cwd)))
        }
        fresh <- session_new(provider, model, cwd, session_key = session_arg,
                             surface = "console")
        return(list(session = fresh, sessionId = fresh$sessionId,
                    history = list(), resumed = FALSE))
    }
    note <- NULL
    if (isTRUE(session_arg)) {
        latest <- session_latest(cwd = cwd)
        if (!is.null(latest)) {
            return(list(
                        session = latest,
                        sessionId = latest$sessionId,
                        history = disk_messages_to_history(latest$messages),
                        resumed = TRUE
                ))
        }
        note <- session_none_note(cwd, how = "/sessions all")
    }
    fresh <- session_new(provider, model, cwd, surface = "console")
    list(session = fresh, sessionId = fresh$sessionId,
         history = list(), resumed = FALSE, note = note)
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
                           turn = event$turn_number,
                           auto_run_id = session$auto_run_id,
                           call_id = event$call$call_id,
                           turn_id = event$call$turn_id),
                 error = function(e) NULL
        )
    }
}
