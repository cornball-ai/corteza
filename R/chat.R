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
    color <- ansi_colors()
    cat(sprintf(
                "%scorteza chat%s | %s%s%s @ %s%s%s | %d tools | %s/help, /quit%s%s\n\n",
                color$cyan, color$reset,
                color$bold, display_model, color$reset,
                color$bold, provider, color$reset,
                n_tools,
                color$dim, color$reset,
            if (resumed_count > 0L) {
                sprintf(" %s| resumed (%d msgs)%s",
                        color$dim, resumed_count, color$reset)
            } else {
                ""
            }
        ))

    # /r evals are buffered here and prepended to the next real user
    # message, so the LLM sees what the user evaluated locally.
    pending_r_context <- character(0)

    while (TRUE) {
        prompt <- read_prompt_input("> ")
        if (length(prompt) == 0L) {
            cat("\nBye.\n")
            break
        }
        if (nchar(trimws(prompt)) == 0) {
            next
        }
        sp <- trimws(prompt)
        # Trailing-backslash continuation: a non-slash line ending
        # with an unescaped `\` drops into paste mode seeded with the
        # line so far. Slash commands are exempt — they have their
        # own arg parsing. `\\` at end = literal trailing backslash.
        # `from_paste` blocks the slash-command dispatcher below from
        # reinterpreting a paste that happens to start with `/`
        # (filenames, code snippets, etc.) as a corteza command.
        from_paste <- FALSE
        if (!startsWith(sp, "/")) {
            cont_seed <- backslash_continuation_seed(prompt)
            if (!is.null(cont_seed)) {
                joined <- read_paste_block(seed = cont_seed,
                                           heredoc = TRUE)
                if (is.null(joined)) {
                    next
                }
                prompt <- joined
                sp <- trimws(prompt)
                from_paste <- TRUE
            }
        }
        if (!from_paste && startsWith(sp, "/")) {
            parts <- strsplit(sp, "\\s+")[[1]]
            cmd <- tolower(parts[1])

            if (cmd %in% c("/quit", "/exit", "/q")) {
                if (ws_enabled) {
                    ws_prune()
                    tryCatch(ws_save(disk_session$sessionId),
                             error = function(e) NULL)
                }
                cat(sprintf("%sBye.%s\n", color$dim, color$reset))
                break
            }
            if (cmd %in% c("/clear", "/reset", "/new")) {
                # Archive the current session's workspace so it stays
                # resumable, then spin up a fresh on-disk + in-memory
                # session. The old transcript is left on disk.
                if (ws_enabled) {
                    tryCatch(ws_save(disk_session$sessionId),
                             error = function(e) NULL)
                }
                # Drop the tool-output buffer for the outgoing session
                # so a /clear actually clears (otherwise /last would
                # still surface results from the old conversation).
                tool_buffer_reset(disk_session$session)
                fresh <- session_new(provider, model, cwd)
                disk_session <- list(session = fresh,
                                     sessionId = fresh$sessionId,
                                     resumed = FALSE)
                turn_session$history <- list()
                turn_session$sessionId <- fresh$sessionId
                turn_session$disk_session <- fresh
                # Re-register the tool-buffer observer against the new
                # session so subsequent tool calls land in the fresh
                # buffer.
                turn_session$on_tool <- Filter(function(obs) {
                    !identical(attr(obs, "kind"), "tool_buffer")
                }, turn_session$on_tool %||% list())
                obs <- tool_buffer_observer(fresh)
                attr(obs, "kind") <- "tool_buffer"
                add_observer(turn_session, obs)
                pending_r_context <- character(0)
                cat(sprintf("%sCleared. New session: %s%s\n\n",
                            color$dim, fresh$sessionId, color$reset))
                next
            }
            if (cmd == "/help") {
                cat(chat_help_text())
                next
            }
            if (cmd == "/tools") {
                cat(chat_format_tools_list(turn_session))
                next
            }
            if (cmd == "/model") {
                if (length(parts) < 2L) {
                    cat(sprintf("Current model: %s\nUsage: /model <name>\n",
                                turn_session$model_map$cloud %||% "(default)"))
                    next
                }
                turn_session$model_map$cloud <- parts[2]
                model <- parts[2]
                cat(sprintf("Model set to %s\n", parts[2]))
                next
            }
            if (cmd == "/provider") {
                if (length(parts) < 2L) {
                    cat(sprintf("Current provider: %s\nUsage: /provider <name>\n",
                                turn_session$provider %||% "(default)"))
                    next
                }
                turn_session$provider <- parts[2]
                provider <- parts[2]
                cat(sprintf("Provider set to %s\n", parts[2]))
                next
            }
            if (cmd == "/spawn") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /spawn <task>\n",
                                color$dim, color$reset))
                    cat("       /spawn <task> --model <name>\n")
                    cat("       /spawn <task> --preset investigate|work|minimal\n")
                    cat("       /spawn <task> --tools read_file,grep_files,...\n")
                    next
                }
                args <- parse_spawn_flags(paste(parts[-1], collapse = " "))
                tryCatch({
                    sub_id <- subagent_spawn(
                        task = args$task, model = args$model,
                        tools = args$tools, preset = args$preset,
                        parent_session = turn_session
                    )
                    info <- .subagent_registry[[sub_id]]
                    handle <- if (!is.null(info$seq)) {
                        as.character(info$seq)
                    } else {
                        substr(sub_id, 1L, 8L)
                    }
                    cat(sprintf("%sSpawned subagent [%s]%s (id %s%s%s)\n",
                                color$green, handle, color$reset,
                                color$dim, sub_id, color$reset))
                    cat(sprintf("%sUse /ask %s <prompt> to query%s\n",
                                color$dim, handle, color$reset))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                color$bright_magenta, color$reset, e$message))
                })
                next
            }
            if (cmd == "/agents") {
                cat(format_subagent_list(subagent_list()), "\n")
                next
            }
            if (cmd == "/ask") {
                if (length(parts) < 3L) {
                    cat(sprintf("%sUsage:%s /ask <id-or-seq> <prompt>\n",
                                color$dim, color$reset))
                    next
                }
                sub_id <- parts[2]
                sub_prompt <- paste(parts[3:length(parts)], collapse = " ")
                cat(sprintf("%sQuerying subagent %s...%s\n",
                            color$dim, sub_id, color$reset))
                tryCatch({
                    res <- subagent_query(sub_id, sub_prompt)
                    cat(sprintf("%s%s%s\n", color$cyan, res, color$reset))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                color$bright_magenta, color$reset, e$message))
                })
                next
            }
            if (cmd == "/queue") {
                if (length(parts) < 3L) {
                    cat(sprintf("%sUsage:%s /queue <id-or-seq> <prompt>\n",
                                color$dim, color$reset))
                    next
                }
                sub_id <- parts[2]
                sub_prompt <- paste(parts[3:length(parts)], collapse = " ")
                tryCatch({
                    subagent_query(sub_id, sub_prompt, wait = FALSE)
                    cat(sprintf("%sQueued for subagent %s; collect with /collect %s%s\n",
                                color$dim, sub_id, sub_id, color$reset))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                color$bright_magenta, color$reset, e$message))
                })
                next
            }
            if (cmd == "/collect") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /collect <id-or-seq>\n",
                                color$dim, color$reset))
                    next
                }
                sub_id <- parts[2]
                cat(sprintf("%sCollecting from subagent %s...%s\n",
                            color$dim, sub_id, color$reset))
                tryCatch({
                    res <- subagent_collect(sub_id)
                    if (is.null(res)) {
                        cat(sprintf("%sStill working; try /collect %s again.%s\n",
                                    color$yellow, sub_id, color$reset))
                    } else {
                        cat(sprintf("%s%s%s\n", color$cyan, res, color$reset))
                    }
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                color$bright_magenta, color$reset, e$message))
                })
                next
            }
            if (cmd == "/kill") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /kill <id-or-seq>\n",
                                color$dim, color$reset))
                    next
                }
                ok <- tryCatch(subagent_kill(parts[2]),
                               error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                color$bright_magenta,
                                color$reset, e$message))
                    FALSE
                })
                if (isTRUE(ok)) {
                    cat(sprintf("%sSubagent %s terminated%s\n",
                                color$dim, parts[2], color$reset))
                } else if (isFALSE(ok)) {
                    cat(sprintf("%sSubagent not found: %s%s\n",
                                color$yellow, parts[2], color$reset))
                }
                next
            }
            if (cmd == "/sessions") {
                cat(format_session_list(session_list()), "\n")
                next
            }
            if (cmd == "/trace") {
                if (length(parts) >= 2L) {
                    n <- suppressWarnings(as.integer(parts[2]))
                } else {
                    n <- 20L
                }
                if (is.na(n)) {
                    n <- 20L
                }
                trace <- tryCatch(trace_load(disk_session$session$sessionId, n = n),
                                  error = function(e) list())
                if (length(trace) == 0L) {
                    cat("No tool calls recorded for this session.\n")
                } else {
                    cat(format_trace(trace, show_args = TRUE), "\n")
                }
                next
            }
            if (cmd == "/permissions") {
                cat(format_permissions(config), "\n")
                approvals_path <- file.path(cwd, ".corteza", "approvals.json")
                cat(sprintf("Project approvals: %s\n",
                        if (file.exists(approvals_path)) {
                            approvals_path
                        } else {
                            "none"
                        }))
                next
            }
            if (cmd == "/dryrun") {
                turn_session$config$dry_run <- !isTRUE(turn_session$config$dry_run)
                config$dry_run <- turn_session$config$dry_run
                cat(sprintf("Dry-run mode %s\n",
                        if (isTRUE(turn_session$config$dry_run))
                            "enabled (tools preview only)"
                        else "disabled"))
                next
            }
            if (cmd == "/paste") {
                # /paste [optional text]: read a multi-line block via
                # the shared helper, then fall through to turn(). Mark
                # from_paste so the /r local-eval shortcut below
                # doesn't reinterpret pasted content that happens to
                # start with `/r `.
                rest <- if (length(parts) >= 2L) {
                    paste(parts[-1], collapse = " ")
                } else ""
                joined <- read_paste_block(seed = trimws(rest))
                if (is.null(joined)) {
                    next
                }
                prompt <- joined
                from_paste <- TRUE
                # Fall through to normal prompt handling below.
            } else if (cmd == "/plan") {
                rest <- if (length(parts) >= 2L) {
                    paste(parts[-1], collapse = " ")
                } else ""
                rest <- trimws(rest)
                if (!nzchar(rest)) {
                    turn_session$plan_mode <- !isTRUE(turn_session$plan_mode)
                    cat(sprintf("%sPlan mode %s%s\n",
                                color$dim,
                            if (isTRUE(turn_session$plan_mode))
                                "enabled (reads only; LLM proposes a plan via exit_plan_mode)"
                            else "disabled",
                                color$reset))
                    next
                }
                turn_session$plan_mode <- TRUE
                cat(sprintf("%sPlan mode enabled.%s\n", color$dim, color$reset))
                prompt <- rest
                # Fall through to normal prompt handling below.
            }
            if (cmd %in% c("/context", "/status")) {
                files <- config$context_files %||% character(0)
                tools <- tryCatch(
                                  skills_as_api_tools(turn_session$tools_filter),
                                  error = function(e) list()
                )
                sys_tok <- estimate_text_tokens(turn_session$system %||% "")
                tools_tok <- estimate_tool_tokens(tools)
                hist_tok <- estimate_history_tokens(
                                                    turn_session$history %||% list()
                )
                total_tok <- as.integer(sys_tok + tools_tok + hist_tok)
                disp_model <- model %||% turn_session$model_map$cloud %||%
                    "(default)"
                limit <- context_limit_for_model(disp_model)
                # Codex-style header: corteza version, model, dir,
                # session id. /status is now an alias of /context
                # showing the same block.
                status_info <- list(
                                    corteza = as.character(utils::packageVersion("corteza")),
                                    model = sprintf("%s @ %s", disp_model,
                                                    turn_session$provider %||% provider),
                                    dir = cwd,
                                    session = disk_session$session$sessionKey %||%
                                        disk_session$session$sessionId %||% "(unset)"
                )
                cat(format_context_block(
                                         used = total_tok,
                                         limit = limit,
                                         breakdown = list(system = sys_tok,
                                                          tools = tools_tok,
                                                          history = hist_tok),
                                         compact_pct = config$context_compact_pct %||% 90L,
                                         warn_pct = config$context_warn_pct %||% 75L,
                                         high_pct = config$context_high_pct %||% 90L,
                                         crit_pct = config$context_crit_pct %||% 95L,
                                         files = files,
                                         palette = color,
                                         status_info = status_info
                    ), "\n", sep = "")
                next
            }
            if (cmd == "/compact") {
                # Live conversation state in chat() lives on
                # turn_session$history; disk_session$session$messages
                # only contains what was loaded at startup (or the
                # last compaction marker) because chat() persists via
                # transcript_append, not session_add_message. Wrap
                # the live history in a session-shaped list so the
                # shared do_compact() sees the actual current turns.
                live_messages <- turn_session$history %||% list()
                if (length(live_messages) < 2L) {
                    cat("Nothing to compact.\n")
                    next
                }
                result <- do_compact(list(messages = live_messages),
                                     turn_session$provider,
                                     turn_session$model_map$cloud)
                if (!is.null(result) && nzchar(result$summary)) {
                    turn_session$history <- list(
                        list(role = "assistant", content = result$summary)
                    )
                    transcript_compact(disk_session$session, result$summary)
                    cat("Compacted.\n")
                }
                next
            }
            if (cmd == "/doctor") {
                tools <- skills_as_api_tools(turn_session$tools_filter)
                disp_model <- model %||% turn_session$model_map$cloud %||%
                    "(default)"
                docs <- tryCatch(list_skill_docs(),
                                 error = function(e) character())
                cat(format_doctor_report(
                                         cwd = cwd,
                                         session = disk_session$session,
                                         provider = turn_session$provider %||% provider,
                                         display_model = disp_model,
                                         tools = tools,
                                         config = config,
                                         context_files = config$context_files %||% character(),
                                         skill_docs = docs
                    ), "\n")
                next
            }
            if (cmd == "/config") {
                disp_model <- model %||% turn_session$model_map$cloud %||%
                    "(default)"
                cat(format_config_summary(
                                          config = config,
                                          provider = turn_session$provider %||% provider,
                                          display_model = disp_model,
                                          opts = list(port = config$port,
                                                      tools = turn_session$tools_filter,
                                                      dry_run = isTRUE(turn_session$config$dry_run))
                    ), "\n")
                next
            }
            if (cmd == "/last") {
                n <- if (length(parts) >= 2L) {
                    suppressWarnings(as.integer(parts[2]))
                } else {
                    1L
                }
                if (is.na(n)) n <- 1L
                outputs <- tool_buffer_list(disk_session$session)
                if (length(outputs) == 0L) {
                    cat(sprintf("%sNo tool outputs yet.%s\n",
                                color$dim, color$reset))
                    next
                }
                if (n < 1L || n > length(outputs)) {
                    cat(sprintf("%sInvalid index. Have %d outputs.%s\n",
                                color$yellow, length(outputs), color$reset))
                    next
                }
                entry <- outputs[[n]]
                cat(sprintf("\n%s%s%s @ %s\n",
                            color$cyan, entry$name, color$reset,
                            format(entry$time, "%H:%M:%S")))
                if (length(entry$args) > 0L) {
                    cat(sprintf("%sArgs: %s%s\n",
                                color$dim,
                                jsonlite::toJSON(entry$args, auto_unbox = TRUE),
                                color$reset))
                }
                cat(sprintf("%s%s%s\n", color$dim,
                            strrep("-", 40), color$reset))
                cat(entry$result, "\n")
                next
            }
            if (cmd == "/outputs") {
                outputs <- tool_buffer_list(disk_session$session)
                if (length(outputs) == 0L) {
                    cat(sprintf("%sNo tool outputs yet.%s\n",
                                color$dim, color$reset))
                    next
                }
                cat(sprintf("\n%sRecent tool outputs:%s\n",
                            color$bold, color$reset))
                for (i in seq_along(outputs)) {
                    entry <- outputs[[i]]
                    lines <- length(strsplit(entry$result %||% "", "\n",
                                             fixed = TRUE)[[1]])
                    cat(sprintf("  %s[%d]%s %s%s%s (%d lines) @ %s\n",
                                color$dim, i, color$reset,
                                color$cyan, entry$name, color$reset,
                                lines, format(entry$time, "%H:%M:%S")))
                }
                cat(sprintf("\n%sUse /last [N] to view output%s\n",
                            color$dim, color$reset))
                next
            }
            if (cmd == "/diff") {
                ref <- if (length(parts) >= 2L) parts[2] else NULL
                material <- collect_git_diff(ref)
                if (!isTRUE(material$ok)) {
                    cat(sprintf("%s%s%s\n", color$yellow, material$text,
                                color$reset))
                } else {
                    cat(sprintf("\n%sDiff against %s%s\n",
                                color$cyan, material$target, color$reset))
                    cat(colorize_diff(material$diff), "\n")
                }
                next
            }
            if (cmd == "/review") {
                ref <- if (length(parts) >= 2L) parts[2] else NULL
                material <- collect_git_diff(ref)
                if (!isTRUE(material$ok)) {
                    cat(sprintf("%s%s%s\n", color$yellow, material$text,
                                color$reset))
                    next
                }
                provider_check <- provider_status(
                                                  turn_session$provider %||% provider,
                                                  model
                )
                if (!isTRUE(provider_check$ok)) {
                    cat(sprintf("%sReview unavailable: %s%s\n",
                                color$yellow, provider_check$message,
                                color$reset))
                    next
                }
                cat(sprintf("%sReviewing diff against %s...%s\n",
                            color$dim, material$target, color$reset))
                review_result <- run_review(
                                            turn_session$provider %||% provider,
                                            model, material$target,
                                            material$status, material$diff
                )
                if (inherits(review_result, "error")) {
                    cat(sprintf("%sReview failed: %s%s\n",
                                color$bright_magenta,
                                conditionMessage(review_result),
                                color$reset))
                } else {
                    cat(review_result$content %||% "", "\n")
                }
                next
            }
            # /remember /recall /flush are dead in the CLI too: their
            # implementations rely on memory_store / memory_search /
            # strip_tags / parse_tags helpers that don't exist in the
            # package. Skipping the chat() port to match reality.
            if (cmd %in% c("/skill", "/skills")) {
                if (length(parts) >= 2L) {
                    subcmd <- parts[2]
                } else {
                    subcmd <- "list"
                }
                if (subcmd == "list") {
                    tryCatch({
                        cat(format_skill_list(skill_list_installed()), "\n")
                    }, error = function(e) cat(sprintf("Error: %s\n", e$message)))
                } else if (subcmd == "install" && length(parts) >= 3L) {
                    src <- parts[3]
                    force <- "--force" %in% parts
                    tryCatch({
                        nm <- skill_install(src, force = force)
                        cat(sprintf("Installed skill: %s\n", nm))
                    }, error = function(e) cat(sprintf("Error: %s\n", e$message)))
                } else if (subcmd == "remove" && length(parts) >= 3L) {
                    nm <- parts[3]
                    tryCatch({
                        skill_remove(nm)
                        cat(sprintf("Removed skill: %s\n", nm))
                    }, error = function(e) cat(sprintf("Error: %s\n", e$message)))
                } else if (subcmd == "test" && length(parts) >= 3L) {
                    pth <- parts[3]
                    tryCatch({
                        result <- skill_test(pth)
                        if (result$failed == 0L) {
                            cat(sprintf("%d test(s) passed\n", result$passed))
                        } else {
                            cat(sprintf("%d passed, %d failed\n",
                                        result$passed, result$failed))
                        }
                    }, error = function(e) cat(sprintf("Error: %s\n", e$message)))
                } else {
                    cat("Usage:\n")
                    cat("  /skill list\n")
                    cat("  /skill install <path|url> [--force]\n")
                    cat("  /skill remove <name>\n")
                    cat("  /skill test <path>\n")
                }
                next
            }
            # /r is handled separately below to keep its existing
            # multi-line pending_r_context plumbing. /plan <text> and
            # /paste fall through here too: those branches above
            # rewrote `prompt` to the buffer contents, so we want
            # regular prompt handling instead of an "Unknown command"
            # complaint that would discard the buffer.
            if (!startsWith(sp, "/r ") && cmd != "/plan" && cmd != "/paste") {
                cat(sprintf("%sUnknown command: %s. Type /help for the list.%s\n",
                            color$yellow, cmd, color$reset))
                next
            }
        }
        if (!from_paste && startsWith(trimws(prompt), "/r ")) {
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
            if (nchar(result_text) > 0) {
                cat(result_text, "\n", sep = "")
            }
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

        cat(sprintf("%s\u25cf%s Thinking with %s%s%s\n",
                    color$cyan, color$reset,
                    color$bold, model %||% "(provider default)",
                    color$reset))
        pre_turn_len <- length(turn_session$history %||% list())
        turn_start <- Sys.time()
        result <- tryCatch(
                           turn(prompt, turn_session),
                           interrupt = function(c) {
            cat(sprintf("\n%sInterrupted.%s\n", color$yellow, color$reset))
            # turn() didn't return, so its history update never landed.
            # Stitch the user prompt and an interruption marker into
            # turn_session$history so the next turn's LLM call sees
            # this exchange was aborted instead of silently dropping it.
            marker <- "[Interrupted by user before completing.]"
            turn_session$history <- c(
                                      turn_session$history %||% list(),
                                      list(list(role = "user", content = prompt),
                                           list(role = "assistant", content = marker))
            )
            transcript_append(disk_session$session, "assistant", marker)
            NULL
        },
                           error = function(e) {
            message(sprintf("%sError:%s %s",
                            color$bright_magenta, color$reset, e$message))
            NULL
        }
        )
        if (is.null(result)) {
            # Interrupt or error path. Still print the timing footer
            # so the user sees how long the aborted turn ran.
            cat(turn_footer_line(turn_start, palette = color), "\n",
                sep = "")
            next
        }

        reply <- result$reply %||% ""
        if (nchar(reply) == 0) {
            cat(sprintf("%s[No response text]%s\n\n", color$dim, color$reset))
        } else {
            cat(reply, "\n\n")
        }
        cat(turn_footer_line(turn_start, palette = color), "\n", sep = "")
        transcript_append(disk_session$session, "assistant", reply)

        # Archival hook: opt-in via config$archival$enabled. Mutates
        # turn_session$history in place when triggers fire.
        maybe_archive_turn(
                           turn_session = turn_session, prompt = prompt,
                           pre_turn_len = pre_turn_len, result = result, config = config,
                           parent_session_id = disk_session$session$sessionId,
                           max_turns_hit = isTRUE(grepl("Max turns", reply)),
                           depth = 0L
        )
    }

    invisible(disk_session$session)
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
        lines <- cli_approval_lines(
                                    call,
                                    decision,
                                    cwd = cwd,
                                    persistent_label = persistent_label
        )
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
        cat(sprintf("%s●%s User replied:\n  %s⎿  %s%s\n\n",
                    color$cyan, color$reset,
                    color$dim, summary, color$reset))

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

