# Interactive chat inside an R session

#' Start Interactive Chat
#'
#' Run a conversational agent inside your R session. Tools execute as direct
#' function calls, no MCP server needed.
#'
#' @param provider LLM provider: "anthropic", "openai", or "ollama".
#'   Defaults to config value or "anthropic".
#' @param model Model name. Defaults to config value or provider default.
#' @param tools Character vector of tool names or categories to enable.
#'   Categories: file, code, r, data, web, git, chat, memory.
#'   Use "core" for file+code+git, "all" for everything (default).
#' @param session Session resume control. NULL (default) starts fresh,
#'   TRUE resumes the latest session, or a character session key to
#'   resume a specific session.
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
chat <- function(provider = NULL, model = NULL, tools = NULL, session = NULL) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        stop("llm.api package required for chat(). ",
             "Install with: install.packages('llm.api')",
             call. = FALSE)
    }
    if (!interactive()) {
        stop("chat() requires an interactive R session", call. = FALSE)
    }

    # Load config
    cwd <- getwd()
    config <- load_config(cwd)
    provider <- provider %||% config$provider %||% "anthropic"
    model <- model %||% config$model

    # Check API key before entering the loop
    key_var <- switch(provider,
                      anthropic = "ANTHROPIC_API_KEY",
                      openai = "OPENAI_API_KEY",
                      NULL
    )
    if (!is.null(key_var) && nchar(Sys.getenv(key_var, "")) == 0) {
        stop(sprintf("%s not set. Add it to ~/.Renviron", key_var),
             call. = FALSE)
    }

    # Register skills (same as serve())
    ensure_skills()
    load_skills(path.expand("~/.llamar/skills"))
    load_skills(file.path(cwd, ".llamar", "skills"))
    load_skill_docs(path.expand("~/.llamar/skills"))
    load_skill_docs(file.path(cwd, ".llamar", "skills"))

    # Load skill packages from config
    load_skill_packages(config)

    options(llamar.tools = tools)

    # Load context + build tool list
    system_prompt <- load_context(cwd)
    api_tools <- skills_as_api_tools(tools)
    tools_json <- tryCatch(
                           jsonlite::toJSON(api_tools, auto_unbox = TRUE),
                           error = function(e) ""
    )

    # Display model
    display_model <- model %||% switch(provider,
                                       anthropic = "claude-sonnet-4-20250514",
                                       openai = "gpt-4o",
                                       ollama = "llama3.2",
                                       "(default)"
    )

    # Workspace config
    ws_enabled <- isTRUE(config$workspace$enabled %||% TRUE)

    # Session resume/create + workspace init
    session_arg <- session
    if (is.character(session_arg)) {
        # Resume by session key
        resumed <- session_load(session_arg)
        if (!is.null(resumed)) {
            ws_load(resumed$sessionId)
            session <- resumed
            history <- lapply(resumed$messages, function(m) {
                text <- if (is.list(m$content) && length(m$content) > 0 &&
                              !is.null(m$content[[1]]$text)) {
                    m$content[[1]]$text
                } else {
                    as.character(m$content)
                }
                list(role = m$role, content = text)
            })
            cat(sprintf("Resumed session (%d messages)\n",
                        length(resumed$messages)))
        } else {
            ws_clear()
            session <- session_new(provider, model, cwd,
                                   session_key = session_arg)
            history <- list()
        }
    } else if (isTRUE(session_arg)) {
        # Resume latest session
        latest <- session_latest()
        if (!is.null(latest)) {
            ws_load(latest$sessionId)
            session <- latest
            history <- lapply(latest$messages, function(m) {
                text <- if (is.list(m$content) && length(m$content) > 0 &&
                              !is.null(m$content[[1]]$text)) {
                    m$content[[1]]$text
                } else {
                    as.character(m$content)
                }
                list(role = m$role, content = text)
            })
            cat(sprintf("Resumed latest session (%d messages)\n",
                        length(latest$messages)))
        } else {
            ws_clear()
            session <- session_new(provider, model, cwd)
            history <- list()
        }
    } else {
        # New session
        ws_clear()
        session <- session_new(provider, model, cwd)
        history <- list()

        # Scan globalenv for existing objects
        if (ws_enabled && isTRUE(config$workspace$scan_globalenv %||% TRUE)) {
            scan_limit <- config$workspace$scan_max_bytes %||% 50e6
            registered <- ws_scan_globalenv(max_bytes = scan_limit)
            if (length(registered) > 0) {
                cat(sprintf("Workspace: registered %d objects from R session\n",
                            length(registered)))
            }
        }
    }

    # Initialize context engine
    ce_init(cwd, config)

    # If resuming, rebuild conversation index from history
    if (length(history) > 0) {
        for (i in seq_along(history)) {
            ce_index_turn(i, history[[i]]$role, history[[i]]$content %||% "")
        }
    }

    # Tool handler - direct function calls, no MCP
    turn_number <- length(history)
    tool_handler <- function(name, args) {
        turn_number <<- turn_number + 1L
        ws_set_turn(turn_number)
        name <- unsanitize_tool_name(name)
        cat(sprintf("  [%s] ", name))
        start <- Sys.time()
        result <- call_tool(name, args %||% list())
        text <- result$content[[1]]$text
        elapsed <- as.numeric(
                              difftime(Sys.time(), start, units = "secs")) * 1000
        lines <- length(strsplit(text, "\n")[[1]])
        cat(sprintf("(%d lines)\n", lines))
        tryCatch(
                 trace_add(session$sessionId, name, args, text,
                           success = TRUE, elapsed_ms = round(elapsed),
                           turn = turn_number),
                 error = function(e) NULL
        )

        # Update file index if a file was written
        if (name %in% c("base::writeLines", "write_file")) {
            written_path <- args$con %||% args$path %||% args$file
            if (!is.null(written_path)) {
                ce_update_files(written_path)
            }
        }

        text
    }

    # Suppress structured JSON logs (they're for MCP server, not interactive use)
    set_log_enabled(FALSE)
    on.exit({
        set_log_enabled(TRUE)
        ce_shutdown()
    })

    # REPL
    n_tools <- length(api_tools)
    file_stats <- ce_file_stats()
    cat(sprintf(
                "llamaR chat | %s @ %s | %d tools | %d files indexed | /quit to exit\n\n",
                display_model, provider, n_tools, file_stats[["files"]]
        ))

    while (TRUE) {
        prompt <- readline("> ")
        if (nchar(trimws(prompt)) == 0) {
            next
        }
        if (trimws(prompt) %in% c("/quit", "/exit", "/q")) {
            if (ws_enabled) {
                ws_prune()
                tryCatch(ws_save(session$sessionId), error = function(e) NULL)
            }
            cat("Bye.\n")
            break
        }

        # /r <code> - eval R code directly (auto-prints like the R REPL)
        if (startsWith(trimws(prompt), "/r ")) {
            code <- sub("^/r\\s+", "", trimws(prompt))
            tryCatch({
                result <- withVisible(eval(parse(text = code),
                        envir = .GlobalEnv))
                if (result$visible) print(result$value)
            }, error = function(e) message("Error: ", e$message))
            next
        }

        transcript_append(session, "user", prompt)

        # Poll pre-computed context
        ce_poll()

        # Index user turn
        turn_number <- turn_number + 1L
        ws_set_turn(turn_number)
        ce_index_turn(turn_number, "user", prompt)

        # Compute context payload (uses context engine)
        payload <- ce_rerank(prompt, system_prompt, tools_json)

        result <- tryCatch(
                           llm.api::agent(
                prompt = prompt,
                tools = api_tools,
                tool_handler = tool_handler,
                system = payload$system,
                model = model,
                provider = provider,
                history = history,
                verbose = FALSE
            ),
                           error = function(e) {
            message("Error: ", e$message)
            NULL
        }
        )

        if (is.null(result)) {
            next
        }

        cat(result$content, "\n\n")
        transcript_append(session, "assistant", result$content)
        history <- result$history

        # Index assistant turn + extract metadata
        tool_calls <- ce_extract_tool_calls(result)
        files_touched <- ce_extract_files_touched(result)
        ce_index_turn(turn_number, "assistant", result$content,
                      tool_calls = tool_calls,
                      files_touched = files_touched)

        # Update symbols if files changed
        if (length(files_touched) > 0) {
            ce_update_symbols(.context_engine$cwd %||% cwd)
        }

        # Pre-compute next context (async if callr available)
        if (ce_should_precompute(result$content)) {
            ce_precompute(system_prompt, tools_json)
        }
    }

    invisible(session)
}

