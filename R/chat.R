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
chat <- function(provider = NULL, model = NULL, tools = NULL) {
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
    options(llamar.tools = tools)

    # Load context + build tool list
    system_prompt <- load_context(cwd)
    api_tools <- skills_as_api_tools(tools)

    # Create session
    session <- session_new(provider, model, cwd)

    # Display model
    display_model <- model %||% switch(provider,
                                       anthropic = "claude-sonnet-4-20250514",
                                       openai = "gpt-4o",
                                       ollama = "llama3.2",
                                       "(default)"
    )

    # Tool handler - direct function calls, no MCP
    turn_number <- 0L
    tool_handler <- function(name, args) {
        turn_number <<- turn_number + 1L
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
        text
    }

    # Suppress structured JSON logs (they're for MCP server, not interactive use)
    set_log_enabled(FALSE)
    on.exit(set_log_enabled(TRUE))

    # REPL
    history <- list()
    n_tools <- length(api_tools)
    cat(sprintf("llamaR chat | %s @ %s | %d tools | /r to eval R | /quit to exit\n\n",
                display_model, provider, n_tools))

    while (TRUE) {
        prompt <- readline("> ")
        if (nchar(trimws(prompt)) == 0) {
            next
        }
        if (trimws(prompt) %in% c("/quit", "/exit", "/q")) {
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

        result <- tryCatch(
                           llm.api::agent(
                prompt = prompt,
                tools = api_tools,
                tool_handler = tool_handler,
                system = system_prompt,
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
    }

    invisible(session)
}

