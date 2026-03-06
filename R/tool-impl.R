# MCP Tool Implementations
# Actual implementations of tools exposed by the MCP server

# File operations ----

tool_read_file <- function(args) {
    path <- path.expand(args$path)
    if (!file.exists(path)) {
        return(err(paste("File not found:", path)))
    }

    lines <- readLines(path, warn = FALSE)
    if (!is.null(args$lines)) {
        lines <- head(lines, args$lines)
    }
    ok(paste(lines, collapse = "\n"))
}

tool_write_file <- function(args) {
    path <- path.expand(args$path)
    content <- args$content

    tryCatch({
        writeLines(content, path)
        ok(paste("Written", nchar(content), "chars to", path))
    }, error = function(e) {
        err(paste("Write failed:", e$message))
    })
}

tool_list_files <- function(args) {
    path <- path.expand(args$path %||% ".")
    pattern <- args$pattern
    recursive <- isTRUE(args$recursive)

    if (!dir.exists(path)) {
        return(err(paste("Directory not found:", path)))
    }

    if (!is.null(pattern)) {
        if (recursive) {
            files <- Sys.glob(file.path(path, "**", pattern))
        } else {
            files <- Sys.glob(file.path(path, pattern))
        }
    } else {
        files <- list.files(path, full.names = TRUE, recursive = recursive)
    }

    if (length(files) == 0) {
        return(ok("No files found"))
    }
    ok(paste(files, collapse = "\n"))
}

tool_grep_files <- function(args) {
    pattern <- args$pattern
    path <- path.expand(args$path %||% ".")
    file_pattern <- args$file_pattern %||% "*.R"

    files <- Sys.glob(file.path(path, file_pattern))
    if (length(files) == 0) {
        return(ok("No files to search"))
    }

    results <- character()
    for (f in files) {
        lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
        if (is.null(lines)) {
            next
        }

        hits <- grep(pattern, lines)
        if (length(hits) > 0) {
            for (i in hits) {
                results <- c(results, sprintf("%s:%d: %s", f, i, lines[i]))
            }
        }
    }

    if (length(results) == 0) {
        return(ok("No matches found"))
    }
    ok(paste(results, collapse = "\n"))
}

# Code execution ----

tool_run_r <- function(args) {
    code <- args$code
    result <- tryCatch({
        out <- capture.output(eval(parse(text = code), envir = globalenv()))
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })
    ok(result)
}

tool_run_r_script <- function(args) {
    code <- args$code
    timeout <- args$timeout %||% 30L

    # Write code to temp file (avoids shell escaping issues)
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp))
    writeLines(code, tmp)

    result <- tryCatch({
        out <- system2("r", c("-f", tmp), stdout = TRUE, stderr = TRUE,
                       timeout = timeout)
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })
    ok(result)
}

tool_bash <- function(args) {
    cmd <- args$command
    timeout <- args$timeout %||% 30

    result <- tryCatch({
        out <- system(cmd, intern = TRUE, timeout = timeout)
        paste(out, collapse = "\n")
    }, error = function(e) {
        paste("Error:", e$message)
    })
    ok(result)
}

# R-specific ----

tool_r_help <- function(args) {
    topic <- args$topic
    pkg <- args$package

    # Use fyi package for documentation (generates fyi.md-style output)
    if (!requireNamespace("fyi", quietly = TRUE)) {
        return(err("fyi package not installed. Install with: install.packages('fyi')"))
    }

    tryCatch({
        # If topic looks like a package name, get full package info
        if (is.null(pkg) && topic %in% rownames(installed.packages())) {
            out <- capture.output(fyi::fyi(topic))
            ok(paste(out, collapse = "\n"))
        } else {
            # For functions, try to find the package and get info
            pkg_name <- pkg %||% tryCatch({
                # Find which package contains this function
                envs <- search()
                for (e in envs) {
                    if (exists(topic, where = e, mode = "function")) {
                        sub("^package:", "", e)
                    }
                }
                NULL
            }, error = function(e) NULL)

            if (!is.null(pkg_name) && pkg_name != ".GlobalEnv") {
                out <- capture.output(fyi::fyi(pkg_name))
                ok(paste(out, collapse = "\n"))
            } else {
                err(paste("Could not find package for:", topic))
            }
        }
    }, error = function(e) {
        err(paste("Help error:", e$message))
    })
}

tool_installed_packages <- function(args) {
    pattern <- args$pattern

    pkgs <- rownames(installed.packages())
    if (!is.null(pattern)) {
        pkgs <- grep(pattern, pkgs, value = TRUE, ignore.case = TRUE)
    }

    if (length(pkgs) == 0) {
        return(ok("No packages found"))
    }
    ok(paste(sort(pkgs), collapse = "\n"))
}

# Data ----

tool_read_csv <- function(args) {
    path <- path.expand(args$path)
    n_head <- args$head %||% 10
    show_summary <- args$summary %||% TRUE

    if (!file.exists(path)) {
        return(err(paste("File not found:", path)))
    }

    tryCatch({
        df <- read.csv(path)
        parts <- character()

        # Dimensions
        parts <- c(parts,
                   sprintf("Dimensions: %d rows x %d columns", nrow(df), ncol(df)))
        parts <- c(parts,
                   sprintf("Columns: %s", paste(names(df), collapse = ", ")))
        parts <- c(parts, "")

        # Summary
        if (show_summary) {
            parts <- c(parts, "Summary:", capture.output(summary(df)), "")
        }

        # Head
        parts <- c(parts, sprintf("First %d rows:", min(n_head, nrow(df))))
        parts <- c(parts, capture.output(print(head(df, n_head))))

        ok(paste(parts, collapse = "\n"))
    }, error = function(e) {
        err(paste("CSV read error:", e$message))
    })
}

# Web ----

tool_web_search <- function(args) {
    query <- args$query
    max_results <- args$max_results %||% 5L

    api_key <- Sys.getenv("TAVILY_API_KEY")
    if (nchar(api_key) == 0) {
        return(err("TAVILY_API_KEY not set in .Renviron"))
    }

    tryCatch({
        body <- list(
                     api_key = api_key,
                     query = query,
                     max_results = max_results,
                     include_answer = TRUE
        )

        h <- curl::new_handle()
        curl::handle_setopt(h,
                            customrequest = "POST",
                            postfields = jsonlite::toJSON(body, auto_unbox = TRUE)
        )
        curl::handle_setheaders(h, "Content-Type" = "application/json")

        resp <- curl::curl_fetch_memory("https://api.tavily.com/search",
                                        handle = h)

        if (resp$status_code >= 400) {
            return(err(paste("Tavily API error:", resp$status_code)))
        }

        data <- jsonlite::fromJSON(rawToChar(resp$content),
                                   simplifyVector = FALSE)

        # Format results
        parts <- character()

        # Include AI-generated answer if available
        if (!is.null(data$answer) && nchar(data$answer) > 0) {
            parts <- c(parts, "Answer:", data$answer, "")
        }

        parts <- c(parts, "Results:")
        for (r in data$results) {
            parts <- c(parts, sprintf("- %s", r$title))
            parts <- c(parts, sprintf("  %s", r$url))
            if (!is.null(r$content)) {
                snippet <- substr(r$content, 1, 200)
                if (nchar(r$content) > 200) snippet <- paste0(snippet, "...")
                parts <- c(parts, sprintf("  %s", snippet))
            }
            parts <- c(parts, "")
        }

        ok(paste(parts, collapse = "\n"))
    }, error = function(e) {
        err(paste("Search error:", e$message))
    })
}

tool_fetch_url <- function(args) {
    url <- args$url
    method <- toupper(args$method %||% "GET")

    tryCatch({
        con <- url(url, method = method)
        on.exit(close(con))
        content <- paste(readLines(con, warn = FALSE), collapse = "\n")
        ok(content)
    }, error = function(e) {
        err(paste("Fetch error:", e$message))
    })
}

# Git ----

tool_git_status <- function(args) {
    path <- path.expand(args$path %||% ".")
    cmd <- sprintf("git -C %s status --short", shQuote(path))
    result <- tryCatch(
                       system(cmd, intern = TRUE),
                       error = function(e) paste("Error:", e$message)
    )
    if (length(result) == 0) {
        result <- "Working tree clean"
    }
    ok(paste(result, collapse = "\n"))
}

tool_git_diff <- function(args) {
    path <- path.expand(args$path %||% ".")
    if (isTRUE(args$staged)) {
        staged <- "--staged"
    } else {
        staged <- ""
    }
    cmd <- sprintf("git -C %s diff %s", shQuote(path), staged)
    result <- tryCatch(
                       system(cmd, intern = TRUE),
                       error = function(e) paste("Error:", e$message)
    )
    if (length(result) == 0) {
        result <- "No changes"
    }
    ok(paste(result, collapse = "\n"))
}

tool_git_log <- function(args) {
    path <- path.expand(args$path %||% ".")
    n <- args$n %||% 10
    cmd <- sprintf("git -C %s log --oneline -n %d", shQuote(path), n)
    result <- tryCatch(
                       system(cmd, intern = TRUE),
                       error = function(e) paste("Error:", e$message)
    )
    ok(paste(result, collapse = "\n"))
}

# Memory ----

tool_memory_store <- function(args) {
    fact <- args$fact
    scope <- args$scope %||% "project"

    # Use memory module for storage (handles tags, categorization)
    tryCatch({
        memory_store(fact, scope = scope, cwd = getwd())
        clean_fact <- strip_tags(fact)
        tags <- parse_tags(fact)
        tag_str <- if (length(tags) > 0) {
            sprintf(" [%s]", paste0("#", tags, collapse = " "))
        } else {
            ""
        }
        ok(sprintf("Stored%s: %s%s",
                if (scope == "global") " (global)" else "",
                   clean_fact, tag_str))
    }, error = function(e) {
        err(paste("Memory store error:", e$message))
    })
}

tool_memory_recall <- function(args) {
    query <- args$query
    scope <- args$scope %||% "both"

    tryCatch({
        results <- memory_search(query, scope = scope, cwd = getwd())
        if (length(results) == 0) {
            ok(sprintf("No memories found matching: %s", query))
        } else {
            formatted <- format_memory_results(results)
            ok(formatted)
        }
    }, error = function(e) {
        err(paste("Memory search error:", e$message))
    })
}

# Memory file access ----

tool_memory_get <- function(args) {
    path <- args$path
    if (is.null(path) || nchar(trimws(path)) == 0) {
        return(err("path is required"))
    }

    workspace <- get_workspace_dir()

    # Security: only allow MEMORY.md or files under memory/ within workspace
    # Validate relative path first (no traversal)
    if (grepl("\\.\\.", path)) {
        return(err("Access denied: path must be within workspace"))
    }

    # Only allow MEMORY.md or memory/*.md
    if (path != "MEMORY.md" && !grepl("^memory/", path)) {
        return(err("Access denied: only MEMORY.md or memory/*.md files allowed"))
    }

    full_path <- file.path(workspace, path)

    if (!file.exists(full_path)) {
        return(err(paste("File not found:", path)))
    }

    lines <- readLines(full_path, warn = FALSE)

    # Apply line range if specified
    from <- args$from %||% 1L
    if (from < 1) {
        from <- 1L
    }
    if (from > length(lines)) {
        return(ok("(no content at specified line range)"))
    }

    if (!is.null(args$lines)) {
        end <- min(from + args$lines - 1L, length(lines))
    } else {
        end <- length(lines)
    }

    ok(paste(lines[from:end], collapse = "\n"))
}

# Chat (llm.api) ----

tool_chat <- function(args) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        return(err("llm.api not installed. Install with: install.packages('llm.api')"))
    }

    prompt <- args$prompt
    provider <- args$provider %||% "ollama"
    model <- args$model
    system_prompt <- args$system
    temperature <- args$temperature %||% 0.7

    tryCatch({
        result <- llm.api::chat(
                                prompt = prompt,
                                provider = provider,
                                model = model,
                                system = system_prompt,
                                temperature = temperature,
                                stream = FALSE
        )
        ok(result$content)
    }, error = function(e) {
        err(paste("Chat error:", e$message))
    })
}

tool_chat_models <- function(args) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        return(err("llm.api not installed"))
    }

    provider <- args$provider %||% "ollama"

    tryCatch({
        if (provider == "ollama") {
            # Query ollama API for models
            result <- tryCatch({
                con <- url("http://localhost:11434/api/tags")
                on.exit(close(con))
                data <- jsonlite::fromJSON(paste(readLines(con, warn = FALSE),
                        collapse = ""))
                models <- data$models$name
                if (length(models) == 0) "No models found"
                else paste(models, collapse = "\n")
            }, error = function(e) {
                "Ollama not running or no models installed"
            })
            ok(result)
        } else if (provider == "local") {
            models <- llm.api::list_local_models()
            if (length(models) == 0) ok("No local models found")
            else ok(paste(basename(models), collapse = "\n"))
        } else {
            ok(paste("Model listing not supported for provider:", provider))
        }
    }, error = function(e) {
        err(paste("Error listing models:", e$message))
    })
}

# Skill Registration ----

#' Register all built-in skills
#'
#' Creates skill specs for all built-in tools and registers them.
#' Called on package load.
#'
#' @return Invisible character vector of registered skill names
#' @noRd
register_builtin_skills <- function() {
    # File operations
    register_skill(skill_spec(
                              name = "read_file",
                              description = "Read contents of a file",
                              params = list(
                path = list(type = "string",
                            description = "File path to read", required = TRUE),
                lines = list(type = "integer",
                             description = "Max lines to read (default: all)")
            ),
                              handler = function(args, ctx) tool_read_file(args)
        ))

    register_skill(skill_spec(
                              name = "write_file",
                              description = "Write content to a file",
                              params = list(
                path = list(type = "string",
                            description = "File path to write",
                            required = TRUE),
                content = list(type = "string",
                               description = "Content to write",
                               required = TRUE)
            ),
                              handler = function(args, ctx) tool_write_file(args)
        ))

    register_skill(skill_spec(
                              name = "list_files",
                              description = "List files in a directory",
                              params = list(
                path = list(type = "string", description = "Directory path",
                            required = TRUE),
                pattern = list(type = "string",
                               description = "Glob pattern (optional)"),
                recursive = list(type = "boolean",
                                 description = "Search recursively (default: false)")
            ),
                              handler = function(args, ctx) tool_list_files(args)
        ))

    register_skill(skill_spec(
                              name = "grep_files",
                              description = "Search file contents with regex pattern",
                              params = list(
                pattern = list(type = "string",
                               description = "Regex pattern to search",
                               required = TRUE),
                path = list(type = "string",
                            description = "Directory to search (default: .)"),
                file_pattern = list(type = "string",
                                    description = "File glob pattern (default: *.R)")
            ),
                              handler = function(args, ctx) tool_grep_files(args)
        ))

    # Code execution
    register_skill(skill_spec(
                              name = "run_r",
                              description = "Execute R code and return result",
                              params = list(
                code = list(type = "string",
                            description = "R code to execute", required = TRUE)
            ),
                              handler = function(args, ctx) tool_run_r(args)
        ))

    register_skill(skill_spec(
                              name = "run_r_script",
                              description = "Execute R code in a clean subprocess via littler. Use for scripts that modify packages, run tests, or need isolation from the server.",
                              params = list(
                code = list(type = "string",
                            description = "R code to execute", required = TRUE),
                timeout = list(type = "integer",
                               description = "Timeout in seconds (default: 30)")
            ),
                              handler = function(args, ctx) tool_run_r_script(args)
        ))

    register_skill(skill_spec(
                              name = "bash",
                              description = "Run a shell command",
                              params = list(
                command = list(type = "string",
                               description = "Shell command to execute",
                               required = TRUE),
                timeout = list(type = "integer",
                               description = "Timeout in seconds (default: 30)")
            ),
                              handler = function(args, ctx) tool_bash(args)
        ))

    # R-specific
    register_skill(skill_spec(
                              name = "r_help",
                              description = "Get R package documentation using fyi (exports, internals, options)",
                              params = list(
                topic = list(type = "string",
                             description = "Package or function name",
                             required = TRUE),
                package = list(type = "string",
                               description = "Package to search in (optional)")
            ),
                              handler = function(args, ctx) tool_r_help(args)
        ))

    register_skill(skill_spec(
                              name = "installed_packages",
                              description = "List installed R packages, optionally filtered",
                              params = list(
                pattern = list(type = "string",
                               description = "Regex to filter package names")
            ),
                              handler = function(args, ctx) tool_installed_packages(args)
        ))

    # Data
    register_skill(skill_spec(
                              name = "read_csv",
                              description = "Read a CSV file and return summary or head",
                              params = list(
                path = list(type = "string", description = "Path to CSV file",
                            required = TRUE),
                head = list(type = "integer",
                            description = "Number of rows to show (default: 10)"),
                summary = list(type = "boolean",
                               description = "Include summary statistics (default: true)")
            ),
                              handler = function(args, ctx) tool_read_csv(args)
        ))

    # Web
    register_skill(skill_spec(
                              name = "web_search",
                              description = "Search the web using Tavily API",
                              params = list(
                query = list(type = "string", description = "Search query",
                             required = TRUE),
                max_results = list(type = "integer",
                                   description = "Max results to return (default: 5)")
            ),
                              handler = function(args, ctx) tool_web_search(args)
        ))

    register_skill(skill_spec(
                              name = "fetch_url",
                              description = "Fetch content from a URL",
                              params = list(
                url = list(type = "string", description = "URL to fetch",
                           required = TRUE),
                method = list(type = "string",
                              description = "HTTP method (default: GET)")
            ),
                              handler = function(args, ctx) tool_fetch_url(args)
        ))

    # Git
    register_skill(skill_spec(
                              name = "git_status",
                              description = "Get git repository status",
                              params = list(
                path = list(type = "string",
                            description = "Repository path (default: .)")
            ),
                              handler = function(args, ctx) tool_git_status(args)
        ))

    register_skill(skill_spec(
                              name = "git_diff",
                              description = "Show git diff",
                              params = list(
                path = list(type = "string",
                            description = "Repository path (default: .)"),
                staged = list(type = "boolean",
                              description = "Show staged changes only")
            ),
                              handler = function(args, ctx) tool_git_diff(args)
        ))

    register_skill(skill_spec(
                              name = "git_log",
                              description = "Show recent git commits",
                              params = list(
                path = list(type = "string",
                            description = "Repository path (default: .)"),
                n = list(type = "integer",
                         description = "Number of commits (default: 10)")
            ),
                              handler = function(args, ctx) tool_git_log(args)
        ))

    # Chat
    register_skill(skill_spec(
                              name = "chat",
                              description = "Chat with an LLM (requires llm.api). Supports ollama, claude, openai providers.",
                              params = list(
                prompt = list(type = "string",
                              description = "The message to send",
                              required = TRUE),
                provider = list(type = "string",
                                description = "Provider: ollama, claude, openai (default: ollama)"),
                model = list(type = "string",
                             description = "Model name (default: provider-specific)"),
                system = list(type = "string",
                              description = "System prompt (optional)"),
                temperature = list(type = "number",
                                   description = "Temperature 0-1 (default: 0.7)")
            ),
                              handler = function(args, ctx) tool_chat(args)
        ))

    register_skill(skill_spec(
                              name = "chat_models",
                              description = "List available models for chat",
                              params = list(
                provider = list(type = "string",
                                description = "Provider to list models for (default: ollama)")
            ),
                              handler = function(args, ctx) tool_chat_models(args)
        ))

    # Memory
    register_skill(skill_spec(
                              name = "memory_store",
                              description = "Store a fact or preference for future sessions. Use for user preferences, project conventions, or important context worth remembering.",
                              params = list(
                fact = list(type = "string",
                            description = "The fact or preference to remember",
                            required = TRUE),
                scope = list(type = "string", description = "project = this directory only, global = all projects",
                             enum = list("project", "global"))
            ),
                              handler = function(args, ctx) tool_memory_store(args)
        ))

    register_skill(skill_spec(
                              name = "memory_recall",
                              description = "Search memories for facts, preferences, or context. Use to recall user preferences, project conventions, or past decisions.",
                              params = list(
                query = list(type = "string",
                             description = "Search query (keyword or #tag)",
                             required = TRUE),
                scope = list(type = "string", description = "Where to search: project, global, or both",
                             enum = list("both", "project", "global"))
            ),
                              handler = function(args, ctx) tool_memory_recall(args)
        ))

    register_skill(skill_spec(
                              name = "memory_get",
                              description = "Read a memory file (MEMORY.md or memory/*.md) with optional line range",
                              params = list(
                path = list(type = "string",
                            description = "File path relative to workspace (e.g., MEMORY.md or memory/2025-01-15.md)", required = TRUE),
                from = list(type = "integer",
                            description = "Start line (1-based)"),
                lines = list(type = "integer",
                             description = "Number of lines to return")
            ),
                              handler = function(args, ctx) tool_memory_get(args)
        ))

    # Subagent tools
    register_skill(skill_spec(
                              name = "spawn_subagent",
                              description = "Spawn a specialized subagent for a task. Use for parallel work or tasks requiring focused attention.",
                              params = list(
                task = list(type = "string",
                            description = "Task description for the subagent",
                            required = TRUE),
                model = list(type = "string",
                             description = "Optional model override"),
                tools = list(type = "array",
                             items = list(type = "string"),
                             description = "Optional tool filter (list of tool names)")
            ),
                              handler = function(args, ctx) {
        tryCatch({
            id <- subagent_spawn(
                                 task = args$task,
                                 model = args$model,
                                 tools = args$tools,
                                 parent_session = ctx$session
            )
            ok(sprintf("Spawned subagent %s for: %s", id, args$task))
        }, error = function(e) {
            err(paste("Spawn failed:", e$message))
        })
    }
        ))

    register_skill(skill_spec(
                              name = "query_subagent",
                              description = "Send a prompt to a running subagent and get the response.",
                              params = list(
                id = list(type = "string", description = "Subagent ID",
                          required = TRUE),
                prompt = list(type = "string", description = "Prompt to send",
                              required = TRUE)
            ),
                              handler = function(args, ctx) {
        tryCatch({
            result <- subagent_query(args$id, args$prompt)
            ok(result)
        }, error = function(e) {
            err(paste("Query failed:", e$message))
        })
    }
        ))

    register_skill(skill_spec(
                              name = "list_subagents",
                              description = "List all active subagents.",
                              params = list(),
                              handler = function(args, ctx) {
        agents <- subagent_list()
        ok(format_subagent_list(agents))
    }
        ))

    register_skill(skill_spec(
                              name = "kill_subagent",
                              description = "Terminate a running subagent.",
                              params = list(
                id = list(type = "string",
                          description = "Subagent ID to terminate",
                          required = TRUE)
            ),
                              handler = function(args, ctx) {
        success <- subagent_kill(args$id)
        if (success) {
            ok(sprintf("Subagent %s terminated", args$id))
        } else {
            err(sprintf("Subagent not found: %s", args$id))
        }
    }
        ))

    invisible(list_skills())
}

# Dispatcher ----

#' Call a tool by name
#'
#' Delegates to the skill system. Falls back to legacy dispatch if skill not found.
#'
#' @param name Tool name
#' @param args List of arguments
#' @param ctx Optional context (cwd, session, etc.)
#' @param timeout Timeout in seconds (default 30)
#' @param dry_run If TRUE, validate only without executing
#' @return MCP tool result
#' @noRd
call_tool <- function(name, args, ctx = list(), timeout = 30L,
                      dry_run = FALSE) {
    args <- args %||% list()

    # Try skill system first
    skill <- get_skill(name)
    if (!is.null(skill)) {
        return(skill_run(skill, args, ctx, timeout, dry_run))
    }

    # Fallback: unknown tool
    err(paste("Unknown tool:", name))
}

