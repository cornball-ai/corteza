# Configuration management for llamaR
# Handles global and project-level config

#' Default context files to load
#' @noRd
default_context_files <- function() {
    c("README.md", "PLAN.md", "fyi.md", "AGENTS.md", "MEMORY.md")
}

#' Global context files loaded from ~/.llamar/workspace/
#' @noRd
global_context_files <- function() {
    c("SOUL.md", "USER.md", "MEMORY.md")
}

#' Get workspace directory path
#' @noRd
get_workspace_dir <- function() {
    path.expand("~/.llamar/workspace")
}

#' Load configuration from JSON file
#'
#' @param path Path to config file
#' @return List with config, or empty list if not found
#' @noRd
load_config_file <- function(path) {
    if (!file.exists(path)) {
        return(list())
    }

    tryCatch({
        cfg <- jsonlite::fromJSON(path, simplifyVector = TRUE)
        # Ensure context_files is a character vector
        if (!is.null(cfg$context_files) && is.list(cfg$context_files)) {
            cfg$context_files <- unlist(cfg$context_files)
        }
        cfg
    }, error = function(e) {
        list()
    })
}

#' Load merged configuration (global + project)
#'
#' Merges global config (~/.llamar/config.json) with project config
#' (.llamar/config.json). Project config takes precedence.
#'
#' @param cwd Working directory for project config
#' @return List with merged configuration
#' @noRd
load_config <- function(cwd = getwd()) {
    # Global config
    global_path <- path.expand("~/.llamar/config.json")
    global <- load_config_file(global_path)

    # Project config

    project_path <- file.path(cwd, ".llamar", "config.json")
    project <- load_config_file(project_path)

    # Merge (project overrides global)
    config <- global
    for (name in names(project)) {
        config[[name]] <- project[[name]]
    }

    # Apply defaults
    if (is.null(config$context_files)) {
        config$context_files <- default_context_files()
    }
    if (is.null(config$provider)) {
        config$provider <- "anthropic"
    }
    # Context warning thresholds (percentage)
    # Hidden until warn_pct, then yellow -> orange -> red
    if (is.null(config$context_warn_pct)) {
        config$context_warn_pct <- 75L
    }
    if (is.null(config$context_high_pct)) {
        config$context_high_pct <- 90L
    }
    if (is.null(config$context_crit_pct)) {
        config$context_crit_pct <- 95L
    }
    # Auto-compaction threshold (percentage)
    if (is.null(config$context_compact_pct)) {
        config$context_compact_pct <- 80L
    }

    # Memory flush before compaction
    if (is.null(config$memory_flush_enabled)) {
        config$memory_flush_enabled <- TRUE
    }
    if (is.null(config$memory_flush_prompt)) {
        config$memory_flush_prompt <- paste0(
            "Pre-compaction memory flush. ",
            "Store durable memories now using write_file to memory/YYYY-MM-DD.md ",
            "in the workspace. Include: preferences discovered, decisions made, ",
            "technical details worth preserving. ",
            "If nothing to store, reply with exactly: NO_REPLY")
    }

    # Tool approval settings
    if (is.null(config$approval_mode)) {
        config$approval_mode <- "ask" # "ask", "allow", "deny"
    }
    if (is.null(config$dangerous_tools)) {
        config$dangerous_tools <- c("bash", "run_r", "run_r_script",
                                    "base::writeLines")
    }
    # Per-tool permissions (overrides dangerous_tools)
    # config$permissions = list(bash = "deny", read_file = "allow")
    if (is.null(config$permissions)) {
        config$permissions <- list()
    }

    # Filesystem sandboxing
    # config$allowed_paths - if set, only these paths are accessible
    # config$denied_paths - these paths are always blocked
    if (is.null(config$denied_paths)) {
        config$denied_paths <- c(
                                 "~/.ssh",
                                 "~/.gnupg",
                                 "~/.aws",
                                 "~/.config/gcloud",
                                 "~/.kube",
                                 "~/.docker"
        )
    }
    # Note: allowed_paths is NULL by default (no restriction)

    # Skill paths (additional directories to load skills from)
    if (is.null(config$skill_paths)) {
        config$skill_paths <- character()
    }

    # Default skill packages (R packages registered as tools)
    if (is.null(config$skill_packages)) {
        config$skill_packages <- list(
                                      list(package = "base", functions = c("readLines", "writeLines",
                    "list.files", "file.exists", "file.copy", "file.remove",
                    "dir.create", "Sys.glob")),
                                      list(package = "utils", functions = c("installed.packages",
                    "read.csv", "head", "tail"))
        )
    }

    # Default timeout for skill execution (seconds)
    if (is.null(config$skill_timeout)) {
        config$skill_timeout <- 30L
    }

    # Dry-run mode (validate tools without executing)
    if (is.null(config$dry_run)) {
        config$dry_run <- FALSE
    }

    # Rate limits per provider
    # Example: { "anthropic": { "tokens_per_hour": 100000, "requests_per_minute": 60 } }
    if (is.null(config$rate_limits)) {
        config$rate_limits <- list()
    }

    # Subagent configuration
    if (is.null(config$subagents)) {
        config$subagents <- list()
    }
    sub <- config$subagents
    if (is.null(sub$enabled)) {
        sub$enabled <- TRUE
    }
    if (is.null(sub$max_concurrent)) {
        sub$max_concurrent <- 3L
    }
    if (is.null(sub$timeout_minutes)) {
        sub$timeout_minutes <- 30L
    }
    if (is.null(sub$allow_nested)) {
        sub$allow_nested <- FALSE
    }
    if (is.null(sub$default_tools)) {
        sub$default_tools <- c("base::readLines", "base::writeLines",
                               "bash", "grep_files")
    }
    if (is.null(sub$base_port)) {
        sub$base_port <- 7851L
    }
    config$subagents <- sub

    # Voice mode config
    if (is.null(config$voice)) {
        config$voice <- list()
    }
    voice <- config$voice
    if (is.null(voice$enabled)) {
        voice$enabled <- FALSE
    }
    # TTS config
    if (is.null(voice$tts)) {
        voice$tts <- list()
    }
    if (is.null(voice$tts$backend)) {
        voice$tts$backend <- "qwen3" # qwen3, chatterbox, openai, elevenlabs
    }
    if (is.null(voice$tts$voice)) {
        voice$tts$voice <- "default"
    }
    if (is.null(voice$tts$port)) {
        voice$tts$port <- 7812L # qwen3-tts-api default port
    }
    # STT config
    if (is.null(voice$stt)) {
        voice$stt <- list()
    }
    if (is.null(voice$stt$backend)) {
        voice$stt$backend <- "whisper" # whisper (native), api
    }
    if (is.null(voice$stt$port)) {
        voice$stt$port <- 4123L # only used for api backend
    }
    if (is.null(voice$stt$model)) {
        voice$stt$model <- "base" # whisper model: tiny, base, small, medium, large
    }
    # Audio config
    if (is.null(voice$audio)) {
        voice$audio <- list()
    }
    if (is.null(voice$audio$input_device)) {
        voice$audio$input_device <- NULL # Use default device
    }
    if (is.null(voice$audio$sample_rate)) {
        voice$audio$sample_rate <- 16000L
    }
    if (is.null(voice$audio$format)) {
        voice$audio$format <- "wav"
    }
    config$voice <- voice

    # Workspace config (managed runtime state)
    if (is.null(config$workspace)) {
        config$workspace <- list()
    }
    ws <- config$workspace
    if (is.null(ws$enabled)) {
        ws$enabled <- TRUE
    }
    if (is.null(ws$budget_chars)) {
        ws$budget_chars <- 8000L
    }
    if (is.null(ws$capture_results)) {
        ws$capture_results <- TRUE
    }
    if (is.null(ws$max_result_size)) {
        ws$max_result_size <- 50000L
    }
    config$workspace <- ws

    # Channels config (matches openclaw structure)
    if (is.null(config$channels)) {
        config$channels <- list()
    }

    # Signal channel config (channels.signal.*)
    if (is.null(config$channels$signal)) {
        config$channels$signal <- list()
    }
    sig <- config$channels$signal
    if (is.null(sig$enabled)) {
        sig$enabled <- FALSE
    }
    if (is.null(sig$httpHost)) {
        sig$httpHost <- "127.0.0.1"
    }
    if (is.null(sig$httpPort)) {
        sig$httpPort <- 8080L
    }
    # sig$httpUrl - optional, overrides httpHost/httpPort
    # sig$account - required, no default
    # sig$allowFrom - optional allowlist (E.164 numbers)
    # sig$cliPath - optional path to signal-cli
    # Chunking config (matches openclaw)
    if (is.null(sig$textChunkLimit)) {
        sig$textChunkLimit <- 4000L
    }
    if (is.null(sig$chunkMode)) {
        sig$chunkMode <- "length" # "length" or "newline"
    }
    config$channels$signal <- sig

    config
}

#' Get context files from config
#'
#' @param cwd Working directory
#' @return Character vector of context file names to look for
#' @noRd
get_context_files <- function(cwd = getwd()) {
    config <- load_config(cwd)
    config$context_files
}

