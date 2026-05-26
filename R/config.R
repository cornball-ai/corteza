# Configuration management for corteza
# Handles global and project-level config

#' Get workspace directory path
#' @noRd
get_workspace_dir <- function() {
    corteza_data_path("workspace")
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
        # simplifyDataFrame = FALSE keeps arrays of objects as lists of
        # named lists rather than collapsing them into a data.frame.
        # Otherwise [{"package":"x","functions":[...]}] in skill_packages
        # arrives here as a 1-row data.frame and downstream code that
        # iterates with `for (spec in specs)` gets columns instead of
        # rows. simplifyVector still flattens scalar arrays
        # (["fortunes"] -> c("fortunes")), so the string form keeps
        # working.
        cfg <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                                  simplifyDataFrame = FALSE)
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
#' Merges global config from \code{tools::R_user_dir("corteza", "config")}
#' with project config (\code{.corteza/config.json}). Project config
#' takes precedence.
#'
#' @param cwd Working directory for project config
#' @return List with merged configuration
#' @noRd
load_config <- function(cwd = getwd()) {
    global_path <- corteza_config_path("config.json")
    global <- load_config_file(global_path)

    # Project config

    project_path <- file.path(cwd, ".corteza", "config.json")
    project <- load_config_file(project_path)

    # Merge (project overrides global)
    config <- global
    for (name in names(project)) {
        config[[name]] <- project[[name]]
    }

    # Apply defaults
    if (is.null(config$context_files)) {
        config$context_files <- character(0)
    }
    if (is.null(config$provider)) {
        config$provider <- "anthropic"
    }
    if (is.null(config$port)) {
        config$port <- 7850L
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
        config$context_compact_pct <- 90L
    }

    # SOUL.md and USER.md inclusion (passed to saber::agent_context).
    # NULL = use saber's default for the agent (which includes them).
    # FALSE = explicitly skip.
    if (is.null(config$context_include_soul)) {
        config$context_include_soul <- NULL
    }
    if (is.null(config$context_include_user)) {
        config$context_include_user <- NULL
    }
    # Tool approval settings
    if (is.null(config$approval_mode)) {
        config$approval_mode <- "ask" # "ask", "allow", "deny"
    }
    if (is.null(config$dangerous_tools)) {
        config$dangerous_tools <- default_dangerous_tools()
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
        config$denied_paths <- default_denied_paths()
    }
    # Note: allowed_paths is NULL by default (no restriction)

    # Skill paths (additional directories to load skills from)
    if (is.null(config$skill_paths)) {
        config$skill_paths <- character()
    }

    # Default skill packages (R packages registered as tools)
    if (is.null(config$skill_packages)) {
        config$skill_packages <- list(
                                      list(package = "base", functions = c("file.exists", "file.copy",
                    "file.remove", "dir.create", "Sys.glob")),
                                      list(package = "utils", functions = c("read.csv", "head", "tail"))
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

    # Legacy memory tools (slash commands /remember /recall /flush)
    if (is.null(config$legacy_memory_tools_enabled)) {
        config$legacy_memory_tools_enabled <- FALSE
    }
    # Auto-flush memories before context compaction
    if (is.null(config$memory_flush_enabled)) {
        config$memory_flush_enabled <- TRUE
    }
    # Prompt sent to the model during the pre-compaction memory flush.
    if (is.null(config$memory_flush_prompt)) {
        config$memory_flush_prompt <- paste0(
            "Pre-compaction memory flush. ",
            "Store durable memories now using write_file to memory/YYYY-MM-DD.md ",
            "in the workspace. Include: preferences discovered, decisions made, ",
            "technical details worth preserving. ",
            "If nothing to store, reply with exactly: NO_REPLY")
    }
    # Include daily memory logs in context
    if (is.null(config$context_include_memory_logs)) {
        config$context_include_memory_logs <- FALSE
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
    # MCP exposure is opt-in: serve() must not hand subagent tools to an
    # (often unattended) MCP client by default, since a spawned child
    # spends autonomously on the host's credentials. See serve().
    if (is.null(sub$expose_over_mcp)) {
        sub$expose_over_mcp <- FALSE
    }
    # When exposure is on, cap cumulative subagent spend over MCP. USD
    # cap default $5.00; <= 0 disables it. Token cap (for cost-blind
    # providers) is off unless set.
    if (is.null(sub$mcp_spend_cap_usd)) {
        sub$mcp_spend_cap_usd <- 5.00
    }
    if (is.null(sub$mcp_spend_cap_tokens)) {
        sub$mcp_spend_cap_tokens <- NA_integer_
    }
    if (is.null(sub$default_tools)) {
        sub$default_tools <- c("base::readLines", "base::writeLines",
                               "bash", "grep_files")
    }
    if (is.null(sub$base_port)) {
        sub$base_port <- 7851L
    }
    # Child context compaction. Working subagents (not archive holders)
    # may compact their own in-memory history when it grows past the
    # effective threshold. The on-disk transcript is unaffected.
    if (is.null(sub$context_compaction)) {
        sub$context_compaction <- list()
    }
    cc <- sub$context_compaction
    if (is.null(cc$mode)) {
        # inherit_strict: effective threshold = min(parent, child).
        # inherit: use parent's context_compact_pct verbatim.
        # off: never compact.
        cc$mode <- "inherit_strict"
    }
    if (is.null(cc$compact_pct)) {
        cc$compact_pct <- 75L
    }
    if (is.null(cc$keep_recent_turns)) {
        cc$keep_recent_turns <- 1L
    }
    if (is.null(cc$min_messages)) {
        cc$min_messages <- 6L
    }
    if (is.null(cc$timeout_seconds)) {
        cc$timeout_seconds <- 60L
    }
    sub$context_compaction <- cc
    config$subagents <- sub

    # Archival (retroactive-extraction) configuration. Default off so
    # CRAN users see no behavior change. When enabled, finished turns
    # collapse into subagents that hold the full transcript; the parent
    # context keeps {summary, subagent_id} and the live-subagents block
    # in the system prompt lets the LLM pick query_subagent or
    # spawn_subagent as a normal tool decision.
    if (is.null(config$archival)) {
        config$archival <- list()
    }
    arc <- config$archival
    if (is.null(arc$enabled)) {
        arc$enabled <- FALSE
    }
    if (is.null(arc$trigger)) {
        arc$trigger <- list()
    }
    if (is.null(arc$trigger$on_max_turns)) {
        arc$trigger$on_max_turns <- TRUE
    }
    if (is.null(arc$trigger$token_threshold)) {
        arc$trigger$token_threshold <- 8000L
    }
    if (is.null(arc$trigger$tool_call_threshold)) {
        arc$trigger$tool_call_threshold <- 5L
    }
    if (is.null(arc$trigger$depth_cap)) {
        arc$trigger$depth_cap <- 3L
    }
    if (is.null(arc$summary)) {
        arc$summary <- list()
    }
    if (is.null(arc$summary$style)) {
        arc$summary$style <- "structured"
    }
    # arc$summary$model: NULL means "match parent provider/model".
    if (is.null(arc$summary$timeout_seconds)) {
        arc$summary$timeout_seconds <- 60L
    }
    # Async archival: spawn + seed + persist sync, summary in
    # callr::r_bg. Default TRUE so the parent CLI never blocks on a
    # slow LLM. Set FALSE to fall back to synchronous (with the same
    # timeout but blocking).
    if (is.null(arc$async)) {
        arc$async <- TRUE
    }
    config$archival <- arc

    # Archival requires the subagent runtime. Refuse to load a config
    # that opts into archival while disabling subagents instead of
    # silently overriding.
    if (isTRUE(config$archival$enabled) && !isTRUE(config$subagents$enabled)) {
        stop("archival.enabled requires subagents.enabled. ",
             "Set subagents.enabled: true or archival.enabled: false.",
             call. = FALSE)
    }

    # Workspace config (managed runtime state)
    if (is.null(config$workspace)) {
        config$workspace <- list()
    }
    ws <- config$workspace
    if (is.null(ws$enabled)) {
        ws$enabled <- TRUE
    }
    if (is.null(ws$budget_chars)) {
        ws$budget_chars <- 32000L
    }
    if (is.null(ws$capture_results)) {
        ws$capture_results <- TRUE
    }
    if (is.null(ws$max_result_size)) {
        ws$max_result_size <- 50000L
    }
    if (is.null(ws$scan_globalenv)) {
        ws$scan_globalenv <- TRUE
    }
    if (is.null(ws$scan_max_bytes)) {
        ws$scan_max_bytes <- 52428800L # 50MB
    }
    if (is.null(ws$max_object_summary_chars)) {
        ws$max_object_summary_chars <- 2000L
    }
    config$workspace <- ws

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

