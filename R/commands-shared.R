# The commands 2 surfaces run alike: the terminal and chat()
# (run_repl_loop(), R/repl.R) and an ACP session (R/acp-commands.R). Each
# prints what it has to say; the terminal shows it, and ACP sends what
# was printed as the reply. `ctx` carries the session (`session`), its
# config, directory and palette, and its record (`disk_session$session`).

# /status and /context: the version, model, directory, trust and session,
# and what the next request carries against the window.
.repl_status <- function(ctx) {
    cat(.context_edit_status(ctx$session), "\n")
    files <- ctx$config$context_files %||% character(0)
    sources <- ctx$session$context_manifest$sources %||% NULL
    # What the next request carries (R/request-prefix.R).
    parts <- tryCatch(session_request_parts(ctx$session),
                      error = function(e) list(system = "", tools = list()))
    tools <- parts$tools
    sys_tok <- estimate_text_tokens(parts$system)
    tools_tok <- estimate_tool_tokens(tools)
    hist_tok <- estimate_history_tokens(ctx$session$history %||% list())
    total_tok <- as.integer(sys_tok + tools_tok + hist_tok)
    disp_model <- ctx$model %||% ctx$session$model_map$cloud %||% "(default)"
    # The window and the trigger the compactor will use
    # (R/compact-trigger.R): a context_window in the config and the cap
    # both show here.
    trig <- context_trigger(ctx$session, ctx$config)
    limit <- trig$window
    # Codex-style header: corteza version, model, dir, session id.
    # /status is now an alias of /context showing the same block.
    status_info <- list(
                        corteza = as.character(utils::packageVersion("corteza")),
                        model = sprintf("%s @ %s", disp_model,
                                        ctx$session$provider %||% ctx$provider),
                        dir = ctx$cwd,
                        trusted = trust_status_word(ctx$session$project_trusted,
            project_trusted(ctx$cwd)),
                        session = ctx$disk_session$session$sessionKey %||%
                        ctx$disk_session$session$sessionId %||% "(unset)"
    )
    cat(format_context_block(
                             used = total_tok,
                             limit = limit,
                             breakdown = list(system = sys_tok, tools = tools_tok,
                history = hist_tok),
                             compact_pct = trig$effective_pct,
                             warn_pct = ctx$config$context_warn_pct %||% 75L,
                             high_pct = ctx$config$context_high_pct %||% 90L,
                             crit_pct = ctx$config$context_crit_pct %||% 95L,
                             files = files,
                             sources = sources,
                             palette = ctx$palette,
                             status_info = status_info,
                             note = .context_trigger_lines(trig)
        ), "\n", sep = "")
    left <- trust_left_alone(ctx$session, ctx$cwd)
    if (length(left)) {
        cat(left, sep = "\n")
        cat("\n")
    }
    invisible(NULL)
}

# /tasks, and /tasks clear.
.repl_cmd_tasks <- function(ctx, parts) {
    if (length(parts) >= 2L && identical(tolower(parts[2]), "clear")) {
        ctx$session$tasks <- list()
        ctx$session$tasks_dirty <- FALSE
        # Persist immediately. If the user clears and exits before
        # another assistant turn, the post-turn dirty-flag sync would
        # never fire and the clear would be lost.
        ctx$disk_session$session$tasks <- list()
        tryCatch(session_save(ctx$disk_session$session),
                 error = function(e) NULL)
        cat("Task list cleared.\n")
        return(invisible(NULL))
    }
    rendered <- format_task_list_display(ctx$session$tasks %||% list(),
        palette = ctx$palette)
    if (!is.null(rendered)) {
        cat(rendered, "\n", sep = "")
    }
    # A resumed session can hold tasks from before the list was off.
    # They print, and the note says the model does not see them.
    if (!.task_list_on(ctx$session)) {
        cat(.task_list_off_note(), "\n", sep = "")
    } else if (is.null(rendered)) {
        cat("No tasks.\n")
    }
    invisible(NULL)
}

# /compact: summarize the conversation now, whatever its size. TRUE when
# it compacted; FALSE, with a message, when it could not.
.compact_now <- function(session, config) {
    tryCatch(maybe_compact_turn_session(
                                        session,
                                        config,
                                        kind = session$kind %||% NULL,
                                        tools = tryCatch(session_request_parts(session)$tools,
                error = function(e) NULL),
                                        system = tryCatch(session_request_parts(session)$system,
                error = function(e) session$system),
                                        threshold = 0,
                                        min_messages = 2L,
                                        reason = "manual"
        ), error = function(e) {
        message("Compaction failed: ", conditionMessage(e))
        FALSE
    })
}
