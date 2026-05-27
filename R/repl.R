# Shared REPL loop for corteza interactive surfaces.
#
# run_repl_loop() owns the read-eval-print loop body extracted from
# chat(). chat() builds a `ctx` environment carrying loop state plus a
# small set of injected hooks (read_input, palette, render_reply,
# help_text, new_session_fn) and calls this. A future commit can point
# the CLI at the same loop by supplying its own hooks.
#
# ctx fields:
#   Mutable state (reassigned in the loop): disk_session,
#     pending_r_context, last_assistant_response, provider, model
#   Read state: config, cwd, ws_enabled, session (the turn_session env)
#   Hooks: read_input(prompt_str), palette (color list),
#     render_reply(text), help_text(), new_session_fn(),
#     handle_copy(text), format_tools(session), turn_fn(prompt, session)
#
# Build a terse one-line context indicator string. Color escalates by
# threshold: dim below warn, yellow at warn, magenta at high, red at
# crit. `thresholds` is a list(warn=, high=, crit=, compact=). Returns
# a plain string (no trailing newline); caller prints it.
#
# @noRd
.repl_context_indicator <- function(used, limit, palette, thresholds) {
    limit <- as.numeric(limit %||% 0)
    if (!is.null(limit) && limit > 0) {
        pct <- 100 * used / limit
    } else {
        pct <- 0
    }
    warn <- thresholds$warn %||% 75
    high <- thresholds$high %||% 90
    crit <- thresholds$crit %||% 95
    compact <- thresholds$compact %||% 90
    col <- if (pct >= crit) {
        palette$red %||% ""
    } else if (pct >= high) {
        palette$magenta %||% ""
    } else if (pct >= warn) {
        palette$yellow %||% ""
    } else {
        palette$dim %||% ""
    }
    if (limit > 0) {
        limit_k <- sprintf("%.0fK", limit / 1000)
    } else {
        limit_k <- "?"
    }
    sprintf("%scontext %.0f%% of %s (compact at %.0f%%)%s", col, pct,
            limit_k, compact, palette$reset %||% "")
}

#' Sentinel returned by .repl_interruptible() when the wrapped op was
#' interrupted. Callers check inherits(x, "repl_interrupted") and skip.
#' @noRd
.repl_interrupted_sentinel <- structure(list(), class = "repl_interrupted")

#' Run a potentially long-blocking REPL op so an interrupt returns to
#' the prompt instead of aborting the session. Returns expr's value,
#' or the sentinel on interrupt.
#' @noRd
.repl_interruptible <- function(expr, palette = NULL) {
    tryCatch(expr, interrupt = function(c) {
        cat(sprintf("\n%s^C%s\n", palette$dim %||% "", palette$reset %||% ""))
        .repl_interrupted_sentinel
    })
}

# @noRd
run_repl_loop <- function(ctx) {
    while (TRUE) {
        prompt <- ctx$read_input("> ")
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
        # line so far. Slash commands are exempt -- they have their
        # own arg parsing. `\\` at end = literal trailing backslash.
        # `from_paste` blocks the slash-command dispatcher below from
        # reinterpreting a paste that happens to start with `/`
        # (filenames, code snippets, etc.) as a corteza command.
        from_paste <- FALSE
        if (!startsWith(sp, "/")) {
            cont_seed <- backslash_continuation_seed(prompt)
            if (!is.null(cont_seed)) {
                joined <- read_paste_block(seed = cont_seed, heredoc = TRUE)
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
                if (ctx$ws_enabled) {
                    ws_prune()
                    tryCatch(ws_save(ctx$disk_session$sessionId),
                             error = function(e) NULL)
                }
                cat(sprintf("%sBye.%s\n", ctx$palette$dim, ctx$palette$reset))
                break
            }
            if (cmd %in% c("/clear", "/reset", "/new")) {
                # Archive the current session's workspace so it stays
                # resumable, then spin up a fresh on-disk + in-memory
                # session. The old transcript is left on disk.
                if (ctx$ws_enabled) {
                    tryCatch(ws_save(ctx$disk_session$sessionId),
                             error = function(e) NULL)
                }
                # Drop the tool-output buffer for the outgoing session
                # so a /clear actually clears (otherwise /last would
                # still surface results from the old conversation).
                tool_buffer_reset(ctx$disk_session$session)
                ctx$disk_session <- ctx$new_session_fn()
                fresh <- ctx$disk_session$session
                ctx$session$history <- list()
                ctx$session$sessionId <- fresh$sessionId
                ctx$session$disk_session <- fresh
                # Re-register the tool-buffer observer against the new
                # session so subsequent tool calls land in the fresh
                # buffer.
                ctx$session$on_tool <- Filter(function(obs) {
                    !identical(attr(obs, "kind"), "tool_buffer")
                }, ctx$session$on_tool %||% list())
                obs <- tool_buffer_observer(fresh)
                attr(obs, "kind") <- "tool_buffer"
                add_observer(ctx$session, obs)
                ctx$pending_r_context <- character(0)
                ctx$last_assistant_response <- ""
                # /clear wipes the task list -- a new conversation
                # has no carried-over commitments.
                ctx$session$tasks <- list()
                ctx$session$tasks_dirty <- TRUE
                # A fresh conversation leaves no live subagents behind:
                # they were spawned for the conversation being cleared,
                # and the wiped history no longer references them.
                # Killing each retires its spend into the process-run
                # total (subagent_kill -> subagent_retire_spend), so
                # /spent still counts it.
                killed <- 0L
                for (sid in ls(.subagent_registry)) {
                    if (isTRUE(tryCatch(subagent_kill(sid),
                                        error = function(e) FALSE))) {
                        killed <- killed + 1L
                    }
                }
                # Spend is process-lifetime: close the current
                # conversation segment and open a fresh one so /spent
                # itemizes each conversation between clears.
                spend_open_segment(ctx$session)
                killed_note <- if (killed > 0L) {
                    sprintf(" (killed %d subagent%s)", killed,
                        if (killed == 1L) "" else "s")
                } else {
                    ""
                }
                cat(sprintf("%sCleared%s. New session: %s%s\n\n",
                            ctx$palette$dim, killed_note, fresh$sessionId,
                            ctx$palette$reset))
                next
            }
            if (cmd == "/help") {
                cat(ctx$help_text())
                next
            }
            if (cmd == "/copy") {
                ctx$handle_copy(ctx$last_assistant_response)
                next
            }
            if (cmd == "/tasks") {
                if (length(parts) >= 2L &&
                    identical(tolower(parts[2]), "clear")) {
                    ctx$session$tasks <- list()
                    ctx$session$tasks_dirty <- FALSE
                    # Persist immediately. If the user clears and
                    # exits before another assistant turn, the
                    # post-turn dirty-flag sync would never fire and
                    # the clear would be lost.
                    ctx$disk_session$session$tasks <- list()
                    tryCatch(session_save(ctx$disk_session$session),
                             error = function(e) NULL)
                    cat("Task list cleared.\n")
                    next
                }
                rendered <- format_task_list_display(
                    ctx$session$tasks %||% list(),
                    palette = ctx$palette)
                if (is.null(rendered)) {
                    cat("No tasks.\n")
                } else {
                    cat(rendered, "\n", sep = "")
                }
                next
            }
            if (cmd == "/tools") {
                cat(ctx$format_tools(ctx$session))
                next
            }
            if (cmd == "/model") {
                if (length(parts) < 2L) {
                    cat(sprintf("Current model: %s\nUsage: /model <name>\n",
                                ctx$session$model_map$cloud %||% "(default)"))
                    next
                }
                ctx$session$model_map$cloud <- parts[2]
                ctx$model <- parts[2]
                if (!is.null(ctx$disk_session)) {
                    ctx$disk_session$session$model <- parts[2]
                }
                cat(sprintf("Model set to %s\n", parts[2]))
                next
            }
            if (cmd == "/provider") {
                if (length(parts) < 2L) {
                    cat(sprintf("Current provider: %s\nUsage: /provider <name>\n",
                                ctx$session$provider %||% "(default)"))
                    next
                }
                ctx$session$provider <- parts[2]
                ctx$provider <- parts[2]
                if (!is.null(ctx$disk_session)) {
                    ctx$disk_session$session$provider <- parts[2]
                }
                cat(sprintf("Provider set to %s\n", parts[2]))
                next
            }
            if (cmd == "/spawn") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /spawn <task>\n",
                                ctx$palette$dim, ctx$palette$reset))
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
                        parent_session = ctx$session
                    )
                    info <- .subagent_registry[[sub_id]]
                    handle <- if (!is.null(info$seq)) {
                        as.character(info$seq)
                    } else {
                        substr(sub_id, 1L, 8L)
                    }
                    cat(sprintf("%sSpawned subagent [%s]%s (id %s%s%s)\n",
                                ctx$palette$green, handle, ctx$palette$reset,
                                ctx$palette$dim, sub_id, ctx$palette$reset))
                    cat(sprintf("%sUse /ask %s <prompt> to query%s\n",
                                ctx$palette$dim, handle, ctx$palette$reset))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                ctx$palette$bright_magenta, ctx$palette$reset, e$message))
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
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                sub_id <- parts[2]
                sub_prompt <- paste(parts[3:length(parts)], collapse = " ")
                cat(sprintf("%sQuerying subagent %s...%s\n",
                            ctx$palette$dim, sub_id, ctx$palette$reset))
                res <- .repl_interruptible(tryCatch({
                    subagent_query(sub_id, sub_prompt)
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                ctx$palette$bright_magenta, ctx$palette$reset, e$message))
                    NULL
                }), ctx$palette)
                if (inherits(res, "repl_interrupted")) {
                    next
                }
                if (!is.null(res)) {
                    cat(sprintf("%s%s%s\n", ctx$palette$cyan, res, ctx$palette$reset))
                }
                next
            }
            if (cmd == "/queue") {
                if (length(parts) < 3L) {
                    cat(sprintf("%sUsage:%s /queue <id-or-seq> <prompt>\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                sub_id <- parts[2]
                sub_prompt <- paste(parts[3:length(parts)], collapse = " ")
                tryCatch({
                    subagent_query(sub_id, sub_prompt, wait = FALSE)
                    cat(sprintf("%sQueued for subagent %s; collect with /collect %s%s\n",
                                ctx$palette$dim, sub_id, sub_id, ctx$palette$reset))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                ctx$palette$bright_magenta, ctx$palette$reset, e$message))
                })
                next
            }
            if (cmd == "/collect") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /collect <id-or-seq>\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                sub_id <- parts[2]
                cat(sprintf("%sCollecting from subagent %s...%s\n",
                            ctx$palette$dim, sub_id, ctx$palette$reset))
                res <- .repl_interruptible(tryCatch({
                    list(ok = TRUE, value = subagent_collect(sub_id))
                }, error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                ctx$palette$bright_magenta, ctx$palette$reset, e$message))
                    NULL
                }), ctx$palette)
                if (inherits(res, "repl_interrupted")) {
                    next
                }
                if (!is.null(res)) {
                    if (is.null(res$value)) {
                        cat(sprintf("%sStill working; try /collect %s again.%s\n",
                                    ctx$palette$yellow, sub_id, ctx$palette$reset))
                    } else {
                        cat(sprintf("%s%s%s\n", ctx$palette$cyan, res$value,
                                    ctx$palette$reset))
                    }
                }
                next
            }
            if (cmd == "/kill") {
                if (length(parts) < 2L) {
                    cat(sprintf("%sUsage:%s /kill <id-or-seq>\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                ok <- tryCatch(subagent_kill(parts[2]),
                               error = function(e) {
                    cat(sprintf("%sError:%s %s\n",
                                ctx$palette$bright_magenta,
                                ctx$palette$reset, e$message))
                    FALSE
                })
                if (isTRUE(ok)) {
                    cat(sprintf("%sSubagent %s terminated%s\n",
                                ctx$palette$dim, parts[2], ctx$palette$reset))
                } else if (isFALSE(ok)) {
                    cat(sprintf("%sSubagent not found: %s%s\n",
                                ctx$palette$yellow, parts[2], ctx$palette$reset))
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
                trace <- tryCatch(trace_load(ctx$disk_session$session$sessionId, n = n),
                                  error = function(e) list())
                if (length(trace) == 0L) {
                    cat("No tool calls recorded for this session.\n")
                } else {
                    cat(format_trace(trace, show_args = TRUE), "\n")
                }
                next
            }
            if (cmd == "/permissions") {
                cat(format_permissions(ctx$config), "\n")
                approvals_path <- file.path(ctx$cwd, ".corteza", "approvals.json")
                cat(sprintf("Project approvals: %s\n",
                        if (file.exists(approvals_path)) {
                            approvals_path
                        } else {
                            "none"
                        }))
                next
            }
            if (cmd == "/dryrun") {
                ctx$session$config$dry_run <- !isTRUE(ctx$session$config$dry_run)
                ctx$config$dry_run <- ctx$session$config$dry_run
                cat(sprintf("Dry-run mode %s\n",
                        if (isTRUE(ctx$session$config$dry_run))
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
                    ctx$session$plan_mode <- !isTRUE(ctx$session$plan_mode)
                    cat(sprintf("%sPlan mode %s%s\n",
                                ctx$palette$dim,
                            if (isTRUE(ctx$session$plan_mode))
                                "enabled (reads only; LLM proposes a plan via exit_plan_mode)"
                            else "disabled",
                                ctx$palette$reset))
                    next
                }
                ctx$session$plan_mode <- TRUE
                cat(sprintf("%sPlan mode enabled.%s\n", ctx$palette$dim, ctx$palette$reset))
                prompt <- rest
                # Fall through to normal prompt handling below.
            }
            if (cmd %in% c("/spent", "/cost")) {
                cat(format_spend(ctx$session, palette = ctx$palette), "\n", sep = "")
                next
            }
            if (cmd %in% c("/context", "/status")) {
                files <- ctx$config$context_files %||% character(0)
                tools <- tryCatch(
                                  skills_as_api_tools(ctx$session$tools_filter),
                                  error = function(e) list()
                )
                sys_tok <- estimate_text_tokens(ctx$session$system %||% "")
                tools_tok <- estimate_tool_tokens(tools)
                hist_tok <- estimate_history_tokens(
                    ctx$session$history %||% list()
                )
                total_tok <- as.integer(sys_tok + tools_tok + hist_tok)
                disp_model <- ctx$model %||% ctx$session$model_map$cloud %||%
                "(default)"
                limit <- context_limit_for_model(disp_model)
                # Codex-style header: corteza version, model, dir,
                # session id. /status is now an alias of /context
                # showing the same block.
                status_info <- list(
                                    corteza = as.character(utils::packageVersion("corteza")),
                                    model = sprintf("%s @ %s", disp_model,
                        ctx$session$provider %||% ctx$provider),
                                    dir = ctx$cwd,
                                    session = ctx$disk_session$session$sessionKey %||%
                                    ctx$disk_session$session$sessionId %||% "(unset)"
                )
                cat(format_context_block(
                        used = total_tok,
                        limit = limit,
                        breakdown = list(system = sys_tok,
                            tools = tools_tok,
                            history = hist_tok),
                        compact_pct = ctx$config$context_compact_pct %||% 90L,
                        warn_pct = ctx$config$context_warn_pct %||% 75L,
                        high_pct = ctx$config$context_high_pct %||% 90L,
                        crit_pct = ctx$config$context_crit_pct %||% 95L,
                        files = files,
                        palette = ctx$palette,
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
                live_messages <- ctx$session$history %||% list()
                if (length(live_messages) < 2L) {
                    cat("Nothing to compact.\n")
                    next
                }
                result <- .repl_interruptible(
                    do_compact(list(messages = live_messages),
                               ctx$session$provider,
                               ctx$session$model_map$cloud),
                    ctx$palette)
                if (inherits(result, "repl_interrupted")) {
                    next
                }
                if (!is.null(result) && nzchar(result$summary)) {
                    ctx$session$history <- list(
                        list(role = "assistant", content = result$summary)
                    )
                    transcript_compact(ctx$disk_session$session, result$summary)
                    if (!is.null(ctx$disk_session)) {
                        ds <- ctx$disk_session$session
                        ds$compactionCount <- (ds$compactionCount %||% 0L) + 1L
                        ctx$disk_session$session <- ds
                        tryCatch(session_save(ds), error = function(e) NULL)
                    }
                    cat("Compacted.\n")
                }
                next
            }
            if (cmd == "/flush") {
                # Manual memory flush: ask the LLM to write durable
                # memories from the live conversation. Shares one
                # in-process implementation (run_memory_flush) across
                # chat() and the CLI; tools execute in-process so any
                # subagent the flush spawns lands in the one shared
                # registry.
                live_messages <- ctx$session$history %||% list()
                if (length(live_messages) < 2L) {
                    cat(sprintf("%sNothing to flush (no conversation yet).%s\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                cat(sprintf("%sFlushing memories...%s\n",
                            ctx$palette$cyan, ctx$palette$reset))
                flush_result <- .repl_interruptible(run_memory_flush(ctx),
                    ctx$palette)
                if (inherits(flush_result, "repl_interrupted")) {
                    next
                }
                if (!is.null(flush_result)) {
                    content <- flush_result$content %||% ""
                    if (!startsWith(trimws(content), "NO_REPLY")) {
                        cat(sprintf("%sMemories flushed.%s\n",
                                    ctx$palette$green, ctx$palette$reset))
                    } else {
                        cat(sprintf("%sNothing to flush.%s\n",
                                    ctx$palette$dim, ctx$palette$reset))
                    }
                }
                next
            }
            if (cmd == "/doctor") {
                tools <- skills_as_api_tools(ctx$session$tools_filter)
                disp_model <- ctx$model %||% ctx$session$model_map$cloud %||%
                "(default)"
                docs <- tryCatch(list_skill_docs(),
                                 error = function(e) character())
                cat(format_doctor_report(
                        cwd = ctx$cwd,
                        session = ctx$disk_session$session,
                        provider = ctx$session$provider %||% ctx$provider,
                        display_model = disp_model,
                        tools = tools,
                        config = ctx$config,
                        context_files = ctx$config$context_files %||% character(),
                        skill_docs = docs
                    ), "\n")
                next
            }
            if (cmd == "/config") {
                disp_model <- ctx$model %||% ctx$session$model_map$cloud %||%
                "(default)"
                cat(format_config_summary(
                        config = ctx$config,
                        provider = ctx$session$provider %||% ctx$provider,
                        display_model = disp_model,
                        opts = list(port = ctx$config$port,
                                    tools = ctx$session$tools_filter,
                                    dry_run = isTRUE(ctx$session$config$dry_run))
                    ), "\n")
                next
            }
            if (cmd == "/last") {
                n <- if (length(parts) >= 2L) {
                    suppressWarnings(as.integer(parts[2]))
                } else {
                    1L
                }
                if (is.na(n)) {
                    n <- 1L
                }
                outputs <- tool_buffer_list(ctx$disk_session$session)
                if (length(outputs) == 0L) {
                    cat(sprintf("%sNo tool outputs yet.%s\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                if (n < 1L || n > length(outputs)) {
                    cat(sprintf("%sInvalid index. Have %d outputs.%s\n",
                                ctx$palette$yellow, length(outputs), ctx$palette$reset))
                    next
                }
                entry <- outputs[[n]]
                cat(sprintf("\n%s%s%s @ %s\n",
                            ctx$palette$cyan, entry$name, ctx$palette$reset,
                            format(entry$time, "%H:%M:%S")))
                if (length(entry$args) > 0L) {
                    cat(sprintf("%sArgs: %s%s\n",
                                ctx$palette$dim,
                                jsonlite::toJSON(entry$args, auto_unbox = TRUE),
                                ctx$palette$reset))
                }
                cat(sprintf("%s%s%s\n", ctx$palette$dim,
                            strrep("-", 40), ctx$palette$reset))
                cat(entry$result, "\n")
                next
            }
            if (cmd == "/outputs") {
                outputs <- tool_buffer_list(ctx$disk_session$session)
                if (length(outputs) == 0L) {
                    cat(sprintf("%sNo tool outputs yet.%s\n",
                                ctx$palette$dim, ctx$palette$reset))
                    next
                }
                cat(sprintf("\n%sRecent tool outputs:%s\n",
                            ctx$palette$bold, ctx$palette$reset))
                for (i in seq_along(outputs)) {
                    entry <- outputs[[i]]
                    lines <- length(strsplit(entry$result %||% "", "\n",
                            fixed = TRUE)[[1]])
                    cat(sprintf("  %s[%d]%s %s%s%s (%d lines) @ %s\n",
                                ctx$palette$dim, i, ctx$palette$reset,
                                ctx$palette$cyan, entry$name, ctx$palette$reset,
                                lines, format(entry$time, "%H:%M:%S")))
                }
                cat(sprintf("\n%sUse /last [N] to view output%s\n",
                            ctx$palette$dim, ctx$palette$reset))
                next
            }
            if (cmd == "/diff") {
                if (length(parts) >= 2L) {
                    ref <- parts[2]
                } else {
                    ref <- NULL
                }
                material <- collect_git_diff(ref)
                if (!isTRUE(material$ok)) {
                    cat(sprintf("%s%s%s\n", ctx$palette$yellow, material$text,
                                ctx$palette$reset))
                } else {
                    cat(sprintf("\n%sDiff against %s%s\n",
                                ctx$palette$cyan, material$target, ctx$palette$reset))
                    cat(colorize_diff(material$diff), "\n")
                }
                next
            }
            if (cmd == "/review") {
                if (length(parts) >= 2L) {
                    ref <- parts[2]
                } else {
                    ref <- NULL
                }
                material <- collect_git_diff(ref)
                if (!isTRUE(material$ok)) {
                    cat(sprintf("%s%s%s\n", ctx$palette$yellow, material$text,
                                ctx$palette$reset))
                    next
                }
                provider_check <- provider_status(
                    ctx$session$provider %||% ctx$provider,
                    ctx$model
                )
                if (!isTRUE(provider_check$ok)) {
                    cat(sprintf("%sReview unavailable: %s%s\n",
                                ctx$palette$yellow, provider_check$message,
                                ctx$palette$reset))
                    next
                }
                cat(sprintf("%sReviewing diff against %s...%s\n",
                            ctx$palette$dim, material$target, ctx$palette$reset))
                review_result <- run_review(
                    ctx$session$provider %||% ctx$provider,
                    ctx$model, material$target,
                    material$status, material$diff
                )
                if (inherits(review_result, "error")) {
                    cat(sprintf("%sReview failed: %s%s\n",
                                ctx$palette$bright_magenta,
                                conditionMessage(review_result),
                                ctx$palette$reset))
                } else {
                    cat(review_result$content %||% "", "\n")
                }
                next
            }
            # /remember /recall are dead in the CLI too: their
            # implementations rely on memory_store / memory_search /
            # strip_tags / parse_tags helpers that don't exist in the
            # package. Skipping the chat() port to match reality.
            # (/flush is live above -- it's a plain agent turn pointed
            # at config$memory_flush_prompt, no special memory helpers.)
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
                            ctx$palette$yellow, cmd, ctx$palette$reset))
                next
            }
        }
        if (!from_paste && startsWith(trimws(prompt), "/r ")) {
            code <- sub("^/r\\s+", "", trimws(prompt))
            # RStudio's Ctrl+Enter on a multi-line statement (or a
            # user typing one manually) arrives one readline at a
            # time. Mirror R's "+" continuation prompt: read more
            # lines until the expression parses, or we hit a hard
            # cap to keep a runaway loop from blocking the REPL.
            cont_cap <- 100L
            while (!.r_expr_complete(code) && cont_cap > 0L) {
                more <- tryCatch(ctx$read_input("+ "),
                                 error = function(e) character())
                if (length(more) == 0L || is.na(more[1L])) {
                    break
                }
                code <- paste(code, more[1L], sep = "\n")
                cont_cap <- cont_cap - 1L
            }
            r_out <- .repl_interruptible(run_r_eval(code), ctx$palette)
            if (inherits(r_out, "repl_interrupted")) {
                next
            }
            if (nchar(r_out$text) > 0L) {
                cat(r_out$text, "\n", sep = "")
            }
            ctx$pending_r_context <- c(
                                       ctx$pending_r_context,
                                       sprintf("[/r] %s\n%s", code, r_out$staged)
            )
            next
        }
        # `! <cmd>` runs a shell command locally, prints output, and
        # stages it for the next LLM message (same buffer as /r so the
        # staged context the LLM sees is uniform). The space after `!`
        # disambiguates from prompts that legitimately start with `!`
        # (emphasis, markdown headings in some flavors, etc.).
        if (!from_paste && startsWith(sp, "! ")) {
            cmd <- sub("^!\\s+", "", sp)
            shell_out <- .repl_interruptible(
                run_bang_shell(cmd, cwd = ctx$cwd), ctx$palette)
            if (inherits(shell_out, "repl_interrupted")) {
                next
            }
            if (nchar(shell_out$text) > 0L) {
                cat(shell_out$text, "\n", sep = "")
            }
            ctx$pending_r_context <- c(
                                       ctx$pending_r_context,
                                       sprintf("[!] %s\n%s", cmd, shell_out$staged)
            )
            next
        }

        if (length(ctx$pending_r_context) > 0) {
            prompt <- paste(c(ctx$pending_r_context, prompt), collapse = "\n\n")
            ctx$pending_r_context <- character(0)
        }
        transcript_append(ctx$disk_session$session, "user", prompt)

        cat(sprintf("%s\u25cf%s Thinking with %s%s%s\n",
                    ctx$palette$cyan, ctx$palette$reset,
                    ctx$palette$bold,
                    resolve_provider_model(ctx$provider, ctx$model) %||%
                    "(provider default)",
                    ctx$palette$reset))
        pre_turn_len <- length(ctx$session$history %||% list())
        turn_start <- Sys.time()
        result <- tryCatch(
                           ctx$turn_fn(prompt, ctx$session),
                           corteza_user_deny = function(c) {
            # User picked "3. Deny" at the approval prompt. Abort the
            # whole turn so the LLM doesn't cascade through more tool
            # calls. apply_exit_marker repairs any tool_use blocks
            # that landed in turn_session$history during this turn
            # (via the history_callback in turn()) so they don't 400
            # the next API call on dangling tool_use_id.
            cat(sprintf("\n%sDenied -- aborting turn.%s\n",
                        ctx$palette$yellow, ctx$palette$reset))
            marker <- user_deny_marker(c$tool %||% "?")
            apply_exit_marker(ctx$session, prompt, pre_turn_len, marker,
                              placeholder = "[Denied by user before execution]")
            transcript_append(ctx$disk_session$session, "assistant", marker)
            NULL
        },
                           interrupt = function(c) {
            # Ctrl+C in terminal R, Esc in RStudio. Same handling:
            # apply_exit_marker repairs any unfinished tool_use
            # blocks in turn_session$history (preserving completed
            # tool calls that the history_callback already mirrored
            # into the session env) and appends the interrupt marker.
            cat(sprintf("\n%sInterrupted.%s\n", ctx$palette$yellow, ctx$palette$reset))
            marker <- user_interrupt_marker()
            apply_exit_marker(ctx$session, prompt, pre_turn_len, marker)
            transcript_append(ctx$disk_session$session, "assistant", marker)
            NULL
        },
                           error = function(e) {
            message(sprintf("%sError:%s %s",
                            ctx$palette$bright_magenta, ctx$palette$reset, e$message))
            NULL
        }
        )
        # Sync task-list mutations to the on-disk session record
        # regardless of turn outcome. The intercept prints inline as
        # changes happen, so there's no end-of-turn display here --
        # but an approved plan should survive an aborted turn (codex
        # finding: prior code only synced on success, so an interrupt
        # right after approval threw away the just-approved plan).
        if (isTRUE(ctx$session$tasks_dirty)) {
            ctx$disk_session$session$tasks <- ctx$session$tasks
            tryCatch(session_save(ctx$disk_session$session),
                     error = function(e) NULL)
            ctx$session$tasks_dirty <- FALSE
        }

        if (is.null(result)) {
            # Interrupt or error path. Still print the timing footer
            # so the user sees how long the aborted turn ran.
            cat(turn_footer_line(turn_start, palette = ctx$palette), "\n", sep = "")
            next
        }

        reply <- result$reply %||% ""
        if (nchar(reply) == 0) {
            cat(sprintf("%s[No response text]%s\n\n", ctx$palette$dim, ctx$palette$reset))
        } else {
            ctx$render_reply(reply)
            ctx$last_assistant_response <- reply
        }
        cat(turn_footer_line(turn_start, palette = ctx$palette), "\n", sep = "")
        transcript_append(ctx$disk_session$session, "assistant", reply)

        # Archival hook: opt-in via config$archival$enabled. Mutates
        # turn_session$history in place when triggers fire.
        maybe_archive_turn(
                           turn_session = ctx$session, prompt = prompt,
                           pre_turn_len = pre_turn_len, result = result, config = ctx$config,
                           parent_session_id = ctx$disk_session$session$sessionId,
                           max_turns_hit = isTRUE(grepl("Max turns", reply)),
                           depth = 0L
        )

        # Accumulate this turn's spend into the session tally (both
        # surfaces share ctx$session, persistent across the loop).
        # Main-agent turns only; subagent spend is out of scope.
        session_accumulate_spend(ctx$session, result$usage)

        # Post-turn context accounting (both chat() and the CLI go
        # through here). Mirror the /context handler's math against the
        # *live* history (ctx$session$history), not session$messages,
        # then print a terse one-line indicator and auto-compact when we
        # cross the compaction threshold.
        sys_tok <- estimate_text_tokens(ctx$session$system %||% "")
        tools_tok <- estimate_tool_tokens(
            tryCatch(skills_as_api_tools(ctx$session$tools_filter),
                     error = function(e) list())
        )
        hist_tok <- estimate_history_tokens(ctx$session$history %||% list())
        used <- as.integer(sys_tok + tools_tok + hist_tok)
        # A model-less chat() session (no explicit model, model_map$cloud
        # still the placeholder) resolves to the provider default so the
        # meter shows the real limit; context_limit_for_model tolerates a
        # NULL here regardless.
        model <- ctx$model %||% ctx$session$model_map$cloud %||%
        default_provider_model(ctx$session$provider)
        limit <- context_limit_for_model(model)
        if (!is.null(limit) && limit > 0L) {
            pct <- 100 * used / limit
        } else {
            pct <- 0
        }
        compact_pct <- ctx$config$context_compact_pct %||% 90
        cat(.repl_context_indicator(
                                    used, limit, ctx$palette,
                                    list(warn = ctx$config$context_warn_pct %||% 75,
                    high = ctx$config$context_high_pct %||% 90,
                    crit = ctx$config$context_crit_pct %||% 95,
                    compact = compact_pct)
            ), "\n")

        if (pct >= compact_pct && length(ctx$session$history) > 2) {
            # Optional pre-compaction memory flush so durable facts get
            # written before the turns that mention them are summarized
            # away. Tolerate NULL/errors -- a flush failure must not
            # block compaction.
            flush_ran <- FALSE
            flush_interrupted <- FALSE
            if (isTRUE(ctx$config$memory_flush_enabled)) {
                cat(sprintf("%sFlushing memories before compaction...%s\n",
                            ctx$palette$dim, ctx$palette$reset))
                fr <- .repl_interruptible(
                    tryCatch(run_memory_flush(ctx), error = function(e) NULL),
                    ctx$palette)
                if (inherits(fr, "repl_interrupted")) {
                    flush_interrupted <- TRUE
                } else {
                    flush_ran <- !is.null(fr)
                }
            }
            # An interrupt during the pre-flush skips compaction entirely
            # rather than firing off another blocking network call.
            comp <- if (flush_interrupted) {
                NULL
            } else {
                .repl_interruptible(
                                    tryCatch(
                        do_compact(
                                   list(messages = ctx$session$history),
                                   ctx$session$provider,
                                   ctx$session$model_map$cloud),
                        error = function(e) NULL),
                                    ctx$palette)
            }
            if (!inherits(comp, "repl_interrupted") && !is.null(comp) &&
                nzchar(comp$summary)) {
                ctx$session$history <- list(
                    list(role = "assistant", content = comp$summary)
                )
                transcript_compact(ctx$disk_session$session, comp$summary)
                if (!is.null(ctx$disk_session)) {
                    ds <- ctx$disk_session$session
                    ds$compactionCount <- (ds$compactionCount %||% 0L) + 1L
                    if (isTRUE(flush_ran)) {
                        ds$memoryFlushCompactionCount <-
                        (ds$memoryFlushCompactionCount %||% 0L) + 1L
                    }
                    ctx$disk_session$session <- ds
                    tryCatch(session_save(ds), error = function(e) NULL)
                }
                cat(sprintf("%sAuto-compacted (context was %.0f%%).%s\n",
                            ctx$palette$dim, pct, ctx$palette$reset))
            }
        }
    }

    invisible(NULL)
}

