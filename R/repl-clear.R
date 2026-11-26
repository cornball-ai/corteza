# Shared by the local slash dispatcher and the remote input reader.
.repl_clear <- function(ctx) {
    # Jobs this conversation queued and has not started keep what it
    # allows them, taken now, while the session is still this one's
    # (R/job-conversation.R).
    if (isTRUE(ctx$session$.repl_jobs_ready)) {
        job_conversation_hold(ctx$session)
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
    # A new conversation is a new session (WP14): its tools and system
    # text are built again, and nothing told to the last one stands.
    .repl_session_rebuild(ctx)
    # The doer follows: its checkpoint moves to the new conversation,
    # which starts with the R objects and no history (R/job-conversation.R).
    if (isTRUE(ctx$session$.repl_jobs_ready)) {
        job_conversation_clear(ctx$session, fresh$sessionId,
                               checkpoint_key = paste0("repl:", fresh$sessionId))
    }
    # The reviewer's earlier calls were the old conversation's.
    ctx$session$.review_log <- NULL
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
        if (isTRUE(tryCatch(subagent_kill(sid), error = function(e) FALSE))) {
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
    invisible(NULL)
}

# When a new answer to /trust takes effect at the terminal and in chat().
.repl_trust_when <- function() {
    "the next conversation (/clear starts one)"
}

# The session a new conversation starts with, as the terminal and chat()
# start one (WP14): the config, the tool table and the system text taken
# again from the directory, so a /trust answered since takes effect.
# What a flag or a command set in this process (`ctx$config_set`: --dry-run,
# /permissions, /context-edit and the rest) carries over. A system text
# the caller gave chat() stays as given. The mode, the model and the
# history are the caller's business, not this function's.
.repl_session_rebuild <- function(ctx) {
    s <- ctx$session
    cwd <- s$cwd %||% ctx$cwd %||% getwd()
    config <- tryCatch(load_config(cwd), error = function(e) {
        message("/clear kept the session's config and tools: ",
                conditionMessage(e))
        NULL
    })
    if (!is.null(config)) {
        for (key in unique(ctx$config_set %||% character())) {
            config[key] <- list(s$config[[key]])
        }
        s$config <- config
        ctx$config <- config
        s$project_trusted <- project_trusted(cwd)
        s$tool_table <- session_tool_table(cwd, config)
        load_project_skills(cwd, code = FALSE)
        if (!is.null(s$context_manifest)) {
            catalog <- build_instruction_catalog(cwd)
            bundle <- load_context_bundle(cwd, instruction_catalog = catalog,
                include_instruction_catalog =
                instruction_reader_selected(s$tools_filter),
                prefix_sources = s$context_prefix_sources %||% list())
            s$system <- bundle$system
            s$context_manifest <- bundle$manifest
            s$context_prefix_sources <- bundle$prefix_sources
            s$instruction_catalog <- catalog
        }
        review_apply(s)
    }
    # Composed again at the next turn, availability tested again.
    s$.prefix_base <- NULL
    s$.request_prefix <- NULL
    s$.prefix_seen <- NULL
    invisible(s)
}
