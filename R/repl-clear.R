# Shared by the local slash dispatcher and the remote input reader.
.repl_clear <- function(ctx) {
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
    invisible(NULL)
}
