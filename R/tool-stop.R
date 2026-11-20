# A stop that reaches a running tool (WP26, stage 2).
#
# turn() asks a session's stop function before each tool call and just
# before it runs (R/turn-stop.R). A tool that can run long asks it while
# it waits, every .TOOL_STOP_POLL_MS: the session's in-process executor
# (.make_tool_handler()) names the session whose call runs, here, for
# the call's length, and the tool reads it through .tool_stop_requested().
# Outside a session's call, and for a session with no stop function
# (every surface but ACP today), the answer is FALSE and the tool waits
# as before.
#
# What a stopped tool does: `bash`, `cmd` and `run_r_script` end their
# process tree; `run_r` interrupts its worker, and ends it when it does
# not come back (.run_r_worker_execute()). Each returns a result saying
# so, and the turn stops at its next check.

.tool_runtime <- new.env(parent = emptyenv())

# How often a waiting tool asks, in milliseconds.
.TOOL_STOP_POLL_MS <- 200L

# Evaluate `expr`, a tool call of `session`'s, with the session named as
# the one a waiting tool asks about.
.tool_stop_with <- function(session, expr) {
    previous <- .tool_runtime$session
    .tool_runtime$session <- session
    on.exit(.tool_runtime$session <- previous, add = TRUE)
    expr
}

.tool_stop_requested <- function() {
    session <- .tool_runtime$session
    !is.null(session) && .turn_stop_requested(session)
}

# Wait for `proc` (a processx process) to end, for at most `timeout`
# seconds, asking for a stop between waits. "done" when it ended by
# itself; "timeout" or "stopped" when this ended it, with its process
# tree.
.tool_process_wait <- function(proc, timeout) {
    deadline <- Sys.time() + as.numeric(timeout)
    repeat {
        left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
        if (left <= 0) {
            .tool_process_end(proc)
            return("timeout")
        }
        proc$wait(as.integer(min(.TOOL_STOP_POLL_MS, ceiling(left * 1000))))
        if (!proc$is_alive()) {
            return("done")
        }
        if (.tool_stop_requested()) {
            .tool_process_end(proc)
            return("stopped")
        }
    }
}

.tool_process_end <- function(proc) {
    tryCatch(proc$kill_tree(), error = function(e) {
        tryCatch(proc$kill(), error = function(e2) NULL)
    })
    invisible(NULL)
}

# What a process wrote to `path`, as text. A NUL byte, which a string
# cannot hold, is dropped.
.tool_read_output <- function(path) {
    size <- file.size(path)
    if (is.na(size) || size == 0) {
        return("")
    }
    bytes <- readBin(path, "raw", size)
    rawToChar(bytes[bytes != as.raw(0L)])
}
