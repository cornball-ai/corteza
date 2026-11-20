# A turn can be asked to stop (WP20a, rule 10).
#
# A session may carry `stop_requested`, a function of no arguments that
# returns TRUE once a stop is asked for. turn() asks it before each
# model request, before each tool call, again just before the call runs
# (after any wait at the gate or the approval prompt) and, when the turn
# streams, on each delta. A stop ends the turn with status `cancelled`,
# its history repaired as after an interrupt (apply_exit_marker()): a
# call that got no result gets a placeholder and the interrupt marker
# follows, so the next request is valid. ACP's session/cancel is the
# first caller (WP26). Ctrl+C at the terminal and remote control's stop
# keep their own handling (R/interrupt.R, .remote_check_stop()).
#
# A tool that waits long asks it too, while it waits (R/tool-stop.R).
#
# Not reachable: the wait for a reply before its first delta, and a
# provider that does not stream. llm.api runs no hook there.

.turn_stop_requested <- function(session) {
    f <- session$stop_requested
    is.function(f) && isTRUE(tryCatch(f(), error = function(e) FALSE))
}

# Classed "interrupt", like /auto's escalation, so the error handlers
# around tool dispatch and llm.api's request callback let it through to
# turn().
.turn_stop_condition <- function() {
    structure(class = c("corteza_stop", "interrupt", "condition"),
              list(message = "A stop was asked for", call = NULL))
}

.turn_stop_check <- function(session) {
    if (.turn_stop_requested(session)) {
        stop(.turn_stop_condition())
    }
    invisible(NULL)
}

# The request callback: asked at each request's start, before anything
# is sent, then the audit's.
.turn_request_callback <- function(session, audit_cb) {
    if (!is.function(session$stop_requested)) {
        return(audit_cb)
    }
    function(event) {
        if (identical(event$phase, "start")) {
            .turn_stop_check(session)
        }
        if (is.function(audit_cb)) {
            audit_cb(event)
        }
        invisible(NULL)
    }
}

# The streaming sink, asked on each delta. A stop cancels the reply as
# the voice brain does: llm_cancelled, which agent() answers by
# returning with the history so far. `on_stop` tells turn() that the
# cancel was a stop.
.turn_stop_sink <- function(session, push, on_stop) {
    if (!is.function(session$stop_requested)) {
        return(push)
    }
    function(delta) {
        if (.turn_stop_requested(session)) {
            on_stop()
            llm.api::llm_cancel("A stop was asked for")
        }
        push(delta)
    }
}

# Evaluate the model call, turning a stop raised inside it into a
# cancelled response.
.turn_stop_catch <- function(expr, on_stop) {
    tryCatch(expr, corteza_stop = function(c) {
        on_stop()
        list(cancelled = TRUE)
    })
}
