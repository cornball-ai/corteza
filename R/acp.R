# corteza as an Agent Client Protocol agent: `corteza acp` (WP26).
#
# An ACP client (an editor, or a relay that shows a session in a chat
# room) starts the command and drives it over standard input and output.
# Each ACP session is a corteza session in the directory the client
# names, built as chat() builds one (session_setup(), R/acp-session.R),
# and each prompt is one turn() (R/acp-turn.R).
#
# One R thread runs the loop. Between turns it takes the messages in the
# order they came. While a turn runs, the turn's stop function and its
# waits for the client read standard input and handle what came
# (.acp_service()): a cancel, the client's answer to a permission
# request, and the requests that need no turn, answered at once. A
# prompt for another session waits until this one's ends.

# Run the loop until the client closes standard input. `input` and
# `send` are for tests: a connection to read and a function that takes
# each line written. Without `send`, protocol lines go to file
# descriptor 1 through a connection of their own, and what R prints goes
# to standard error until the loop ends.
.acp_main <- function(provider = NULL, model = NULL, tools = NULL,
                      input = NULL, send = NULL) {
    st <- .acp_state(.acp_channel(input, send), provider, model, tools)
    if (is.null(send)) {
        sink(stderr(), type = "output")
        on.exit(sink(type = "output"), add = TRUE)
    }
    on.exit(.acp_close_all(st), add = TRUE)
    repeat {
        msg <- .acp_take_next(st)
        if (is.null(msg)) {
            break
        }
        .acp_dispatch(st, msg)
    }
    invisible(NULL)
}

# The options of `corteza acp`: --provider, --model, --tools (a comma
# list). Anything else is an error, said on standard error, since
# standard output is the client's.
.acp_args <- function(args) {
    opts <- list(provider = NULL, model = NULL, tools = NULL)
    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]
        if (arg %in% c("--provider", "--model", "--tools") &&
            i < length(args)) {
            value <- args[[i + 1L]]
            switch(arg,
                   "--provider" = opts$provider <- value,
                   "--model" = opts$model <- value,
                   "--tools" = opts$tools <- strsplit(value, ",", fixed = TRUE)[[1L]])
            i <- i + 2L
        } else {
            stop(sprintf("corteza acp takes --provider, --model and --tools; not '%s'.",
                         arg), call. = FALSE)
        }
    }
    opts
}

.acp_state <- function(ch, provider = NULL, model = NULL, tools = NULL) {
    st <- new.env(parent = emptyenv())
    st$ch <- ch
    st$provider <- provider
    st$model <- model
    st$tools <- tools
    st$initialized <- FALSE
    # Open sessions, by id.
    st$sessions <- list()
    # The running prompt's state (R/acp-turn.R), or NULL between turns.
    st$live <- NULL
    # Prompts for other sessions that came while one ran, in order.
    st$waiting <- list()
    # Our requests the client has yet to answer, and its answers, by id.
    st$pending <- character()
    st$answers <- list()
    # Closes of the running session, answered after its prompt is.
    st$after <- list()
    # What a session's opening has to say (.acp_notes_send()).
    st$notes <- list()
    st
}

# The next message to handle: a prompt that waited, else the next to
# come. NULL at the end of the input, or once nothing can be sent.
.acp_take_next <- function(st) {
    if (isTRUE(st$ch$broken)) {
        return(NULL)
    }
    if (length(st$waiting)) {
        msg <- st$waiting[[1L]]
        st$waiting <- st$waiting[-1L]
        return(msg)
    }
    .acp_next(st$ch)
}

.acp_dispatch <- function(st, msg) {
    if (isTRUE(msg$.unreadable)) {
        .acp_send(st$ch, .acp_error(NULL, -32700L,
                                    "A line was not a JSON-RPC message."))
        return(invisible(NULL))
    }
    kind <- .acp_kind(msg)
    if (identical(kind, "response")) {
        return(.acp_answer_in(st, msg))
    }
    if (identical(kind, "notification")) {
        if (identical(msg$method, "session/cancel")) {
            .acp_cancel(st, msg$params$sessionId)
        } else {
            .acp_log("left alone a notification of ", msg$method)
        }
        return(invisible(NULL))
    }
    if (!identical(kind, "request")) {
        .acp_send(st$ch, .acp_error(msg$id, -32600L, "Not a JSON-RPC request."))
        return(invisible(NULL))
    }
    reply <- tryCatch(.acp_handle(st, msg),
                      acp_refusal = function(e) {
        .acp_error(msg$id, e$code, conditionMessage(e))
    },
                      error = function(e) {
        .acp_error(msg$id, -32603L, conditionMessage(e))
    },
                      interrupt = function(e) {
        .acp_error(msg$id, -32603L, "Interrupted.")
    })
    # NULL: answered later, as a close of the running session is.
    if (!is.null(reply)) {
        .acp_send(st$ch, reply)
    }
    .acp_notes_send(st)
    if (identical(msg$method, "session/prompt")) {
        .acp_after_prompt(st)
    }
    invisible(NULL)
}

# The answer to a request, or NULL for one answered later.
.acp_handle <- function(st, msg) {
    method <- msg$method
    params <- msg$params %||% .acp_object()
    if (!st$initialized && !identical(method, "initialize")) {
        .acp_refuse("Send initialize first.", -32600L)
    }
    result <- switch(method, initialize = .acp_initialize(st, params),
                     "session/new" = .acp_session_new(st, params),
                     "session/resume" = .acp_session_resume(st, params),
                     "session/list" = .acp_session_list(st, params),
                     "session/close" = .acp_session_close(st, params, msg$id),
                     "session/set_mode" = .acp_set_mode(st, params),
                     "session/prompt" = .acp_prompt(st, params),
                     .acp_refuse(paste("Method not found:", method), -32601L))
    if (is.null(result)) {
        return(NULL)
    }
    .acp_result(msg$id, result)
}

.acp_initialize <- function(st, params) {
    if (st$initialized) {
        .acp_refuse("Already initialized.", -32600L)
    }
    st$initialized <- TRUE
    # Version 1, whatever the client asked for: the agent answers with
    # the version it takes, and a client that cannot take it leaves.
    list(protocolVersion = 1L,
         agentCapabilities = list(
                                  loadSession = FALSE,
                                  promptCapabilities = list(image = FALSE, audio = FALSE,
                embeddedContext = FALSE),
                                  mcpCapabilities = list(http = FALSE, sse = FALSE),
                                  sessionCapabilities = list(list = .acp_object(),
                resume = .acp_object(),
                close = .acp_object())),
         agentInfo = list(name = "corteza", title = "corteza",
                          version = as.character(utils::packageVersion("corteza"))),
         authMethods = list())
}

# A session/update notification.
.acp_update <- function(st, sid, update) {
    .acp_send(st$ch, .acp_notification("session/update",
                                       list(sessionId = sid, update = update)))
}

# A cancel stops the running prompt of its session (R/acp-turn.R). A
# prompt for it that only waits is answered `cancelled`, unrun.
.acp_cancel <- function(st, sid) {
    live <- st$live
    if (!is.null(live) && identical(live$sid, sid)) {
        live$cancelled <- TRUE
        return(invisible(NULL))
    }
    unrun <- function(msg) {
        identical(msg$method, "session/prompt") &&
        identical(msg$params$sessionId, sid)
    }
    for (msg in Filter(unrun, st$waiting)) {
        .acp_send(st$ch, .acp_result(msg$id, list(stopReason = "cancelled")))
    }
    st$waiting <- Filter(Negate(unrun), st$waiting)
    while (!is.null(msg <- .acp_take(st$ch, unrun))) {
        .acp_send(st$ch, .acp_result(msg$id, list(stopReason = "cancelled")))
    }
    invisible(NULL)
}

# An answer to one of our requests. One that answers none still open (a
# permission request a cancel ended) is dropped.
.acp_answer_in <- function(st, msg) {
    key <- as.character(msg$id)
    if (length(key) == 1L && key %in% st$pending) {
        st$answers[[key]] <- msg
    } else {
        .acp_log("dropped an answer to no open request (", key, ")")
    }
    invisible(NULL)
}

# Send a request and wait for its answer, handling what else comes
# (.acp_service()). NULL when the running prompt is cancelled, or the
# input ends, before the client answers.
.acp_ask <- function(st, method, params) {
    st$ch$next_id <- st$ch$next_id + 1L
    id <- st$ch$next_id
    key <- as.character(id)
    st$pending <- c(st$pending, key)
    on.exit({
        st$pending <- setdiff(st$pending, key)
        st$answers[[key]] <- NULL
    }, add = TRUE)
    .acp_send(st$ch, .acp_request(id, method, params))
    repeat {
        # A cancel wins over an answer read with it: the turn stops.
        if (isTRUE(st$live$cancelled)) {
            return(NULL)
        }
        answer <- st$answers[[key]]
        if (!is.null(answer)) {
            return(answer)
        }
        if (st$ch$closed) {
            return(NULL)
        }
        .acp_service(st, 200L)
    }
}

# While a turn runs: read what has come, waiting up to `wait_ms`, and
# handle it. A prompt waits, unless it is for the running session, which
# takes one at a time. Everything else is handled now: a cancel, an
# answer, and the requests that need no turn (session/list,
# session/set_mode, session/new and session/resume for another
# directory, session/close), each answered at once.
.acp_service <- function(st, wait_ms = 0L) {
    .acp_fill(st$ch, wait_ms)
    while (length(st$ch$queue)) {
        msg <- st$ch$queue[[1L]]
        st$ch$queue <- st$ch$queue[-1L]
        if (identical(.acp_kind(msg), "request") &&
            identical(msg$method, "session/prompt")) {
            if (identical(msg$params$sessionId, st$live$sid)) {
                .acp_send(st$ch, .acp_error(msg$id, -32600L,
                        "A prompt is running in this session."))
            } else {
                st$waiting <- c(st$waiting, list(msg))
            }
            next
        }
        # Another session's work leaves the running one where it was.
        here <- getwd()
        .acp_dispatch(st, msg)
        if (!identical(getwd(), here)) {
            setwd(here)
        }
    }
    invisible(NULL)
}

# After a prompt's answer: the closes of its session that came while it
# ran.
.acp_after_prompt <- function(st) {
    after <- st$after
    st$after <- list()
    for (a in after) {
        .acp_forget(st, a$sid)
        .acp_send(st$ch, .acp_result(a$id, .acp_object()))
    }
    invisible(NULL)
}

# At the end of the input: every session's workers stop, and its record
# is saved.
.acp_close_all <- function(st) {
    for (sid in names(st$sessions)) {
        tryCatch(.acp_forget(st, sid), error = function(e) NULL)
    }
    invisible(NULL)
}
