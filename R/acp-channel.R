# The client's end of the line, as `corteza acp` reads and writes it
# (R/acp.R). Messages go out through a connection of their own to file
# descriptor 1, and R's standard output goes to standard error while the
# loop runs (.acp_main()), so a cat() or print() in a tool or a package
# cannot corrupt the stream. A test hands in a pipe for the input and a
# function for the output.

.acp_channel <- function(input = NULL, send = NULL) {
    # processx documents its connection to a file descriptor for Unix
    # descriptors, and nobody has run this on Windows yet.
    if ((is.null(input) || is.null(send)) && .Platform$OS.type == "windows") {
        stop(paste("corteza acp runs on Linux and macOS in this release;",
                   "on Windows, use the terminal or chat()."), call. = FALSE)
    }
    ch <- new.env(parent = emptyenv())
    ch$input <- input %||% processx::conn_create_fd(0L, close = FALSE)
    if (is.null(send)) {
        out <- processx::conn_create_fd(1L, close = FALSE)
        send <- function(line) .acp_write_fd(out, line)
    }
    ch$send <- send
    ch$queue <- list()
    ch$closed <- FALSE
    ch$broken <- FALSE
    ch$next_id <- 0L
    ch
}

# One line to the descriptor. conn_write() returns what a full pipe did
# not take, which goes again until none is left.
.acp_write_fd <- function(con, line) {
    left <- charToRaw(enc2utf8(paste0(line, "\n")))
    while (length(left)) {
        left <- processx::conn_write(con, left)
        if (length(left)) {
            Sys.sleep(0.01)
        }
    }
    invisible(NULL)
}

# A write that fails means the client has gone: the channel counts as
# closed, so a running turn stops (.acp_stop()) and the loop ends
# (.acp_take_next()). Said once, on standard error.
.acp_send <- function(ch, msg) {
    if (isTRUE(ch$broken)) {
        return(invisible(NULL))
    }
    tryCatch(ch$send(.acp_encode(msg)), error = function(e) {
        ch$broken <- TRUE
        ch$closed <- TRUE
        .acp_log("the client stopped reading: ", conditionMessage(e))
    })
    invisible(NULL)
}

# A line for the person reading the agent's standard error.
.acp_log <- function(...) {
    cat("corteza acp: ", ..., "\n", sep = "", file = stderr())
    invisible(NULL)
}

# Read what has come into the queue, waiting up to `wait_ms` for it (-1
# waits until something comes). It reads only once the poll says there
# is something to read: a read on an idle pipe would wait for the
# client. A read that gives nothing at a complete line is the end of the
# input.
.acp_fill <- function(ch, wait_ms = 0L) {
    if (ch$closed) {
        return(invisible(NULL))
    }
    state <- processx::poll(list(ch$input), as.integer(wait_ms))[[1L]]
    if (!identical(state, "ready")) {
        return(invisible(NULL))
    }
    lines <- processx::conn_read_lines(ch$input)
    if (!length(lines) && !processx::conn_is_incomplete(ch$input)) {
        ch$closed <- TRUE
    }
    msgs <- Filter(Negate(is.null), lapply(lines, .acp_decode))
    ch$queue <- c(ch$queue, msgs)
    invisible(NULL)
}

# The next message, waiting until one comes. NULL at the end of the
# input.
.acp_next <- function(ch) {
    while (!length(ch$queue)) {
        if (ch$closed) {
            return(NULL)
        }
        .acp_fill(ch, -1L)
    }
    msg <- ch$queue[[1L]]
    ch$queue <- ch$queue[-1L]
    msg
}

# Take out of the queue the first message `match` holds TRUE for, or
# NULL.
.acp_take <- function(ch, match) {
    hit <- which(vapply(ch$queue, function(m) isTRUE(match(m)), logical(1)))
    if (!length(hit)) {
        return(NULL)
    }
    msg <- ch$queue[[hit[1L]]]
    ch$queue <- ch$queue[-hit[1L]]
    msg
}
