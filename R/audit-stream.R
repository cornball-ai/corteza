# Single-writer observational streams. Authority remains in job/approval and
# journal commits; a failure here reports missing evidence and never retries work.
.audit_runtime <- new.env(parent = emptyenv())
.audit_runtime$counter <- 0
.audit_runtime$noticed <- list()

.audit_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

.audit_id <- function() {
    .audit_runtime$counter <- .audit_runtime$counter + 1
    sprintf("%s-%x-%s", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
            Sys.getpid(),
            format(.audit_runtime$counter, scientific = FALSE, trim = TRUE))
}

.audit_notice <- function(text = "Corteza audit recording is failing; work continues with incomplete records.",
                          kind = "write") {
    if (isTRUE(.audit_runtime$noticed[[kind]])) {
        return(invisible(NULL))
    }
    .audit_runtime$noticed[[kind]] <- TRUE
    tryCatch(message(text),
             error = function(e) NULL)
    invisible(NULL)
}

.audit_root <- function(session) {
    if (!is.null(session$audit_dir)) {
        return(session$audit_dir)
    }
    # Resolve independently of journal_enabled: that switch controls history.
    dir <- session$journal_dir
    if (is.null(dir) && !is.null(session$sessionId) &&
        !is.null(session$journal_agent_id)) {
        dir <- file.path(sessions_dir(session$journal_agent_id),
                         paste0(session$sessionId, ".journal"))
    }
    if (!is.null(dir)) {
        file.path(dir, "audit")
    } else {
        corteza_data_path("audit")
    }
}

.audit_state <- function(session) {
    if (!is.environment(session) || isFALSE(session$audit_enabled) ||
        isFALSE(session$config$audit$enabled)) {
        return(NULL)
    }
    root <- path.expand(.audit_root(session))
    if (!startsWith(root, "/") && !startsWith(root, "\\\\") && !grepl("^[A-Za-z]:", root)) {
        root <- file.path(getwd(), root)
    }
    # Resolve symlinked ancestors only after the directory exists. Otherwise
    # macOS's /var -> /private/var changes the root after the first write and
    # the next auxiliary call silently starts a second writer.
    if (!dir.exists(root)) {
        mask <- Sys.umask("0077")
        tryCatch(dir.create(root, recursive = TRUE, showWarnings = FALSE,
                            mode = "0700"), finally = Sys.umask(mask))
    }
    root <- normalizePath(root, winslash = "/", mustWork = FALSE)
    old <- session$.audit_stream
    if (!is.null(old) && identical(old$root, root) &&
        identical(old$pid, Sys.getpid())) {
        return(old)
    }
    state <- new.env(parent = emptyenv())
    state$root <- root
    state$pid <- Sys.getpid()
    state$turns <- 0
    state$lost <- 0
    state$reserved <- FALSE
    # An exclusive directory reservation survives restart/PID reuse and does
    # not touch R's RNG. A failed reservation never licenses an existing file.
    repeat {
        state$id <- .audit_id()
        state$path <- file.path(root, paste0(state$id, ".jsonl"))
        state$reservation <- file.path(root, paste0(".", state$id, ".writer"))
        if (!file.exists(state$path) && !file.exists(state$reservation)) {
            break
        }
    }
    session$.audit_stream <- state
    state
}

.audit_write <- function(state, record) {
    previous <- Sys.umask("0077")
    on.exit(Sys.umask(previous), add = TRUE)
    dir.create(state$root, recursive = TRUE, showWarnings = FALSE,
               mode = "0700")
    if (!isTRUE(state$reserved)) {
        if (file.exists(state$path) ||
            !dir.create(state$reservation, mode = "0700")) {
            stop("audit stream reservation unavailable", call. = FALSE)
        }
        state$reserved <- TRUE
    }
    bytes <- charToRaw(paste0(jsonlite::toJSON(record, auto_unbox = TRUE,
                null = "null", na = "null", digits = NA), "\n"))
    # Preserve a torn tail as a malformed line, rather than joining new JSON to
    # it. The reader reports it; neither recovery nor append silently erases it.
    if (file.exists(state$path)) {
        size <- file.info(state$path)$size
    } else {
        size <- 0
    }
    if (is.finite(size) && size > 0) {
        con <- file(state$path, "rb")
        last <- tryCatch({
            seek(con, where = size - 1, origin = "start")
            readBin(con, "raw", 1L)
        }, finally = close(con))
        if (!identical(last, charToRaw("\n"))) {
            bytes <- c(charToRaw("\n"), bytes)
        }
    }
    con <- file(state$path, "ab")
    tryCatch({ writeBin(bytes, con) ; flush(con) }, finally = close(con))
    # Once bytes are committed, the stream file guards the identity. Keep a
    # reservation only when a writer has not yet completed its first write.
    if (!isTRUE(state$released)) {
        state$released <- unlink(state$reservation, recursive = TRUE) == 0L
    }
    TRUE
}

.audit_append <- function(state, type, ...) {
    if (is.null(state)) {
        return(invisible(FALSE))
    }
    now <- .audit_now()
    record <- c(list(version = 1L, type = type, ts = now, stream_id = state$id),
                list(...))
    ok <- tryCatch(suppressWarnings({
        if (state$lost > 0) {
            .audit_write(state, list(version = 1L, type = "gap", ts = now,
                                     stream_id = state$id,
                                     lost_records = state$lost,
                                     first_failure = state$first_failure,
                                     last_failure = state$last_failure))
            state$lost <- 0
        }
        .audit_write(state, record)
    }), error = function(e) FALSE)
    if (!isTRUE(ok)) {
        if (state$lost == 0) {
            state$first_failure <- now
        }
        state$lost <- state$lost + 1
        state$last_failure <- now
        .audit_notice()
    }
    invisible(isTRUE(ok))
}

.audit_safe <- function(expr) {
    tryCatch(suppressWarnings(expr), error = function(e) {
        .audit_notice()
        NULL
    })
}

# Scope the source supplied by the caller; never recover it from stale session
# fields. This also supports nested turns on the same session.
.audit_with_origin <- function(session, origin, expr) {
    previous <- session$audit_origin
    session$audit_origin <- origin
    on.exit(session$audit_origin <- previous, add = TRUE)
    force(expr)
}
