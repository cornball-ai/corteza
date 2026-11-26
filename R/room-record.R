# A room's conversation, kept for its monitor.
#
# A room's monitor rules on what the rules in code leave open
# (R/supervisor.R). It used to be told the task in one line: the message
# that started the turn, or a job's task. A "yes" two messages back, or a
# "don't touch the tests" before the task, never reached it. It now reads
# the conversation, as the terminal's reviewer does (R/review.R).
#
# The archive's queue (session$transcript, R/archive.R) is no source:
# a flush and /clear empty it, and a restart refills it with the
# backfill's window. So each conversation (R/room-conversation.R) keeps
# a record of its own, `conversation-<id>.jsonl` in the room's journal
# folder, one line per event, appended where the queue is fed
# (bot_transcript_add()) and never emptied. /clear starts the next
# conversation's record. Retention removes a record with the journal's
# other files (retention_journal_files()).
#
# Each line says who and what: the event, the sender's Matrix id as the
# event gives it (never read from the text), whether it came live or by
# the restart's backfill, and its kind: a message, a peer's delegated
# request, or a post of the bot's own. A message keeps its text as sent;
# what the bot made of it, an attachment's receipt, is a note of its own.
#
# The first line says how the record begins: `conversation` when it
# begins with the conversation (a /clear, or a session that holds
# nothing from before), `partial` otherwise. A later break is a `gap`
# line. The monitor rules only on a whole conversation
# (bot_record_conversation()): anything else goes to a person.

# Where conversation `conversation` of session `key` is kept. NULL for an
# id corteza did not make (job_new_id()).
bot_record_path <- function(cfg, key, conversation) {
    if (!is.character(key) || length(key) != 1L || is.na(key) ||
        !nzchar(key) || !is.character(conversation) ||
        length(conversation) != 1L || is.na(conversation) ||
        !grepl("^[A-Za-z0-9-]+$", conversation)) {
        return(NULL)
    }
    file.path(bot_journal_dir(cfg, key),
              paste0("conversation-", conversation, ".jsonl"))
}

# Records an append failed to write, by path, in this process. The
# session writes a gap line at its next append; until one is written,
# the record on disk does not show the break, so the reader asks here.
.bot_record_failed <- new.env(parent = emptyenv())

# Keep the record of the conversation `session` holds. `start` is how a
# record with no file yet begins: "conversation" or "partial".
# `cleared` marks a conversation /clear started, whose beginning is known.
bot_record_attach <- function(session, cfg, key, start, cleared = FALSE) {
    path <- bot_record_path(cfg, key, session_conversation(session))
    if (is.null(path)) {
        session$review_record <- NULL
        return(invisible(NULL))
    }
    rec <- list(path = path, start = start, cleared = isTRUE(cleared),
                started = file.exists(path), ids = character(), last = NULL)
    if (rec$started) {
        held <- bot_record_read(path)
        rec$ids <- held$ids %||% character()
        if (length(rec$ids)) {
            rec$last <- rec$ids[[length(rec$ids)]]
        }
    }
    session$review_record <- rec
    invisible(rec)
}

# A line for an event. `kind` is "message", "request" or "post" (the
# bot's own). `event` is the transport's message, for what someone else
# sent: its sender and its text as sent are taken from it, and `text` is
# kept only for a post. `note` is what the bot made of a message. A
# message with no `event` names no sender, and the monitor will not
# judge by its record.
bot_record_event_line <- function(event_id, text, kind, event = NULL,
                                  note = NULL, via = "live") {
    line <- list(type = "event", event = as.character(event_id), kind = kind,
                 via = via, text = text)
    if (!is.null(event)) {
        line$sender <- as.character(event$sender %||% "")
        line$text <- as.character(event$body %||% "")
        at <- suppressWarnings(as.numeric(event$ts))
        if (length(at) == 1L && is.finite(at)) {
            line$at <- at
        }
    }
    if (is.character(note) && length(note) == 1L && !is.na(note) &&
        nzchar(note)) {
        line$note <- note
    }
    line
}

# Append `line` to the session's record. The record's first line goes in
# with it when the record has none yet. A failed write is remembered, and
# the next append writes a gap line ahead of its own.
bot_record_write <- function(session, line) {
    rec <- session$review_record
    if (!is.list(rec) || is.null(rec$path)) {
        return(invisible(FALSE))
    }
    if (identical(line$type, "event") && line$event %in% rec$ids) {
        return(invisible(FALSE))
    }
    lines <- list()
    if (!isTRUE(rec$started)) {
        start <- rec$start
        # The restart's backfill brings what was said before this
        # process, and its window need not reach back to the start.
        if (identical(line$via, "backfill") && !isTRUE(rec$cleared)) {
            start <- "partial"
        }
        lines <- c(lines, list(list(type = "start", start = start,
                                    conversation = session_conversation(session),
                                    at = job_now())))
    }
    if (exists(rec$path, envir = .bot_record_failed, inherits = FALSE)) {
        lines <- c(lines, list(list(type = "gap", at = job_now(),
                                    why = "a line could not be written")))
    }
    lines <- c(lines, list(line))
    ok <- tryCatch({
        bot_record_append(rec$path, lines)
        TRUE
    }, error = function(e) {
        log_event("room_record_unwritten", level = "warn",
                  error = conditionMessage(e))
        FALSE
    })
    if (ok) {
        rec$started <- TRUE
        if (exists(rec$path, envir = .bot_record_failed, inherits = FALSE)) {
            rm(list = rec$path, envir = .bot_record_failed)
        }
        if (identical(line$type, "event")) {
            rec$ids <- c(rec$ids, line$event)
        }
    } else {
        assign(rec$path, TRUE, envir = .bot_record_failed)
    }
    session$review_record <- rec
    invisible(ok)
}

bot_record_append <- function(path, lines) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    text <- vapply(lines, function(l) {
        as.character(jsonlite::toJSON(l, auto_unbox = TRUE, null = "null",
                                      digits = NA))
    }, character(1))
    con <- file(path, open = "ab")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(enc2utf8(paste0(paste(text, collapse = "\n"), "\n"))),
             con)
    invisible(TRUE)
}

# The conversation has a break the record cannot fill: say so with a gap
# line, or, before the record has its first line, begin it `partial`.
bot_record_break <- function(session, why) {
    rec <- session$review_record
    if (!is.list(rec) || is.null(rec$path)) {
        return(invisible(FALSE))
    }
    if (!isTRUE(rec$started)) {
        rec$start <- "partial"
        session$review_record <- rec
        return(invisible(TRUE))
    }
    bot_record_write(session, list(type = "gap", at = job_now(), why = why))
}

# What a restart owes the records it finds (Codex, #286). A record from
# before this process holds what the bot saw up to its last event, and
# what was said while the bot was down reaches it only through the
# restart's backfill. So a restart keeps, on its session registry, each
# room's backfill window, and a record from before it continues whole
# only when its room's window reaches back to the record's last event.
# Any other gets a gap line when a session takes it: the rooms could not
# be listed, the room's history could not be read, the backfill failed
# partway, or the window does not reach back far enough, as for a thread
# quiet for longer than the window. A registry no restart made has none
# (bot_poll() building its own, which its cursor carries).
#
# Kept as an attribute of the registry, which is an environment, so it
# is the registry's own and never one of its sessions.
bot_record_restart <- function(registry, create = FALSE) {
    if (!is.environment(registry)) {
        return(NULL)
    }
    state <- attr(registry, "record_restart", exact = TRUE)
    if (is.null(state) && isTRUE(create)) {
        state <- new.env(parent = emptyenv())
        state$windows <- list()
        state$checked <- new.env(parent = emptyenv())
        attr(registry, "record_restart") <- state
    }
    state
}

# Check the record `session` took, once a process: it continues whole,
# or it gets a gap line now, before anything else is written to it.
bot_record_restart_check <- function(session, restart, room_id) {
    rec <- session$review_record
    if (!is.environment(restart) || !is.list(rec) || is.null(rec$path) ||
        exists(rec$path, envir = restart$checked, inherits = FALSE)) {
        return(invisible(TRUE))
    }
    assign(rec$path, TRUE, envir = restart$checked)
    # A record with no file yet begins in this process.
    if (!isTRUE(rec$started)) {
        return(invisible(TRUE))
    }
    window <- restart$windows[[room_id]]
    if (!is.null(window) && !is.null(rec$last) && rec$last %in% window) {
        return(invisible(TRUE))
    }
    why <- if (is.null(window)) {
        "the bot restarted, and could not read the room's history"
    } else {
        paste("the bot was not running, and the backfill did not reach",
              "back to the last event on record")
    }
    bot_record_write(session, list(type = "gap", at = job_now(), why = why))
    rec <- session$review_record
    rec$gapped <- TRUE
    session$review_record <- rec
    invisible(FALSE)
}

# An event the restart's backfill found: message `i` of the window whose
# event ids are `ids`, in order. One already on record is not written
# again, nor one older than the last on record, which would go in out of
# order. A window that does not reach back to the last event on record
# may have missed what came between, which a gap line says first; in a
# restart, the check above said it when the session took the record.
bot_record_backfill <- function(session, ids, i, line) {
    rec <- session$review_record
    if (!is.list(rec) || is.null(rec$path) || line$event %in% rec$ids) {
        return(invisible(FALSE))
    }
    if (!is.null(rec$last)) {
        j <- match(rec$last, ids)
        if (!is.na(j) && i <= j) {
            return(invisible(FALSE))
        }
        if (is.na(j) && !isTRUE(rec$gapped)) {
            rec$gapped <- TRUE
            session$review_record <- rec
            bot_record_write(session, list(type = "gap", at = job_now(),
                    why = paste("the bot was not running, and the backfill",
                                "did not reach back to the last event on record")))
        }
    }
    bot_record_write(session, line)
}

# The record at `path`: its lines, the number it could not read, and the
# event ids on it, in order. NULL when the file cannot be read.
bot_record_read <- function(path) {
    raw <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE),
                    error = function(e) NULL)
    if (is.null(raw)) {
        return(NULL)
    }
    parsed <- lapply(raw, function(l) {
        x <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                      error = function(e) NULL)
        if (is.list(x) && is.character(x$type) && length(x$type) == 1L) {
            x
        }
    })
    ok <- !vapply(parsed, is.null, logical(1))
    lines <- parsed[ok]
    ids <- vapply(lines, function(x) {
        if (identical(x$type, "event") && is.character(x$event) &&
                  length(x$event) == 1L) {
            x$event
        } else {
            NA_character_
        }
    }, character(1))
    list(lines = lines, bad = sum(!ok), ids = ids[!is.na(ids)])
}

# The conversation as the monitor reads it, a line each, oldest first:
# the lines of the people who may approve (`approvers`, bot_approvers())
# as "Person: ...", anyone else's, another bot's or a peer's request
# included, as "Other (<id>): ...", and the bot's posts as "Worker: ...",
# whole, before the line that follows them. What the bot made of a
# message follows it in brackets, marked as the bot's.
#
# Returns list(lines, why). `why` says why the monitor cannot judge by
# it, and is NULL when it can. It cannot when nobody may approve, when
# the record is missing or holds a line it cannot read, when it does not
# begin with the conversation or has a break, when it holds no line from
# a person, or when it runs past `budget` characters. Nothing is left
# out to make it fit.
bot_record_conversation <- function(path, approvers, budget = 100000L) {
    unread <- function(why) list(lines = character(), why = why)
    if (!length(approvers)) {
        return(unread("nobody in this room may approve its calls"))
    }
    if (is.null(path) || !file.exists(path)) {
        return(unread("there is no record of this conversation to judge it by"))
    }
    if (exists(path, envir = .bot_record_failed, inherits = FALSE)) {
        return(unread("a line of this conversation could not be written to its record"))
    }
    held <- bot_record_read(path)
    if (is.null(held)) {
        return(unread("the record of this conversation could not be read"))
    }
    if (held$bad > 0L) {
        noun <- "lines"
        if (held$bad == 1L) {
            noun <- "line"
        }
        return(unread(sprintf("%d %s of the record could not be read",
                              held$bad, noun)))
    }
    if (!length(held$lines) || !identical(held$lines[[1L]]$type, "start")) {
        return(unread("the record does not say how it begins"))
    }
    if (!identical(held$lines[[1L]]$start, "conversation")) {
        return(unread(paste("the record does not reach back to the start of",
                            "the conversation; /clear starts one that does")))
    }
    one <- function(x) {
        is.character(x) && length(x) == 1L && !is.na(x)
    }
    lines <- character()
    posts <- character()
    person <- FALSE
    for (x in held$lines[-1L]) {
        if (identical(x$type, "gap")) {
            why <- "lines are missing"
            if (one(x$why)) {
                why <- x$why
            }
            return(unread(paste("the record has a break:", why)))
        }
        if (!identical(x$type, "event") || !one(x$text) ||
            !one(x$kind) || !x$kind %in% c("message", "request", "post")) {
            return(unread("a line of the record could not be read"))
        }
        if (identical(x$kind, "post")) {
            posts <- c(posts, x$text)
            next
        }
        if (!one(x$sender) || !nzchar(x$sender)) {
            return(unread("a line of the record names no sender"))
        }
        said <- .sanitize_inline(x$text, max_chars = budget)
        if (one(x$note) && nzchar(x$note)) {
            said <- paste0(said, " [the bot's note on it, not the sender's words: ",
                           .sanitize_inline(x$note, max_chars = budget), "]")
        }
        if (!nzchar(said)) {
            next
        }
        if (length(posts)) {
            lines <- c(lines, paste("Worker:",
                                    .sanitize_inline(paste(posts, collapse = "\n"),
                        max_chars = budget)))
            posts <- character()
        }
        if (identical(x$kind, "message") && x$sender %in% approvers) {
            person <- TRUE
            lines <- c(lines, paste("Person:", said))
        } else {
            lines <- c(lines, sprintf("Other (%s): %s",
                                      .sanitize_inline(x$sender, max_chars = 200L), said))
        }
    }
    if (!person) {
        return(unread("the record holds no message from the people who may approve"))
    }
    if (sum(nchar(lines)) > budget) {
        return(unread(.review_record_limit_reason(budget)))
    }
    list(lines = lines, why = NULL)
}

# Who may approve in `room_id` now: membership is read fresh, as for an
# approval prompt (bot_reaction_approval()). Nobody when it cannot be read.
bot_room_approvers <- function(cfg, room_id) {
    chat <- tryCatch(bot_chat_client(cfg, save_cursor = FALSE),
                     error = function(e) NULL)
    if (is.null(chat)) {
        return(character())
    }
    self_id <- tryCatch(chat.api::chat_whoami(chat)$id,
                        error = function(e) cfg$user_id)
    members <- tryCatch(chat.api::chat_members(chat, room_id),
                        error = function(e) NULL)
    bot_approvers(cfg, members, bot_known_bots(cfg, self_id))
}

# The conversation a job's request is judged by: the one that asked for
# it, read when the request comes, so what was said since is in it. A
# hand-off is judged by the asking room's. A job from before 0.8.1 names
# none.
bot_job_conversation <- function(cfg, job, budget = 100000L) {
    origin <- job$origin %||% list()
    from <- origin$from
    if (is.list(from)) {
        key <- from$session_key
        conversation <- from$conversation
        room <- from$room
    } else {
        key <- origin$session_key
        conversation <- origin$conversation
        room <- origin$room
    }
    path <- bot_record_path(cfg, key, conversation)
    if (is.null(path) || is.null(room)) {
        return(list(lines = character(),
                    why = "the job does not say which conversation asked for it"))
    }
    bot_record_conversation(path, bot_room_approvers(cfg, room), budget)
}

# Earlier supervised calls in a room session's own turns, one line each,
# newest last, as job_monitor_earlier() gives a job's.
bot_supervised_earlier <- function(s) {
    vapply(s$.supervised_log %||% list(), supervisor_earlier_line, character(1))
}
