# AgentVoice turns: Converse streams a reply, ReportTurn reconciles the
# room record with what was actually heard.
#
# THE DELTA STREAM IS THE TURN'S TEXT. The client synthesises speech
# from the deltas this file sends, and ReportTurn's text_heard counts
# code points of that stream concatenated in order -- so the record kept
# for a turn is built from the deltas as they are sent, not from any
# other view of the reply. Whatever divergence exists between the two is
# invisible to the user, who only ever heard the stream.
#
# POST AT TURN END, EDIT AT REPORT. The reply is posted to the room when
# generation finishes, and the ReportTurn truncation is an edit down to
# the heard prefix. A session that dies without reporting leaves the
# full text standing, which is the documented fallback (the missing
# report is itself the diagnostic); the alternative -- post only at
# ReportTurn -- degrades to a partially-heard reply vanishing from
# history entirely.

# The spoken-register instruction appended to a voice session's system
# prompt. The reply text is used twice -- synthesized aloud verbatim
# AND posted to the room -- and the client is contractually barred from
# altering it (text_heard and the truncation edit count code points of
# the reply AS THE AGENT HOLDS IT), so speakable text has to be born
# speakable. First live run without this: a markdown table read aloud
# as "asterisk five bend forward". Override via voice.speech_style.
.VOICE_SPEECH_REGISTER <- paste0(
    "Voice register: your reply is read aloud verbatim by a speech ",
    "synthesizer, and stored in the room as the same plain text. Write ",
    "plain spoken prose only. No markdown syntax of any kind: no ",
    "asterisks, underscores, pipes, backticks, code fences, headings, ",
    "bullet or numbered list markers, and no tables. Say numbers, ",
    "scores, times, and abbreviations the way they are spoken aloud. ",
    "Keep sentences short. When content is inherently visual, describe ",
    "it in words instead.")

# The system prompt for a voice session: the room system plus the
# spoken-register instruction, or a configured override of that
# instruction (voice.speech_style, a non-empty string).
voice_speech_system <- function(system, style = NULL) {
    if (!is.character(style) || length(style) != 1L || is.na(style) ||
        !nzchar(style)) {
        style <- .VOICE_SPEECH_REGISTER
    }
    if (!is.character(system) || length(system) != 1L || is.na(system) ||
        !nzchar(system)) {
        return(style)
    }
    paste(system, style, sep = "\n\n")
}

# One R session per room per voice process, built on first use. Fresh
# rather than shared with the poll loop: this is a separate process, and
# in-memory session state does not cross processes. What DOES cross is
# the room itself -- the session is seeded from room history on
# creation, so the first voice turn stands on the conversation so far
# (including replies this or any other process posted), and a restart
# recovers everything the room holds.
#
# Voice sessions speak: the system prompt gains the spoken-register
# instruction here, at birth, so every turn of the session generates
# speakable text. Room-poll sessions are untouched -- markdown stays
# the register for typed chat.
voice_room_session <- function(state, room_id) {
    s <- state$rooms[[room_id]]
    if (is.null(s)) {
        s <- bot_new_session(state$cfg_fn(), room_id = room_id)
        s$system <- voice_speech_system(s$system,
                                        state$cfg_fn()$voice$speech_style)
        .voice_backfill(state, s, room_id)
        assign(room_id, s, envir = state$rooms)
    }
    s
}

# The tail of the room's history, as turns. Mirrors the shape
# bot_backfill_sessions() builds for the poll loop, minus the
# sender-attribution machinery: v1 live voice is one human and one bot
# per room, so there is nobody to disambiguate.
.voice_backfill <- function(state, s, room_id) {
    msgs <- tryCatch(state$hooks$history(room_id), error = function(e) {
        message("corteza voice: no history backfill for ", room_id, ": ",
                conditionMessage(e))
        NULL
    })
    added <- 0L
    for (m in msgs) {
        # Ordinary messages only: notices and emotes carry no turn.
        if (!identical(m$kind, "message")) {
            next
        }
        body <- m$body
        if (is.null(body) || !nzchar(body)) {
            next
        }
        if (isTRUE(m$self)) {
            role <- "assistant"
        } else {
            role <- "user"
        }
        s$history <- c(s$history %||% list(),
                       list(list(role = role, content = body)))
        added <- added + 1L
    }
    invisible(added)
}

# Default run_turn hook: the shared turn() machinery, deltas forwarded
# through the session seam that turn() passes to llm.api.
.voice_run_turn <- function(state, room_id, text, on_delta) {
    s <- voice_room_session(state, room_id)
    s$on_delta <- on_delta
    on.exit(s$on_delta <- NULL, add = TRUE)
    bot_run_turn_in_cwd(text, s)
}

# Converse: one turn, streamed. TurnStart with the turn id must go out
# before any delta -- the client cannot report a turn it has no id for.
.voice_converse <- function(state, ev) {
    req <- RProtoBuf::read(.voice_type("ConverseRequest"), ev$request)
    rec <- voice_session_auth(state, req$session_id, ev$metadata)
    text <- req$text
    if (!nzchar(text)) {
        voice_refuse("INVALID_ARGUMENT", "text is required")
    }
    turn_id <- voice_id(state)

    start <- .voice_type("ConverseEvent")$new()
    start$start <- .voice_type("TurnStart")$new(turn_id = turn_id)
    grpc::grpc_send(ev, start)

    relay <- .voice_stream_cb(function(delta) {
        m <- .voice_type("ConverseEvent")$new()
        m$delta <- .voice_type("TextDelta")$new(text = delta)
        grpc::grpc_send(ev, m)
    }, state$hooks$cancel)
    reply <- state$hooks$run_turn(state, rec$room_id, text, relay$fun)

    # A provider (or an llm.api too old for on_delta) that streamed
    # nothing still produced a reply. One delta carrying all of it keeps
    # the contract -- the concatenated stream IS the turn text -- rather
    # than a silent stream followed by a room post from nowhere.
    if (relay$empty() && is.character(reply) && nzchar(reply)) {
        relay$fun(reply)
    }
    full <- relay$text()

    done <- .voice_type("ConverseEvent")$new()
    done$end <- .voice_type("TurnEnd")$new()
    if (relay$alive()) {
        try(grpc::grpc_send(ev, done), silent = TRUE)
    }
    try(grpc::grpc_finish(ev), silent = TRUE)

    # Post the full reply now (see header). A post that fails leaves
    # event_id NULL; ReportTurn then has nothing to edit and says so.
    event_id <- tryCatch(state$hooks$post(rec$room_id, full),
                         error = function(e) {
        message("corteza voice: could not post reply ", "to ", rec$room_id,
                ": ", conditionMessage(e))
        NULL
    })
    assign(turn_id,
           list(text = full, event_id = event_id, stored = NULL),
           envir = rec$turns)
    invisible(NULL)
}

# The delta relay for one Converse call. Deltas append (unlike
# provisional transcripts, which replace), so granularity is free:
# whatever llm.api hands over goes out as is. The buffer, not the
# stream, is the turn's record -- text() is what gets posted.
#
# A send that fails means the peer hung up, and the relay CANCELS the
# generation right there (the hook raises llm_cancelled, which
# agent()'s stream loop catches and turns into an immediate return with
# the partial reply). This is the barge-in latency fix: the server is
# single-threaded, so a generation left running would make the user's
# next turn wait out a tail nobody is listening to, at full token
# price. The failing delta is buffered BEFORE the cancel is raised, so
# the room record keeps everything that was generated.
#
# `cancel` may not return (the default raises); a test hook that does
# return is also fine -- the relay just goes quiet either way.
.voice_stream_cb <- function(send, cancel) {
    buf <- character()
    alive <- TRUE
    list(fun = function(delta) {
        buf[[length(buf) + 1L]] <<- delta
        if (!alive) {
            return(invisible(NULL))
        }
        ok <- tryCatch(send(delta), error = function(e) FALSE)
        if (!isTRUE(ok)) {
            alive <<- FALSE
            cancel()
        }
        invisible(NULL)
    },
         alive = function() alive,
         empty = function() length(buf) == 0L,
         text = function() paste(buf, collapse = ""))
}

# ReportTurn: truncate the stored reply to what was heard.
.voice_report <- function(state, ev) {
    req <- RProtoBuf::read(.voice_type("ReportTurnRequest"), ev$request)
    rec <- voice_session_auth(state, req$session_id, ev$metadata)
    turn <- rec$turns[[req$turn_id]]
    if (is.null(turn)) {
        voice_refuse("NOT_FOUND", "no such turn: %s", req$turn_id)
    }
    # Presence, not value: explicit 0 is a legitimate report (barge-in
    # before the first word), absence is a client that said nothing --
    # reading it as 0 would erase a fully-heard reply on its say-so.
    if (!req$has("text_heard")) {
        voice_refuse("INVALID_ARGUMENT",
                     paste0("text_heard is required; report 0 explicitly ",
                            "if nothing was heard"))
    }
    if (!is.null(turn$stored)) {
        # Reported already. Idempotent answer, no second edit: the first
        # report decided what the room holds.
        resp <- .voice_type("ReportTurnResponse")$new()
        resp$stored_text <- turn$stored
        grpc::grpc_reply(ev, resp)
        return(invisible(NULL))
    }
    # A turn whose post failed has no room record AT ALL, so no report
    # against it can be honoured -- including a fully-heard one, where
    # answering OK with stored_text would claim a message the history
    # does not hold. The check sits before the truncation math on
    # purpose: it is about the post, not about whether an edit is due.
    if (is.null(turn$event_id)) {
        voice_refuse("INTERNAL",
                     paste0("the reply was never posted to the room, so ",
                            "there is nothing to report against"))
    }
    heard <- as.numeric(req$text_heard)
    if (is.na(heard)) {
        voice_refuse("INVALID_ARGUMENT", "text_heard is not a number")
    }
    stored <- voice_truncate(turn$text, heard)
    if (!identical(stored, turn$text)) {
        # If the edit fails the room still shows the full text, and
        # answering OK would hand the client a stored_text the history
        # does not hold -- the exact mismatch this RPC exists to
        # prevent. Refuse instead; the client may retry.
        tryCatch(state$hooks$edit(rec$room_id, turn$event_id, stored),
                 error = function(e) {
            voice_refuse("UNAVAILABLE",
                         "could not edit the room record: %s",
                         conditionMessage(e))
        })
    }
    turn$stored <- stored
    assign(req$turn_id, turn, envir = rec$turns)
    resp <- .voice_type("ReportTurnResponse")$new()
    resp$stored_text <- stored
    grpc::grpc_reply(ev, resp)
    invisible(NULL)
}

# The first `heard` Unicode code points, tidied back to a word boundary
# when the cut falls mid-word. R's substr() counts characters, which on
# a UTF-8 string are code points -- the one unit both ends can produce
# exactly (the proto pins this).
#
# `heard` stays numeric: the wire type is uint32, whose ceiling
# (4294967295) is a VALID value that as.integer() would turn into NA.
# Every count at or past the text's length means "heard it all".
voice_truncate <- function(text, heard) {
    text <- enc2utf8(text)
    heard <- as.numeric(heard)
    total <- nchar(text, type = "chars")
    if (heard >= total) {
        return(text)
    }
    if (heard <= 0L) {
        return("")
    }
    prefix <- substr(text, 1L, heard)
    boundary <- substr(text, heard + 1L, heard + 1L)
    last <- substr(prefix, heard, heard)
    if (!grepl("[[:space:]]", boundary) && !grepl("[[:space:]]", last)) {
        # Cut mid-word: drop the partial word rather than storing half
        # of one the user never heard finished.
        prefix <- sub("[^[:space:]]+$", "", prefix)
    }
    sub("[[:space:]]+$", "", prefix)
}
