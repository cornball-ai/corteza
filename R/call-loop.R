# The call-mode agent's loop: hear, transcribe, think, speak.
#
# One participant in a call, built from four collaborators the loop
# does not own, so each can be faked in a test and swapped in a
# deployment:
#
#   media  the SFU leg: on_audio(callback), publish(samples, rate),
#          poll(timeout) -> events, identity (this participant's).
#          livekitr behind it in production (call_media_livekit()).
#   stt    function(samples, rate) -> text        (call_stt in production)
#   tts    function(text) -> list(samples, rate)   (call_tts)
#   brain  turn(text, speaker, send) -> list(turn_id, text, alive),
#          report(turn_id, heard)                 (voice_turn, voice_turn_report)
#
# Audio arrives per participant inside media$poll(); the detector in
# call-audio.R cuts it into utterances, which queue. One at a time, an
# utterance is transcribed and, when the policy says the agent should
# answer, handed to the brain. The brain streams text; whole sentences
# are synthesized and published as they complete, in short chunks with
# a poll between them, so a participant who starts talking over the
# agent is heard within a chunk. That is barge-in: publishing stops,
# the generation is cancelled through the brain's send contract, and
# the brain is told how much of the reply was heard so the room record
# matches what was said aloud.
#
# R is one thread, so nothing here is concurrent: transcription and
# synthesis block the loop, and audio that arrives meanwhile waits in
# livekitr's native queue. The loop keeps those blocks short (one
# utterance, one sentence) and polls between them.

# The published chunk length. Barge-in is noticed between chunks, so
# this bounds how long the agent keeps talking over someone.
CALL_PUBLISH_CHUNK_MS <- 200L
# The detector sees frames this long however the poll batched them.
CALL_VAD_FRAME_MS <- 20L
# After one person's turn, their next words within this many seconds
# are answered without being addressed, so a conversation continues.
CALL_FOLLOW_UP_S <- 20

# A loop over its collaborators. `opts`: `answer` ("addressed" or
# "always"; see call_should_answer), `names` (what the agent is called,
# for being addressed), `speaker` (function(identity) -> name), `vad`
# (arguments to vad_new), `clock` (seconds).
call_loop_new <- function(media, stt, tts, brain, opts = list()) {
    cl <- new.env(parent = emptyenv())
    cl$media <- media
    cl$stt <- stt
    cl$tts <- tts
    cl$brain <- brain
    cl$answer <- opts$answer %||% "addressed"
    if (!cl$answer %in% c("addressed", "always")) {
        stop("voice.call.answer must be \"addressed\" or \"always\"",
             call. = FALSE)
    }
    cl$names <- tolower(as.character(opts$names %||% character()))
    cl$speaker <- opts$speaker %||% call_speaker_name
    cl$clock <- opts$clock %||% function() as.numeric(Sys.time())
    cl$vad_opts <- opts$vad %||% list()
    cl$rate <- as.integer(opts$rate %||% CALL_AUDIO_RATE)
    cl$log <- opts$log %||% function(...) message("corteza call: ", ...)
    cl$vads <- new.env(parent = emptyenv())
    cl$participants <- character()
    cl$queue <- list()
    cl$speaking <- FALSE
    cl$barge_in <- NULL
    cl$last_turn <- NULL
    cl$turns <- 0L
    cl$stopped <- FALSE
    media$on_audio(function(pcm, info) .call_hear(cl, pcm, info))
    cl
}

# The audio callback: every participant's PCM, as it arrives.
.call_hear <- function(cl, pcm, info) {
    identity <- info$identity %||% "?"
    if (identical(identity, cl$media$identity)) {
        return(invisible(NULL))
    }
    pcm <- pcm_mono(pcm, info$channels %||% 1L)
    v <- cl$vads[[identity]]
    if (is.null(v)) {
        v <- do.call(vad_new, c(list(rate = cl$rate), cl$vad_opts))
        assign(identity, v, envir = cl$vads)
    }
    n <- as.integer(cl$rate * CALL_VAD_FRAME_MS / 1000)
    starts <- seq.int(1L, length(pcm), by = n)
    for (s in starts) {
        frame <- pcm[s:min(s + n - 1L, length(pcm))]
        for (ev in vad_feed(v, frame)) {
            if (identical(ev$type, "start") && cl$speaking) {
                # Someone started talking while the agent is: barge-in.
                cl$barge_in <- identity
            } else if (identical(ev$type, "end")) {
                cl$queue[[length(cl$queue) + 1L]] <- list(identity = identity,
                    samples = ev$samples, ms = ev$ms, at = cl$clock())
            }
        }
    }
    invisible(NULL)
}

# One iteration: poll the media (which runs the audio callback), take
# the room events, then answer the oldest utterance if there is one.
# Returns TRUE when it did some work beyond polling.
call_loop_step <- function(cl, timeout = 0.05) {
    events <- cl$media$poll(timeout)
    for (ev in events) {
        .call_room_event(cl, ev)
    }
    if (!length(cl$queue)) {
        return(FALSE)
    }
    utt <- cl$queue[[1L]]
    cl$queue <- cl$queue[-1L]
    .call_answer(cl, utt)
    TRUE
}

# Run steps until `until()` says stop, the media says the room ended,
# or call_loop_stop() was called.
call_loop_run <- function(cl, until = function() FALSE, timeout = 0.05) {
    while (!cl$stopped && !isTRUE(until())) {
        call_loop_step(cl, timeout)
    }
    invisible(cl)
}

call_loop_stop <- function(cl) {
    cl$stopped <- TRUE
    invisible(cl)
}

.call_room_event <- function(cl, ev) {
    type <- ev$type %||% ""
    id <- ev$identity %||% ev$participant %||% NULL
    if (identical(type, "participant_connected") && !is.null(id)) {
        cl$participants <- union(cl$participants, id)
    } else if (identical(type, "participant_disconnected") && !is.null(id)) {
        cl$participants <- setdiff(cl$participants, id)
        if (exists(id, envir = cl$vads, inherits = FALSE)) {
            rm(list = id, envir = cl$vads)
        }
    } else if (identical(type, "disconnected") ||
        (identical(type, "connection_state_changed") &&
            identical(ev$state %||% "", "disconnected"))) {
        cl$log("the room ended")
        cl$stopped <- TRUE
    }
    invisible(NULL)
}

# Who the humans in the call are: everyone heard from or announced,
# minus the agent.
call_humans <- function(cl) {
    setdiff(union(cl$participants, ls(cl$vads, all.names = TRUE)),
            cl$media$identity %||% character())
}

# A participant's name from a LiveKit identity. MatrixRTC identities are
# "@user:server:DEVICE"; the user's localpart is what people are called.
call_speaker_name <- function(identity) {
    m <- regmatches(identity, regexec("^@([^:]+):", identity))[[1L]]
    if (length(m) == 2L) {
        m[[2L]]
    } else {
        identity
    }
}

# Should the agent answer these words? With one other person in the
# call, always: it is a conversation. With several, when the agent is
# addressed by one of its names, or when the same person spoke to it
# within CALL_FOLLOW_UP_S, so a back-and-forth does not need the name
# in every sentence. `answer = "always"` answers everyone.
call_should_answer <- function(cl, text, identity) {
    if (identical(cl$answer, "always") || length(call_humans(cl)) <= 1L) {
        return(TRUE)
    }
    low <- tolower(text)
    for (nm in cl$names) {
        if (nzchar(nm) &&
            grepl(paste0("\\b", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", nm),
                         "\\b"),
                  low, perl = TRUE)) {
            return(TRUE)
        }
    }
    last <- cl$last_turn
    if (!is.null(last) && identical(last$identity, identity) &&
        cl$clock() - last$at <= CALL_FOLLOW_UP_S) {
        return(TRUE)
    }
    FALSE
}

# Transcribe an utterance and, if it is for the agent, answer it aloud.
.call_answer <- function(cl, utt) {
    text <- tryCatch(cl$stt(utt$samples, cl$rate), error = function(e) {
        cl$log("transcription failed: ", conditionMessage(e))
        NULL
    })
    if (is.null(text) || !nzchar(text)) {
        return(invisible(FALSE))
    }
    cl$log(cl$speaker(utt$identity), " said: ", text)
    if (!call_should_answer(cl, text, utt$identity)) {
        return(invisible(FALSE))
    }
    speaker <- if (length(call_humans(cl)) > 1L) cl$speaker(utt$identity)
    .call_speak_turn(cl, text, speaker, utt$identity)
    invisible(TRUE)
}

# Run one turn of the brain and say the reply as it comes.
.call_speak_turn <- function(cl, text, speaker, identity) {
    say <- .call_sayer(cl)
    cl$speaking <- TRUE
    cl$barge_in <- NULL
    on.exit({
        cl$speaking <- FALSE
    }, add = TRUE)
    turn <- tryCatch(cl$brain$turn(text, speaker, say$send),
                     error = function(e) {
        cl$log("the turn failed: ", conditionMessage(e))
        NULL
    })
    if (is.null(turn)) {
        return(invisible(NULL))
    }
    if (turn$alive) {
        # Generation ended normally: say what is left.
        say$flush()
    }
    cl$turns <- cl$turns + 1L
    cl$last_turn <- list(identity = identity, at = cl$clock())
    heard <- say$heard(turn$text)
    if (!is.null(cl$barge_in)) {
        cl$log("interrupted by ", cl$speaker(cl$barge_in), " after ", heard,
               " of ", nchar(turn$text, type = "chars"), " characters")
    }
    if (heard < nchar(turn$text, type = "chars")) {
        tryCatch(cl$brain$report(turn$turn_id, heard), error = function(e) {
            cl$log("could not report the turn: ", conditionMessage(e))
        })
    }
    invisible(turn)
}

# The sayer for one turn: collects deltas, synthesizes and publishes
# each sentence as soon as it is complete, and knows how much was heard.
#
# `send(delta)` is the brain's delta sink: TRUE while the peer listens,
# FALSE once a barge-in stopped playback (which cancels generation).
# `flush()` says whatever is left when generation ends. `heard(text)` is
# the count of code points of the final reply that were played, by the
# midpoint rule on the sentence under way when playback stopped.
.call_sayer <- function(cl) {
    pending <- ""
    spoken <- 0L # code points published whole
    fraction <- 0 # of the sentence under way, when stopped
    stopped <- FALSE
    said_to <- 0L # end offset in the running text of the last spoken piece
    speak <- function(piece) {
        if (stopped) {
            return(FALSE)
        }
        audio <- tryCatch(cl$tts(piece$text), error = function(e) {
            cl$log("synthesis failed: ", conditionMessage(e))
            NULL
        })
        if (is.null(audio) || !length(audio$samples)) {
            # Unsaid, but the text stands: count it as heard so the
            # record is not cut for a synthesis fault.
            spoken <<- piece$end
            return(TRUE)
        }
        n <- as.integer(audio$rate * CALL_PUBLISH_CHUNK_MS / 1000)
        total <- length(audio$samples)
        done <- 0L
        while (done < total) {
            chunk <- audio$samples[(done + 1L):min(done + n, total)]
            cl$media$publish(chunk, audio$rate)
            done <- done + length(chunk)
            cl$media$poll(0)
            if (!is.null(cl$barge_in)) {
                stopped <<- TRUE
                fraction <<- done / total
                return(FALSE)
            }
        }
        spoken <<- piece$end
        TRUE
    }
    # Sentences complete in `pending` so far: all but the last piece,
    # which may still be growing.
    say_complete <- function() {
        pieces <- voice_sentences(pending)
        if (length(pieces) < 2L) {
            return(TRUE)
        }
        for (p in pieces[-length(pieces)]) {
            if (!speak(list(text = p$text, end = said_to + p$end))) {
                return(FALSE)
            }
        }
        last <- pieces[[length(pieces)]]
        said_to <<- said_to + pieces[[length(pieces) - 1L]]$end
        pending <<- last$text
        TRUE
    }
    list(
         send = function(delta) {
        if (stopped) {
            return(FALSE)
        }
        pending <<- paste0(pending, delta)
        say_complete()
    },
         flush = function() {
        if (stopped || !nzchar(trimws(pending))) {
            return(invisible(NULL))
        }
        for (p in voice_sentences(pending)) {
            if (!speak(list(text = p$text, end = said_to + p$end))) {
                break
            }
        }
        pending <<- ""
        invisible(NULL)
    },
         heard = function(text) {
        total <- nchar(text, type = "chars")
        if (!stopped) {
            return(total)
        }
        if (fraction > 0.5) {
            # The sentence under way counts: find its end in the text.
            pieces <- voice_sentences(text)
            ends <- vapply(pieces, function(p) p$end, 1L)
            nxt <- ends[ends > spoken]
            return(if (length(nxt)) min(nxt) else total)
        }
        min(spoken, total)
    })
}
