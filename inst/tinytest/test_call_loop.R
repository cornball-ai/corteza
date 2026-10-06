library(tinytest)

# The call loop (R/call-loop.R) over faked collaborators: utterances
# are transcribed and answered aloud a sentence at a time, barge-in
# stops the agent and trims the record, and the policy decides whom to
# answer.

rate <- corteza:::CALL_AUDIO_RATE
ms_of <- function(ms) as.integer(round(rate * ms / 1000))
tone <- function(ms, dbfs = -20) {
    amp <- 32768 * 10^(dbfs / 20) * sqrt(2)
    as.integer(round(amp * sin(2 * pi * 220 * seq_len(ms_of(ms)) / rate)))
}
quiet <- function(ms) {
    set.seed(ms)
    as.integer(round(rnorm(ms_of(ms), sd = 32768 * 10^(-60 / 20))))
}
# An utterance as the SFU would deliver it: quiet, speech, quiet.
utterance <- function(ms = 1000) c(quiet(400), tone(ms), quiet(900))

# A fake SFU leg. `incoming` is a list of what the next polls deliver:
# list(audio = identity, pcm = ...) runs the audio callback, list(event
# = list(...)) is a room event. `publish` records chunks and can be told
# to inject audio after the n-th chunk, which is how a barge-in is
# staged.
fake_media <- function(self = "@bot:ex:DEV") {
    m <- new.env()
    m$identity <- self
    m$incoming <- list()
    m$published <- list()
    m$cb <- NULL
    m$polls <- 0L
    m$barge_after <- NA_integer_
    m$barge_from <- "@ann:ex:PHONE"
    m$on_audio <- function(callback) m$cb <<- callback
    m$poll <- function(timeout) {
        m$polls <- m$polls + 1L
        events <- list()
        # Deliver one batch per poll, in order.
        if (length(m$incoming)) {
            item <- m$incoming[[1L]]
            m$incoming <- m$incoming[-1L]
            if (!is.null(item$audio)) {
                m$cb(item$pcm, list(identity = item$audio, channels = 1L,
                                    sample_rate = rate))
            } else if (!is.null(item$event)) {
                events[[1L]] <- item$event
            }
        }
        events
    }
    m$publish <- function(samples, rate) {
        m$published[[length(m$published) + 1L]] <- list(n = length(samples),
                                                        rate = rate)
        if (!is.na(m$barge_after) && length(m$published) == m$barge_after) {
            # The human starts talking: 400 ms of sound arrives at the
            # next poll, enough to open an utterance.
            m$incoming <- c(list(list(audio = m$barge_from, pcm = tone(400))),
                            m$incoming)
        }
        invisible(NULL)
    }
    m
}
deliver <- function(m, identity, pcm, batch_ms = 100) {
    n <- ms_of(batch_ms)
    starts <- seq.int(1L, length(pcm), by = n)
    for (s in starts) {
        m$incoming[[length(m$incoming) + 1L]] <- list(audio = identity,
                                                      pcm = pcm[s:min(s + n - 1L, length(pcm))])
    }
}
drain <- function(cl, m, extra = 5L) {
    while (length(m$incoming) || length(cl$queue)) {
        corteza:::call_loop_step(cl, 0)
    }
    for (i in seq_len(extra)) corteza:::call_loop_step(cl, 0)
}

# A fake transcriber keyed by utterance length, a synthesizer that makes
# 100 ms of audio per character at 24 kHz, and a brain that streams the
# scripted deltas until a send refuses.
fake_stt <- function(text = "what time is it") {
    calls <- new.env()
    calls$n <- 0L
    calls$lengths <- integer()
    list(fn = function(samples, rate) {
        calls$n <- calls$n + 1L
        calls$lengths <- c(calls$lengths, length(samples))
        text
    }, calls = calls)
}
fake_tts <- function() {
    log <- new.env()
    log$texts <- character()
    list(fn = function(text) {
        log$texts <- c(log$texts, text)
        list(samples = rep(1000L, 2400L * nchar(text)), rate = 24000L)
    }, log = log)
}
fake_brain <- function(deltas) {
    log <- new.env()
    log$turns <- list()
    log$reports <- list()
    list(turn = function(text, speaker, send) {
        log$turns[[length(log$turns) + 1L]] <- list(text = text, speaker = speaker)
        got <- character()
        alive <- TRUE
        for (d in deltas) {
            got <- c(got, d)
            if (!isTRUE(send(d))) {
                alive <- FALSE
                break
            }
        }
        list(turn_id = sprintf("t%d", length(log$turns)), text = paste(got, collapse = ""),
             alive = alive)
    }, report = function(turn_id, heard) {
        log$reports[[length(log$reports) + 1L]] <- list(turn_id = turn_id, heard = heard)
        "stored"
    }, log = log)
}
quiet_log <- function(...) invisible(NULL)

# ---- one human, one utterance, a spoken reply ----
local({
    m <- fake_media()
    stt <- fake_stt()
    tts <- fake_tts()
    brain <- fake_brain(c("It is ", "three o'clock. ", "Time for ", "tea."))
    cl <- corteza:::call_loop_new(m, stt$fn, tts$fn, brain, list(log = quiet_log))
    deliver(m, "@ann:ex:PHONE", utterance(1000))
    drain(cl, m)
    # The utterance reached the transcriber whole: speech plus the
    # detector's preroll and closing quiet.
    expect_identical(stt$calls$n, 1L)
    expect_true(stt$calls$lengths[[1L]] >= ms_of(1000 + 100 + 700) - ms_of(60))
    # One human: no speaker name, and the policy did not need one.
    expect_identical(brain$log$turns[[1L]], list(text = "what time is it", speaker = NULL))
    # Sentences were synthesized as they completed: the first when the
    # second delta closed it, the last at the flush.
    expect_identical(tts$log$texts, c("It is three o'clock. ", "Time for tea."))
    # Published in 200 ms chunks at the synthesizer's rate.
    rates <- unique(vapply(m$published, function(p) p$rate, 1L))
    expect_identical(rates, 24000L)
    sizes <- vapply(m$published, function(p) p$n, 1L)
    expect_true(all(sizes <= 4800L))
    expect_identical(sum(sizes), 2400L * nchar("It is three o'clock. Time for tea."))
    # Heard whole: no report.
    expect_identical(length(brain$log$reports), 0L)
    expect_false(cl$speaking)
    expect_identical(cl$turns, 1L)
})

# ---- barge-in: the agent stops, generation is cancelled, the record
# is trimmed to what was heard ----
local({
    m <- fake_media()
    stt <- fake_stt()
    tts <- fake_tts()
    brain <- fake_brain(c("One two. ", "Three four. ", "Five six. ", "Seven."))
    cl <- corteza:::call_loop_new(m, stt$fn, tts$fn, brain, list(log = quiet_log))
    # "One two. " is 9 chars = 21600 samples = 5 chunks (4800 each); the
    # human starts talking after the 7th chunk, 2 chunks into "Three
    # four. " (11 chars, 6 chunks): less than half of it was played.
    m$barge_after <- 7L
    deliver(m, "@ann:ex:PHONE", utterance(800))
    drain(cl, m)
    expect_identical(cl$barge_in, "@ann:ex:PHONE")
    # Publishing stopped within a chunk of the interruption.
    expect_true(length(m$published) <= 9L)
    # The brain was cancelled: not every delta was sent.
    expect_true(nchar(brain$log$turns[[1L]]$text) == 0L ||
                length(tts$log$texts) <= 2L)
    # Reported: heard up to the end of the first sentence only.
    expect_identical(length(brain$log$reports), 1L)
    expect_identical(brain$log$reports[[1L]]$heard, 9L)
    # The interrupting words are themselves an utterance to answer, once
    # they end; the agent is no longer speaking.
    expect_false(cl$speaking)
})

# Barge-in past the midpoint of a sentence counts that sentence.
local({
    m <- fake_media()
    tts <- fake_tts()
    brain <- fake_brain(c("One two. ", "Three four. ", "Five."))
    cl <- corteza:::call_loop_new(m, fake_stt()$fn, tts$fn, brain, list(log = quiet_log))
    # 5 chunks for the first sentence, then 4 of the second's 6.
    m$barge_after <- 9L
    deliver(m, "@ann:ex:PHONE", utterance(800))
    drain(cl, m)
    # The end of "Three four. ", trailing space included; the truncation
    # trims it.
    expect_identical(brain$log$reports[[1L]]$heard, 21L)
})

# ---- the policy with several people ----
local({
    m <- fake_media()
    tts <- fake_tts()
    brain <- fake_brain(c("Yes."))
    now <- 1000
    cl <- corteza:::call_loop_new(m, function(s, r) "hey cornelius what is up", tts$fn,
                                  brain, list(names = c("Cornelius", "corny"),
                                              clock = function() now,
                                              log = quiet_log))
    m$incoming[[1L]] <- list(event = list(type = "participant_connected",
                                          identity = "@ann:ex:PHONE"))
    m$incoming[[2L]] <- list(event = list(type = "participant_connected",
                                          identity = "@bob:ex:LAPTOP"))
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(sort(corteza:::call_humans(cl)), c("@ann:ex:PHONE", "@bob:ex:LAPTOP"))
    # Addressed by name: answered, with the speaker named.
    expect_identical(length(brain$log$turns), 1L)
    expect_identical(brain$log$turns[[1L]]$speaker, "ann")
    # Not addressed, from someone else: not answered.
    cl$stt <- function(s, r) "bob here, nothing for the bot"
    deliver(m, "@bob:ex:LAPTOP", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 1L)
    # A follow-up from the person the agent just answered, within the
    # window: answered without the name.
    cl$stt <- function(s, r) "and tomorrow?"
    now <- now + 10
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 2L)
    # Past the window, not addressed: not answered.
    now <- now + 60
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 2L)
    # The name match is a word: "corny" in "acorny" is not it.
    cl$stt <- function(s, r) "acorny thing"
    deliver(m, "@bob:ex:LAPTOP", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 2L)
    # answer = "always" answers everyone.
    cl$answer <- "always"
    deliver(m, "@bob:ex:LAPTOP", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 3L)
    expect_identical(brain$log$turns[[3L]]$speaker, "bob")
    # Someone leaving is forgotten.
    m$incoming[[1L]] <- list(event = list(type = "participant_disconnected",
                                          identity = "@bob:ex:LAPTOP"))
    corteza:::call_loop_step(cl, 0)
    expect_identical(corteza:::call_humans(cl), "@ann:ex:PHONE")
})
expect_identical(corteza:::call_speaker_name("@ann:ex:PHONE"), "ann")
expect_identical(corteza:::call_speaker_name("guest-7"), "guest-7")
expect_error(corteza:::call_loop_new(fake_media(), function(s, r) "", fake_tts()$fn,
                                     fake_brain("x"), list(answer = "sometimes")),
             "voice.call.answer")

# ---- nothing heard, a failing route, a failing turn: the loop goes on ----
local({
    m <- fake_media()
    tts <- fake_tts()
    brain <- fake_brain(c("Yes."))
    logged <- character()
    cl <- corteza:::call_loop_new(m, function(s, r) "", tts$fn, brain,
                                  list(log = function(...) logged <<- c(logged, paste0(...))))
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 0L)
    cl$stt <- function(s, r) stop("route down")
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 0L)
    expect_true(any(grepl("transcription failed: route down", logged)))
    cl$stt <- function(s, r) "hello"
    cl$brain <- list(turn = function(...) stop("provider down"), report = brain$report)
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_true(any(grepl("the turn failed: provider down", logged)))
    expect_false(cl$speaking)
    # Synthesis failing leaves the text standing, unspoken and uncut.
    cl$brain <- brain
    cl$tts <- function(text) stop("no voice")
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(length(brain$log$turns), 1L)
    expect_identical(length(brain$log$reports), 0L)
    expect_true(any(grepl("synthesis failed: no voice", logged)))
    # Our own track, should it ever arrive, is not an utterance.
    cl$stt <- function(s, r) stop("should not transcribe ourselves")
    deliver(m, m$identity, utterance(600))
    drain(cl, m)
    expect_identical(length(cl$queue), 0L)
})

# ---- the room ending stops the loop ----
local({
    m <- fake_media()
    cl <- corteza:::call_loop_new(m, function(s, r) "", fake_tts()$fn, fake_brain("x"),
                                  list(log = quiet_log))
    m$incoming[[1L]] <- list(event = list(type = "connection_state_changed",
                                          state = "disconnected"))
    steps <- 0L
    corteza:::call_loop_run(cl, until = function() {
        steps <<- steps + 1L
        steps > 50L
    }, timeout = 0)
    expect_true(cl$stopped)
    expect_true(steps < 50L)
    cl2 <- corteza:::call_loop_new(fake_media(), function(s, r) "", fake_tts()$fn,
                                   fake_brain("x"), list(log = quiet_log))
    corteza:::call_loop_stop(cl2)
    expect_identical(corteza:::call_loop_run(cl2, timeout = 0), cl2)
})

# ---- the production brain adapter over a faked voice state ----
local({
    posted <- list()
    edits <- list()
    hooks <- list(
        run_turn = function(state, room_id, text, on_delta) {
            for (d in c("Hello there. ", "How are you?")) on_delta(d)
            "Hello there. How are you?"
        },
        post = function(room_id, text) {
            posted[[length(posted) + 1L]] <<- text
            "$e1"
        },
        edit = function(room_id, event_id, text) {
            edits[[length(edits) + 1L]] <<- text
            TRUE
        },
        cancel = function() invisible(NULL))
    state <- corteza:::voice_state(function() list(), hooks)
    brain <- corteza:::call_brain(state, "!r:ex")
    t <- brain$turn("hi", NULL, function(d) TRUE)
    expect_identical(t$text, "Hello there. How are you?")
    expect_identical(posted, list("Hello there. How are you?"))
    expect_identical(brain$report(t$turn_id, 13), "Hello there.")
    expect_identical(edits, list("Hello there."))
    # Through the loop: barge-in reaches the room record.
    m <- fake_media()
    cl <- corteza:::call_loop_new(m, function(s, r) "hi", fake_tts()$fn, brain,
                                  list(log = quiet_log))
    # "Hello there. " is 13 chars = 7 chunks; stop after 8: one chunk
    # into the second sentence.
    m$barge_after <- 8L
    deliver(m, "@ann:ex:PHONE", utterance(600))
    drain(cl, m)
    expect_identical(edits[[2L]], "Hello there.")
})
