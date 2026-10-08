library(tinytest)

# The voice brain as functions (R/voice-turn.R): a turn and its report,
# usable by any media backend, with the world faked out.

brain <- function(deltas = c("Hello ", "there."), post = function(room, text) "$e1") {
    log <- new.env()
    log$posted <- list()
    log$edits <- list()
    log$prompts <- character()
    hooks <- list(
        run_turn = function(state, room_id, text, on_delta) {
            log$prompts <- c(log$prompts, text)
            for (d in deltas) on_delta(d)
            paste(deltas, collapse = "")
        },
        post = function(room_id, text) {
            log$posted[[length(log$posted) + 1L]] <- list(room = room_id, text = text)
            post(room_id, text)
        },
        edit = function(room_id, event_id, text) {
            log$edits[[length(log$edits) + 1L]] <- list(room = room_id, id = event_id,
                                                        text = text)
            invisible(TRUE)
        },
        cancel = function() invisible(NULL))
    state <- corteza:::voice_state(function() list(), hooks)
    list(state = state, log = log)
}

# A turn streams its deltas, posts the whole, and records itself.
local({
    b <- brain()
    turns <- new.env(parent = emptyenv())
    got <- character()
    # A send answers TRUE for delivered; the stream stops at anything else.
    t <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) {
        got <<- c(got, d)
        TRUE
    }, turns = turns)
    expect_identical(got, c("Hello ", "there."))
    expect_identical(t$text, "Hello there.")
    expect_identical(t$event_id, "$e1")
    expect_true(t$alive)
    expect_true(nzchar(t$turn_id))
    expect_identical(b$log$posted[[1L]], list(room = "!r:ex", text = "Hello there."))
    expect_identical(turns[[t$turn_id]]$text, "Hello there.")
    expect_null(turns[[t$turn_id]]$stored)
    # No speaker: the words go to the session as said.
    expect_identical(b$log$prompts, "hi")
    # A speaker is named in front of them.
    corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) NULL, turns = turns,
                         speaker = "Ann")
    expect_identical(b$log$prompts[[2L]], "Ann: hi")
    expect_identical(corteza:::voice_speaker_text("x", NULL), "x")
    expect_identical(corteza:::voice_speaker_text("x", ""), "x")
    expect_identical(corteza:::voice_speaker_text("x", "Bo"), "Bo: x")
})

# A send that fails stops the stream and the turn says so; the record
# still holds everything generated.
local({
    b <- brain(deltas = c("One. ", "Two. ", "Three."))
    turns <- new.env(parent = emptyenv())
    n <- 0L
    t <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) {
        n <<- n + 1L
        if (n == 2L) stop("peer gone")
        TRUE
    }, turns = turns)
    expect_false(t$alive)
    expect_identical(t$text, "One. Two. Three.")
    expect_identical(n, 2L)
})

# A provider that streamed nothing: one delta with the whole reply.
local({
    b <- brain(deltas = character())
    b$state$hooks$run_turn <- function(state, room_id, text, on_delta) "All at once."
    turns <- new.env(parent = emptyenv())
    got <- character()
    t <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) {
        got <<- c(got, d)
        TRUE
    }, turns = turns)
    expect_identical(got, "All at once.")
    expect_identical(t$text, "All at once.")
})

# The report truncates the post to what was heard, once.
local({
    b <- brain(deltas = c("Hello there. ", "How are you?"))
    turns <- new.env(parent = emptyenv())
    t <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) TRUE, turns = turns)
    stored <- corteza:::voice_turn_report(b$state, "!r:ex", turns, t$turn_id, 13)
    expect_identical(stored, "Hello there.")
    expect_identical(b$log$edits[[1L]], list(room = "!r:ex", id = "$e1",
                                             text = "Hello there."))
    # A second report answers the first's result and edits nothing.
    expect_identical(corteza:::voice_turn_report(b$state, "!r:ex", turns, t$turn_id, 0),
                     "Hello there.")
    expect_identical(length(b$log$edits), 1L)
    # Heard whole: no edit.
    t2 <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) TRUE, turns = turns)
    expect_identical(corteza:::voice_turn_report(b$state, "!r:ex", turns, t2$turn_id, 999),
                     "Hello there. How are you?")
    expect_identical(length(b$log$edits), 1L)
    # Nothing heard: an empty post.
    t3 <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) TRUE, turns = turns)
    expect_identical(corteza:::voice_turn_report(b$state, "!r:ex", turns, t3$turn_id, 0), "")
    expect_identical(b$log$edits[[2L]]$text, "")
})

# Refusals: an unknown turn, a turn never posted, a failed edit, a
# count that is not a number.
local({
    b <- brain()
    turns <- new.env(parent = emptyenv())
    refusal <- function(expr) {
        tryCatch({ expr; NULL }, corteza_voice_refusal = function(e) e$status)
    }
    expect_identical(refusal(corteza:::voice_turn_report(b$state, "!r:ex", turns, "nope", 1)),
                     "NOT_FOUND")
    expect_identical(refusal(corteza:::voice_turn_report(b$state, "!r:ex", turns, "", 1)),
                     "NOT_FOUND")
    unposted <- brain(post = function(room, text) stop("homeserver down"))
    t <- corteza:::voice_turn(unposted$state, "!r:ex", "hi", function(d) TRUE, turns = turns)
    expect_null(t$event_id)
    expect_identical(refusal(corteza:::voice_turn_report(unposted$state, "!r:ex", turns,
                                                         t$turn_id, 1)), "INTERNAL")
    t2 <- corteza:::voice_turn(b$state, "!r:ex", "hi", function(d) TRUE, turns = turns)
    expect_identical(refusal(corteza:::voice_turn_report(b$state, "!r:ex", turns,
                                                         t2$turn_id, "many")),
                     "INVALID_ARGUMENT")
    b$state$hooks$edit <- function(room_id, event_id, text) stop("edit failed")
    expect_identical(refusal(corteza:::voice_turn_report(b$state, "!r:ex", turns,
                                                         t2$turn_id, 3)), "UNAVAILABLE")
    # Not stored: a retry can still succeed.
    expect_null(turns[[t2$turn_id]]$stored)
})
