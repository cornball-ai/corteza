library(tinytest)

# The mailbox between a call worker and the bot process
# (R/call-worker.R): requests answered, commands read, a post that
# waits, a leave that ends the loop. The child side is driven here
# in-process; the process itself is exercised at home against a
# LiveKit server (below).

mailbox <- function() {
    dir <- tempfile("call-")
    dir.create(file.path(dir, "answers"), recursive = TRUE)
    file.create(file.path(dir, "requests.jsonl"))
    file.create(file.path(dir, "commands.jsonl"))
    w <- new.env(parent = emptyenv())
    w$dir <- dir
    w$read_from <- 0L
    w$room_id <- "!r:ex"
    box <- new.env(parent = emptyenv())
    box$dir <- dir
    box$n <- 0L
    box$read_from <- 0L
    box$wait_s <- 2
    list(w = w, box = box, dir = dir)
}

# A note and an edit go through; a post gets its event id back.
local({
    mb <- mailbox()
    posted <- list()
    edits <- list()
    notes <- character()
    pump <- function() {
        corteza:::call_worker_pump(mb$w, post = function(room, text) {
            posted[[length(posted) + 1L]] <<- list(room, text)
            "$e1"
        }, edit = function(room, id, text) {
            edits[[length(edits) + 1L]] <<- list(room, id, text)
        }, note = function(text) notes <<- c(notes, text))
    }
    corteza:::.call_request(mb$box, list(type = "note", text = "hello"))
    expect_identical(length(pump()), 1L)
    expect_identical(notes, "hello")
    # Nothing new: nothing handled, nothing re-read.
    expect_identical(length(pump()), 0L)
    # An edit is answered, and the child reads the answer.
    ticks <- 0L
    mb$box$tick <- function() {
        ticks <<- ticks + 1L
        # The parent pumps while the child waits (in life, another
        # process; here, from the child's own tick).
        pump()
    }
    ans <- corteza:::.call_request(mb$box, list(type = "edit", event_id = "$e1",
                                                text = "shorter"), wait = TRUE)
    expect_true(isTRUE(ans$ok))
    expect_identical(edits[[1L]], list("!r:ex", "$e1", "shorter"))
    expect_true(ticks >= 1L)
    # A post returns the event id the parent minted.
    ans <- corteza:::.call_request(mb$box, list(type = "post", text = "Hi."), wait = TRUE)
    expect_identical(ans$event_id, "$e1")
    expect_identical(posted[[1L]], list("!r:ex", "Hi."))
    # The parent's failure is the child's error, by message.
    mb$box$tick <- function() {
        corteza:::call_worker_pump(mb$w, post = function(room, text) stop("homeserver down"),
                                   edit = function(...) NULL)
    }
    expect_error(corteza:::.call_request(mb$box, list(type = "post", text = "x"),
                                         wait = TRUE), "homeserver down")
    # No parent at all: the wait ends.
    mb$box$tick <- NULL
    mb$box$wait_s <- 0.3
    expect_error(corteza:::.call_request(mb$box, list(type = "post", text = "x"),
                                         wait = TRUE), "no answer from the bot process")
    # The request is still there for a parent that comes back late; a
    # junk line in the file is skipped, not fatal.
    cat("not json\n", file = file.path(mb$dir, "requests.jsonl"), append = TRUE)
    late <- pump()
    expect_identical(length(late), 1L)
    expect_identical(late[[1L]]$type, "post")
    expect_identical(length(pump()), 0L)
    unlink(mb$dir, recursive = TRUE)
})

# Commands reach the child in order and once.
local({
    mb <- mailbox()
    corteza:::call_worker_command(mb$w, list(type = "key", identity = "@a:ex:D",
                                             key = jsonlite::base64_enc(as.raw(1:16)),
                                             index = 3L))
    corteza:::call_worker_command(mb$w, list(type = "leave"))
    cmds <- corteza:::.call_commands(mb$box)
    expect_identical(length(cmds), 2L)
    expect_identical(cmds[[1L]]$type, "key")
    expect_identical(cmds[[1L]]$identity, "@a:ex:D")
    expect_identical(jsonlite::base64_dec(cmds[[1L]]$key), as.raw(1:16))
    expect_identical(cmds[[1L]]$index, 3L)
    expect_identical(cmds[[2L]]$type, "leave")
    expect_identical(length(corteza:::.call_commands(mb$box)), 0L)
    unlink(mb$dir, recursive = TRUE)
})

expect_identical(corteza:::call_e2ee_options()$kdf, "hkdf")
expect_identical(corteza:::call_e2ee_options()$key_ring_size, 256L)

# ---- the process, against a LiveKit server ----
# Needs a server (LIVEKITR_TEST_URL, dev keys) and livekitr's native
# library; the person in the call is this process. Transcription and
# synthesis are the configured routes, so the config points them at a
# fake router served from this process's HTTP hook... which the child
# cannot share. Instead the child's cfg points at a URL that fails, and
# what is checked here is the process plumbing: it joins, hears, asks
# the parent to log, and leaves on command.
url <- Sys.getenv("LIVEKITR_TEST_URL", "")
if (at_home() && nzchar(url) && requireNamespace("livekitr", quietly = TRUE) &&
    isTRUE(tryCatch(livekitr::livekit_available(), error = function(e) FALSE))) {
    room <- paste0("corteza-test-", paste(sample(letters, 6), collapse = ""))
    cfg <- list(provider = "anthropic", model = "claude-haiku-4-5-20251001",
                voice = list(stt = list(url = "http://127.0.0.1:9", model = "m"),
                             tts = list(url = "http://127.0.0.1:9", model = "m")))
    w <- corteza:::call_worker_start(list(
        url = url, jwt = livekitr::lk_mint_token("devkey", "secret", room, "@bot:ex:DEV"),
        room_id = "!r:ex", cfg = cfg, names = "bot", identity = "@bot:ex:DEV"))
    on.exit(corteza:::call_worker_close(w), add = TRUE)
    notes <- character()
    pump <- function() {
        corteza:::call_worker_pump(w, post = function(...) "$e", edit = function(...) NULL,
                                   note = function(text) notes <<- c(notes, text))
    }
    deadline <- Sys.time() + 20
    while (!any(grepl("in the call as @bot:ex:DEV", notes)) && Sys.time() < deadline) {
        pump()
        Sys.sleep(0.2)
    }
    expect_true(any(grepl("in the call as @bot:ex:DEV", notes)))
    expect_true(corteza:::call_worker_alive(w))
    # A person speaks; the child hears, cuts the utterance, and tries
    # the (unreachable) transcription route: the failure is logged
    # through the mailbox, which proves the whole path up to the route.
    human <- livekitr::lk_connect(url, livekitr::lk_mint_token("devkey", "secret", room,
                                                                 "@ann:ex:PHONE"))
    for (i in 1:10) livekitr::lk_poll(human, 0.1)
    rate <- 16000L
    tone <- as.integer(8000 * sin(2 * pi * 220 * seq_len(rate * 1.2) / rate))
    livekitr::lk_publish_pcm(human, c(integer(rate * 0.3), tone, integer(rate)), rate)
    deadline <- Sys.time() + 20
    while (!any(grepl("transcription failed", notes)) && Sys.time() < deadline) {
        livekitr::lk_poll(human, 0.05)
        pump()
    }
    expect_true(any(grepl("transcription failed", notes)))
    livekitr::lk_disconnect(human)
    # Leave on command: the loop returns and the process reports.
    result <- corteza:::call_worker_close(w)
    expect_false(corteza:::call_worker_alive(w))
    expect_null(result$error)
    expect_identical(result$value$turns, 0L)
    # The person had left by then, and the room event said so.
    expect_identical(result$value$humans, character())
}
