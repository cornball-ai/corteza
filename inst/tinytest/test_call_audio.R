library(tinytest)

# Audio arithmetic for the call-mode agent (R/call-audio.R): where an
# utterance starts and ends, WAV in and out, and how a reply is split
# for synthesis and counted when playback stops.

rate <- corteza:::CALL_AUDIO_RATE
ms_of <- function(ms) as.integer(round(rate * ms / 1000))
# A tone at a level in dBFS, and quiet noise.
tone <- function(ms, dbfs = -20, hz = 220) {
    n <- ms_of(ms)
    amp <- 32768 * 10^(dbfs / 20) * sqrt(2)
    as.integer(round(amp * sin(2 * pi * hz * seq_len(n) / rate)))
}
quiet <- function(ms, dbfs = -60) {
    set.seed(ms + 1L)
    n <- ms_of(ms)
    as.integer(round(rnorm(n, sd = 32768 * 10^(dbfs / 20))))
}
frames <- function(samples, frame_ms = 20) {
    n <- ms_of(frame_ms)
    split(samples, ceiling(seq_along(samples) / n))
}
feed_all <- function(v, samples) {
    out <- list()
    for (f in frames(samples)) {
        out <- c(out, corteza:::vad_feed(v, f))
    }
    out
}
types <- function(events) vapply(events, function(e) e$type, "")

# ---- level ----
expect_identical(corteza:::pcm_dbfs(integer()), -Inf)
expect_identical(corteza:::pcm_dbfs(c(0L, 0L)), -Inf)
expect_true(abs(corteza:::pcm_dbfs(tone(100, -20)) - (-20)) < 0.5)
expect_true(corteza:::pcm_dbfs(tone(100, -40)) < corteza:::pcm_dbfs(tone(100, -20)))

# ---- endpointing ----

# Quiet, speech, quiet: one utterance, opened after start_ms of sound
# and closed after end_ms of quiet, carrying the speech and its preroll.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, c(quiet(1000), tone(1200), quiet(1000)))
    expect_identical(types(ev), c("start", "end"))
    end <- ev[[2L]]
    expect_true(end$ms >= 1100 && end$ms <= 1300)
    expect_false(isTRUE(end$cut))
    # The samples hold the tone, the closing quiet, and the preroll: 300
    # ms, of which 200 is the sound that opened the utterance.
    expect_true(length(end$samples) >= ms_of(1200 + 100 + 700) - ms_of(40))
    expect_true(length(end$samples) <= ms_of(1200 + 100 + 700) + ms_of(40))
    expect_false(v$speaking)
    expect_identical(length(v$buffer), 0L)
})

# Quiet alone never opens; the floor settles at the quiet level, not
# below the minimum.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, quiet(3000, dbfs = -62))
    expect_identical(length(ev), 0L)
    expect_true(v$floor > -70 && v$floor < -55)
    v2 <- corteza:::vad_new()
    feed_all(v2, integer(ms_of(2000)))
    expect_true(v2$floor >= -70)
})

# A burst shorter than min_ms is dropped, not transcribed.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, c(quiet(500), tone(240), quiet(1000)))
    expect_identical(types(ev), c("start", "drop"))
})

# A sound shorter than start_ms never opens.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, c(quiet(500), tone(120), quiet(1000)))
    expect_identical(length(ev), 0L)
})

# A background louder than the starting floor opens once, as if it were
# speech; within seconds the floor has risen to it and closed that
# utterance, and from then on it is floor: speech above it opens, sound
# at its level does not.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, quiet(12000, dbfs = -40))
    expect_identical(types(ev), c("start", "end"))
    expect_true(ev[[2L]]$ms < 10000)
    expect_true(v$floor > -45 && v$floor < -38)
    ev <- feed_all(v, c(tone(1000, dbfs = -41), quiet(1000, dbfs = -40)))
    expect_identical(length(ev), 0L)
    ev <- feed_all(v, c(tone(1000, dbfs = -20), quiet(1000, dbfs = -40)))
    expect_identical(types(ev), c("start", "end"))
})

# A long sentence does not end itself: the floor rises slowly on sound,
# and the pauses in speech pull it back down.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, c(quiet(500), tone(8000), quiet(1000)))
    expect_identical(types(ev), c("start", "end"))
    expect_true(ev[[2L]]$ms >= 7900)
    v <- corteza:::vad_new()
    spoken <- c(quiet(500), unlist(lapply(1:12, function(i) c(tone(1500), quiet(300)))),
                quiet(1000))
    ev <- feed_all(v, spoken)
    expect_identical(types(ev), c("start", "end"))
    expect_true(ev[[2L]]$ms >= 21000)
})

# Past max_ms a monologue is cut into pieces and goes on.
local({
    v <- corteza:::vad_new(max_ms = 2000)
    ev <- feed_all(v, c(quiet(500), tone(5000), quiet(1000)))
    expect_identical(types(ev), c("start", "end", "start", "end", "start", "end"))
    expect_true(isTRUE(ev[[2L]]$cut))
    expect_true(isTRUE(ev[[4L]]$cut))
    expect_false(isTRUE(ev[[6L]]$cut))
    total <- sum(vapply(ev[c(2L, 4L, 6L)], function(e) length(e$samples), 1L))
    # Everything from the preroll to the closing quiet is in the pieces.
    expect_true(total >= ms_of(5000 + 100 + 700) - ms_of(60))
    expect_true(total <= ms_of(5000 + 100 + 700) + ms_of(60))
})

# A pause shorter than end_ms stays inside the utterance.
local({
    v <- corteza:::vad_new()
    ev <- feed_all(v, c(quiet(500), tone(800), quiet(400), tone(800), quiet(1000)))
    expect_identical(types(ev), c("start", "end"))
    expect_true(ev[[2L]]$ms >= 1900)
})

# Frames of uneven sizes work, and an empty frame is nothing.
local({
    v <- corteza:::vad_new()
    expect_identical(corteza:::vad_feed(v, integer()), list())
    s <- c(quiet(500), tone(1000), quiet(1000))
    sizes <- rep(ms_of(c(10, 30, 50)), length.out = 200)
    ends <- pmin(cumsum(sizes), length(s))
    starts <- c(1L, utils::head(ends, -1L) + 1L)
    ev <- list()
    for (i in seq_along(sizes)) {
        if (starts[[i]] > length(s)) break
        ev <- c(ev, corteza:::vad_feed(v, s[starts[[i]]:ends[[i]]]))
    }
    expect_identical(types(ev), c("start", "end"))
})

# ---- WAV ----
local({
    s <- tone(100)
    bytes <- corteza:::wav_encode(s, rate)
    expect_identical(length(bytes), 44L + 2L * length(s))
    expect_identical(rawToChar(bytes[1:4]), "RIFF")
    back <- corteza:::wav_decode(bytes)
    expect_identical(back$samples, s)
    expect_identical(back$rate, rate)
    expect_identical(back$channels, 1L)
    # Empty audio is a valid file.
    e <- corteza:::wav_decode(corteza:::wav_encode(integer(), 24000L))
    expect_identical(e$samples, integer())
    expect_identical(e$rate, 24000L)
    # Stereo in, by channel count.
    st <- corteza:::wav_decode(corteza:::wav_encode(c(1L, -1L, 2L, -2L), 8000L, 2L))
    expect_identical(st$channels, 2L)
    expect_identical(corteza:::pcm_mono(st$samples, 2L), c(0L, 0L))
    expect_identical(corteza:::pcm_mono(c(10L, 20L, 30L, 40L), 2L), c(15L, 35L))
    expect_identical(corteza:::pcm_mono(c(10L, 20L), 1L), c(10L, 20L))
})

# A chunk before data (LIST) is skipped; 32-bit float is read; other
# formats are refused by name; junk is refused.
local({
    s <- c(1000L, -1000L, 0L)
    bytes <- corteza:::wav_encode(s, 16000L)
    list_chunk <- c(charToRaw("LIST"), as.raw(c(4L, 0L, 0L, 0L)), charToRaw("INFO"))
    with_list <- c(bytes[1:36], list_chunk, bytes[37:length(bytes)])
    expect_identical(corteza:::wav_decode(with_list)$samples, s)

    f <- c(0.5, -0.5, 0, 2)
    con <- rawConnection(raw(0), "wb")
    writeChar("RIFF", con, eos = NULL)
    writeBin(36L + 16L, con, size = 4L, endian = "little")
    writeChar("WAVEfmt ", con, eos = NULL)
    writeBin(16L, con, size = 4L, endian = "little")
    writeBin(3L, con, size = 2L, endian = "little")
    writeBin(1L, con, size = 2L, endian = "little")
    writeBin(24000L, con, size = 4L, endian = "little")
    writeBin(24000L * 4L, con, size = 4L, endian = "little")
    writeBin(4L, con, size = 2L, endian = "little")
    writeBin(32L, con, size = 2L, endian = "little")
    writeChar("data", con, eos = NULL)
    writeBin(16L, con, size = 4L, endian = "little")
    writeBin(f, con, size = 4L, endian = "little")
    fl <- rawConnectionValue(con)
    close(con)
    got <- corteza:::wav_decode(fl)
    expect_identical(got$rate, 24000L)
    expect_identical(got$samples, c(16384L, -16384L, 0L, 32767L))

    bad <- bytes
    bad[21:22] <- as.raw(c(2L, 0L))
    expect_error(corteza:::wav_decode(bad), "unsupported format 2")
    expect_error(corteza:::wav_decode(charToRaw("not a wav file at all")), "RIFF")
    expect_error(corteza:::wav_encode(c(1L, 40000L), 16000L), "int16")
})

# ---- spoken text ----
local({
    p <- corteza:::voice_sentences("Hello there. How are you?  Fine!")
    expect_identical(vapply(p, function(x) x$text, ""),
                     c("Hello there. ", "How are you?  ", "Fine!"))
    expect_identical(vapply(p, function(x) x$end, 1L), c(13L, 27L, 32L))
    # The ends are offsets into the whole, so truncating there keeps
    # whole sentences.
    expect_identical(corteza:::voice_truncate("Hello there. How are you?  Fine!", 13),
                     "Hello there.")
    one <- corteza:::voice_sentences("No punctuation here")
    expect_identical(length(one), 1L)
    expect_identical(one[[1L]]$end, 19L)
    expect_identical(corteza:::voice_sentences(""), list())
    # Line breaks split; a trailing break makes no empty piece.
    lb <- corteza:::voice_sentences("First line\nSecond line\n")
    expect_identical(vapply(lb, function(x) x$text, ""),
                     c("First line\n", "Second line\n"))
    # Decimal points and a closing quote.
    q <- corteza:::voice_sentences("It cost 3.50 today. \"Really?\" she said.")
    expect_identical(vapply(q, function(x) x$text, ""),
                     c("It cost 3.50 today. ", "\"Really?\" ", "she said."))
    # Code points, not bytes.
    u <- corteza:::voice_sentences("Café ouvert. Oui.")
    expect_identical(u[[1L]]$end, 13L)
})

# How much was heard when playback stopped.
local({
    p <- corteza:::voice_sentences("One. Two. Three.")
    expect_identical(corteza:::voice_heard(p, 0L, 0), 0L)
    expect_identical(corteza:::voice_heard(p, 0L, 0.4), 0L)
    expect_identical(corteza:::voice_heard(p, 0L, 0.6), 5L)
    expect_identical(corteza:::voice_heard(p, 1L, 0), 5L)
    expect_identical(corteza:::voice_heard(p, 1L, 0.9), 10L)
    expect_identical(corteza:::voice_heard(p, 3L, 0), 16L)
    expect_identical(corteza:::voice_heard(p, 5L, 0.9), 16L)
    expect_identical(corteza:::voice_heard(list(), 0L, 1), 0L)
})
