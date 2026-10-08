# Audio for the call-mode agent.
#
# In a call the agent is a participant: the SFU delivers one PCM
# stream per other participant (livekitr::lk_on_audio), and the agent
# publishes what it says (livekitr::lk_publish_pcm). Transcription is a
# file-based route (one request per utterance), so the agent decides
# where an utterance starts and ends itself, from signal energy, and
# packs it as WAV. Synthesis comes back as WAV too, and goes out as
# samples. Nothing here knows about the SFU or the routes; it is the
# arithmetic between them, and it is pure so it can be tested on
# vectors.
#
# The energy detector is the same idea the FluffyChat client used for
# barge-in (an adaptive noise floor and a margin above it), used here
# for endpointing as well, since the transcription route does none.
# It is not a speech model: a door slamming is "speech" until the
# transcript comes back empty, which is what the minimum length and the
# empty-transcript check downstream are for.

# The rate the agent asks the SFU for on the subscribe side: what the
# transcription models want, and a quarter of the wire rate, so the
# detector does a quarter of the work.
CALL_AUDIO_RATE <- 16000L

# Signal level of int16 samples in dBFS (0 is full scale; silence is
# -Inf).
pcm_dbfs <- function(samples) {
    if (!length(samples)) {
        return(-Inf)
    }
    rms <- sqrt(mean(as.numeric(samples) ^ 2))
    if (rms <= 0) {
        return(-Inf)
    }
    20 * log10(rms / 32768)
}

# ---- Endpointing ----------------------------------------------------------

# A detector for one participant's stream.
#
# `start_ms` of level above the floor plus `margin_db` opens an
# utterance; `end_ms` below closes it. The floor follows the level of
# quiet frames with time constant `floor_tc_ms` and never falls below
# `floor_min_db`, so a silent line does not put the floor at -Inf and
# fire on the first breath. Loud frames raise it too, but slowly
# (`floor_rise_tc_ms`): a fan or a constant hum becomes floor within
# seconds, while the pauses in real speech keep pulling the floor back
# down at the fast rate, so a sentence does not end itself. `preroll_ms`
# of audio from before the opening is kept, since the first syllable is
# what opened it. An utterance shorter than `min_ms` of speech is
# dropped as a noise burst, and one longer than `max_ms` is closed
# where it stands and a new one opened, so a monologue still reaches
# the transcriber in pieces.
vad_new <- function(rate = CALL_AUDIO_RATE, start_ms = 200, end_ms = 700,
                    margin_db = 10, floor_tc_ms = 500,
                    floor_rise_tc_ms = 10000, floor_init_db = -55,
                    floor_min_db = -70, preroll_ms = 300, min_ms = 300,
                    max_ms = 30000) {
    v <- new.env(parent = emptyenv())
    v$rate <- as.integer(rate)
    v$start_ms <- start_ms
    v$end_ms <- end_ms
    v$margin_db <- margin_db
    v$floor_tc_ms <- floor_tc_ms
    v$floor_rise_tc_ms <- floor_rise_tc_ms
    v$floor_min_db <- floor_min_db
    v$preroll_ms <- preroll_ms
    v$min_ms <- min_ms
    v$max_ms <- max_ms
    v$floor <- floor_init_db
    v$speaking <- FALSE
    v$above_ms <- 0
    v$below_ms <- 0
    v$total_ms <- 0
    v$open_quiet_ms <- 0
    v$preroll <- list()
    v$preroll_total <- 0
    v$buffer <- list()
    v
}

# Move the floor toward this frame's level: quickly on a quiet frame,
# slowly up on a loud one.
.vad_adapt <- function(v, level, loud, ms) {
    target <- max(level, v$floor_min_db)
    if (loud) {
        tc <- v$floor_rise_tc_ms
    } else {
        tc <- v$floor_tc_ms
    }
    if (loud && target < v$floor) {
        return(invisible(NULL))
    }
    v$floor <- v$floor + (target - v$floor) * min(1, ms / tc)
    invisible(NULL)
}

# Feed a frame of int16 samples. Returns a list of events, each a list
# with `type`: "start" when an utterance opens, "end" with `samples`
# (int16, preroll included) and `ms` of speech when one closes, "drop"
# when what opened was too short to be speech. A frame of any length
# works; the SFU's are 20 ms.
vad_feed <- function(v, frame) {
    frame <- as.integer(frame)
    ms <- length(frame) / v$rate * 1000
    if (ms <= 0) {
        return(list())
    }
    level <- pcm_dbfs(frame)
    loud <- level >= v$floor + v$margin_db
    .vad_adapt(v, level, loud, ms)
    events <- list()
    if (!v$speaking) {
        if (loud) {
            v$above_ms <- v$above_ms + ms
        } else {
            v$above_ms <- 0
        }
        .vad_preroll_push(v, frame, ms)
        if (v$above_ms >= v$start_ms) {
            v$speaking <- TRUE
            v$buffer <- v$preroll
            v$total_ms <- v$preroll_total
            # The preroll's quiet part is not speech, for the length
            # check at the close.
            v$open_quiet_ms <- max(0, v$preroll_total - v$above_ms)
            v$below_ms <- 0
            v$above_ms <- 0
            v$preroll <- list()
            v$preroll_total <- 0
            events[[length(events) + 1L]] <- list(type = "start")
        }
        return(events)
    }
    v$buffer[[length(v$buffer) + 1L]] <- frame
    v$total_ms <- v$total_ms + ms
    if (loud) {
        v$below_ms <- 0
    } else {
        v$below_ms <- v$below_ms + ms
    }
    if (v$below_ms >= v$end_ms) {
        events[[length(events) + 1L]] <- .vad_close(v)
    } else if (v$total_ms >= v$max_ms) {
        events[[length(events) + 1L]] <- .vad_close(v, cut = TRUE)
        # Still loud: the next utterance opens at once, with no
        # preroll, since the sound is continuous.
        v$speaking <- TRUE
        v$total_ms <- 0
        v$open_quiet_ms <- 0
        events[[length(events) + 1L]] <- list(type = "start")
    }
    events
}

.vad_preroll_push <- function(v, frame, ms) {
    v$preroll[[length(v$preroll) + 1L]] <- frame
    v$preroll_total <- v$preroll_total + ms
    while (length(v$preroll) > 1L && v$preroll_total > v$preroll_ms) {
        v$preroll_total <- v$preroll_total - length(v$preroll[[1L]]) / v$rate * 1000
        v$preroll <- v$preroll[-1L]
    }
}

# Close the open utterance: an "end" with its samples, or a "drop" when
# the speech in it (what is not trailing quiet) is too short.
.vad_close <- function(v, cut = FALSE) {
    samples <- unlist(v$buffer, use.names = FALSE)
    if (cut) {
        trailing <- 0
    } else {
        trailing <- v$below_ms
    }
    speech_ms <- v$total_ms - v$open_quiet_ms - trailing
    v$speaking <- FALSE
    v$buffer <- list()
    v$total_ms <- 0
    v$open_quiet_ms <- 0
    v$below_ms <- 0
    v$above_ms <- 0
    if (speech_ms < v$min_ms) {
        return(list(type = "drop", ms = speech_ms))
    }
    list(type = "end", samples = samples, ms = speech_ms, cut = cut)
}

# ---- WAV -------------------------------------------------------------------

# int16 samples to a RIFF/WAVE byte string, 16-bit PCM.
wav_encode <- function(samples, rate, channels = 1L) {
    samples <- as.integer(samples)
    if (anyNA(samples) || any(samples > 32767L | samples < -32768L)) {
        stop("wav_encode: samples must be int16", call. = FALSE)
    }
    rate <- as.integer(rate)
    channels <- as.integer(channels)
    n_bytes <- length(samples) * 2L
    con <- rawConnection(raw(0), "wb")
    on.exit(close(con), add = TRUE)
    writeChar("RIFF", con, eos = NULL)
    writeBin(36L + n_bytes, con, size = 4L, endian = "little")
    writeChar("WAVEfmt ", con, eos = NULL)
    writeBin(16L, con, size = 4L, endian = "little")
    writeBin(1L, con, size = 2L, endian = "little")
    writeBin(channels, con, size = 2L, endian = "little")
    writeBin(rate, con, size = 4L, endian = "little")
    writeBin(rate * channels * 2L, con, size = 4L, endian = "little")
    writeBin(channels * 2L, con, size = 2L, endian = "little")
    writeBin(16L, con, size = 2L, endian = "little")
    writeChar("data", con, eos = NULL)
    writeBin(n_bytes, con, size = 4L, endian = "little")
    if (length(samples)) {
        writeBin(samples, con, size = 2L, endian = "little")
    }
    rawConnectionValue(con)
}

# A RIFF/WAVE byte string to list(samples (int16), rate, channels).
# 16-bit PCM and 32-bit float are read; anything else is refused by
# name. Chunks other than fmt and data (LIST, fact, ...) are skipped.
wav_decode <- function(bytes) {
    if (!is.raw(bytes) || length(bytes) < 12L ||
        !identical(rawToChar(bytes[1:4]), "RIFF") ||
        !identical(rawToChar(bytes[9:12]), "WAVE")) {
        stop("wav_decode: not a RIFF/WAVE file", call. = FALSE)
    }
    u32 <- function(at) {
        sum(as.numeric(bytes[at:(at + 3L)]) * 256 ^ (0:3))
    }
    u16 <- function(at) {
        sum(as.numeric(bytes[at:(at + 1L)]) * 256 ^ (0:1))
    }
    pos <- 13L
    fmt <- NULL
    data <- NULL
    while (pos + 8L <= length(bytes) + 1L) {
        id <- rawToChar(bytes[pos:(pos + 3L)])
        size <- u32(pos + 4L)
        body <- pos + 8L
        if (identical(id, "fmt ")) {
            fmt <- list(format = u16(body), channels = u16(body + 2L),
                        rate = u32(body + 4L), bits = u16(body + 14L))
            # WAVE_FORMAT_EXTENSIBLE carries the real format in its
            # sub-format GUID; its first two bytes are the code.
            if (identical(fmt$format, 65534) && size >= 26L) {
                fmt$format <- u16(body + 24L)
            }
        } else if (identical(id, "data")) {
            end <- min(body + size - 1L, length(bytes))
            if (end >= body) {
                data <- bytes[body:end]
            } else {
                data <- raw(0)
            }
        }
        pos <- body + size + (size %% 2L)
    }
    if (is.null(fmt) || is.null(data)) {
        stop("wav_decode: no fmt or data chunk", call. = FALSE)
    }
    if (identical(fmt$format, 1) && identical(fmt$bits, 16)) {
        samples <- readBin(data, "integer", n = length(data) %/% 2L, size = 2L,
                           signed = TRUE, endian = "little")
    } else if (identical(fmt$format, 3) && identical(fmt$bits, 32)) {
        x <- readBin(data, "double", n = length(data) %/% 4L, size = 4L,
                     endian = "little")
        samples <- as.integer(round(pmax(-1, pmin(1, x)) * 32767))
    } else {
        stop(sprintf("wav_decode: unsupported format %d at %d bits; 16-bit PCM or 32-bit float only",
                     fmt$format, fmt$bits), call. = FALSE)
    }
    list(samples = samples, rate = as.integer(fmt$rate),
         channels = as.integer(fmt$channels))
}

# Interleaved multi-channel samples to one channel, by averaging.
pcm_mono <- function(samples, channels) {
    channels <- as.integer(channels)
    if (channels <= 1L || !length(samples)) {
        return(as.integer(samples))
    }
    n <- length(samples) %/% channels
    m <- matrix(as.numeric(samples[seq_len(n * channels)]), nrow = channels)
    as.integer(round(colMeans(m)))
}

# ---- Spoken text -----------------------------------------------------------

# A reply split where speech pauses, for synthesis a piece at a time:
# the first sentence is heard while the rest is still being made. Each
# piece carries `end`, its last code point's position in the whole
# reply, which is what a report of how much was heard counts in.
voice_sentences <- function(text) {
    text <- enc2utf8(text)
    if (!nzchar(text)) {
        return(list())
    }
    # A break after sentence punctuation (with any closing quote or
    # bracket) that is followed by white space, and at a line break.
    m <- gregexpr("[.!?…][\"')\\]]*[[:space:]]+|\n+", text, perl = TRUE)[[1L]]
    total <- nchar(text, type = "chars")
    if (m[[1L]] == -1L) {
        return(list(list(text = text, end = total)))
    }
    cuts <- as.integer(m) + attr(m, "match.length")
    bounds <- unique(c(1L, cuts[cuts <= total], total + 1L))
    out <- list()
    for (i in seq_len(length(bounds) - 1L)) {
        piece <- substr(text, bounds[[i]], bounds[[i + 1L]] - 1L)
        if (!nzchar(trimws(piece))) {
            next
        }
        out[[length(out) + 1L]] <- list(text = piece,
                                        end = bounds[[i + 1L]] - 1L)
    }
    out
}

# How much of a reply was heard when playback stopped: every piece
# published whole, plus the one under way if more than half of its
# audio had gone out (the client's midpoint rule, applied to what the
# agent published). `done` is how many pieces were published whole,
# `fraction` how much of the next one was.
voice_heard <- function(pieces, done, fraction = 0) {
    if (done <= 0L && fraction <= 0.5) {
        return(0L)
    }
    idx <- done
    if (fraction > 0.5) {
        idx <- idx + 1L
    }
    idx <- min(idx, length(pieces))
    if (idx <= 0L) {
        return(0L)
    }
    as.integer(pieces[[idx]]$end)
}
