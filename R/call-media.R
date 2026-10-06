# The call-mode agent's speech routes: OpenAI-shaped transcription and
# synthesis over HTTP, one request per utterance and per sentence.
#
# These are the routes the gpu.host router serves; the agent posts to
# them with curl, which corteza already imports. Base URLs, model names
# and the credential come from the bot config under `voice`:
#
#   "voice": {
#     "stt": {"url": "http://router:8080", "model": "whisper-small",
#             "language": "en", "key_env": "GPU_HOST_KEY"},
#     "tts": {"url": "http://router:8080", "model": "chatterbox",
#             "voice": "default", "key_env": "GPU_HOST_KEY"}
#   }
#
# `key` holds a credential literally, `key_env` names the variable that
# does; neither is required, since a router on the tailnet may not ask.
# Every request goes through `http`, a function the tests replace, so
# the shapes sent and the answers read are checked without a router.

CALL_STT_ROUTE <- "/v1/audio/transcriptions"
CALL_TTS_ROUTE <- "/v1/audio/speech"
CALL_STT_TIMEOUT_MS <- 30000L
CALL_TTS_TIMEOUT_MS <- 60000L

# The two routes' settings, validated loudly, from a bot config.
call_media_config <- function(cfg) {
    voice <- cfg$voice
    one <- function(which, required) {
        x <- voice[[which]]
        if (!is.list(x)) {
            stop(sprintf("voice.%s is not configured: it needs at least url and model",
                         which), call. = FALSE)
        }
        for (f in required) {
            v <- x[[f]]
            if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
                stop(sprintf("voice.%s.%s must be a non-empty string", which,
                             f),
                     call. = FALSE)
            }
        }
        # `[[`: `x$key` would answer with key_env when key is absent.
        key <- x[["key"]]
        key_env <- x[["key_env"]]
        if (is.null(key) && is.character(key_env) && nzchar(key_env)) {
            key <- Sys.getenv(key_env, unset = "")
            if (!nzchar(key)) {
                stop(sprintf("voice.%s.key_env names %s, which is not set", which,
                             key_env), call. = FALSE)
            }
        }
        if (is.character(key) && nzchar(key)) {
            x[["key"]] <- key
        } else {
            x[["key"]] <- NULL
        }
        x$url <- sub("/+$", "", x$url)
        x
    }
    stt <- one("stt", c("url", "model"))
    tts <- one("tts", c("url", "model"))
    tts$format <- tts$format %||% "wav"
    if (!identical(tts$format, "wav")) {
        stop("voice.tts.format: only \"wav\" is read", call. = FALSE)
    }
    list(stt = stt, tts = tts)
}

# Transcribe int16 samples at `rate`. Returns the text, trimmed; "" when
# the route heard nothing in it.
call_stt <- function(media, samples, rate, http = .call_http) {
    cfg <- media$stt
    wav <- wav_encode(samples, rate)
    path <- tempfile("utterance-", fileext = ".wav")
    on.exit(unlink(path), add = TRUE)
    writeBin(wav, path)
    form <- list(file = curl::form_file(path, type = "audio/wav",
                                        name = "utterance.wav"),
                 model = cfg$model, response_format = "json")
    if (is.character(cfg$language) && nzchar(cfg$language)) {
        form$language <- cfg$language
    }
    if (is.character(cfg$prompt) && nzchar(cfg$prompt)) {
        form$prompt <- cfg$prompt
    }
    res <- http(paste0(cfg$url, CALL_STT_ROUTE), headers = .call_auth(cfg),
                form = form, timeout_ms = CALL_STT_TIMEOUT_MS)
    .call_check(res, "transcription")
    ans <- tryCatch(jsonlite::fromJSON(rawToChar(res$body), simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.list(ans) || !is.character(ans$text) || length(ans$text) != 1L) {
        stop("the transcription route answered without a text field",
             call. = FALSE)
    }
    trimws(enc2utf8(ans$text))
}

# Synthesize text. Returns list(samples (int16, mono), rate).
call_tts <- function(media, text, http = .call_http) {
    cfg <- media$tts
    body <- list(model = cfg$model, input = text, response_format = "wav")
    if (is.character(cfg$voice) && nzchar(cfg$voice)) {
        body$voice <- cfg$voice
    }
    if (is.numeric(cfg$speed) && length(cfg$speed) == 1L) {
        body$speed <- cfg$speed
    }
    res <- http(paste0(cfg$url, CALL_TTS_ROUTE),
                headers = c(.call_auth(cfg), "content-type" = "application/json"),
                json = jsonlite::toJSON(body, auto_unbox = TRUE),
                timeout_ms = CALL_TTS_TIMEOUT_MS)
    .call_check(res, "synthesis")
    wav <- wav_decode(res$body)
    list(samples = pcm_mono(wav$samples, wav$channels), rate = wav$rate)
}

.call_auth <- function(cfg) {
    key <- cfg[["key"]]
    if (is.null(key)) {
        return(character())
    }
    c(authorization = paste("Bearer", key))
}

# A route's answer, or an error naming the route, the status, and what
# the route said (its first line, briefly).
.call_check <- function(res, what) {
    status <- as.integer(res$status)
    if (identical(status, 200L)) {
        return(invisible(NULL))
    }
    said <- tryCatch(rawToChar(res$body), error = function(e) "")
    said <- substr(gsub("[[:space:]]+", " ", said), 1L, 200L)
    stop(sprintf("the %s route answered HTTP %s%s", what,
            if (is.na(status)) "?" else status,
            if (nzchar(said)) paste0(": ", said) else ""), call. = FALSE)
}

# Default transport: one POST, multipart (`form`) or JSON (`json`).
# Returns list(status, body (raw)). A connection failure is an error
# the caller reports as the route being unreachable.
.call_http <- function(url, headers = character(), form = NULL, json = NULL,
                       timeout_ms = 30000L) {
    h <- curl::new_handle(timeout_ms = as.integer(timeout_ms),
                          connecttimeout_ms = 5000L)
    if (length(headers)) {
        curl::handle_setheaders(h, .list = as.list(headers))
    }
    if (!is.null(form)) {
        do.call(curl::handle_setform, c(list(h), form))
    } else {
        curl::handle_setopt(h, post = TRUE, postfields = charToRaw(as.character(json)))
    }
    res <- curl::curl_fetch_memory(url, handle = h)
    list(status = res$status_code, body = res$content)
}
