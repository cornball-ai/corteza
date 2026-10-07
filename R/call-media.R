# The call-mode agent's speech routes: transcription and synthesis over
# HTTP, one request per utterance and per sentence, on one of two wires.
#
# The agent posts with curl, which corteza already imports. Base URLs,
# model names and the credential come from the bot config under `voice`:
#
#   "voice": {
#     "stt": {"url": "http://router:8080", "model": "whisper-small",
#             "language": "en", "key_env": "GPU_HOST_KEY"},
#     "tts": {"url": "http://router:8080", "model": "chatterbox",
#             "voice": "default", "key_env": "GPU_HOST_KEY"}
#   }
#
# `wire` picks the shape. "openai" (the default) is the OpenAI audio
# API: multipart `/v1/audio/transcriptions`, JSON `/v1/audio/speech`
# answering a WAV. "gpu-host" is the fleet's gpu-host/1 wire, spoken by
# a gpu.ctl host directly: one `POST /infer` with the entry (`model`),
# a content-hash key and the input, where whisper takes `audio_b64` and
# chatterbox takes `text` and `voice_b64` (the reference clip, so
# `voice` is a path to a WAV) and answers float32 PCM with the rate in
# the `X-GpuHost-Meta` header.
#
# `key` holds a credential literally, `key_env` names the variable that
# does, and `key_file` a file of raw bytes sent base64-encoded (the
# gpu-host token, read at each request so a rotation needs no restart);
# none is required, since a router on the tailnet may not ask. Every
# request goes through `http`, a function the tests replace, so the
# shapes sent and the answers read are checked without a host.

CALL_STT_ROUTE <- "/v1/audio/transcriptions"
CALL_TTS_ROUTE <- "/v1/audio/speech"
CALL_GPU_HOST_ROUTE <- "/infer"
CALL_GPU_HOST_PROTOCOL <- "gpu-host/1"
CALL_STT_TIMEOUT_MS <- 30000L
CALL_TTS_TIMEOUT_MS <- 60000L
CALL_WIRES <- c("openai", "gpu-host")

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
        x$wire <- x$wire %||% "openai"
        if (!is.character(x$wire) || length(x$wire) != 1L ||
            !x$wire %in% CALL_WIRES) {
            stop(sprintf("voice.%s.wire must be one of %s", which,
                         paste(dQuote(CALL_WIRES, FALSE), collapse = ", ")),
                 call. = FALSE)
        }
        # `[[`: `x$key` would answer with key_env when key is absent.
        key <- x[["key"]]
        key_env <- x[["key_env"]]
        key_file <- x[["key_file"]]
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
        if (!is.null(key_file)) {
            key_file <- path.expand(key_file)
            if (!file.exists(key_file)) {
                stop(sprintf("voice.%s.key_file: no file at %s", which, key_file),
                     call. = FALSE)
            }
            x[["key_file"]] <- key_file
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
    if (identical(tts$wire, "gpu-host")) {
        # The reference clip travels in every request; read it once.
        voice <- tts[["voice"]]
        if (!is.character(voice) || length(voice) != 1L ||
            !file.exists(path.expand(voice))) {
            stop("voice.tts.voice must be the path of a reference WAV on the ",
                 "gpu-host wire", call. = FALSE)
        }
        voice <- path.expand(voice)
        tts$voice_b64 <- jsonlite::base64_enc(readBin(voice, "raw",
                file.size(voice)))
    }
    list(stt = stt, tts = tts)
}

# Transcribe int16 samples at `rate`. Returns the text, trimmed; "" when
# the route heard nothing in it.
call_stt <- function(media, samples, rate, http = .call_http) {
    cfg <- media$stt
    wav <- wav_encode(samples, rate)
    if (identical(cfg$wire, "gpu-host")) {
        value <- .call_gpu_host(cfg,
                                list(audio_b64 = jsonlite::base64_enc(wav)),
                                CALL_STT_TIMEOUT_MS, "transcription", http)
        ans <- .call_gpu_host_value(value$res, "transcription")
        if (!is.list(ans) || !is.character(ans$text) ||
            length(ans$text) != 1L) {
            stop("the transcription entry answered without a text field",
                 call. = FALSE)
        }
        return(trimws(enc2utf8(ans$text)))
    }
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
    if (identical(cfg$wire, "gpu-host")) {
        value <- .call_gpu_host(cfg,
                                list(text = text, voice_b64 = cfg$voice_b64),
                                CALL_TTS_TIMEOUT_MS, "synthesis", http)
        return(.call_gpu_host_pcm(value$res, "synthesis"))
    }
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
    if (is.null(key) && !is.null(cfg[["key_file"]])) {
        f <- cfg[["key_file"]]
        key <- jsonlite::base64_enc(readBin(f, "raw", file.size(f)))
    }
    if (is.null(key)) {
        return(character())
    }
    c(authorization = paste("Bearer", key))
}

# ---- The gpu-host/1 wire ---------------------------------------------

# One inference on a gpu.ctl host: `POST /infer` with the protocol, the
# entry, a key, and the input. Returns list(res) with the checked reply.
.call_gpu_host <- function(cfg, input, timeout_ms, what, http = .call_http) {
    body <- list(v = CALL_GPU_HOST_PROTOCOL, entry = cfg$model,
                 key = .call_gpu_host_key(cfg$model, input), input = input)
    res <- http(paste0(cfg$url, CALL_GPU_HOST_ROUTE),
                headers = c(.call_auth(cfg), "content-type" = "application/json"),
                json = jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA,
                                        null = "null"),
                timeout_ms = timeout_ms)
    .call_check(res, what)
    list(res = res)
}

# The request key: a content hash over the entry and every input field,
# the way gpu.host derives it (sorted names, each name and value
# length-framed), so the host dedups a retry and never shares a result
# between different requests. Prefixed as corteza's own.
.call_gpu_host_key <- function(entry, input) {
    frame <- function(bytes) c(charToRaw(sprintf("%d:", length(bytes))), bytes)
    parts <- list()
    for (k in sort(names(input))) {
        v <- input[[k]]
        bytes <- if (is.raw(v)) {
            v
        } else {
            charToRaw(as.character(jsonlite::toJSON(v, auto_unbox = TRUE,
                        digits = NA, null = "null")))
        }
        parts <- c(parts, list(frame(charToRaw(k)), frame(bytes)))
    }
    bytes <- unlist(parts, use.names = FALSE) %||% raw(0L)
    paste0("corteza-", entry, "-",
           digest::digest(bytes, algo = "sha256", serialize = FALSE))
}

# A JSON reply: the `value` of an `{ok, value}` envelope.
.call_gpu_host_value <- function(res, what) {
    txt <- tryCatch(rawToChar(res$body), error = function(e) "")
    out <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.list(out)) {
        stop(sprintf("the %s entry did not answer JSON: %s", what,
                     substr(txt, 1L, 200L)), call. = FALSE)
    }
    if (!isTRUE(out$ok)) {
        stop(sprintf("the %s entry failed: %s", what,
                     paste(unlist(out$error) %||% "no reason given",
                           collapse = " ")), call. = FALSE)
    }
    out$value
}

# An audio reply: float32 little-endian PCM with the rate in the meta
# header, as list(samples (int16), rate).
.call_gpu_host_pcm <- function(res, what) {
    type <- .call_header(res, "content-type")
    if (!grepl("octet-stream", type, fixed = TRUE)) {
        said <- tryCatch(rawToChar(res$body), error = function(e) "")
        stop(sprintf("the %s entry returned no audio (%s): %s", what,
                if (nzchar(type)) type else "no type",
                     substr(said, 1L, 200L)), call. = FALSE)
    }
    meta <- tryCatch(jsonlite::fromJSON(.call_header(res, "x-gpuhost-meta")),
                     error = function(e) NULL)
    rate <- suppressWarnings(as.integer(meta$sample_rate))
    if (!length(rate) || is.na(rate)) {
        stop(sprintf("the %s entry answered audio without a sample_rate", what),
             call. = FALSE)
    }
    x <- readBin(res$body, "numeric", n = length(res$body) %/% 4L, size = 4L,
                 endian = "little")
    list(samples = as.integer(round(pmax(pmin(x, 1), -1) * 32767)), rate = rate)
}

.call_header <- function(res, name) {
    h <- res$headers
    if (!length(h)) {
        return("")
    }
    v <- h[[tolower(name)]] %||% h[[name]]
    if (is.null(v)) {
        ""
    } else {
        as.character(v)[[1L]]
    }
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
# Returns list(status, body (raw), headers (named, lower-case)). A
# connection failure is an error the caller reports as the route being
# unreachable.
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
    headers <- tryCatch(curl::parse_headers_list(res$headers),
                        error = function(e) list())
    list(status = res$status_code, body = res$content, headers = headers)
}
