library(tinytest)

# The call-mode agent's speech routes (R/call-media.R): config, the
# requests sent, and the answers read, against a fake router.

cfg <- list(voice = list(
    stt = list(url = "http://router:8080/", model = "whisper-small", language = "en"),
    tts = list(url = "http://router:8080", model = "chatterbox", voice = "ann")))

# ---- config ----
m <- corteza:::call_media_config(cfg)
expect_identical(m$stt$url, "http://router:8080")
expect_null(m$stt$key)
expect_identical(m$tts$format, "wav")
expect_error(corteza:::call_media_config(list(voice = list(stt = cfg$voice$stt))),
             "voice.tts is not configured")
expect_error(corteza:::call_media_config(list(voice = list(
    stt = list(url = "x"), tts = cfg$voice$tts))), "voice.stt.model")
expect_error(corteza:::call_media_config(list(voice = list(
    stt = cfg$voice$stt, tts = c(cfg$voice$tts, format = "mp3")))), "only \"wav\"")
# A credential: literal, or from the environment, which must be set.
with_key <- cfg
with_key$voice$stt$key <- "k-literal"
expect_identical(corteza:::call_media_config(with_key)$stt$key, "k-literal")
from_env <- cfg
from_env$voice$tts$key_env <- "CORTEZA_TEST_ROUTER_KEY"
Sys.unsetenv("CORTEZA_TEST_ROUTER_KEY")
expect_error(corteza:::call_media_config(from_env), "CORTEZA_TEST_ROUTER_KEY, which is not set")
Sys.setenv(CORTEZA_TEST_ROUTER_KEY = "k-env")
expect_identical(corteza:::call_media_config(from_env)$tts$key, "k-env")
# And it is sent, not the variable's name (`$` would have matched
# key_env for an absent key).
expect_identical(corteza:::.call_auth(corteza:::call_media_config(from_env)$tts),
                 c(authorization = "Bearer k-env"))
expect_identical(corteza:::.call_auth(list(key_env = "X")), character())
Sys.unsetenv("CORTEZA_TEST_ROUTER_KEY")

# ---- a fake router that records what it was sent ----
seen <- new.env()
fake <- function(status = 200L, body = NULL, tts_wav = NULL) {
    function(url, headers = character(), form = NULL, json = NULL, timeout_ms) {
        seen$url <- url
        seen$headers <- headers
        seen$form <- form
        seen$json <- json
        seen$timeout_ms <- timeout_ms
        if (!is.null(form)) {
            # The file part: read what would be uploaded.
            seen$wav <- readBin(form$file$path, "raw", file.info(form$file$path)$size)
            seen$filename <- form$file$name
        }
        if (!is.null(tts_wav)) {
            return(list(status = status, body = tts_wav))
        }
        list(status = status, body = charToRaw(body %||% ""))
    }
}

# ---- transcription ----
local({
    samples <- as.integer(round(3000 * sin(seq_len(1600) / 10)))
    text <- corteza:::call_stt(corteza:::call_media_config(with_key), samples, 16000L,
                               http = fake(body = '{"text": "  hello there \\n"}'))
    expect_identical(text, "hello there")
    expect_identical(seen$url, "http://router:8080/v1/audio/transcriptions")
    expect_identical(seen$headers[["authorization"]], "Bearer k-literal")
    expect_identical(seen$form$model, "whisper-small")
    expect_identical(seen$form$language, "en")
    expect_identical(seen$form$response_format, "json")
    expect_identical(seen$filename, "utterance.wav")
    expect_identical(seen$timeout_ms, corteza:::CALL_STT_TIMEOUT_MS)
    # What was uploaded is the utterance as 16 kHz WAV.
    back <- corteza:::wav_decode(seen$wav)
    expect_identical(back$samples, samples)
    expect_identical(back$rate, 16000L)
    # The temp file is gone afterwards.
    expect_false(file.exists(seen$form$file$path))
    # No credential configured: no authorization header.
    corteza:::call_stt(m, samples, 16000L, http = fake(body = '{"text":"x"}'))
    expect_false("authorization" %in% names(seen$headers))
    # Nothing heard is "".
    expect_identical(corteza:::call_stt(m, samples, 16000L,
                                        http = fake(body = '{"text":""}')), "")
    # Failures name the route and what it said.
    expect_error(corteza:::call_stt(m, samples, 16000L,
                                    http = fake(503L, "upstream   down")),
                 "transcription route answered HTTP 503: upstream down")
    expect_error(corteza:::call_stt(m, samples, 16000L, http = fake(body = "{}")),
                 "without a text field")
    expect_error(corteza:::call_stt(m, samples, 16000L,
                                    http = function(...) stop("could not connect")),
                 "could not connect")
})

# ---- synthesis ----
local({
    speech <- as.integer(round(8000 * sin(seq_len(2400) / 7)))
    wav <- corteza:::wav_encode(speech, 24000L)
    out <- corteza:::call_tts(m, "Hello there.", http = fake(tts_wav = wav))
    expect_identical(out$samples, speech)
    expect_identical(out$rate, 24000L)
    expect_identical(seen$url, "http://router:8080/v1/audio/speech")
    expect_identical(seen$headers[["content-type"]], "application/json")
    sent <- jsonlite::fromJSON(seen$json)
    expect_identical(sent$model, "chatterbox")
    expect_identical(sent$input, "Hello there.")
    expect_identical(sent$voice, "ann")
    expect_identical(sent$response_format, "wav")
    expect_identical(seen$timeout_ms, corteza:::CALL_TTS_TIMEOUT_MS)
    # Stereo comes back mono.
    st <- corteza:::wav_encode(c(100L, 300L, -100L, -300L), 24000L, 2L)
    expect_identical(corteza:::call_tts(m, "x", http = fake(tts_wav = st))$samples,
                     c(200L, -200L))
    expect_error(corteza:::call_tts(m, "x", http = fake(500L, "no such voice")),
                 "synthesis route answered HTTP 500: no such voice")
    expect_error(corteza:::call_tts(m, "x", http = fake(body = "not audio")), "RIFF")
})

# ---- the gpu-host/1 wire ----
# One POST /infer per request: protocol, entry, content-hash key, input;
# whisper takes audio_b64 and answers an {ok, value} envelope; chatterbox
# takes text and the reference clip and answers float32 PCM with the
# rate in a header. The token file's bytes travel base64-encoded.
local({
    token <- tempfile("gpuhost-token-")
    writeBin(as.raw(1:32), token)
    clip <- tempfile("ref-", fileext = ".wav")
    writeBin(corteza:::wav_encode(as.integer(sin(seq_len(2400) / 10) * 8000), 24000L),
             clip)
    gcfg <- list(voice = list(
        stt = list(wire = "gpu-host", url = "http://g5:7878/", model = "whisper-small",
                   key_file = token),
        tts = list(wire = "gpu-host", url = "http://g5:7878", model = "chatterbox-turbo",
                   voice = clip, key_file = token)))
    m <- corteza:::call_media_config(gcfg)
    expect_identical(m$stt$wire, "gpu-host")
    expect_identical(m$stt$key_file, token)
    expect_identical(jsonlite::base64_dec(m$tts$voice_b64),
                     readBin(clip, "raw", file.size(clip)))
    expect_identical(corteza:::.call_auth(m$stt),
                     c(authorization = paste("Bearer", jsonlite::base64_enc(as.raw(1:32)))))
    # Refusals in the config.
    bad <- gcfg
    bad$voice$tts$voice <- "/nonexistent/ref.wav"
    expect_error(corteza:::call_media_config(bad), "reference WAV")
    bad <- gcfg
    bad$voice$stt$wire <- "grpc"
    expect_error(corteza:::call_media_config(bad), "voice.stt.wire")
    bad <- gcfg
    bad$voice$stt$key_file <- "/nonexistent/token"
    expect_error(corteza:::call_media_config(bad), "no file at")

    seen <- new.env()
    fake <- function(status = 200L, body = raw(0), headers = list()) {
        function(url, headers_sent = character(), form = NULL, json = NULL,
                 timeout_ms = 0L, ...) {
            seen$url <- url
            seen$headers <- headers_sent
            seen$body <- jsonlite::fromJSON(as.character(json), simplifyVector = FALSE)
            list(status = status, body = if (is.raw(body)) body else charToRaw(body),
                 headers = headers)
        }
    }
    # The fake takes `headers` positionally as the second argument.
    fake2 <- function(...) {
        f <- fake(...)
        function(url, headers = character(), form = NULL, json = NULL, timeout_ms = 0L) {
            f(url, headers_sent = headers, form = form, json = json, timeout_ms = timeout_ms)
        }
    }

    # Transcription.
    samples <- as.integer(sin(seq_len(16000) / 5) * 10000)
    text <- corteza:::call_stt(m, samples, 16000L,
                               http = fake2(body = '{"ok": true, "value": {"text": " hello there ", "segments": []}}'))
    expect_identical(text, "hello there")
    expect_identical(seen$url, "http://g5:7878/infer")
    expect_identical(seen$headers[["content-type"]], "application/json")
    expect_true(startsWith(seen$headers[["authorization"]], "Bearer "))
    expect_identical(seen$body$v, "gpu-host/1")
    expect_identical(seen$body$entry, "whisper-small")
    expect_identical(jsonlite::base64_dec(seen$body$input$audio_b64),
                     corteza:::wav_encode(samples, 16000L))
    expect_true(grepl("^corteza-whisper-small-[0-9a-f]{64}$", seen$body$key))
    # The key is the content's: same audio, same key; other audio, another.
    k1 <- seen$body$key
    corteza:::call_stt(m, samples, 16000L,
                       http = fake2(body = '{"ok": true, "value": {"text": "x"}}'))
    expect_identical(seen$body$key, k1)
    corteza:::call_stt(m, samples[-1L], 16000L,
                       http = fake2(body = '{"ok": true, "value": {"text": "x"}}'))
    expect_false(identical(seen$body$key, k1))
    # Refusals.
    expect_error(corteza:::call_stt(m, samples, 16000L,
                                    http = fake2(body = '{"ok": false, "error": "no card"}')),
                 "transcription entry failed: no card")
    expect_error(corteza:::call_stt(m, samples, 16000L,
                                    http = fake2(503L, '{"error": "busy"}')),
                 "transcription route answered HTTP 503")
    expect_error(corteza:::call_stt(m, samples, 16000L,
                                    http = fake2(body = '{"ok": true, "value": {}}')),
                 "without a text field")

    # Synthesis: float32 in, int16 out, the rate from the header.
    pcm <- c(0, 0.5, -0.5, 1, -1, 2)
    audio <- writeBin(pcm, raw(), size = 4L, endian = "little")
    out <- corteza:::call_tts(m, "Hello.",
                              http = fake2(body = audio,
                                           headers = list("content-type" = "application/octet-stream",
                                                          "x-gpuhost-meta" = '{"sample_rate": 24000, "reused": false}')))
    expect_identical(out$rate, 24000L)
    expect_identical(out$samples, c(0L, 16384L, -16384L, 32767L, -32767L, 32767L))
    expect_identical(seen$body$entry, "chatterbox-turbo")
    expect_identical(seen$body$input$text, "Hello.")
    expect_identical(seen$body$input$voice_b64, m$tts$voice_b64)
    expect_error(corteza:::call_tts(m, "x", http = fake2(body = audio,
                                                           headers = list("content-type" = "application/octet-stream"))),
                 "without a sample_rate")
    expect_error(corteza:::call_tts(m, "x", http = fake2(body = '{"ok": false, "error": "text too long"}',
                                                           headers = list("content-type" = "application/json"))),
                 "returned no audio")
})
