# Voice mode for llamaR
# High-level functions for speech-to-text and text-to-speech

#' Check if voice mode dependencies are available
#'
#' @param config Config list with voice settings
#' @return List with tts = TRUE/FALSE, stt = TRUE/FALSE, audio = TRUE/FALSE,
#'         reason = character (if not available)
#' @noRd
voice_available <- function(config = list()) {
    result <- list(
                   tts = FALSE,
                   stt = FALSE,
                   audio = FALSE,
                   reason = character()
    )

    # Check audio hardware
    audio <- audio_available()
    result$audio <- audio$available
    if (!audio$available) {
        result$reason <- c(result$reason, audio$reason)
    }

    # Check tts.api package
    if (requireNamespace("tts.api", quietly = TRUE)) {
        # Check if backend is reachable
        tts_cfg <- config$voice$tts %||% list()
        backend <- tts_cfg$backend %||% "chatterbox"

        if (backend == "qwen3") {
            port <- tts_cfg$port %||% 7811L
            result$tts <- tryCatch({
                tts.api::qwen3_available(port = port, timeout = 2)
            }, error = function(e) FALSE)
            if (!result$tts) {
                result$reason <- c(result$reason,
                                   sprintf("TTS backend (qwen3) not reachable on port %d",
                        port))
            }
        } else if (backend == "chatterbox") {
            port <- tts_cfg$port %||% 7810L
            result$tts <- tryCatch({
                tts.api::chatterbox_available(port = port, timeout = 2)
            }, error = function(e) FALSE)
            if (!result$tts) {
                result$reason <- c(result$reason,
                                   sprintf("TTS backend (chatterbox) not reachable on port %d",
                        port))
            }
        } else if (backend == "openai") {
            # OpenAI TTS doesn't need local server check
            result$tts <- TRUE
        } else if (backend == "elevenlabs") {
            # ElevenLabs needs API key
            result$tts <- tryCatch({
                key <- tts.api:::.elevenlabs_api_key()
                !is.null(key) && nchar(key) > 0
            }, error = function(e) FALSE)
            if (!result$tts) {
                result$reason <- c(result$reason, "ElevenLabs API key not set")
            }
        } else {
            result$reason <- c(result$reason,
                               sprintf("Unknown TTS backend: %s", backend))
        }
    } else {
        result$reason <- c(result$reason, "tts.api package not installed")
    }

    # Check stt.api package
    if (requireNamespace("stt.api", quietly = TRUE)) {
        stt_cfg <- config$voice$stt %||% list()
        backend <- stt_cfg$backend %||% "api"

        if (backend == "api") {
            # Check if API is reachable
            port <- stt_cfg$port %||% 4123L
            base_url <- sprintf("http://127.0.0.1:%d", port)
            health <- tryCatch({
                stt.api::set_stt_base(base_url)
                stt.api::stt_health()
            }, error = function(e) list(ok = FALSE))
            result$stt <- isTRUE(health$ok)
            if (!result$stt) {
                result$reason <- c(result$reason,
                                   sprintf("STT backend (API) not reachable on port %d", port))
            }
        } else if (backend == "whisper") {
            # Native whisper via audio.whisper - use stt_health to check
            health <- tryCatch({
                stt.api::stt_health()
            }, error = function(e) list(ok = FALSE))
            result$stt <- isTRUE(health$ok)
            if (!result$stt) {
                result$reason <- c(result$reason,
                                   health$message %||% "audio.whisper not available")
            }
        } else {
            result$stt <- FALSE
            result$reason <- c(result$reason,
                               sprintf("Unknown STT backend: %s", backend))
        }
    } else {
        result$reason <- c(result$reason, "stt.api package not installed")
    }

    result
}

#' Listen for speech and transcribe
#'
#' Records audio using push-to-talk (Enter to stop) and transcribes using stt.api
#'
#' @param config Config list with voice settings
#' @param show_status Show recording status messages
#' @return Transcribed text, or NULL if cancelled/failed
#' @noRd
voice_listen <- function(config = list(), show_status = TRUE) {
    if (!requireNamespace("stt.api", quietly = TRUE)) {
        stop("stt.api package is required for voice input")
    }

    # Get config
    stt_cfg <- config$voice$stt %||% list()
    audio_cfg <- config$voice$audio %||% list()

    backend <- stt_cfg$backend %||% "whisper"
    port <- stt_cfg$port %||% 4123L
    model <- stt_cfg$model %||% "base"
    sample_rate <- audio_cfg$sample_rate %||% 16000L
    device <- audio_cfg$input_device

    # Set up STT API if using API backend
    if (backend == "api") {
        base_url <- sprintf("http://127.0.0.1:%d", port)
        stt.api::set_stt_base(base_url)
    }

    # Create temp file for recording
    audio_file <- tempfile(fileext = ".wav")
    on.exit(unlink(audio_file), add = TRUE)

    if (show_status) {
        cat("\033[33mRecording... (press Enter to stop)\033[0m\n")
    }

    # Start recording in background
    pid <- audio_record_ptt(
                            file = audio_file,
                            max_duration = 60L,
                            device = device,
                            sample_rate = sample_rate
    )

    # Wait for Enter
    invisible(readline())

    # Stop recording
    audio_stop_recording(pid)

    # Check if file exists and has content
    if (!file.exists(audio_file) || file.size(audio_file) < 1000) {
        if (show_status) {
            cat("\033[2mNo audio recorded\033[0m\n")
        }
        return(NULL)
    }

    if (show_status) {
        cat("\033[2mTranscribing...\033[0m ")
    }

    # Transcribe
    result <- tryCatch({
        stt.api::stt(
                     file = audio_file,
                     model = model,
                     backend = backend,
                     response_format = "text"
        )
    }, error = function(e) {
        if (show_status) {
            cat(sprintf("\033[91mError: %s\033[0m\n", e$message))
        }
        NULL
    })

    if (!is.null(result) && show_status) {
        # Clear "Transcribing..." line and show result
        cat(sprintf("\033[92m\"%s\"\033[0m\n", result))
    }

    result
}

#' Speak text using TTS
#'
#' @param text Text to speak
#' @param config Config list with voice settings
#' @param show_status Show playback status
#' @return Invisible TRUE on success
#' @noRd
voice_speak <- function(text, config = list(), show_status = TRUE) {
    if (!requireNamespace("tts.api", quietly = TRUE)) {
        stop("tts.api package is required for voice output")
    }

    if (is.null(text) || nchar(trimws(text)) == 0) {
        return(invisible(FALSE))
    }

    # Get config
    tts_cfg <- config$voice$tts %||% list()
    backend <- tts_cfg$backend %||% "qwen3"
    voice <- tts_cfg$voice %||% "default"
    port <- tts_cfg$port %||% 7811L

    # Set up TTS base URL for local backends
    if (backend %in% c("chatterbox", "qwen3")) {
        base_url <- sprintf("http://127.0.0.1:%d", port)
        tts.api::set_tts_base(base_url)
    }

    # Create temp file for audio
    audio_file <- tempfile(fileext = ".wav")
    on.exit(unlink(audio_file), add = TRUE)

    if (show_status) {
        cat("\033[2mGenerating speech...\033[0m ")
    }

    # Generate speech
    result <- tryCatch({
        tts.api::tts(
                     input = text,
                     voice = voice,
                     file = audio_file,
                     backend = backend
        )
        TRUE
    }, error = function(e) {
        if (show_status) {
            cat(sprintf("\033[91mTTS error: %s\033[0m\n", e$message))
        }
        FALSE
    })

    if (!result) {
        return(invisible(FALSE))
    }

    if (show_status) {
        cat("\033[32m\033[0m\n") # speaker emoji
    }

    # Play audio
    tryCatch({
        audio_play(audio_file, wait = TRUE)
    }, error = function(e) {
        if (show_status) {
            cat(sprintf("\033[91mPlayback error: %s\033[0m\n", e$message))
        }
    })

    invisible(TRUE)
}

#' Get voice mode status message
#'
#' @param config Config list
#' @return Character string describing voice mode status
#' @noRd
voice_status <- function(config = list()) {
    avail <- voice_available(config)

    parts <- character()

    if (avail$audio) {
        parts <- c(parts, "audio: ok")
    } else {
        parts <- c(parts, "audio: unavailable")
    }

    if (avail$tts) {
        tts_backend <- config$voice$tts$backend %||% "qwen3"
        parts <- c(parts, sprintf("tts: %s", tts_backend))
    } else {
        parts <- c(parts, "tts: unavailable")
    }

    if (avail$stt) {
        stt_backend <- config$voice$stt$backend %||% "whisper"
        parts <- c(parts, sprintf("stt: %s", stt_backend))
    } else {
        parts <- c(parts, "stt: unavailable")
    }

    paste(parts, collapse = ", ")
}

#' Check if voice mode can be enabled
#'
#' @param config Config list
#' @return TRUE if all voice dependencies are available
#' @noRd
voice_can_enable <- function(config = list()) {
    avail <- voice_available(config)
    avail$audio && avail$tts && avail$stt
}

