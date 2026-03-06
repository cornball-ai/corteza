# Audio recording and playback for voice mode
# Uses system tools: arecord/aplay (ALSA) on Linux, sox as fallback

#' Check if audio system is available
#'
#' @return List with available = TRUE/FALSE and details about what's available
#' @noRd
audio_available <- function() {
    result <- list(available = FALSE, record = NULL, play = NULL, reason = NULL)

    # Check recording tools (in order of preference)
    if (Sys.which("arecord") != "") {
        result$record <- "arecord"
    } else if (Sys.which("sox") != "") {
        result$record <- "sox"
    }

    # Check playback tools
    if (Sys.which("aplay") != "") {
        result$play <- "aplay"
    } else if (Sys.which("paplay") != "") {
        result$play <- "paplay"
    } else if (Sys.which("sox") != "") {
        result$play <- "sox"
    } else if (Sys.which("ffplay") != "") {
        result$play <- "ffplay"
    }

    if (is.null(result$record)) {
        result$reason <- "No recording tool found (need arecord or sox)"
    } else if (is.null(result$play)) {
        result$reason <- "No playback tool found (need aplay, paplay, sox, or ffplay)"
    } else {
        result$available <- TRUE
    }

    result
}

#' Record audio to file
#'
#' Records audio using system tools. On Linux, uses arecord (ALSA) by default,
#' falls back to sox if arecord is not available.
#'
#' @param file Output file path (WAV format)
#' @param duration Maximum recording duration in seconds (NULL = until stopped)
#' @param device Audio input device (NULL = default)
#' @param sample_rate Sample rate in Hz (default: 16000)
#' @param format Audio format: "wav" (default)
#' @return Invisible TRUE on success, error on failure
#' @noRd
audio_record <- function(file, duration = NULL, device = NULL,
                         sample_rate = 16000L, format = "wav") {
    audio <- audio_available()
    if (!audio$available) {
        stop(audio$reason)
    }

    # Build command based on available tool
    if (audio$record == "arecord") {
        # arecord options:
        # -f cd = CD quality (16-bit, stereo, 44100Hz) - but we want mono 16kHz
        # -t wav = WAV format
        # -r = sample rate
        # -c 1 = mono
        # -D = device
        # -d = duration
        args <- c(
                  "-t", format,
                  "-f", "S16_LE", # 16-bit signed little-endian
                  "-r", as.character(sample_rate),
                  "-c", "1" # mono
        )
        if (!is.null(device)) {
            args <- c(args, "-D", device)
        }
        if (!is.null(duration)) {
            args <- c(args, "-d", as.character(duration))
        }
        args <- c(args, file)

        # Run recording
        status <- system2("arecord", args, stdout = FALSE, stderr = FALSE)

    } else if (audio$record == "sox") {
        # sox rec options
        args <- c(
                  "-r", as.character(sample_rate),
                  "-c", "1", # mono
                  "-b", "16", # 16-bit
                  file
        )
        if (!is.null(duration)) {
            args <- c(args, "trim", "0", as.character(duration))
        }

        status <- system2("sox", c("-d", args), stdout = FALSE, stderr = FALSE)
    }

    if (status != 0) {
        stop("Recording failed with status ", status)
    }

    invisible(TRUE)
}

#' Record audio with push-to-talk (press Enter to stop)
#'
#' Starts recording in the background, waits for Enter key, then stops.
#'
#' @param file Output file path
#' @param max_duration Maximum duration in seconds (safety limit)
#' @param device Audio input device
#' @param sample_rate Sample rate
#' @return Invisible TRUE on success
#' @noRd
audio_record_ptt <- function(file, max_duration = 60L, device = NULL,
                             sample_rate = 16000L) {
    audio <- audio_available()
    if (!audio$available) {
        stop(audio$reason)
    }

    # Start recording in background
    if (audio$record == "arecord") {
        args <- c(
                  "-t", "wav",
                  "-f", "S16_LE",
                  "-r", as.character(sample_rate),
                  "-c", "1",
                  "-d", as.character(max_duration)
        )
        if (!is.null(device)) {
            args <- c(args, "-D", device)
        }
        args <- c(args, file)

        # Run in background, capture PID
        cmd <- sprintf("arecord %s & echo $!",
                       paste(shQuote(args), collapse = " "))
        pid <- system(sprintf("bash -c '%s'", cmd), intern = TRUE,
                      ignore.stderr = TRUE)
        pid <- as.integer(pid[length(pid)])

    } else if (audio$record == "sox") {
        args <- c(
                  "-d",
                  "-r", as.character(sample_rate),
                  "-c", "1",
                  "-b", "16",
                  file,
                  "trim", "0", as.character(max_duration)
        )

        cmd <- sprintf("sox %s & echo $!", paste(shQuote(args), collapse = " "))
        pid <- system(sprintf("bash -c '%s'", cmd), intern = TRUE,
                      ignore.stderr = TRUE)
        pid <- as.integer(pid[length(pid)])
    }

    # Return the PID so caller can stop it
    pid
}

#' Stop a background recording process
#'
#' @param pid Process ID from audio_record_ptt
#' @return Invisible TRUE
#' @noRd
audio_stop_recording <- function(pid) {
    if (!is.null(pid) && !is.na(pid)) {
        # Send SIGINT to gracefully stop recording
        system2("kill", c("-INT", as.character(pid)),
                stdout = FALSE, stderr = FALSE)
        # Wait a moment for file to be finalized
        Sys.sleep(0.2)
    }
    invisible(TRUE)
}

#' Play audio file
#'
#' @param file Audio file path
#' @param wait Wait for playback to complete (default: TRUE)
#' @return Invisible TRUE on success
#' @noRd
audio_play <- function(file, wait = TRUE) {
    if (!file.exists(file)) {
        stop("Audio file not found: ", file)
    }

    audio <- audio_available()
    if (is.null(audio$play)) {
        stop("No playback tool found")
    }

    # Build command based on available tool
    if (audio$play == "aplay") {
        args <- c("-q", file) # -q = quiet
        status <- system2("aplay", args, wait = wait,
                          stdout = FALSE, stderr = FALSE)

    } else if (audio$play == "paplay") {
        args <- file
        status <- system2("paplay", args, wait = wait,
                          stdout = FALSE, stderr = FALSE)

    } else if (audio$play == "sox") {
        # sox play command
        args <- c("-q", file)
        status <- system2("play", args, wait = wait,
                          stdout = FALSE, stderr = FALSE)

    } else if (audio$play == "ffplay") {
        # ffplay with minimal output
        args <- c("-nodisp", "-autoexit", "-loglevel", "quiet", file)
        status <- system2("ffplay", args, wait = wait,
                          stdout = FALSE, stderr = FALSE)
    }

    if (wait && status != 0) {
        stop("Playback failed with status ", status)
    }

    invisible(TRUE)
}

#' List available audio input devices
#'
#' @return Character vector of device names, or NULL if unavailable
#' @noRd
audio_list_devices <- function() {
    if (Sys.which("arecord") != "") {
        # arecord -L lists devices
        output <- system2("arecord", "-L", stdout = TRUE, stderr = FALSE)
        # Filter to actual device lines (not descriptions)
        devices <- output[!grepl("^\\s", output) & nchar(output) > 0]
        return(devices)
    }
    NULL
}

