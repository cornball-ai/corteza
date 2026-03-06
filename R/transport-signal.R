# Signal Transport
# Connects to signal-cli daemon via HTTP JSON-RPC + SSE

# ============================================================================
# Pure helper functions (top-level, testable)
# ============================================================================

#' Check if signal-cli daemon is running
#'
#' @param base_url Base URL of the daemon
#' @return TRUE if daemon responds, FALSE otherwise
#' @noRd
signal_check_daemon <- function(base_url) {
    url <- sprintf("%s/api/v1/check", base_url)
    tryCatch({
        h <- curl::new_handle()
        curl::handle_setopt(h, timeout = 5)
        resp <- curl::curl_fetch_memory(url, handle = h)
        resp$status_code == 200
    }, error = function(e) FALSE)
}

#' Send JSON-RPC request to signal-cli daemon
#'
#' @param base_url Base URL of the daemon
#' @param method RPC method name
#' @param params List of parameters
#' @return Result from RPC call, or NULL for 201 responses
#' @noRd
signal_rpc_request <- function(base_url, method, params = list()) {
    url <- sprintf("%s/api/v1/rpc", base_url)
    body <- jsonlite::toJSON(list(
                                  jsonrpc = "2.0",
                                  method = method,
                                  params = params,
                                  id = as.character(as.numeric(Sys.time()) * 1000)
        ), auto_unbox = TRUE, null = "null")

    h <- curl::new_handle()
    curl::handle_setopt(h,
                        customrequest = "POST",
                        postfields = body,
                        timeout = 30
    )
    curl::handle_setheaders(h, "Content-Type" = "application/json")

    resp <- curl::curl_fetch_memory(url, handle = h)

    if (resp$status_code >= 400) {
        stop("Signal RPC error: HTTP ", resp$status_code, call. = FALSE)
    }

    if (resp$status_code == 201) {
        return(NULL)
    }

    result <- jsonlite::fromJSON(rawToChar(resp$content),
                                 simplifyVector = FALSE)
    if (!is.null(result$error)) {
        stop("Signal RPC error: ", result$error$message %||% "Unknown",
             call. = FALSE)
    }
    result$result
}

#' Parse attachments from Signal message
#'
#' @param attachments List of attachment objects from dataMessage
#' @return Normalized list of attachments
#' @noRd
signal_parse_attachments <- function(attachments) {
    if (is.null(attachments) || length(attachments) == 0) {
        return(list())
    }

    lapply(attachments, function(att) {
        list(
             id = att$id,
             contentType = att$contentType %||% "application/octet-stream",
             filename = att$filename,
             size = att$size,
             width = att$width,
             height = att$height,
             path = att$file
        )
    })
}

#' Parse a Signal envelope into a normalized message
#'
#' @param envelope Envelope object from SSE event
#' @param allow_from Character vector of allowed sender numbers (empty = allow all)
#' @return Normalized message list, or NULL if not a valid message
#' @noRd
signal_parse_envelope <- function(envelope, allow_from = character()) {
    if (is.null(envelope)) {
        return(NULL)
    }

    sender <- envelope$source %||% envelope$sourceNumber
    if (is.null(sender)) {
        return(NULL)
    }

    # Handle receipt messages (delivery/read)
    if (!is.null(envelope$receiptMessage)) {
        receipt <- envelope$receiptMessage
        return(message_normalize(
                                 text = "",
                                 sender = sender,
                                 channel = "signal",
                                 id = as.character(envelope$timestamp),
                                 metadata = list(
                    type = "receipt",
                    receipt_type = if (isTRUE(receipt$isDelivery)) "delivery"
                    else if (isTRUE(receipt$isRead)) "read"
                    else "unknown",
                    timestamps = receipt$timestamps,
                    timestamp = envelope$timestamp
                )
            ))
    }

    # Handle reaction messages
    if (!is.null(envelope$dataMessage$reaction)) {
        reaction <- envelope$dataMessage$reaction
        return(message_normalize(
                                 text = "",
                                 sender = sender,
                                 channel = "signal",
                                 id = as.character(envelope$timestamp),
                                 metadata = list(
                    type = "reaction",
                    emoji = reaction$emoji,
                    is_remove = isTRUE(reaction$isRemove),
                    target_sender = reaction$targetAuthor,
                    target_timestamp = reaction$targetSentTimestamp,
                    timestamp = envelope$timestamp
                )
            ))
    }

    # Get message content
    dm <- envelope$dataMessage
    if (is.null(dm)) {
        return(NULL)
    }

    # Check allowlist
    if (length(allow_from) > 0 && !sender %in% allow_from) {
        log_msg("Signal: ignoring message from", sender, "(not in allow_from)")
        return(NULL)
    }

    text <- dm$message %||% ""
    attachments <- signal_parse_attachments(dm$attachments)

    # Require text or attachments
    if (nchar(text) == 0 && length(attachments) == 0) {
        return(NULL)
    }

    timestamp <- envelope$timestamp %||% as.numeric(Sys.time()) * 1000

    # Extract group info
    group_id <- NULL
    group_name <- NULL
    if (!is.null(dm$groupInfo)) {
        group_id <- dm$groupInfo$groupId
        group_name <- dm$groupInfo$groupName
    }

    message_normalize(
                      text = text,
                      sender = sender,
                      channel = "signal",
                      id = as.character(timestamp),
                      attachments = attachments,
                      metadata = list(
                                      group_id = group_id,
                                      group_name = group_name,
                                      is_group = !is.null(group_id),
                                      timestamp = timestamp
        )
    )
}

#' Parse SSE event JSON into a normalized message
#'
#' @param data_json JSON string from SSE data field
#' @param allow_from Character vector of allowed senders
#' @return Normalized message, or NULL on error/invalid
#' @noRd
signal_parse_sse_event <- function(data_json, allow_from = character()) {
    tryCatch({
        event <- jsonlite::fromJSON(data_json, simplifyVector = FALSE)
        signal_parse_envelope(event$envelope, allow_from)
    }, error = function(e) {
        log_msg("Signal: failed to parse event:", e$message)
        NULL
    })
}

#' Process SSE buffer and extract complete events
#'
#' @param buffer Current buffer string
#' @param current_event Current partial event being built
#' @param allow_from Allowed senders for filtering
#' @return List with: buffer (remaining), current_event, messages (list of parsed messages)
#' @noRd
sse_process_buffer <- function(buffer, current_event,
                               allow_from = character()) {
    messages <- list()

    while (grepl("\n", buffer)) {
        newline_pos <- regexpr("\n", buffer)[1]
        line <- substr(buffer, 1, newline_pos - 1)
        buffer <- substr(buffer, newline_pos + 1, nchar(buffer))
        line <- sub("\r$", "", line)

        if (line == "") {
            # Empty line = end of event
            if (!is.null(current_event$data)) {
                msg <- signal_parse_sse_event(current_event$data, allow_from)
                if (!is.null(msg)) {
                    messages <- c(messages, list(msg))
                }
            }
            current_event <- list()
        } else if (startsWith(line, "data:")) {
            data_value <- sub("^data: ?", "", line)
            current_event$data <- if (is.null(current_event$data)) {
                data_value
            } else {
                paste0(current_event$data, "\n", data_value)
            }
        } else if (startsWith(line, "event:")) {
            current_event$event <- sub("^event: ?", "", line)
        } else if (startsWith(line, "id:")) {
            current_event$id <- sub("^id: ?", "", line)
        }
        # Ignore comment lines (starting with :)
    }

    list(buffer = buffer, current_event = current_event, messages = messages)
}

# ============================================================================
# Transport factory
# ============================================================================

#' Signal transport
#'
#' Requires signal-cli running in daemon mode:
#'   signal-cli -a +1234567890 daemon --http 127.0.0.1:8080
#'
#' @param config List with (matches openclaw channels.signal.*):
#'   - httpHost: Daemon host (default: "127.0.0.1")
#'   - httpPort: Daemon port (default: 8080)
#'   - httpUrl: Full URL (overrides httpHost/httpPort)
#'   - account: Signal account phone number (required)
#'   - allowFrom: Vector of allowed sender numbers (optional)
#' @return Transport object
#' @noRd
transport_signal <- function(config = list()) {
    # Resolve base URL
    if (!is.null(config$httpUrl) && nchar(trimws(config$httpUrl)) > 0) {
        base_url <- sub("/+$", "", trimws(config$httpUrl))
    } else {
        host <- config$httpHost %||% "127.0.0.1"
        port <- config$httpPort %||% 8080L
        base_url <- sprintf("http://%s:%d", host, port)
    }

    account <- config$account
    if (is.null(account)) {
        stop("Signal transport requires 'account' (phone number)",
             call. = FALSE)
    }

    allow_from <- config$allowFrom %||% character()
    text_chunk_limit <- config$textChunkLimit %||% 4000L
    chunk_mode <- config$chunkMode %||% "length"

    # SSE state (no account param needed in single-account mode)
    sse_url <- sprintf("%s/api/v1/events", base_url)
    running <- FALSE

    # Helper: make RPC call with this base_url
    rpc <- function(method, params = list()) {
        signal_rpc_request(base_url, method, params)
    }

    # Reuse a single handle to avoid exhausting curl's connection pool
    sse_handle <- curl::new_handle()

    # Poll SSE for messages
    poll_messages <- function(timeout_secs = 1) {
        curl::handle_reset(sse_handle)
        curl::handle_setopt(sse_handle, timeout = timeout_secs)
        curl::handle_setheaders(sse_handle, "Accept" = "text/event-stream")

        buffer <- ""
        current_event <- list()
        all_messages <- list()

        tryCatch({
            curl::curl_fetch_stream(sse_url, function(data) {
                chunk <- rawToChar(data)
                buffer <<- paste0(buffer, chunk)

                result <- sse_process_buffer(buffer, current_event, allow_from)
                buffer <<- result$buffer
                current_event <<- result$current_event
                all_messages <<- c(all_messages, result$messages)

                TRUE
            }, handle = sse_handle)
        }, error = function(e) {
            if (!grepl("Timeout|timed out|cannot open", e$message,
                       ignore.case = TRUE)) {
                log_msg("Signal SSE error:", e$message)
            }
        })

        all_messages
    }

    list(
         type = "signal",
         config = config,

         check = function() signal_check_daemon(base_url),

         receive = function(timeout = 1) {
        poll_messages(timeout)
    },

         receive_one = function(timeout = 30) {
        start_time <- Sys.time()
        while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
            msgs <- poll_messages(1)
            if (length(msgs) > 0) return(msgs[[1]])
        }
        NULL
    },

         send = function(msg) {
        recipient <- msg$metadata$reply_to %||% msg$sender
        if (is.null(recipient)) {
            stop("Signal send: no recipient", call. = FALSE)
        }

        group_id <- msg$metadata$group_id
        chunks <- chunk_text_with_mode(msg$text, text_chunk_limit, chunk_mode)
        if (length(chunks) == 0) chunks <- ""

        # Collect valid attachment paths
        attachment_paths <- character()
        if (!is.null(msg$attachments)) {
            for (att in msg$attachments) {
                if (!is.null(att$path) && file.exists(att$path)) {
                    attachment_paths <- c(attachment_paths, att$path)
                }
            }
        }

        results <- list()
        for (i in seq_along(chunks)) {
            params <- list(message = chunks[[i]], account = account)

            if (!is.null(group_id)) {
                params$groupId <- group_id
            } else {
                params$recipient <- list(recipient)
            }

            # Attach files to last chunk only
            if (i == length(chunks) && length(attachment_paths) > 0) {
                params$attachments <- as.list(attachment_paths)
            }

            results <- c(results, list(rpc("send", params)))
            if (length(chunks) > 1) Sys.sleep(0.1)
        }

        invisible(results)
    },

         send_typing = function(recipient, group_id = NULL, stop = FALSE) {
        params <- list(account = account)
        if (!is.null(group_id)) {
            params$groupId <- group_id
        } else {
            params$recipient <- list(recipient)
        }
        if (stop) params$stop <- TRUE

        tryCatch(rpc("sendTyping", params), error = function(e) NULL)
        invisible(NULL)
    },

         send_reaction = function(target_sender, target_timestamp, emoji,
                                  group_id = NULL, remove = FALSE) {
        params <- list(
                       account = account,
                       targetAuthor = target_sender,
                       targetTimestamp = target_timestamp,
                       emoji = emoji
        )
        if (!is.null(group_id)) {
            params$groupId <- group_id
        } else {
            params$recipient <- list(target_sender)
        }
        if (remove) params$remove <- TRUE

        tryCatch(rpc("sendReaction", params), error = function(e) {
            log_msg("Signal: failed to send reaction:", e$message)
            NULL
        })
        invisible(NULL)
    },

         list_groups = function() {
        tryCatch({
            result <- rpc("listGroups", list(account = account))
            if (is.null(result)) return(list())
            lapply(result, function(g) {
                list(
                     id = g$id,
                     name = g$name,
                     description = g$description,
                     members = g$members,
                     is_admin = g$isAdmin %||% FALSE,
                     is_blocked = g$isBlocked %||% FALSE
                )
            })
        }, error = function(e) {
            log_msg("Signal: failed to list groups:", e$message)
            list()
        })
    },

         get_account = function() {
        tryCatch({
            result <- rpc("listAccounts", list())
            if (is.null(result)) return(NULL)
            for (acc in result) {
                if (acc$number == account) {
                    return(list(
                                number = acc$number,
                                uuid = acc$uuid,
                                device_id = acc$deviceId
                        ))
                }
            }
            NULL
        }, error = function(e) {
            log_msg("Signal: failed to get account info:", e$message)
            NULL
        })
    },

         close = function() {
        running <<- FALSE
        invisible(NULL)
    }
    )
}

