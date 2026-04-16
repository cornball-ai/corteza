# Matrix channel adapter.
#
# Exposes the llamaR agent over a Matrix room via the mx.api package.
# The bot long-polls /sync so incoming messages are handled with
# sub-second latency; no cron or webhook plumbing required.
#
# mx.api is in Suggests since most users won't enable a Matrix channel.
# The matrix_* functions hard-stop with an install hint if it's missing.

matrix_require_mx <- function() {
    if (!requireNamespace("mx.api", quietly = TRUE)) {
        stop(
             "Matrix integration requires the 'mx.api' package. ",
             "Install it with install.packages(\"mx.api\") ",
             "(or remotes::install_github(\"cornball-ai/mx.api\")).",
             call. = FALSE
        )
    }
}

matrix_config_path <- function() path.expand("~/.llamar/matrix.json")

matrix_load_config <- function() {
    path <- matrix_config_path()
    if (!file.exists(path)) {
        stop(
             "Matrix not configured. Call matrix_configure() first.",
             call. = FALSE
        )
    }
    jsonlite::fromJSON(path, simplifyVector = TRUE)
}

matrix_save_config <- function(cfg) {
    path <- matrix_config_path()
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), path)
    Sys.chmod(path, mode = "0600")
    invisible(cfg)
}

matrix_session <- function(cfg) {
    mx.api::mx_session(
                       server = cfg$server,
                       token = cfg$token,
                       user_id = cfg$user_id,
                       device_id = cfg$device_id
    )
}

#' Configure the Matrix channel for this host
#'
#' Logs in to a Matrix homeserver as the bot account, joins (or records)
#' the target room, and writes credentials to ~/.llamar/matrix.json with
#' file mode 0600. Call once per host. Model and provider are optional
#' defaults the poll loop uses unless overridden at call time.
#'
#' @param server Character. Homeserver base URL.
#' @param user Character. Bot localpart or full Matrix ID.
#' @param password Character. Bot password. Stored locally so the bot
#'   can re-authenticate if its access token is invalidated.
#' @param room Character. Room ID or alias the bot should read and post
#'   to. If the bot has been invited but not joined, it will be joined.
#' @param model Character or NULL. Default model name passed to
#'   \code{llm.api::chat}.
#' @param provider Character. Default LLM provider.
#'
#' @return The saved configuration, invisibly.
#' @export
matrix_configure <- function(server, user, password, room, model = NULL,
                             provider = "auto") {
    matrix_require_mx()
    s <- mx.api::mx_login(server, user, password)
    room_id <- mx.api::mx_room_join(s, room)

    cfg <- list(
                server = server,
                user = user,
                password = password,
                token = s$token,
                user_id = s$user_id,
                device_id = s$device_id,
                room_id = room_id,
                model = model,
                provider = provider,
                sync_token = NULL
    )
    matrix_save_config(cfg)
    message(sprintf("Configured %s in room %s", s$user_id, room_id))
    invisible(cfg)
}

#' Send a message to the configured Matrix room
#'
#' @param text Character. Plain text body.
#' @param msgtype Character. Matrix msgtype, default "m.text".
#'
#' @return The event ID of the sent message.
#' @export
matrix_send <- function(text, msgtype = "m.text") {
    matrix_require_mx()
    cfg <- matrix_load_config()
    s <- matrix_session(cfg)
    mx.api::mx_send(s, cfg$room_id, text, msgtype = msgtype)
}

matrix_extract_messages <- function(sync_resp, room_id, self_id) {
    room <- sync_resp$rooms$join[[room_id]]
    if (is.null(room)) {
        return(list())
    }
    events <- room$timeline$events
    if (!length(events)) {
        return(list())
    }

    out <- list()
    for (ev in events) {
        if (isTRUE(ev$type == "m.room.message") &&
            isTRUE(ev$sender != self_id) &&
            isTRUE(ev$content$msgtype == "m.text") &&
            !is.null(ev$content$body)) {
            out[[length(out) + 1L]] <- list(
                event_id = ev$event_id,
                sender = ev$sender,
                body = ev$content$body
            )
        }
    }
    out
}

matrix_default_system <- function(cfg) {
    sprintf(
            paste(
                  "You are %s, an assistant running in R on behalf of %s.",
                  "Reply concisely over Matrix chat. Plain text, no markdown",
                  "headings. Assume messages may be received out of order."
        ),
            cfg$user_id, cfg$user
    )
}

#' One iteration of sync-and-reply
#'
#' Fetches new messages from the configured room, passes each user
#' message to \code{\link[llm.api]{chat}}, posts the reply back, and
#' persists the sync token. Safe to call repeatedly (idempotent per
#' sync token).
#'
#' On first run there is no saved sync token, so this call establishes
#' a baseline and returns without processing any history.
#'
#' @param system Character or NULL. System prompt; default derived
#'   from the bot identity.
#' @param model Character or NULL. Model override; falls back to the
#'   model stored in the saved configuration.
#' @param provider Character or NULL. Provider override; falls back to
#'   the provider stored in the saved configuration.
#' @param timeout Integer. Long-poll timeout in milliseconds. 0 returns
#'   immediately.
#'
#' @return An integer count of messages replied to, invisibly.
#' @export
matrix_poll <- function(system = NULL, model = NULL, provider = NULL,
                        timeout = 0L) {
    matrix_require_mx()
    cfg <- matrix_load_config()
    s <- matrix_session(cfg)
    if (is.null(system)) {
        system <- matrix_default_system(cfg)
    }
    if (is.null(model)) {
        model <- cfg$model
    }
    if (is.null(provider)) {
        provider <- cfg$provider %||% "auto"
    }

    sync <- mx.api::mx_sync(
                            s, since = cfg$sync_token, timeout = as.integer(timeout)
    )

    first_run <- is.null(cfg$sync_token)
    cfg$sync_token <- sync$next_batch
    matrix_save_config(cfg)

    if (first_run) {
        message("matrix_poll: baseline established, no history processed")
        return(invisible(0L))
    }

    msgs <- matrix_extract_messages(sync, cfg$room_id, cfg$user_id)
    if (!length(msgs)) {
        return(invisible(0L))
    }

    for (m in msgs) {
        reply <- tryCatch(
                          llm.api::chat(
                                        prompt = m$body,
                                        model = model,
                                        system = system,
                                        provider = provider
            )$content,
                          error = function(e) sprintf("(llm error: %s)", conditionMessage(e))
        )
        mx.api::mx_send(s, cfg$room_id, reply)
    }
    invisible(length(msgs))
}

#' Run the Matrix adapter as a long-poll loop
#'
#' Calls \code{matrix_poll} in a loop with a long-poll timeout so new
#' messages trigger replies within ~1 second. Intended as the entry
#' point for a systemd user unit.
#'
#' @param timeout Integer. Long-poll timeout in milliseconds.
#' @param system Character or NULL. System prompt override.
#' @param model Character or NULL. Model override.
#' @param provider Character or NULL. Provider override.
#'
#' @return Never returns under normal operation. Crashes on fatal error
#'   so systemd can restart.
#' @export
matrix_run <- function(timeout = 30000L, system = NULL, model = NULL,
                       provider = NULL) {
    message("matrix_run: starting long-poll loop")
    repeat {
        matrix_poll(
                    system = system, model = model,
                    provider = provider, timeout = timeout
        )
    }
}

