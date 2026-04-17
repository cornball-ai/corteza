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

matrix_mx_session <- function(cfg) {
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
#' file mode 0600. Call once per host. Model, provider, tools_filter,
#' and auto_approve_asks are defaults the poll loop uses unless
#' overridden at call time.
#'
#' @param server Character. Homeserver base URL.
#' @param user Character. Bot localpart or full Matrix ID.
#' @param password Character. Bot password. Stored locally so the bot
#'   can re-authenticate if its access token is invalidated.
#' @param room Character. Room ID or alias the bot should read and post
#'   to. If the bot has been invited but not joined, it will be joined.
#' @param model Character or NULL. Default model name.
#' @param provider Character. LLM provider: "anthropic", "openai",
#'   "moonshot", or "ollama".
#' @param tools_filter Character vector or NULL. Passed to
#'   \code{get_tools()} to restrict which tools the bot can invoke.
#'   NULL allows all registered tools.
#' @param auto_approve_asks Logical. When TRUE, tool calls that policy
#'   returns \code{"ask"} for are auto-approved. Suitable for a
#'   personal bot on a trusted tailnet. When FALSE (default) asks are
#'   declined until the thumbs-up reaction protocol lands.
#'
#' @return The saved configuration, invisibly.
#' @export
matrix_configure <- function(server, user, password, room, model = NULL,
                             provider = c("anthropic", "openai", "moonshot", "ollama"),
                             tools_filter = NULL, auto_approve_asks = FALSE) {
    matrix_require_mx()
    provider <- match.arg(provider)

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
                tools_filter = tools_filter,
                auto_approve_asks = isTRUE(auto_approve_asks),
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
    s <- matrix_mx_session(cfg)
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
    sprintf("You are %s, a helpful assistant for %s.",
            cfg$user_id, cfg$user)
}

# Build the approval callback for the Matrix channel. Until the thumbs-up
# reaction protocol lands (PR F), the only options are auto-approve or
# auto-decline. Policy handles the hard cases (personal paths already
# deny outright on matrix); this callback only fires for "ask" verdicts.
matrix_approval_cb <- function(cfg) {
    auto <- isTRUE(cfg$auto_approve_asks)
    function(call, decision) auto
}

# Build a fresh llamaR session from a Matrix config. Does not fetch any
# room history; in-memory history accumulates across turn() calls made
# inside one matrix_run process.
matrix_new_session <- function(cfg, system = NULL, model = NULL,
                               provider = NULL, tools_filter = NULL) {
    if (is.null(system)) {
        system <- matrix_default_system(cfg)
    }
    if (is.null(model)) {
        model <- cfg$model
    }
    if (is.null(provider)) {
        provider <- cfg$provider
    }
    if (is.null(tools_filter)) {
        tools_filter <- cfg$tools_filter
    }
    # JSON round-tripping can turn a NULL tools_filter into list(). Treat
    # anything empty as NULL so the session gets all registered tools.
    if (length(tools_filter) == 0L) {
        tools_filter <- NULL
    }

    new_session(
                channel = "matrix",
                provider = provider %||% "anthropic",
                model_map = list(cloud = model, local = NULL),
                tools_filter = tools_filter,
                system = system,
                approval_cb = matrix_approval_cb(cfg),
                verbose = FALSE
    )
}

#' One iteration of sync-and-reply
#'
#' Fetches new messages from the configured room and runs \code{\link{turn}}
#' against each. On first run there is no saved sync token, so this call
#' establishes a baseline and returns without processing any history.
#'
#' Pass \code{session = NULL} (the default) for a stateless one-shot that
#' cron can invoke. Pass a session created by \code{matrix_new_session} to
#' accumulate conversation history across polls within one process.
#'
#' @param system Character or NULL. System prompt override.
#' @param model Character or NULL. Model override.
#' @param provider Character or NULL. Provider override.
#' @param tools_filter Character vector or NULL. Tool filter override.
#' @param timeout Integer. Long-poll timeout in milliseconds. 0 returns
#'   immediately.
#' @param session Session environment from \code{matrix_new_session}, or
#'   NULL to build a fresh session each call.
#'
#' @return An integer count of messages replied to, invisibly.
#' @export
matrix_poll <- function(system = NULL, model = NULL, provider = NULL,
                        tools_filter = NULL, timeout = 0L, session = NULL) {
    matrix_require_mx()
    cfg <- matrix_load_config()
    mx_sess <- matrix_mx_session(cfg)

    sync <- mx.api::mx_sync(
                            mx_sess,
                            since = cfg$sync_token,
                            timeout = as.integer(timeout)
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

    if (is.null(session)) {
        session <- matrix_new_session(
                                      cfg,
                                      system = system, model = model,
                                      provider = provider, tools_filter = tools_filter
        )
    }

    for (m in msgs) {
        reply <- tryCatch(
                          turn(m$body, session)$reply,
                          error = function(e) sprintf("(agent error: %s)",
                conditionMessage(e))
        )
        if (is.null(reply) || !nzchar(reply)) {
            reply <- "(no reply)"
        }
        mx.api::mx_send(mx_sess, cfg$room_id, reply)
    }
    invisible(length(msgs))
}

#' Run the Matrix adapter as a long-poll loop
#'
#' Creates one session up front and reuses it across polls so conversation
#' history accumulates within the process lifetime. Intended as the entry
#' point for a systemd user unit.
#'
#' @param timeout Integer. Long-poll timeout in milliseconds.
#' @param system Character or NULL. System prompt override.
#' @param model Character or NULL. Model override.
#' @param provider Character or NULL. Provider override.
#' @param tools_filter Character vector or NULL. Tool filter override.
#'
#' @return Never returns under normal operation. Crashes on fatal error
#'   so systemd can restart.
#' @export
matrix_run <- function(timeout = 30000L, system = NULL, model = NULL,
                       provider = NULL, tools_filter = NULL) {
    matrix_require_mx()
    cfg <- matrix_load_config()
    session <- matrix_new_session(
                                  cfg,
                                  system = system, model = model,
                                  provider = provider, tools_filter = tools_filter
    )
    message("matrix_run: starting long-poll loop")
    repeat {
        matrix_poll(timeout = timeout, session = session)
    }
}

