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

matrix_extract_messages <- function(sync_resp, self_id) {
    joined <- sync_resp$rooms$join
    if (!length(joined)) {
        return(list())
    }

    out <- list()
    for (rid in names(joined)) {
        events <- joined[[rid]]$timeline$events
        if (!length(events)) {
            next
        }
        for (ev in events) {
            if (isTRUE(ev$type == "m.room.message") &&
                isTRUE(ev$sender != self_id) &&
                isTRUE(ev$content$msgtype == "m.text") &&
                !is.null(ev$content$body)) {
                out[[length(out) + 1L]] <- list(
                    room_id = rid,
                    event_id = ev$event_id,
                    sender = ev$sender,
                    body = ev$content$body
                )
            }
        }
    }
    out
}

# Pending invites from a sync response: character vector of room_ids
# the bot has been invited to but not yet joined.
matrix_extract_invites <- function(sync_resp) {
    invited <- sync_resp$rooms$invite
    if (!length(invited)) {
        return(character())
    }
    names(invited)
}

matrix_default_system <- function(cfg, room_id = NULL, mx_sess = NULL) {
    base <- sprintf("You are %s, a helpful assistant for %s.",
                    cfg$user_id, cfg$user)
    if (is.null(room_id) || is.null(mx_sess)) {
        return(base)
    }

    name <- tryCatch(mx.api::mx_room_name(mx_sess, room_id),
                     error = function(e) NULL)
    topic <- tryCatch(mx.api::mx_room_topic(mx_sess, room_id),
                      error = function(e) NULL)
    if (is.null(name) && is.null(topic)) {
        return(base)
    }

    parts <- c(base, "\nYou are talking in this room:")
    if (!is.null(name)) {
        parts <- c(parts, sprintf("  Name: %s", name))
    }
    if (!is.null(topic)) {
        parts <- c(parts, sprintf("  Topic: %s", topic))
    }
    paste(parts, collapse = "\n")
}

# Build the approval callback for the Matrix channel. Fires only for
# "ask" verdicts from policy (personal+anything-on-matrix is already
# "deny" in the default tensor). Two modes:
#   auto_approve_asks = TRUE  -> always approve (trusted tailnet use)
#   auto_approve_asks = FALSE -> post an approval prompt to the room,
#                                wait for a thumbs-up / thumbs-down
#                                reaction from a user other than the
#                                bot itself, return TRUE / FALSE.
# Timeout defaults to 60 seconds; configurable via
# cfg$approval_timeout_sec or options("llamaR.matrix_approval_timeout").
matrix_approval_cb <- function(cfg, room_id = cfg$room_id) {
    auto <- isTRUE(cfg$auto_approve_asks)
    force(room_id)
    function(call, decision) {
        if (auto) {
            return(TRUE)
        }
        matrix_reaction_approval(cfg, call, decision, room_id = room_id)
    }
}

# Blocking reaction-based approval. Returns TRUE / FALSE. Never errors
# for run-time issues (network blip, user declines, timeout) — those
# all fall through to FALSE so the LLM sees a clean "declined" string.
matrix_reaction_approval <- function(cfg, call, decision,
                                     room_id = cfg$room_id,
                                     timeout_sec = NULL) {
    if (is.null(timeout_sec)) {
        timeout_sec <- cfg$approval_timeout_sec %||%
        getOption("llamaR.matrix_approval_timeout", 60L)
    }
    timeout_sec <- as.integer(timeout_sec)

    mx_sess <- matrix_mx_session(cfg)
    msg <- matrix_approval_prompt(call, decision, timeout_sec)

    eid <- tryCatch(
                    mx.api::mx_send(mx_sess, room_id, msg),
                    error = function(e) NULL
    )
    if (is.null(eid)) {
        return(FALSE)
    }

    # Add our own 👍 and 👎 reactions so the user can tap either one
    # instead of typing the emoji. (mx_react errors are best-effort.)
    tryCatch(mx.api::mx_react(mx_sess, room_id, eid, "\U0001F44D"),
             error = function(e) NULL)
    tryCatch(mx.api::mx_react(mx_sess, room_id, eid, "\U0001F44E"),
             error = function(e) NULL)

    baseline <- tryCatch(
                         mx.api::mx_sync(mx_sess, timeout = 0L),
                         error = function(e) NULL
    )
    if (is.null(baseline)) {
        return(FALSE)
    }
    since <- baseline$next_batch

    deadline <- Sys.time() + timeout_sec
    while (Sys.time() < deadline) {
        remaining_ms <- max(
                            as.integer((as.numeric(deadline) - as.numeric(Sys.time())) * 1000),
                            1L
        )
        sync <- tryCatch(
                         mx.api::mx_sync(mx_sess, since = since,
                timeout = min(remaining_ms, 30000L)),
                         error = function(e) NULL
        )
        if (is.null(sync)) {
            return(FALSE)
        }
        since <- sync$next_batch

        verdict <- matrix_extract_reaction_verdict(
            sync, cfg$room_id, cfg$user_id, eid
        )
        if (!is.null(verdict)) {
            return(verdict)
        }
    }
    FALSE
}

# Render a short readable approval prompt.
matrix_approval_prompt <- function(call, decision, timeout_sec) {
    args <- call$args %||% list()
    args_str <- if (length(args)) {
        paste(
              mapply(function(k, v) {
            s <- as.character(v)[1L] %||% ""
            if (nchar(s) > 60L) s <- paste0(substr(s, 1L, 57L), "...")
            sprintf("%s=%s", k, s)
        }, names(args), args, USE.NAMES = FALSE),
              collapse = ", "
        )
    } else {
        ""
    }
    sprintf(
            "Approval needed: %s(%s)\nReason: %s\n\U0001F44D approve / \U0001F44E deny  (timeout %ds)",
            call$tool, args_str, decision$reason %||% "ask", timeout_sec
    )
}

# Scan a sync response's timeline for a reaction on event_id from a
# user other than the bot. Returns TRUE (👍), FALSE (👎), or NULL (no
# verdict yet).
matrix_extract_reaction_verdict <- function(sync_resp, room_id, self_id,
    target_event_id) {
    room <- sync_resp$rooms$join[[room_id]]
    if (is.null(room)) {
        return(NULL)
    }
    events <- room$timeline$events
    if (!length(events)) {
        return(NULL)
    }

    approve_keys <- c("\U0001F44D", "\U00002705", "y", "yes", "ok")
    deny_keys <- c("\U0001F44E", "\U0000274C", "n", "no", "nope")

    for (ev in events) {
        if (!isTRUE(ev$type == "m.reaction")) {
            next
        }
        if (isTRUE(ev$sender == self_id)) {
            next
        }
        rel <- ev$content$`m.relates_to`
        if (!is.list(rel)) {
            next
        }
        if (!identical(rel$event_id, target_event_id)) {
            next
        }
        key <- rel$key
        if (!is.character(key) || !length(key)) {
            next
        }
        if (key %in% approve_keys) {
            return(TRUE)
        }
        if (key %in% deny_keys) {
            return(FALSE)
        }
    }
    NULL
}

# Build a fresh llamaR session from a Matrix config. Does not fetch any
# room history; in-memory history accumulates across turn() calls made
# inside one matrix_run process.
matrix_new_session <- function(cfg, system = NULL, model = NULL,
                               provider = NULL, tools_filter = NULL,
                               room_id = NULL) {
    if (is.null(room_id)) {
        room_id <- cfg$room_id
    }
    if (is.null(system)) {
        mx_sess <- tryCatch(matrix_mx_session(cfg), error = function(e) NULL)
        system <- matrix_default_system(cfg, room_id = room_id,
                                        mx_sess = mx_sess)
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

    s <- session_setup(
                       channel = "matrix",
                       cwd = getwd(),
                       provider = provider %||% "anthropic",
                       model = model,
                       tools = tools_filter,
                       system = system,
                       approval_cb = matrix_approval_cb(cfg, room_id = room_id),
                       load_project_context = FALSE,
                       validate_api_key = TRUE,
                       verbose = FALSE
    )
    s$room_id <- room_id
    s
}

# Registry of per-room sessions. env keyed by room_id so each room
# (including new ones cornelius is invited into mid-run) gets its own
# conversation history. Used by matrix_run; matrix_poll in cron mode
# builds a fresh env per call.
matrix_new_session_registry <- function() {
    new.env(parent = emptyenv())
}

matrix_get_or_create_session <- function(registry, room_id, cfg,
    system = NULL, model = NULL,
    provider = NULL, tools_filter = NULL) {
    if (exists(room_id, envir = registry, inherits = FALSE)) {
        return(get(room_id, envir = registry))
    }
    s <- matrix_new_session(
                            cfg,
                            system = system, model = model,
                            provider = provider, tools_filter = tools_filter,
                            room_id = room_id
    )
    assign(room_id, s, envir = registry)
    s
}

# Auto-join any rooms the bot has been invited to. Best-effort: failures
# are logged to stderr but don't abort the poll.
matrix_accept_invites <- function(mx_sess, invites) {
    for (rid in invites) {
        joined <- tryCatch(
                           mx.api::mx_room_join(mx_sess, rid),
                           error = function(e) {
            message(sprintf("matrix: failed to join %s: %s",
                            rid, conditionMessage(e)))
            NULL
        }
        )
        if (!is.null(joined)) {
            message(sprintf("matrix: joined %s", joined))
        }
    }
}

#' One iteration of sync-and-reply
#'
#' Fetches new messages across all joined rooms and runs \code{\link{turn}}
#' against each. Auto-joins any pending invites the bot has received.
#' Replies are sent back to the originating room. On first run there is
#' no saved sync token, so this call establishes a baseline and returns
#' without processing history.
#'
#' Pass \code{sessions = NULL} (the default) for a stateless one-shot —
#' each incoming message builds a fresh session. Pass a registry created
#' by \code{matrix_new_session_registry()} so a long-running
#' \code{matrix_run} keeps a separate history per room (conversations
#' in different rooms don't cross-contaminate).
#'
#' @param system Character or NULL. System prompt override.
#' @param model Character or NULL. Model override.
#' @param provider Character or NULL. Provider override.
#' @param tools_filter Character vector or NULL. Tool filter override.
#' @param timeout Integer. Long-poll timeout in milliseconds. 0 returns
#'   immediately.
#' @param sessions Environment from \code{matrix_new_session_registry()}
#'   keyed by room_id, or NULL to build fresh sessions each call.
#'
#' @return An integer count of messages replied to, invisibly.
#' @export
matrix_poll <- function(system = NULL, model = NULL, provider = NULL,
                        tools_filter = NULL, timeout = 0L, sessions = NULL) {
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

    # Accept new invites before we process this sync's messages so the
    # matching JOIN state is in place before any replies go out. Invites
    # in this sync won't yet appear in rooms$join; the next sync will
    # pick up their timeline.
    invites <- matrix_extract_invites(sync)
    if (length(invites)) {
        matrix_accept_invites(mx_sess, invites)
    }

    if (first_run) {
        message("matrix_poll: baseline established, no history processed")
        return(invisible(0L))
    }

    msgs <- matrix_extract_messages(sync, cfg$user_id)
    if (!length(msgs)) {
        return(invisible(0L))
    }

    # Use the caller-supplied per-room registry, or build a throwaway
    # one for this poll (stateless cron semantics).
    if (is.null(sessions)) {
        sessions <- matrix_new_session_registry()
    }

    for (m in msgs) {
        # Post a read receipt before running the agent so the user sees
        # "seen by <bot>" update immediately, not after the reply is
        # composed. Best-effort — failures don't block the turn.
        tryCatch(
                 mx.api::mx_read_receipt(mx_sess, m$room_id, m$event_id),
                 error = function(e) NULL
        )
        session <- matrix_get_or_create_session(
            sessions, m$room_id, cfg,
            system = system, model = model,
            provider = provider, tools_filter = tools_filter
        )
        reply <- tryCatch(
                          turn(m$body, session)$reply,
                          error = function(e) sprintf("(agent error: %s)",
                conditionMessage(e))
        )
        if (is.null(reply) || !nzchar(reply)) {
            reply <- "(no reply)"
        }
        mx.api::mx_send(mx_sess, m$room_id, reply)
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
    sessions <- matrix_new_session_registry()

    # Catch up on pending invites that predate the saved sync token.
    # Conduit (and some other Matrix servers) only surfaces invites
    # that arrived after the `since` token, so if the bot was offline
    # when an invite was issued, the long-poll loop will never see it.
    # A full (no-since) sync on startup grabs current invite state.
    cfg <- tryCatch(matrix_load_config(), error = function(e) NULL)
    if (!is.null(cfg)) {
        mx_sess <- tryCatch(matrix_mx_session(cfg), error = function(e) NULL)
        if (!is.null(mx_sess)) {
            initial <- tryCatch(
                                mx.api::mx_sync(mx_sess, timeout = 0L),
                                error = function(e) NULL
            )
            invites <- matrix_extract_invites(initial)
            if (length(invites)) {
                matrix_accept_invites(mx_sess, invites)
            }
        }
    }

    message("matrix_run: starting long-poll loop")
    repeat {
        matrix_poll(
                    system = system, model = model,
                    provider = provider, tools_filter = tools_filter,
                    timeout = timeout, sessions = sessions
        )
    }
}

