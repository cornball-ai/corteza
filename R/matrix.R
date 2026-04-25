# Matrix channel adapter.
#
# Exposes the corteza agent over a Matrix room via the mx.api package.
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

matrix_config_path <- function() path.expand("~/.corteza/matrix.json")

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
#' the target room, and writes credentials to ~/.corteza/matrix.json with
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
                    body = ev$content$body,
                    mentions = ev$content$`m.mentions`$user_ids
                )
            }
        }
    }
    out
}

# Format new turns since the session's `ingested_through` watermark
# as a markdown transcript. Returns NULL when nothing new to archive.
matrix_session_to_markdown <- function(session, room_id, room_name = NULL) {
    history <- session$history %||% list()
    start <- (session$ingested_through %||% 0L) + 1L
    if (start > length(history)) return(NULL)
    new_msgs <- history[start:length(history)]
    parts <- vapply(new_msgs, function(m) {
        role <- m$role %||% "?"
        text <- if (is.character(m$content)) {
            paste(m$content, collapse = "\n")
        } else {
            as.character(m$content %||% "")
        }
        sprintf("## %s\n\n%s", role, text)
    }, character(1))
    header <- sprintf("# %s", room_name %||% room_id)
    paste(c(header, "", parts), collapse = "\n\n")
}

# Archive new turns from one room's session to the pensar vault and
# advance the watermark so the same turns aren't re-ingested. Silent
# no-op when pensar isn't installed or there's nothing new.
matrix_archive_session <- function(session, room_id, mx_sess = NULL) {
    if (!requireNamespace("pensar", quietly = TRUE)) return(invisible(NULL))
    history <- session$history %||% list()
    last <- session$ingested_through %||% 0L
    if (length(history) <= last) return(invisible(NULL))

    room_name <- if (!is.null(mx_sess)) {
        tryCatch(mx.api::mx_room_name(mx_sess, room_id),
                 error = function(e) NULL)
    } else {
        NULL
    }
    md <- matrix_session_to_markdown(session, room_id, room_name)
    if (is.null(md)) return(invisible(NULL))
    out <- tryCatch(
        pensar::ingest(content = md, type = "matrix",
                       source = room_name %||% room_id,
                       title = room_name %||% room_id),
        error = function(e) {
            message("matrix_archive_session: pensar::ingest failed: ",
                    conditionMessage(e))
            NULL
        }
    )
    if (!is.null(out)) {
        session$ingested_through <- length(history)
    }
    invisible(out)
}

#' Flush all in-memory matrix sessions to the pensar vault
#'
#' Walks the per-room session registry and archives any turns that
#' haven't been ingested yet via \code{pensar::ingest(type = "matrix")}.
#' Each session tracks an \code{ingested_through} watermark so repeated
#' calls only write new turns. Silent no-op when \code{pensar} is not
#' installed.
#'
#' @param sessions A registry environment built by
#'   \code{matrix_run}/\code{matrix_poll}. Keys are room IDs, values
#'   are session lists carrying \code{$history}.
#' @param mx_sess Optional Matrix session for room-name lookups. When
#'   NULL, the room ID is used as the source identifier.
#'
#' @return Integer count of rooms ingested, invisibly.
#' @export
matrix_archive_all <- function(sessions, mx_sess = NULL) {
    if (!is.environment(sessions)) {
        stop("sessions must be an environment registry", call. = FALSE)
    }
    n <- 0L
    for (room_id in ls(envir = sessions, all.names = TRUE)) {
        s <- get(room_id, envir = sessions, inherits = FALSE)
        before <- s$ingested_through %||% 0L
        matrix_archive_session(s, room_id, mx_sess)
        if ((s$ingested_through %||% 0L) > before) {
            n <- n + 1L
        }
    }
    invisible(n)
}

# Is this a /clear (or /reset, /new) command? In group rooms the body
# will include the @-mention prefix — accept the command at the end of
# the message after any leading mention text, or on its own.
matrix_is_clear_command <- function(body) {
    if (is.null(body) || !nzchar(body)) return(FALSE)
    trimmed <- trimws(body)
    grepl("(^|\\s)/(clear|reset|new)\\s*$", trimmed)
}

# Does this message mention the bot? Checks the explicit m.mentions
# field (emitted by Element and most modern clients) first, then falls
# back to substring matching on the body for @localpart and full MXID.
matrix_message_mentions_self <- function(msg, self_id) {
    mentions <- msg$mentions
    if (length(mentions) && any(self_id %in% unlist(mentions))) {
        return(TRUE)
    }
    body <- msg$body %||% ""
    if (!nzchar(body)) {
        return(FALSE)
    }
    if (grepl(self_id, body, fixed = TRUE)) {
        return(TRUE)
    }
    localpart <- sub("^@", "", sub(":.*$", "", self_id))
    grepl(sprintf("@%s\\b", localpart), body, perl = TRUE, ignore.case = TRUE)
}

# Should the bot respond to this message? DMs: always. Group rooms
# (3+ members, or anything the session recorded as non-DM): only when
# the bot is explicitly mentioned.
matrix_should_respond <- function(msg, session, self_id) {
    if (isTRUE(session$is_dm)) {
        return(TRUE)
    }
    matrix_message_mentions_self(msg, self_id)
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

matrix_default_system <- function(cfg, room_id = NULL, mx_sess = NULL,
                                  cwd = NULL, description = NULL,
                                  room_name = NULL) {
    base <- sprintf("You are %s, a helpful assistant for %s.",
                    cfg$user_id, cfg$user)
    parts <- base

    if (!is.null(cwd) && nzchar(cwd)) {
        parts <- c(parts,
                   sprintf("Working directory: %s", cwd),
                   "Use this as your scope unless the user asks for something else.")
    }
    if (!is.null(room_name) && nzchar(room_name)) {
        parts <- c(parts, sprintf("Room: %s", room_name))
    }
    if (!is.null(description) && nzchar(description)) {
        parts <- c(parts, sprintf("Topic: %s", description))
    }
    paste(parts, collapse = "\n")
}

# Agent name for path-building. "@cornelius:cornball.ai" -> "Cornelius".
matrix_agent_name <- function(cfg) {
    local <- sub("^@", "", sub(":.*$", "", cfg$user_id %||% ""))
    if (!nzchar(local)) {
        return("agent")
    }
    paste0(toupper(substr(local, 1L, 1L)), substr(local, 2L, nchar(local)))
}

# Default agent workspace: ~/<Name>. Created on first use.
matrix_default_cwd <- function(cfg) {
    dir <- path.expand(file.path("~", matrix_agent_name(cfg)))
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dir
}

# Parse a topic string into its cwd + description parts. The
# convention is "<path> | <description>" where <path> starts with
# "~/", "/", or "./". A leading segment that does not look like a
# path is treated as pure description (cwd = NULL).
matrix_parse_topic <- function(topic) {
    if (is.null(topic)) {
        return(list(cwd = NULL, description = NULL))
    }
    topic <- trimws(topic)
    if (!nzchar(topic)) {
        return(list(cwd = NULL, description = NULL))
    }

    parts <- strsplit(topic, "\\s*\\|\\s*", perl = TRUE)[[1]]
    if (length(parts) >= 2L && grepl("^(~/|/|\\./)", parts[1L])) {
        list(cwd = parts[1L], description = paste(parts[-1L], collapse = " | "))
    } else {
        list(cwd = NULL, description = topic)
    }
}

# Effective cwd for a room: topic-supplied path if present and valid,
# otherwise the agent's default workspace. Never returns a non-
# existent directory.
matrix_room_cwd <- function(cfg, room_id, mx_sess = NULL) {
    default_dir <- matrix_default_cwd(cfg)
    if (is.null(room_id) || is.null(mx_sess)) {
        return(default_dir)
    }

    topic <- tryCatch(mx.api::mx_room_topic(mx_sess, room_id),
                      error = function(e) NULL)
    parsed <- matrix_parse_topic(topic)
    if (is.null(parsed$cwd)) {
        return(default_dir)
    }

    candidate <- path.expand(parsed$cwd)
    if (!dir.exists(candidate)) {
        message(sprintf(
                        "matrix: topic cwd %s does not exist; falling back to %s",
                        candidate, default_dir
            ))
        return(default_dir)
    }
    candidate
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
# cfg$approval_timeout_sec or options("corteza.matrix_approval_timeout").
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
        getOption("corteza.matrix_approval_timeout", 60L)
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

# Build a fresh corteza session from a Matrix config. Does not fetch any
# room history; in-memory history accumulates across turn() calls made
# inside one matrix_run process.
matrix_new_session <- function(cfg, system = NULL, model = NULL,
                               provider = NULL, tools_filter = NULL,
                               room_id = NULL) {
    if (is.null(room_id)) {
        room_id <- cfg$room_id
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
    if (length(tools_filter) == 0L) {
        tools_filter <- NULL
    }

    mx_sess <- tryCatch(matrix_mx_session(cfg), error = function(e) NULL)
    room_cwd <- matrix_room_cwd(cfg, room_id, mx_sess)

    if (is.null(system)) {
        room_name <- if (!is.null(mx_sess) && !is.null(room_id)) {
            tryCatch(mx.api::mx_room_name(mx_sess, room_id),
                     error = function(e) NULL)
        } else {
            NULL
        }
        topic_raw <- if (!is.null(mx_sess) && !is.null(room_id)) {
            tryCatch(mx.api::mx_room_topic(mx_sess, room_id),
                     error = function(e) NULL)
        } else {
            NULL
        }
        parsed <- matrix_parse_topic(topic_raw)
        system <- matrix_default_system(
                                        cfg,
                                        cwd = room_cwd,
                                        description = parsed$description,
                                        room_name = room_name
        )
    }

    s <- session_setup(
                       channel = "matrix",
                       cwd = room_cwd,
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
    s$cwd <- room_cwd
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
    s$is_dm <- matrix_detect_dm(cfg, room_id)
    assign(room_id, s, envir = registry)
    s
}

# A DM is a 2-member room where one of the members is the bot itself.
# Anything else (3+ members, or just the bot alone) is a group room
# subject to mention-gating.
matrix_detect_dm <- function(cfg, room_id) {
    mx_sess <- tryCatch(matrix_mx_session(cfg), error = function(e) NULL)
    if (is.null(mx_sess)) {
        return(TRUE) # conservative fallback
    }
    members <- tryCatch(
                        mx.api::mx_room_members(mx_sess, room_id),
                        error = function(e) character()
    )
    length(members) == 2L && cfg$user_id %in% members
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

    replied <- 0L
    for (m in msgs) {
        # Read receipt runs even when we don't reply: the bot has still
        # "seen" the message, and clients use receipts for the
        # latest-read marker.
        tryCatch(
                 mx.api::mx_read_receipt(mx_sess, m$room_id, m$event_id),
                 error = function(e) NULL
        )
        session <- matrix_get_or_create_session(
            sessions, m$room_id, cfg,
            system = system, model = model,
            provider = provider, tools_filter = tools_filter
        )
        # Group rooms: only respond when @-mentioned. DMs: always.
        # Prevents bot-loops between two AIs and stops noise in
        # multi-human rooms.
        if (!matrix_should_respond(m, session, cfg$user_id)) {
            next
        }

        if (matrix_is_clear_command(m$body)) {
            # Archive whatever's in the session before nuking it so the
            # topic isn't lost. Best-effort; failures already log.
            tryCatch(
                matrix_archive_session(session, m$room_id, mx_sess),
                error = function(e) NULL
            )
            if (exists(m$room_id, envir = sessions, inherits = FALSE)) {
                rm(list = m$room_id, envir = sessions)
            }
            mx.api::mx_send(mx_sess, m$room_id,
                            "Cleared. Starting a fresh session.")
            replied <- replied + 1L
            next
        }

        reply <- matrix_run_turn_in_cwd(m$body, session)
        if (is.null(reply) || !nzchar(reply)) {
            reply <- "(no reply)"
        }
        mx.api::mx_send(mx_sess, m$room_id, reply)
        replied <- replied + 1L
    }
    invisible(replied)
}

# Run one turn with R's process-wide getwd() pointed at the session's
# configured workspace. Always restores the original cwd, even if
# turn() errors. Matrix tool calls (bash, run_r) use getwd() for
# relative paths, so this is what actually makes the room's cwd take
# effect.
matrix_run_turn_in_cwd <- function(prompt, session) {
    target <- session$cwd
    orig_wd <- getwd()
    if (!is.null(target) && nzchar(target) && dir.exists(target)) {
        tryCatch(setwd(target), error = function(e) NULL)
    }
    on.exit(tryCatch(setwd(orig_wd), error = function(e) NULL), add = TRUE)

    tryCatch(
             turn(prompt, session)$reply,
             error = function(e) sprintf("(agent error: %s)", conditionMessage(e))
    )
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
    mx_sess <- NULL

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

    signal_dir <- matrix_signal_dir()
    flush_signal <- file.path(signal_dir, "archive.signal")

    message("matrix_run: starting long-poll loop")
    message("matrix_run: flush signal at ", flush_signal)
    repeat {
        matrix_poll(
                    system = system, model = model,
                    provider = provider, tools_filter = tools_filter,
                    timeout = timeout, sessions = sessions
        )
        # Out-of-band archive trigger: another process (e.g. a cornelius
        # systemd timer) drops `archive.signal` to ask the bot to flush
        # all in-memory room sessions to the pensar vault. The bot owns
        # the registry; the schedule lives outside the package.
        matrix_handle_flush_signal(flush_signal, sessions, mx_sess)
    }
}

# Resolve the directory where out-of-band signal files live. Honors
# CORTEZA_STATE_DIR for tests / unusual setups, else the standard
# user state path. Created lazily when first written to.
matrix_signal_dir <- function() {
    env <- Sys.getenv("CORTEZA_STATE_DIR", "")
    if (nzchar(env)) return(env)
    tools::R_user_dir("corteza", "state")
}

#' Ask the running matrix bot to archive sessions to pensar
#'
#' Drops an \code{archive.signal} file in the corteza state directory.
#' The next iteration of the long-poll loop in \code{\link{matrix_run}}
#' picks it up, runs \code{\link{matrix_archive_all}}, and removes the
#' file. Safe to call from any process or scheduler — systemd, Task
#' Scheduler, launchd, cron, or a separate R session — without needing
#' to know the bot's PID or share its memory.
#'
#' @return The signal file path, invisibly.
#' @export
matrix_request_flush <- function() {
    dir <- matrix_signal_dir()
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    sig <- file.path(dir, "archive.signal")
    file.create(sig, showWarnings = FALSE)
    invisible(sig)
}

# Flush sessions to pensar when the signal file exists. Removes the
# file on success so each touch fires exactly one flush. Errors are
# logged, never raised — the long-poll loop must keep running.
matrix_handle_flush_signal <- function(flush_signal, sessions,
                                       mx_sess = NULL) {
    if (!file.exists(flush_signal)) return(invisible(0L))
    n <- tryCatch(
        matrix_archive_all(sessions, mx_sess),
        error = function(e) {
            message("matrix_run: flush failed: ", conditionMessage(e))
            -1L
        }
    )
    tryCatch(file.remove(flush_signal), error = function(e) NULL)
    if (isTRUE(n >= 0L)) {
        message(sprintf("matrix_run: archived %d room(s) to vault", n))
    }
    invisible(n)
}

