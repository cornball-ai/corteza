# AgentVoice: the gRPC service a live-voice client talks to.
#
# Contract: inst/proto/cornball/agent/v1/agent_voice.proto, vendored from
# cornball-ai/fluffychat (the client repo owns the schema; the field
# comments there are the spec). Three RPCs: AllocateVoice opens a session
# and hands back the media endpoints, Converse runs one turn streaming
# text deltas, ReportTurn records how much of a reply was actually heard.
#
# Audio never crosses this service. The client streams PCM to the model
# hosts directly; only text and session control travel here, which is why
# this can run as a plain R process with no realtime constraints.
#
# This is a SEPARATE PROCESS from the room poll loop, started with
# corteza::voice_serve(). The gRPC poll loop and the bot long-poll both
# want the main thread, so one process cannot do both. Room context
# crosses through the room itself: a voice session is seeded from room
# history on first use (voice-turn.R), and the replies it posts reach
# the poll loop the same way -- never through in-process state.
#
# rgrpc and RProtoBuf are Suggests, checked loudly at voice_serve(): live
# voice is off unless a deployment opts in, and most installs never load
# either package.

.VOICE_SERVICE <- "cornball.agent.v1.AgentVoice"

.voice_method <- function(name) {
    sprintf("/%s/%s", .VOICE_SERVICE, name)
}

.voice_env <- new.env(parent = emptyenv())

# Version floors checked at runtime, not just declared, because an
# installed copy loads however old it is (same rule as .CHAT_API_MIN in
# rooms.R, and a test pins the rgrpc constant to the Suggests bound so
# the two cannot drift).
#
# rgrpc: grpc_stream()/grpc_send()/grpc_finish() -- the server-streaming
# surface Converse stands on -- arrived in 0.0.1.5 under the package's
# old name (grpc); 0.1.0 is the first release named rgrpc, so that is
# the floor.
# llm.api: on_delta appeared in 0.1.9.2 but only streamed on every
# provider in 0.1.9.4; below that a voice deployment silently waits for
# the whole reply, which defeats the feature it thinks it enabled.
.VOICE_RGRPC_MIN <- "0.1.0"
.VOICE_LLM_API_MIN <- "0.1.9.4"

voice_require <- function() {
    need <- c("rgrpc", "RProtoBuf")
    have <- vapply(need, requireNamespace, logical(1), quietly = TRUE)
    if (!all(have)) {
        stop("live voice needs ", paste(need[!have], collapse = " and "),
             " installed (they are Suggests, not Imports, because voice is ",
             "opt-in)", call. = FALSE)
    }
    if (utils::packageVersion("rgrpc") < .VOICE_RGRPC_MIN) {
        stop("live voice needs rgrpc >= ", .VOICE_RGRPC_MIN,
             " (server streaming); installed: ",
             utils::packageVersion("rgrpc"), call. = FALSE)
    }
    if (utils::packageVersion("llm.api") < .VOICE_LLM_API_MIN) {
        stop("live voice needs llm.api >= ", .VOICE_LLM_API_MIN,
             " (on_delta streams on every provider); installed: ",
             utils::packageVersion("llm.api"), call. = FALSE)
    }
    invisible(TRUE)
}

# Register the AgentVoice descriptors, once per process.
voice_load_protos <- function() {
    if (isTRUE(.voice_env$loaded)) {
        return(invisible(TRUE))
    }
    root <- system.file("proto", package = "corteza")
    if (!nzchar(root)) {
        stop("the AgentVoice proto is missing from the installed package",
             call. = FALSE)
    }
    RProtoBuf::readProtoFiles2("cornball/agent/v1/agent_voice.proto",
                               protoPath = root)
    .voice_env$loaded <- TRUE
    invisible(TRUE)
}

.voice_type <- function(name) {
    voice_load_protos()
    RProtoBuf::P(paste0("cornball.agent.v1.", name))
}

# name -> wire number, from the descriptor, never from a table here. A
# table could only agree with itself (vientito's wire rule, same reason).
.voice_enum_num <- function(enum, name) {
    v <- RProtoBuf::value(.voice_type(enum), name = name)
    if (is.null(v)) {
        stop(sprintf("%s has no value named %s", enum, name), call. = FALSE)
    }
    v$number()
}

# A refusal that knows its gRPC status. Handlers stop() with one of
# these; the dispatch loop turns it into the wire status. Everything
# else that escapes a handler is INTERNAL, because nothing considered it.
voice_refuse <- function(status, fmt, ...) {
    stop(structure(class = c("corteza_voice_refusal", "error", "condition"),
                   list(message = sprintf(fmt, ...), call = NULL, status = status)))
}

# Everything a running voice server carries. `hooks` exists so tests and
# harnesses can stand in for the world: every side effect the handlers
# have (HTTP, the room turn, posting, editing, membership, history, the
# clock) goes through one. Defaults are the real thing.
#
# `cfg_fn` is a FUNCTION, not a config: Matrix credentials rotate on
# relogin, a voice server lives across many rotations, and anything
# derived from a cached cfg goes stale silently (the rooms.R rule). The
# production cfg_fn is bot_load_config -- disk is authoritative after a
# relogin persists -- and every default hook derives its client through
# .voice_chat() at the moment of the call.
voice_state <- function(cfg_fn, hooks = list()) {
    if (!is.function(cfg_fn)) {
        cfg <- cfg_fn
        cfg_fn <- function() cfg
    }
    st <- new.env(parent = emptyenv())
    st$cfg_fn <- cfg_fn
    st$sessions <- new.env(parent = emptyenv())
    # Per-process key for the keyed-HMAC bearer compare (voice-auth.R).
    st$key <- digest::digest(list(Sys.time(), Sys.getpid(), stats::runif(4)),
                             algo = "sha256")
    st$counter <- 0L
    defaults <- list(
                     http = .voice_http,
                     clock = function() as.numeric(Sys.time()) * 1000,
                     run_turn = .voice_run_turn,
                     post = function(room_id, text) {
        bot_event_id(chat.api::chat_send(.voice_chat(st), room_id, text))
    },
                     edit = function(room_id, event_id, text) {
        chat.api::chat_edit(.voice_chat(st), room_id, event_id, text)
    },
                     members = function(room_id) {
        chat.api::chat_members(.voice_chat(st), room_id)
    },
                     history = function(room_id) {
        chat.api::chat_history(.voice_chat(st), room_id, limit = 30L)$messages
    },
                     cancel = .voice_cancel,
                     ready = function(port) invisible(NULL)
    )
    bad <- setdiff(names(hooks), names(defaults))
    if (length(bad)) {
        stop("unknown voice hook: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    st$hooks <- utils::modifyList(defaults, hooks)
    st$rooms <- new.env(parent = emptyenv())
    st
}

# The transport client, derived from the LIVE config at the point of
# use. Never cache the result: the whole reason this is a function call
# and not a field is that the credentials inside can rotate between any
# two voice RPCs.
.voice_chat <- function(state) {
    bot_chat_client(state$cfg_fn())
}

# Abandon the in-flight generation. Called from inside the delta relay
# the moment the client hangs up on the Converse stream: llm.api closes
# the connection, the provider stops generating, and agent() returns
# the partial reply -- so a barge-in stops costing tokens AND stops
# blocking the user's next turn behind a tail nobody is listening to.
#
# Raises a condition of class llm_cancelled that agent()'s stream loop
# catches. Outside a stream it is an ordinary error (there is nothing
# to cancel), so a run_turn hook that is not llm.api-backed must either
# handle it or override this hook.
.voice_cancel <- function() {
    llm.api::llm_cancel("voice client hung up on the stream")
}

# Unpredictable id for sessions and turns. The bearer match is the real
# authorisation; this only has to never collide and never be guessable
# from a previous one.
voice_id <- function(state) {
    state$counter <- state$counter + 1L
    digest::digest(list(state$key, state$counter, stats::runif(2)),
                   algo = "sha256")
}

#' Serve AgentVoice for live voice clients
#'
#' Runs the gRPC service a fluffychat live-voice client talks to
#' (\code{AllocateVoice}, \code{Converse}, \code{ReportTurn}). Blocks
#' until interrupted. Run it as its own R process, next to -- not inside
#' -- the room poll loop.
#'
#' Requires the \code{rgrpc} and \code{RProtoBuf} packages (Suggests),
#' and a media allocator named in the config: \code{voice.allocator}
#' (the gpu.ctl base URL) plus \code{voice.allocator_token} (the
#' service credential its front requires on every mint). Without
#' either, \code{AllocateVoice} refuses and names the missing key.
#'
#' Voice sessions generate in spoken register: their system prompt
#' carries an instruction to write plain speakable prose (no markdown,
#' numbers said aloud), because the reply is synthesized verbatim.
#' \code{voice.speech_style} in the config replaces that instruction's
#' text; typed room chat is unaffected either way.
#'
#' @param config Config list as from \code{bot_load_config()}, or
#'     \code{NULL} to load it.
#' @param address \code{"host:port"} to bind, or \code{NULL} for the
#'     config's \code{voice.listen}, or \code{"127.0.0.1:7851"}.
#' @param hooks Named list overriding side effects (tests/harnesses
#'     only): \code{http}, \code{clock}, \code{run_turn}, \code{post},
#'     \code{edit}, \code{members}, \code{ready}.
#' @param poll_ms Milliseconds each poll waits for events.
#' @param max_events Stop after handling this many events (tests only;
#'     the default never stops).
#' @return Invisibly, the number of events handled.
#' @export
voice_serve <- function(config = NULL, address = NULL, hooks = list(),
                        poll_ms = 200L, max_events = Inf) {
    voice_require()
    # A literal config stays as given (the caller owns its lifetime); the
    # default re-reads disk on every derivation so credential rotations
    # land (see voice_state).
    cfg_fn <- if (is.null(config)) bot_load_config else function() config
    # The default chat hooks speak chat.api; the same runtime floor the
    # room loop enforces applies here, but only when those defaults are
    # actually in play -- a harness that overrides them all owes nothing
    # to chat.api.
    if (!all(c("post", "edit", "members", "history") %in% names(hooks))) {
        bot_require_mx()
    }
    state <- voice_state(cfg_fn, hooks)
    if (is.null(address)) {
        address <- cfg_fn()$voice$listen
    }
    if (is.null(address)) {
        address <- "127.0.0.1:7851"
    }
    voice_load_protos()
    srv <- rgrpc::grpc_server(address)
    on.exit(rgrpc::grpc_close(srv), add = TRUE)
    port <- rgrpc::grpc_server_port(srv)
    message("corteza voice: serving AgentVoice on ", address, " (port ",
            port, ")")
    state$hooks$ready(port)
    handled <- 0
    repeat {
        handled <- handled + voice_poll_once(state, srv, poll_ms)
        if (handled >= max_events) {
            break
        }
    }
    invisible(handled)
}

# One drain of the server's event queue. Split from voice_serve so a
# test can drive the loop by hand.
voice_poll_once <- function(state, srv, timeout_ms = 100L) {
    evs <- rgrpc::grpc_poll(srv, timeout_ms = as.integer(timeout_ms))
    for (ev in evs) {
        if (!identical(ev$type, "request")) {
            # cancelled: Converse runs synchronously inside its handler,
            # so by the time a cancellation is drained here there is no
            # held call state to release.
            next
        }
        m <- ev$method
        streaming <- identical(m, .voice_method("Converse"))
        handler <- if (identical(m, .voice_method("AllocateVoice"))) {
            .voice_allocate
        } else if (streaming) {
            .voice_converse
        } else if (identical(m, .voice_method("ReportTurn"))) {
            .voice_report
        } else {
            NULL
        }
        if (is.null(handler)) {
            # A method this build does not serve. UNIMPLEMENTED rather
            # than a domain refusal, because nothing considered it.
            rgrpc::grpc_reply(ev, status = "UNIMPLEMENTED",
                             message = sprintf("no such method: %s", m))
            next
        }
        tryCatch(handler(state, ev),
                 corteza_voice_refusal = function(cond) {
            .voice_fail(ev, streaming, cond$status, conditionMessage(cond))
        },
                 error = function(cond) {
            .voice_fail(ev, streaming, "INTERNAL", conditionMessage(cond))
        })
    }
    length(evs)
}

# A streaming call that has already sent events cannot be grpc_reply'd;
# a unary one must be. try() because the peer may already be gone, and a
# dead call is not a server error.
.voice_fail <- function(ev, streaming, status, msg) {
    if (streaming) {
        try(rgrpc::grpc_finish(ev, status = status, message = msg),
            silent = TRUE)
    } else {
        try(rgrpc::grpc_reply(ev, status = status, message = msg), silent = TRUE)
    }
    invisible(NULL)
}

# Default HTTP hook: method, url, body, headers -> list(status, body).
#
# BOUNDED, because the server is synchronous and the peer is
# caller-selected: AllocateVoice dials the homeserver a client NAMED
# before anything about that client is established, so a host that
# accepts and stalls would otherwise hold every RPC hostage for as long
# as it liked. Connect and total timeouts cap the stall; the size cap
# stops a well-timed firehose from ballooning the process. Redirects
# are followed (the .well-known step may serve one) but only a few.
.VOICE_HTTP_MAX_BYTES <- 1e6

.voice_http <- function(method, url, body = NULL, headers = character(),
                        timeout_ms = 10000L, connect_timeout_ms = 5000L) {
    # maxfilesize catches oversize responses that declare a
    # Content-Length; the streaming guard below is the one that holds
    # when they do not (chunked, or lying).
    h <- curl::new_handle(timeout_ms = as.integer(timeout_ms),
                          connecttimeout_ms = as.integer(connect_timeout_ms),
                          followlocation = TRUE, maxredirs = 3L,
                          maxfilesize = .VOICE_HTTP_MAX_BYTES)
    if (identical(method, "POST")) {
        curl::handle_setopt(h, post = TRUE, postfields = body %||% "")
    }
    if (length(headers)) {
        curl::handle_setheaders(h, .list = as.list(headers))
    }
    # Streamed, not buffered: the cap is enforced as bytes ARRIVE, so a
    # peer that never stops talking is cut off at the limit instead of
    # growing this process until the limit is finally consulted.
    sink <- .voice_http_sink(.VOICE_HTTP_MAX_BYTES)
    res <- tryCatch(curl::curl_fetch_stream(url, sink$fun, handle = h),
                    error = function(e) {
        stop("voice http ", method, " failed: ", conditionMessage(e),
             call. = FALSE)
    })
    list(status = as.integer(res$status_code), body = sink$body())
}

# The per-chunk sink for .voice_http: accumulate, refuse the byte that
# crosses the cap. Separate from the fetch so the guard is testable
# without a peer -- libcurl's own maxfilesize abort (known length)
# surfaces as a generic connection error, and only this sink covers the
# unknown-length case.
.voice_http_sink <- function(cap) {
    chunks <- list()
    total <- 0
    list(fun = function(chunk) {
        total <<- total + length(chunk)
        if (total > cap) {
            stop("response larger than ", cap, " bytes", call. = FALSE)
        }
        chunks[[length(chunks) + 1L]] <<- chunk
    },
         body = function() {
        rawToChar(unlist(chunks, use.names = FALSE) %||% raw(0))
    })
}
