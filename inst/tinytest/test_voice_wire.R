# AgentVoice end to end, over real gRPC against a child-process server
# (voice-harness.R) whose hooks fake the world. What this proves that
# test_voice.R cannot: the protos load, the metadata crosses, the oneof
# events arrive in order, proto3 presence survives the wire, and every
# refusal comes back as its intended status -- none of which an
# in-process call can show.

if (!at_home()) {
    exit_file("wire test runs at home only")
}
if (!requireNamespace("grpc", quietly = TRUE) ||
    !requireNamespace("RProtoBuf", quietly = TRUE)) {
    exit_file("needs grpc and RProtoBuf")
}

dir <- tempfile("voice-wire-")
dir.create(dir)
port_file <- file.path(dir, "port")

rscript <- file.path(R.home("bin"), "Rscript")
proc <- processx::process$new(rscript,
                              c("--vanilla", "voice-harness.R", port_file,
                                dir),
                              stderr = file.path(dir, "harness.log"))

port <- NULL
for (i in 1:100) {
    if (file.exists(port_file)) {
        port <- readLines(port_file, warn = FALSE)[1L]
        break
    }
    if (!proc$is_alive()) {
        break
    }
    Sys.sleep(0.1)
}
if (is.null(port)) {
    log <- file.path(dir, "harness.log")
    exit_file(paste("harness never bound:",
                    paste(if (file.exists(log)) readLines(log, warn = FALSE),
                          collapse = " | ")))
}

corteza:::voice_load_protos()
client <- grpc::grpc_client(sprintf("127.0.0.1:%s", port))
svc <- grpc::grpc_service(corteza:::.voice_type("AllocateVoiceRequest"),
                          "cornball.agent.v1.AgentVoice")

MD <- c("authorization" = "Bearer tok-openid-1",
        "matrix-server-name" = "host.example")

unary <- function(method, req, metadata = MD) {
    call <- grpc::grpc_call(client, grpc::grpc_method(svc, method), req,
                            deadline_ms = 10000L, metadata = metadata)
    repeat {
        evs <- grpc::grpc_await(call, timeout_ms = 1000L)
        for (ev in evs) {
            return(ev)
        }
    }
}

# ---------------------------------------------------------------
# AllocateVoice: the happy path relays the grant and mints a session.
# ---------------------------------------------------------------

req <- corteza:::.voice_type("AllocateVoiceRequest")$new(room_id = "!room:h")
ev <- unary("AllocateVoice", req)
expect_equal(ev$status_name, "OK")
grant <- ev$response_message
expect_true(nzchar(grant$session_id))
expect_equal(grant$speech_to_text$host, "stt.tail")
expect_equal(grant$speech_to_text$port, 7871L)
expect_equal(grant$speech_to_text$security,
             corteza:::.voice_enum_num("ChannelSecurity",
                                       "CHANNEL_SECURITY_INSECURE"))
expect_equal(grant$text_to_speech$host, "tts.tail")
expect_equal(grant$token, "media-tok")
expect_true(grant$expires_at_unix_ms > as.numeric(Sys.time()) * 1000)
sid <- grant$session_id

# A token the homeserver rejects never reaches a session.
ev <- unary("AllocateVoice", req,
            metadata = c("authorization" = "Bearer tok-wrong",
                         "matrix-server-name" = "host.example"))
expect_equal(ev$status_name, "UNAUTHENTICATED")
# Absent auth metadata is a refusal, not a guessed host. (The client
# library refuses to send literally empty metadata, so the call carries
# an unrelated entry and neither required one.)
ev <- unary("AllocateVoice", req, metadata = c("x-unrelated" = "1"))
expect_equal(ev$status_name, "UNAUTHENTICATED")

# ---------------------------------------------------------------
# Converse: TurnStart first, deltas append, TurnEnd, clean finish.
# ---------------------------------------------------------------

converse <- function(session_id, text, metadata = MD) {
    s <- grpc::grpc_stream(client, grpc::grpc_method(svc, "Converse"),
                           metadata = metadata, deadline_ms = 10000L)
    grpc::grpc_send(s, corteza:::.voice_type("ConverseRequest")$new(
        session_id = session_id, text = text))
    grpc::grpc_writes_done(s)
    msgs <- list()
    status <- NULL
    for (i in 1:100) {
        evs <- grpc::grpc_await(s, timeout_ms = 1000L)
        for (e in evs) {
            if (!is.null(e$response_message)) {
                msgs[[length(msgs) + 1L]] <- e$response_message
            }
            if (!is.null(e$status_name)) {
                status <- e$status_name
            }
        }
        if (!is.null(status)) {
            break
        }
    }
    list(msgs = msgs, status = status)
}

out <- converse(sid, "say hello")
expect_equal(out$status, "OK")
expect_true(length(out$msgs) >= 3L)
first <- out$msgs[[1L]]
expect_true(first$has("start"))
turn_id <- first$start$turn_id
expect_true(nzchar(turn_id))
deltas <- Filter(function(m) m$has("delta"), out$msgs)
heard_text <- paste(vapply(deltas, function(m) m$delta$text, character(1)),
                    collapse = "")
expect_equal(heard_text, "Hello spoken world.")
last <- out$msgs[[length(out$msgs)]]
expect_true(last$has("end"))

# The reply was posted to the room at turn end, before any report.
posted <- readLines(file.path(dir, "posted"), warn = FALSE)
expect_equal(posted, c("!room:h", "Hello spoken world."))

# A bearer that is not this session's credential gets nothing, even
# though it is a token the federation would verify.
out <- converse(sid, "hi",
                metadata = c("authorization" = "Bearer tok-other",
                             "matrix-server-name" = "host.example"))
expect_equal(out$status, "PERMISSION_DENIED")

# ---------------------------------------------------------------
# ReportTurn: presence-checked, truncating, idempotent.
# ---------------------------------------------------------------

# Absence of text_heard is INVALID_ARGUMENT, never read as zero.
ev <- unary("ReportTurn", corteza:::.voice_type("ReportTurnRequest")$new(
    session_id = sid, turn_id = turn_id))
expect_equal(ev$status_name, "INVALID_ARGUMENT")

# 6 code points of "Hello spoken world." is "Hello " -> stored "Hello",
# and the room record is edited down to match.
ev <- unary("ReportTurn", corteza:::.voice_type("ReportTurnRequest")$new(
    session_id = sid, turn_id = turn_id, text_heard = 6L))
expect_equal(ev$status_name, "OK")
expect_equal(ev$response_message$stored_text, "Hello")
edited <- readLines(file.path(dir, "edited"), warn = FALSE)
expect_equal(edited, c("$evt-1", "Hello"))

# A second report does not edit again; the first decided the record.
ev <- unary("ReportTurn", corteza:::.voice_type("ReportTurnRequest")$new(
    session_id = sid, turn_id = turn_id, text_heard = 19L))
expect_equal(ev$status_name, "OK")
expect_equal(ev$response_message$stored_text, "Hello")

# An unknown turn is NOT_FOUND, not a silent no-op.
ev <- unary("ReportTurn", corteza:::.voice_type("ReportTurnRequest")$new(
    session_id = sid, turn_id = "nope", text_heard = 0L))
expect_equal(ev$status_name, "NOT_FOUND")

# A second allocation with the same credential mints a distinct
# session: sessions belong to allocations, not to users.
ev <- unary("AllocateVoice", req, metadata = MD)
expect_equal(ev$status_name, "OK")
expect_false(identical(ev$response_message$session_id, sid))

grpc::grpc_close(client)
proc$kill()
unlink(dir, recursive = TRUE)
