# AgentVoice logic that needs no wire: truncation, metadata, OpenID
# verification against a faked federation, session binding, grant
# validation. The gRPC/proto layer is exercised live in
# test_voice_wire.R; everything here is the decision-making those
# handlers delegate to, driven through injected hooks.

# ---------------------------------------------------------------
# voice_truncate: the first `heard` code points, tidied to a word
# boundary when the cut falls mid-word.
# ---------------------------------------------------------------

expect_equal(corteza:::voice_truncate("Hello spoken world.", 100L),
             "Hello spoken world.")
expect_equal(corteza:::voice_truncate("Hello spoken world.", 19L),
             "Hello spoken world.")
expect_equal(corteza:::voice_truncate("Hello spoken world.", 0L), "")
# Cut at the space after "Hello": nothing mid-word, trailing space goes.
expect_equal(corteza:::voice_truncate("Hello spoken world.", 6L), "Hello")
# Cut inside "spoken": the partial word is dropped.
expect_equal(corteza:::voice_truncate("Hello spoken world.", 9L), "Hello")
# Cut exactly at the end of "spoken": the word survives.
expect_equal(corteza:::voice_truncate("Hello spoken world.", 12L),
             "Hello spoken")
# Whole text is one partial word: tidying leaves nothing, and that is
# the documented "possibly tidied" outcome, not an error.
expect_equal(corteza:::voice_truncate("Antidisestablishment", 7L), "")
# Code points, not bytes: each of these is multibyte in UTF-8.
expect_equal(corteza:::voice_truncate("café olé sí", 4L),
             "café")
expect_equal(corteza:::voice_truncate("你好 世界", 2L),
             "你好")

# ---------------------------------------------------------------
# voice_bearer: both metadata entries are required, in the documented
# shapes.
# ---------------------------------------------------------------

good <- c("authorization" = "Bearer tok-1",
          "matrix-server-name" = "host.example")
cred <- corteza:::voice_bearer(good)
expect_equal(cred$bearer, "tok-1")
expect_equal(cred$server, "host.example")

# Keys are normalised, so wire-lowercased metadata parses the same.
mixed <- c("Authorization" = "Bearer tok-1",
           "Matrix-Server-Name" = "host.example")
expect_equal(corteza:::voice_bearer(mixed)$bearer, "tok-1")

expect_error(corteza:::voice_bearer(c("matrix-server-name" = "h")),
             "missing metadata: authorization")
expect_error(corteza:::voice_bearer(c("authorization" = "Bearer x")),
             "missing metadata: matrix-server-name")
expect_error(corteza:::voice_bearer(c("authorization" = "tok-1",
                                      "matrix-server-name" = "h")),
             "Bearer")

# Refusals carry their gRPC status.
cond <- tryCatch(corteza:::voice_bearer(character()),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAUTHENTICATED")

# The server name is validated as a name before it is used as one --
# it becomes a URL this process dials.
expect_error(corteza:::voice_bearer(c("authorization" = "Bearer x",
                                      "matrix-server-name" = "not a name")),
             "not a valid server name")
expect_true(corteza:::voice_valid_server_name("host.example"))
expect_true(corteza:::voice_valid_server_name("host.example:8448"))
expect_true(corteza:::voice_valid_server_name("192.168.1.10:8448"))
expect_true(corteza:::voice_valid_server_name("[2001:db8::1]:8448"))
expect_false(corteza:::voice_valid_server_name("host example"))
expect_false(corteza:::voice_valid_server_name("https://host.example"))
expect_false(corteza:::voice_valid_server_name("host.example:99999999"))

# ---------------------------------------------------------------
# voice_discover: delegation wins, otherwise the literal name gets the
# federation default port only when it carries none of its own.
# ---------------------------------------------------------------

http_wk <- function(body, status = 200L) {
    function(method, url, ...) {
        if (grepl(".well-known/matrix/server", url, fixed = TRUE)) {
            return(list(status = status, body = body))
        }
        stop("unexpected url: ", url)
    }
}
expect_equal(corteza:::voice_discover("host.example",
                                      http_wk('{"m.server":"fed.example:443"}')),
             "fed.example:443")
# A PORTLESS delegated name gets the federation default port, exactly
# like a portless direct name: with SRV out of scope (documented
# deviation), the spec's fallback is 8448 -- never a silent 443.
expect_equal(corteza:::voice_discover("host.example",
                                      http_wk('{"m.server":"fed.example"}')),
             "fed.example:8448")
# A delegation that fails the server-name grammar is ignored, not used.
expect_equal(corteza:::voice_discover("host.example",
                                      http_wk('{"m.server":"not a name"}')),
             "host.example:8448")
expect_equal(corteza:::voice_discover("host.example",
                                      http_wk("", status = 404L)),
             "host.example:8448")
expect_equal(corteza:::voice_discover("host.example:8449",
                                      http_wk("", status = 404L)),
             "host.example:8449")
# A .well-known that is down, not just absent, still falls through.
expect_equal(corteza:::voice_discover("host.example",
                                      function(...) stop("net down")),
             "host.example:8448")

# ---------------------------------------------------------------
# voice_verify_openid: userinfo on the named server, and the sub must
# belong to the server that answered.
# ---------------------------------------------------------------

openid_http <- function(sub_json, status = 200L) {
    function(method, url, ...) {
        if (grepl(".well-known", url, fixed = TRUE)) {
            return(list(status = 404L, body = ""))
        }
        if (grepl("/_matrix/federation/v1/openid/userinfo", url,
                  fixed = TRUE)) {
            return(list(status = status, body = sub_json))
        }
        stop("unexpected url: ", url)
    }
}

expect_equal(corteza:::voice_verify_openid("tok", "host.example",
                                           openid_http('{"sub":"@ann:host.example"}')),
             "@ann:host.example")
# A hostile server naming someone else's user: the domain of the sub is
# not the server queried, so the vouching is refused.
cond <- tryCatch(
                 corteza:::voice_verify_openid("tok", "host.example",
                                               openid_http('{"sub":"@victim:other.example"}')),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "PERMISSION_DENIED")
expect_true(grepl("not authoritative", conditionMessage(cond)))
# A token the server rejects.
cond <- tryCatch(
                 corteza:::voice_verify_openid("tok", "host.example",
                                               openid_http("{}", status = 401L)),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAUTHENTICATED")
# An answer with no usable sub.
expect_error(corteza:::voice_verify_openid("tok", "host.example",
                                           openid_http('{"sub":42}')),
             "usable sub")

# ---------------------------------------------------------------
# Sessions: bound to the bearer bytes that opened them, ended by the
# absolute expiry. Every failure is the same PERMISSION_DENIED, so a
# probe cannot confirm a session id it cannot use.
# ---------------------------------------------------------------

now <- 1000
state <- corteza:::voice_state(list(),
                               hooks = list(clock = function() now))
sid <- corteza:::voice_session_new(state, "@ann:host.example", "tok-1",
                                   "!room:h", expires_at_ms = 2000)
md <- function(tok) {
    c("authorization" = paste("Bearer", tok),
      "matrix-server-name" = "host.example")
}
rec <- corteza:::voice_session_auth(state, sid, md("tok-1"))
expect_equal(rec$identity, "@ann:host.example")
expect_equal(rec$room_id, "!room:h")

# A different token -- even a valid one from the same user -- is not
# this session's credential.
cond <- tryCatch(corteza:::voice_session_auth(state, sid, md("tok-2")),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "PERMISSION_DENIED")
# Unknown session id: same answer.
cond <- tryCatch(corteza:::voice_session_auth(state, "nope", md("tok-1")),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "PERMISSION_DENIED")
# Expiry ends the whole session, and the record does not linger.
now <- 2000
cond <- tryCatch(corteza:::voice_session_auth(state, sid, md("tok-1")),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "PERMISSION_DENIED")
expect_null(state$sessions[[sid]])
# Missing metadata refuses before any lookup happens.
cond <- tryCatch(corteza:::voice_session_auth(state, sid, character()),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAUTHENTICATED")

# An unknown hook name is a mistake, not an ignored option.
expect_error(corteza:::voice_state(list(), hooks = list(clok = identity)),
             "unknown voice hook")

# ---------------------------------------------------------------
# voice_validate_grant: every field the client will refuse on is
# refused here first, naming the field.
# ---------------------------------------------------------------

full_grant <- function() {
    list(speech_to_text = list(host = "stt.tail", port = 7871,
                               security = "insecure"),
         text_to_speech = list(host = "tts.tail", port = 7872,
                               security = "tls"),
         token = "media-tok",
         expires_at_unix_ms = 1755e9)
}
g <- corteza:::voice_validate_grant(full_grant())
expect_equal(g$speech_to_text$host, "stt.tail")
expect_equal(g$speech_to_text$port, 7871L)
expect_equal(g$text_to_speech$security, "tls")
expect_equal(g$token, "media-tok")
expect_equal(g$expires_at_unix_ms, 1755e9)

drop_field <- function(path1, path2 = NULL) {
    grant <- full_grant()
    if (is.null(path2)) {
        grant[[path1]] <- NULL
    } else {
        grant[[path1]][[path2]] <- NULL
    }
    grant
}
expect_error(corteza:::voice_validate_grant(drop_field("speech_to_text")),
             "missing speech_to_text")
expect_error(corteza:::voice_validate_grant(drop_field("text_to_speech",
                                                       "host")),
             "text_to_speech has no host")
expect_error(corteza:::voice_validate_grant(drop_field("speech_to_text",
                                                       "port")),
             "no usable port")
expect_error(corteza:::voice_validate_grant(drop_field("token")),
             "no media token")
expect_error(corteza:::voice_validate_grant(drop_field("expires_at_unix_ms")),
             "no expiry")
# A security state that is neither declared value must not relay as
# UNSPECIFIED (which the client refuses on): it refuses here.
bad <- full_grant()
bad$speech_to_text$security <- "none"
expect_error(corteza:::voice_validate_grant(bad), "tls.*insecure")

# ---------------------------------------------------------------
# The allocator itself: unconfigured is a precondition refusal that
# names the config key; a refusing allocator surfaces as UNAVAILABLE.
# ---------------------------------------------------------------

state <- corteza:::voice_state(list(), hooks = list())
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "FAILED_PRECONDITION")
expect_true(grepl("voice.allocator", conditionMessage(cond), fixed = TRUE))

state <- corteza:::voice_state(list(voice = list(allocator = "http://a")),
                               hooks = list(http = function(...) {
                                   list(status = 503L, body = "")
                               }))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAVAILABLE")

# ---------------------------------------------------------------
# Wire-valid values that R's integer would mangle.
# ---------------------------------------------------------------

# uint32's ceiling is a VALID text_heard ("heard it all", emphatically);
# as.integer() would turn it into NA and an INTERNAL refusal.
expect_equal(corteza:::voice_truncate("Hello spoken world.", 4294967295),
             "Hello spoken world.")

# ---------------------------------------------------------------
# Grant boundaries: fractional ports, non-finite or fractional expiry.
# ---------------------------------------------------------------

bad <- full_grant()
bad$speech_to_text$port <- 7871.5
expect_error(corteza:::voice_validate_grant(bad), "no usable port")
bad <- full_grant()
bad$expires_at_unix_ms <- Inf
expect_error(corteza:::voice_validate_grant(bad), "no expiry")
bad <- full_grant()
bad$expires_at_unix_ms <- 1755e9 + 0.5
expect_error(corteza:::voice_validate_grant(bad), "no expiry")

# A syntactically valid grant that is already expired mints a session
# no call could ever authorise against: refused at allocation.
grant_json <- function(expires) {
    paste0('{"speech_to_text":',
           '{"host":"stt.tail","port":7871,"security":"insecure"},',
           '"text_to_speech":',
           '{"host":"tts.tail","port":7872,"security":"insecure"},',
           '"token":"t","expires_at_unix_ms":', expires, "}")
}
state <- corteza:::voice_state(
    list(voice = list(allocator = "http://a")),
    hooks = list(clock = function() 5000,
                 http = function(...) list(status = 200L,
                                           body = grant_json(4000))))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("already expired", conditionMessage(cond)))
state <- corteza:::voice_state(
    list(voice = list(allocator = "http://a")),
    hooks = list(clock = function() 5000,
                 http = function(...) list(status = 200L,
                                           body = grant_json(6000))))
expect_equal(corteza:::voice_allocate_media(state, "!room:h")$token, "t")

# ---------------------------------------------------------------
# Credential rotation: the chat client is derived from the LIVE config
# at every use, never from a snapshot. cfg_fn cycles configs; each
# .voice_chat() call must hand bot_chat_client the latest one.
# ---------------------------------------------------------------

local({
    seen <- list()
    orig <- corteza:::bot_chat_client
    assignInNamespace("bot_chat_client", function(cfg, ...) {
        seen[[length(seen) + 1L]] <<- cfg
        structure(list(), class = "fake_chat")
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_chat_client", orig, ns = "corteza"),
            add = TRUE)
    generation <- 0L
    state <- corteza:::voice_state(function() {
        generation <<- generation + 1L
        list(token = paste0("tok-", generation))
    })
    corteza:::.voice_chat(state)
    corteza:::.voice_chat(state)
    expect_equal(length(seen), 2L)
    expect_equal(seen[[1L]]$token, "tok-1")
    # The second derivation saw the ROTATED config, not a cache.
    expect_equal(seen[[2L]]$token, "tok-2")
})

# ---------------------------------------------------------------
# Backfill: a voice session starts on the room's conversation, and a
# second lookup of the same room does not backfill again.
# ---------------------------------------------------------------

local({
    s <- new.env(parent = emptyenv())
    msgs <- list(
        list(kind = "message", body = "hi bot", self = FALSE,
             sender = "@ann:h"),
        list(kind = "notice", body = "archived: x", self = TRUE),
        list(kind = "message", body = "", self = FALSE),
        list(kind = "message", body = "hello ann", self = TRUE))
    state <- corteza:::voice_state(list(),
                                   hooks = list(history = function(rid) msgs))
    expect_equal(corteza:::.voice_backfill(state, s, "!room:h"), 2L)
    expect_equal(length(s$history), 2L)
    expect_equal(s$history[[1L]], list(role = "user", content = "hi bot"))
    expect_equal(s$history[[2L]], list(role = "assistant",
                                       content = "hello ann"))
    # A history fetch that fails seeds nothing and breaks nothing.
    state2 <- corteza:::voice_state(list(),
                                    hooks = list(history = function(rid) {
        stop("no transport")
    }))
    s2 <- new.env(parent = emptyenv())
    expect_equal(corteza:::.voice_backfill(state2, s2, "!room:h"), 0L)
})

local({
    calls <- 0L
    orig <- corteza:::bot_new_session
    assignInNamespace("bot_new_session", function(cfg, ...) {
        new.env(parent = emptyenv())
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_new_session", orig, ns = "corteza"),
            add = TRUE)
    state <- corteza:::voice_state(list(),
                                   hooks = list(history = function(rid) {
        calls <<- calls + 1L
        list(list(kind = "message", body = "earlier", self = FALSE))
    }))
    s1 <- corteza:::voice_room_session(state, "!room:h")
    expect_equal(length(s1$history), 1L)
    s2 <- corteza:::voice_room_session(state, "!room:h")
    # Same session, and the room history was fetched exactly once.
    expect_identical(s1, s2)
    expect_equal(calls, 1L)
})

# ---------------------------------------------------------------
# The runtime version floors cannot drift from DESCRIPTION: the test
# reads the declared bound rather than restating it.
# ---------------------------------------------------------------

local({
    desc <- read.dcf(system.file("DESCRIPTION", package = "corteza"),
                     fields = "Suggests")[1L, 1L]
    m <- regmatches(desc,
                    regexpr("grpc \\(>= [0-9.]+\\)", desc))
    expect_equal(length(m), 1L)
    declared <- unname(sub("grpc \\(>= ([0-9.]+)\\)", "\\1", m))
    expect_equal(declared, corteza:::.VOICE_GRPC_MIN)
})

# ---------------------------------------------------------------
# Remote I/O is bounded: a peer that accepts (or blackholes) and stalls
# cannot hold the synchronous server past the configured timeouts.
# ---------------------------------------------------------------

if (at_home()) {
    local({
        t0 <- Sys.time()
        # 10.255.255.1 is non-routable: the connect stalls until the
        # connect timeout fires. If some network answers it anyway, the
        # request errors or returns fast -- either way the bound holds.
        res <- tryCatch(corteza:::.voice_http("GET",
                                              "https://10.255.255.1/x",
                                              timeout_ms = 1500L,
                                              connect_timeout_ms = 500L),
                        error = function(e) e)
        elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        expect_true(elapsed < 5)
    })
}
