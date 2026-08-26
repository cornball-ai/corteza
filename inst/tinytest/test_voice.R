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
expect_true(corteza:::voice_valid_server_name("localhost"))
expect_true(corteza:::voice_valid_server_name("a.b-c.d"))
expect_true(corteza:::voice_valid_server_name("192.168.1.10:8448"))
expect_true(corteza:::voice_valid_server_name("[2001:db8::1]:8448"))
expect_false(corteza:::voice_valid_server_name("host example"))
expect_false(corteza:::voice_valid_server_name("https://host.example"))
expect_false(corteza:::voice_valid_server_name("host.example:99999999"))
# Port boundaries: 65535 is the last real port, 65536 fits the spec's
# 1*5DIGIT and no resolver's reality; 0 is not a port either.
expect_true(corteza:::voice_valid_server_name("host.example:65535"))
expect_false(corteza:::voice_valid_server_name("host.example:65536"))
expect_false(corteza:::voice_valid_server_name("host.example:0"))
# Label shape: no empty labels, no hyphen-edged labels, 63-char cap.
expect_false(corteza:::voice_valid_server_name("host..example"))
expect_false(corteza:::voice_valid_server_name(".host.example"))
expect_false(corteza:::voice_valid_server_name("host.example."))
expect_false(corteza:::voice_valid_server_name("-host.example"))
expect_false(corteza:::voice_valid_server_name("host-.example"))
expect_true(corteza:::voice_valid_server_name(
    paste0(strrep("a", 63), ".example")))
expect_false(corteza:::voice_valid_server_name(
    paste0(strrep("a", 64), ".example")))
# IPv4 octets are 0-255, and 999.1.1.1 is not an IPv4 address.
expect_true(corteza:::voice_valid_server_name("255.255.255.255"))
expect_false(corteza:::voice_valid_server_name("999.1.1.1"))
expect_false(corteza:::voice_valid_server_name("1.2.3"))

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
# Spoken register: a voice session's system prompt gains the
# speech-plain instruction; a configured style replaces the default
# text; a session with no room system still gets the register.
# ---------------------------------------------------------------

sys <- corteza:::voice_speech_system("You are cornelius.")
expect_true(startsWith(sys, "You are cornelius.\n\n"))
expect_true(grepl("read aloud", sys, fixed = TRUE))
expect_true(grepl("No markdown", sys, fixed = TRUE))
# a configured override replaces the default instruction, not the room
# system
sys <- corteza:::voice_speech_system("You are cornelius.",
                                     style = "Speak like a pirate.")
expect_identical(sys, "You are cornelius.\n\nSpeak like a pirate.")
expect_false(grepl("No markdown", sys, fixed = TRUE))
# degenerate styles (empty, NA, non-string) fall back to the default
expect_true(grepl("No markdown",
                  corteza:::voice_speech_system("s", style = "")))
expect_true(grepl("No markdown",
                  corteza:::voice_speech_system("s", style = NA)))
# no room system at all: the register stands alone
expect_identical(corteza:::voice_speech_system(NULL),
                 corteza:::.VOICE_SPEECH_REGISTER)
expect_identical(corteza:::voice_speech_system(""),
                 corteza:::.VOICE_SPEECH_REGISTER)

# ---------------------------------------------------------------
# The allocator itself: unconfigured is a precondition refusal that
# names the config key; a refusing allocator surfaces as UNAVAILABLE.
# ---------------------------------------------------------------

state <- corteza:::voice_state(list(), hooks = list())
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "FAILED_PRECONDITION")
expect_true(grepl("voice.allocator", conditionMessage(cond), fixed = TRUE))

# An allocator without its service token can never mint (the front
# requires the credential unconditionally): refused before any HTTP,
# naming the config key.
state <- corteza:::voice_state(list(voice = list(allocator = "http://a")),
                               hooks = list(http = function(...) {
                                   stop("must not be called")
                               }))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "FAILED_PRECONDITION")
expect_true(grepl("voice.allocator_token", conditionMessage(cond),
                  fixed = TRUE))

ALLOC_CFG <- list(voice = list(allocator = "http://a",
                               allocator_token = "svc-tok-1"))

state <- corteza:::voice_state(ALLOC_CFG,
                               hooks = list(http = function(...) {
                                   list(status = 503L, body = "")
                               }))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAVAILABLE")

# A refusal body's `error` sentence is surfaced, not swallowed: it
# names what the allocator disliked.
state <- corteza:::voice_state(ALLOC_CFG,
                               hooks = list(http = function(...) {
                                   list(status = 400L,
                                        body = '{"error":"room_id too long"}')
                               }))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "UNAVAILABLE")
expect_true(grepl("room_id too long", conditionMessage(cond), fixed = TRUE))

# 401 is not an outage: the credential was sent and refused, which is a
# config problem with a name.
state <- corteza:::voice_state(ALLOC_CFG,
                               hooks = list(http = function(...) {
                                   list(status = 401L, body = "{}")
                               }))
cond <- tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
                 corteza_voice_refusal = identity)
expect_equal(cond$status, "FAILED_PRECONDITION")
expect_true(grepl("voice.allocator_token", conditionMessage(cond),
                  fixed = TRUE))

# The request itself speaks the settled contract: protocol version in
# the closed body, the service credential in the authorization header.
seen <- list()
state <- corteza:::voice_state(
    ALLOC_CFG,
    hooks = list(http = function(method, url, body = NULL,
                                 headers = character()) {
        seen <<- list(method = method, url = url, body = body,
                      headers = headers)
        list(status = 503L, body = "")
    }))
tryCatch(corteza:::voice_allocate_media(state, "!room:h"),
         corteza_voice_refusal = identity)
expect_equal(seen$method, "POST")
req <- jsonlite::fromJSON(seen$body, simplifyVector = FALSE)
expect_equal(req$v, "gpu-voice-alloc/1")
expect_equal(req$room_id, "!room:h")
expect_equal(sort(names(req)), c("room_id", "v"))
expect_equal(unname(seen$headers[["authorization"]]), "Bearer svc-tok-1")

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
grant_json <- function(expires, v = "gpu-voice-alloc/1", ok = "true",
                       allocation_id = '"va-1"', room_id = '"!room:h"') {
    paste0('{"ok":', ok, ',"v":"', v, '",',
           '"allocation_id":', allocation_id, ',',
           '"room_id":', room_id, ',',
           '"speech_to_text":',
           '{"host":"stt.tail","port":7871,"security":"insecure"},',
           '"text_to_speech":',
           '{"host":"tts.tail","port":7872,"security":"insecure"},',
           '"token":"t","expires_at_unix_ms":', expires, "}")
}
alloc_state <- function(body) {
    corteza:::voice_state(
        ALLOC_CFG,
        hooks = list(clock = function() 5000,
                     http = function(...) list(status = 200L, body = body)))
}
refusal <- function(body) {
    tryCatch(corteza:::voice_allocate_media(alloc_state(body), "!room:h"),
             corteza_voice_refusal = identity)
}
cond <- refusal(grant_json(4000))
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("already expired", conditionMessage(cond)))

# The envelope refuses by name before the grant is read: a foreign
# protocol version, a 200 that does not say ok, a grant with no id, or
# a grant scoped to some other room.
cond <- refusal(grant_json(6000, v = "gpu-voice-alloc/2"))
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("gpu-voice-alloc/2", conditionMessage(cond), fixed = TRUE))
cond <- refusal(grant_json(6000, ok = "false"))
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("without ok", conditionMessage(cond)))
cond <- refusal(grant_json(6000, allocation_id = '""'))
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("allocation_id", conditionMessage(cond), fixed = TRUE))
cond <- refusal(grant_json(6000, room_id = '"!other:h"'))
expect_equal(cond$status, "INTERNAL")
expect_true(grepl("different room", conditionMessage(cond)))

grant <- suppressMessages(
    corteza:::voice_allocate_media(alloc_state(grant_json(6000)), "!room:h"))
expect_equal(grant$token, "t")
# The allocation id rides along so log lines and revocation can name
# the grant.
expect_equal(grant$allocation_id, "va-1")

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

# The size cap fires WHILE receiving. The sink is the during-receive
# guard: the chunk that crosses the cap errors, everything before it is
# kept, nothing after it is asked for.
local({
    sink <- corteza:::.voice_http_sink(10)
    sink$fun(charToRaw("hello"))
    sink$fun(charToRaw("world"))
    expect_error(sink$fun(charToRaw("!")), "larger than")
    expect_equal(sink$body(), "helloworld")
    empty <- corteza:::.voice_http_sink(10)
    expect_equal(empty$body(), "")
})

# And end to end without a network peer: file:// streams like any
# transfer. Oversize errors (via libcurl's own maxfilesize on a known
# length); a body under the cap comes through intact.
local({
    big <- tempfile(fileext = ".bin")
    writeBin(raw(corteza:::.VOICE_HTTP_MAX_BYTES + 1024), big)
    expect_error(corteza:::.voice_http("GET", paste0("file://", big)),
                 "failed")
    small <- tempfile(fileext = ".txt")
    writeLines("ok", small)
    expect_equal(corteza:::.voice_http("GET",
                                       paste0("file://", small))$body,
                 "ok\n")
    unlink(c(big, small))
})

# ---------------------------------------------------------------
# The delta relay: streams while the peer listens, cancels generation
# the moment it stops, keeps the record either way.
# ---------------------------------------------------------------

local({
    sent <- character()
    cancels <- 0L
    relay <- corteza:::.voice_stream_cb(function(d) {
        sent[[length(sent) + 1L]] <<- d
        TRUE
    }, function() cancels <<- cancels + 1L)
    relay$fun("Hello ")
    relay$fun("world.")
    expect_equal(sent, c("Hello ", "world."))
    expect_equal(relay$text(), "Hello world.")
    expect_true(relay$alive())
    expect_false(relay$empty())
    expect_equal(cancels, 0L)
})

local({
    sends <- 0L
    cancels <- 0L
    relay <- corteza:::.voice_stream_cb(function(d) {
        sends <<- sends + 1L
        if (sends >= 2L) {
            stop("peer gone")
        }
        TRUE
    }, function() cancels <<- cancels + 1L)
    relay$fun("one ")
    relay$fun("two ")
    # The failing delta is buffered before the cancel fires: the room
    # record holds everything generated, heard or not.
    expect_equal(relay$text(), "one two ")
    expect_false(relay$alive())
    expect_equal(cancels, 1L)
    # A hook that returns (a cancelled llm.api never calls again, but a
    # test hook might): later deltas are buffered, never sent, and the
    # cancel is not raised twice.
    relay$fun("three")
    expect_equal(relay$text(), "one two three")
    expect_equal(sends, 2L)
    expect_equal(cancels, 1L)
})

# The default cancel hook raises through the relay (that is how it
# reaches agent()'s stream loop); the delta still makes the record.
local({
    relay <- corteza:::.voice_stream_cb(function(d) FALSE,
                                        function() stop("llm_cancelled"))
    expect_error(relay$fun("cut "), "llm_cancelled")
    expect_equal(relay$text(), "cut ")
    expect_false(relay$alive())
})
