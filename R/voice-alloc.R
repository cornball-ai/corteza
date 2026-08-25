# Media-endpoint allocation for AgentVoice.
#
# The AllocateVoice grant names where the client opens its two media
# streams (speech-to-text, text-to-speech) and carries a short-lived
# bearer those endpoints accept. corteza does not mint that token and
# does not run those hosts: the fleet control plane (gpu.ctl) mints AND
# validates, corteza requests an allocation and relays the grant. HTTP
# for v1, because that is what gpu.ctl speaks today.
#
# Every field is validated loudly on the way through. A grant relayed
# with a missing host or an unsayable security state fails here, naming
# the field, rather than as a client that cannot connect.

# The protocol both sides of the allocation speak. The request carries
# it so the allocator can refuse a caller from the future; the response
# carries it back so corteza never reads fields into a shape the
# allocator did not mean.
.VOICE_ALLOC_PROTOCOL <- "gpu-voice-alloc/1"

# The allocator's answer, validated and normalised to plain values. The
# envelope (ok, v, allocation_id, room_id) is checked here; the grant
# fields mirror the AllocateVoiceResponse proto and are validated in
# voice_validate_grant.
voice_allocate_media <- function(state, room_id) {
    voice_cfg <- state$cfg_fn()$voice
    url <- voice_cfg$allocator
    if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
        voice_refuse("FAILED_PRECONDITION",
                     paste0("no media allocator configured: set ",
                            "voice.allocator to the gpu.ctl base URL"))
    }
    # The allocator requires its service credential unconditionally, so
    # a configured allocator without a token can never mint. Refuse
    # here, naming the key, rather than relaying the 401 the allocator
    # would send.
    token <- voice_cfg$allocator_token
    if (!is.character(token) || length(token) != 1L || !nzchar(token)) {
        voice_refuse("FAILED_PRECONDITION",
                     paste0("the media allocator requires a service ",
                            "token: set voice.allocator_token to the ",
                            "credential the gpu.ctl front expects"))
    }
    body <- jsonlite::toJSON(list(v = .VOICE_ALLOC_PROTOCOL,
                                  room_id = room_id), auto_unbox = TRUE)
    res <- tryCatch(
                    state$hooks$http("POST",
                                     paste0(sub("/+$", "", url), "/v1/voice/allocations"),
                                     body = body,
                                     headers = c("content-type" = "application/json",
                                                 "authorization" = paste("Bearer", token))),
                    error = function(e) {
        voice_refuse("UNAVAILABLE",
                     "the media allocator is unreachable: %s",
                     conditionMessage(e))
    }
    )
    status <- as.integer(res$status)
    if (identical(status, 401L)) {
        # The credential is always sent, so a 401 means the allocator
        # does not accept THIS one: a config problem with a name, not
        # an outage.
        voice_refuse("FAILED_PRECONDITION",
                     paste0("the media allocator rejected the service ",
                            "token (HTTP 401): check voice.allocator_token"))
    }
    if (!identical(status, 200L)) {
        # Refusals carry an `error` sentence naming what the allocator
        # disliked; surface it when there is one.
        err <- tryCatch(jsonlite::fromJSON(res$body,
                                           simplifyVector = FALSE)$error,
                        error = function(e) NULL)
        if (is.character(err) && length(err) == 1L && nzchar(err)) {
            voice_refuse("UNAVAILABLE",
                         "the media allocator refused the request (HTTP %s): %s",
                         status, err)
        }
        voice_refuse("UNAVAILABLE",
                     "the media allocator refused the request (HTTP %s)",
                     status)
    }
    ans <- tryCatch(jsonlite::fromJSON(res$body, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.list(ans)) {
        voice_refuse("INTERNAL",
                     "the media allocator answered with something not JSON")
    }
    # The envelope before the grant: a different protocol version, a
    # 200 that does not say ok, or a grant for some other room is
    # refused by name, never guessed through.
    if (!identical(ans$v, .VOICE_ALLOC_PROTOCOL)) {
        voice_refuse("INTERNAL",
                     "the media allocator speaks %s, this corteza speaks %s",
                     if (is.character(ans$v) && length(ans$v) == 1L &&
            nzchar(ans$v)) ans$v else "<no version>",
                     .VOICE_ALLOC_PROTOCOL)
    }
    if (!isTRUE(ans$ok)) {
        voice_refuse("INTERNAL",
                     "the media allocator answered 200 without ok")
    }
    alloc_id <- ans$allocation_id
    if (!is.character(alloc_id) || length(alloc_id) != 1L ||
        !nzchar(alloc_id)) {
        voice_refuse("INTERNAL",
                     "allocation grant carries no allocation_id")
    }
    if (!identical(ans$room_id, room_id)) {
        voice_refuse("INTERNAL",
                     "allocation %s is for a different room", alloc_id)
    }
    grant <- voice_validate_grant(ans)
    # An already-expired grant would mint a session no call can ever
    # authorise against -- the client learns that only when its first
    # Converse refuses, well after the allocator misbehaved.
    if (grant$expires_at_unix_ms <= state$hooks$clock()) {
        voice_refuse("INTERNAL", "allocation grant is already expired")
    }
    grant$allocation_id <- alloc_id
    # The id exists so log lines and revocation can name the grant.
    message("corteza voice: allocation ", alloc_id, " for ", room_id)
    grant
}

voice_validate_grant <- function(grant) {
    ep <- function(field) {
        e <- grant[[field]]
        if (!is.list(e)) {
            voice_refuse("INTERNAL", "allocation grant is missing %s", field)
        }
        host <- e$host
        if (!is.character(host) || length(host) != 1L || !nzchar(host)) {
            voice_refuse("INTERNAL", "allocation grant %s has no host", field)
        }
        port <- e$port
        if (!is.numeric(port) || length(port) != 1L || is.na(port) ||
            port < 1 || port > 65535 || port != trunc(port)) {
            voice_refuse("INTERNAL", "allocation grant %s has no usable port",
                         field)
        }
        # Two sayable states only. Anything else would relay as
        # UNSPECIFIED, which the client refuses to connect on -- so it
        # refuses here, where the message can name the allocator.
        security <- e$security
        if (!is.character(security) || !security %in% c("tls", "insecure")) {
            voice_refuse("INTERNAL",
                         paste0("allocation grant %s must declare security ",
                                "as \"tls\" or \"insecure\""), field)
        }
        list(host = host, port = as.integer(port), security = security)
    }
    token <- grant$token
    if (!is.character(token) || length(token) != 1L || !nzchar(token)) {
        voice_refuse("INTERNAL", "allocation grant carries no media token")
    }
    expires <- grant$expires_at_unix_ms
    # Finite, positive, integral: an Inf or NaN here would mint a
    # session that never expires (or never validates), silently.
    if (!is.numeric(expires) || length(expires) != 1L || is.na(expires) ||
        !is.finite(expires) || expires <= 0 || expires != trunc(expires)) {
        voice_refuse("INTERNAL", "allocation grant carries no expiry")
    }
    list(speech_to_text = ep("speech_to_text"),
         text_to_speech = ep("text_to_speech"),
         token = token,
         expires_at_unix_ms = as.numeric(expires))
}

# Endpoint list -> proto message, security by descriptor lookup.
.voice_endpoint_msg <- function(ep) {
    m <- .voice_type("Endpoint")$new()
    m$host <- ep$host
    m$port <- ep$port
    m$security <- .voice_enum_num("ChannelSecurity",
                                  paste0("CHANNEL_SECURITY_", toupper(ep$security)))
    m
}

# AllocateVoice: verify identity, check membership, allocate media,
# mint the session. The one RPC that does a federation round trip.
.voice_allocate <- function(state, ev) {
    cred <- voice_bearer(ev$metadata)
    identity <- voice_verify_openid(cred$bearer, cred$server, state$hooks$http)
    req <- RProtoBuf::read(.voice_type("AllocateVoiceRequest"), ev$request)
    room_id <- req$room_id
    if (!nzchar(room_id)) {
        voice_refuse("INVALID_ARGUMENT", "room_id is required")
    }
    # OpenID proved who is asking, not that they belong where they are
    # asking to be. room_id in the request is scoping, not
    # authentication.
    members <- tryCatch(state$hooks$members(room_id), error = function(e) {
        voice_refuse("UNAVAILABLE", "cannot check membership of %s: %s",
                     room_id, conditionMessage(e))
    })
    if (!identity %in% members) {
        voice_refuse("PERMISSION_DENIED", "%s is not a member of %s",
                     identity, room_id)
    }
    grant <- voice_allocate_media(state, room_id)
    sid <- voice_session_new(state, identity, cred$bearer, room_id,
                             grant$expires_at_unix_ms)
    resp <- .voice_type("AllocateVoiceResponse")$new()
    resp$session_id <- sid
    resp$speech_to_text <- .voice_endpoint_msg(grant$speech_to_text)
    resp$text_to_speech <- .voice_endpoint_msg(grant$text_to_speech)
    resp$token <- grant$token
    resp$expires_at_unix_ms <- grant$expires_at_unix_ms
    grpc::grpc_reply(ev, resp)
    invisible(NULL)
}
