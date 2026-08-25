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

# The allocator's answer, validated and normalised to plain values. The
# JSON shape mirrors the AllocateVoiceResponse proto fields.
voice_allocate_media <- function(state, room_id) {
    url <- state$cfg$voice$allocator
    if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
        voice_refuse("FAILED_PRECONDITION",
                     paste0("no media allocator configured: set ",
                            "voice.allocator to the gpu.ctl base URL"))
    }
    body <- jsonlite::toJSON(list(room_id = room_id), auto_unbox = TRUE)
    res <- tryCatch(
                    state$hooks$http("POST",
                                     paste0(sub("/+$", "", url),
                                            "/v1/voice/allocations"),
                                     body = body,
                                     headers = c("content-type" =
                                                     "application/json")),
                    error = function(e) {
                        voice_refuse("UNAVAILABLE",
                                     "the media allocator is unreachable: %s",
                                     conditionMessage(e))
                    }
    )
    if (!identical(as.integer(res$status), 200L)) {
        voice_refuse("UNAVAILABLE",
                     "the media allocator refused the request (HTTP %s)",
                     res$status)
    }
    grant <- tryCatch(jsonlite::fromJSON(res$body, simplifyVector = FALSE),
                      error = function(e) NULL)
    if (!is.list(grant)) {
        voice_refuse("INTERNAL",
                     "the media allocator answered with something not JSON")
    }
    voice_validate_grant(grant)
}

voice_validate_grant <- function(grant) {
    ep <- function(field) {
        e <- grant[[field]]
        if (!is.list(e)) {
            voice_refuse("INTERNAL", "allocation grant is missing %s", field)
        }
        host <- e$host
        if (!is.character(host) || length(host) != 1L || !nzchar(host)) {
            voice_refuse("INTERNAL",
                         "allocation grant %s has no host", field)
        }
        port <- e$port
        if (!is.numeric(port) || length(port) != 1L || is.na(port) ||
            port < 1 || port > 65535) {
            voice_refuse("INTERNAL",
                         "allocation grant %s has no usable port", field)
        }
        # Two sayable states only. Anything else would relay as
        # UNSPECIFIED, which the client refuses to connect on -- so it
        # refuses here, where the message can name the allocator.
        security <- e$security
        if (!is.character(security) ||
            !security %in% c("tls", "insecure")) {
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
    if (!is.numeric(expires) || length(expires) != 1L || is.na(expires) ||
        expires <= 0) {
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
                                  paste0("CHANNEL_SECURITY_",
                                         toupper(ep$security)))
    m
}

# AllocateVoice: verify identity, check membership, allocate media,
# mint the session. The one RPC that does a federation round trip.
.voice_allocate <- function(state, ev) {
    cred <- voice_bearer(ev$metadata)
    identity <- voice_verify_openid(cred$bearer, cred$server,
                                    state$hooks$http)
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
