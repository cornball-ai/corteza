# AgentVoice authentication: Matrix OpenID verification and the session
# records that Converse/ReportTurn are authorised against.
#
# The model, from the proto (the comments there are the spec):
#
# - Every call carries TWO metadata entries: `authorization: Bearer
#   <openid-token>` and `matrix-server-name`. Absent metadata is a
#   refusal, never a guessed host -- verifying against a guessed server
#   is how a token that would never verify appears to.
# - Verification happens EXACTLY ONCE, at AllocateVoice: userinfo on the
#   named server, then the returned sub's domain must equal the server
#   queried. The server name is caller-controlled and a server is only
#   authoritative for its own users; skip the comparison and a hostile
#   homeserver names a victim's user id and inherits their room
#   memberships.
# - OpenID proves identity only. Membership in room_id is a separate
#   check, done by the AllocateVoice handler before minting.
# - The minted session is bound to (identity, bearer bytes as
#   presented). Converse and ReportTurn authorise by bearer match
#   against the session record -- no federation round trip, and a
#   different valid token from the same user is PERMISSION_DENIED all
#   the same. expires_at_unix_ms ends the whole session.

# Pull the two required entries out of gRPC metadata. Keys arrive
# lowercase on the wire, but normalise anyway: a test harness or a
# future client library may not.
voice_bearer <- function(metadata) {
    md <- as.list(metadata)
    names(md) <- tolower(names(md))
    auth <- md[["authorization"]]
    server <- md[["matrix-server-name"]]
    if (!is.character(auth) || !nzchar(auth)) {
        voice_refuse("UNAUTHENTICATED", "missing metadata: authorization")
    }
    if (!grepl("^Bearer .+", auth)) {
        voice_refuse("UNAUTHENTICATED",
                     "authorization must be 'Bearer <openid-token>'")
    }
    if (!is.character(server) || !nzchar(server)) {
        voice_refuse("UNAUTHENTICATED", "missing metadata: matrix-server-name")
    }
    if (!voice_valid_server_name(server)) {
        voice_refuse("UNAUTHENTICATED",
                     "matrix-server-name is not a valid server name")
    }
    list(bearer = sub("^Bearer ", "", auth), server = server)
}

# The Matrix server-name shape (spec appendices): a DNS name, an IPv4
# literal, or a bracketed IPv6 literal, with an optional port. This is
# metadata that becomes a URL this process dials, so it is validated as
# a name before it is ever used as one -- and TIGHTER than the spec's
# ABNF on purpose: the ABNF admits strings no resolver accepts (port
# 65536, empty labels, hyphen-edged labels), and refusing those here
# costs nothing but a clearer error, earlier.
voice_valid_server_name <- function(server) {
    if (!is.character(server) || length(server) != 1L || !nzchar(server)) {
        return(FALSE)
    }
    # Split the optional :port, keeping an IPv6 bracket literal whole.
    m <- regmatches(server,
                    regexec("^(\\[[^]]+\\]|[^:]+)(:([0-9]{1,5}))?$", server))[[1L]]
    if (!length(m)) {
        return(FALSE)
    }
    host <- m[[2L]]
    port <- m[[4L]]
    if (nzchar(port) && (as.integer(port) < 1L || as.integer(port) > 65535L)) {
        return(FALSE)
    }
    if (grepl("^\\[", host)) {
        return(grepl("^\\[[0-9A-Fa-f:.]{2,45}\\]$", host))
    }
    if (grepl("^[0-9.]+$", host)) {
        # IPv4 literal: four octets, each 0-255.
        octets <- strsplit(host, ".", fixed = TRUE)[[1L]]
        if (length(octets) != 4L || !all(grepl("^[0-9]{1,3}$", octets))) {
            return(FALSE)
        }
        return(all(as.integer(octets) <= 255L))
    }
    # DNS name: dot-separated labels, 1-63 chars each, alphanumeric at
    # both ends, hyphens only inside, 255 chars overall. The dot checks
    # are explicit because strsplit silently drops empty trailing
    # pieces, which would wave "host.example." through.
    if (nchar(host) > 255L || grepl("^\\.|\\.\\.|\\.$", host)) {
        return(FALSE)
    }
    labels <- strsplit(host, ".", fixed = TRUE)[[1L]]
    length(labels) > 0L &&
    all(grepl("^[0-9A-Za-z]([0-9A-Za-z-]{0,61}[0-9A-Za-z])?$", labels))
}

# Where a server name's federation API answers. Delegation first
# (.well-known, redirects followed by the HTTP hook), then the name
# with the federation default port. Failures fall through to the
# fallback rather than refusing: a server with no .well-known is the
# normal case, not an error.
#
# DELIBERATE SPEC DEVIATION: the SRV steps (_matrix-fed._tcp, and the
# deprecated _matrix._tcp) are skipped -- base R cannot do SRV lookups
# without a dependency, tailnet deployments do not publish SRV records,
# and hosted homeservers advertise via .well-known. A portless name --
# delegated or direct -- therefore goes straight to the spec's no-SRV
# fallback, port 8448, never to 443.
voice_discover <- function(server, http) {
    res <- tryCatch(
                    http("GET", sprintf("https://%s/.well-known/matrix/server", server)),
                    error = function(e) NULL
    )
    target <- server
    if (!is.null(res) && identical(as.integer(res$status), 200L)) {
        parsed <- tryCatch(jsonlite::fromJSON(res$body),
                           error = function(e) NULL)
        delegated <- parsed[["m.server"]]
        if (is.character(delegated) && length(delegated) == 1L &&
            nzchar(delegated) && voice_valid_server_name(delegated)) {
            target <- delegated
        }
    }
    if (grepl(":[0-9]+$", target)) {
        return(target)
    }
    paste0(target, ":8448")
}

# Verify an OpenID token against the server the caller named, and bind
# the answer to that server. Returns the verified user id.
voice_verify_openid <- function(bearer, server, http) {
    host <- voice_discover(server, http)
    url <- sprintf(paste0("https://%s/_matrix/federation/v1/openid/",
                          "userinfo?access_token=%s"),
                   host, curl::curl_escape(bearer))
    res <- tryCatch(http("GET", url), error = function(e) {
        voice_refuse("UNAUTHENTICATED",
                     "OpenID verification against %s failed: %s",
                     server, conditionMessage(e))
    })
    if (!identical(as.integer(res$status), 200L)) {
        voice_refuse("UNAUTHENTICATED",
                     "the token did not verify against %s (HTTP %s)",
                     server, res$status)
    }
    parsed <- tryCatch(jsonlite::fromJSON(res$body), error = function(e) NULL)
    identity <- parsed$sub
    if (!is.character(identity) || length(identity) != 1L ||
        !grepl("^@[^:]+:.+", identity)) {
        voice_refuse("UNAUTHENTICATED",
                     "%s answered userinfo without a usable sub", server)
    }
    # THE SUB MUST BELONG TO THE SERVER THAT ANSWERED. This comparison
    # is the entire difference between "a server vouched for its user"
    # and "a server named a user".
    domain <- sub("^@[^:]+:", "", identity)
    if (!identical(domain, server)) {
        voice_refuse("PERMISSION_DENIED",
                     "%s is not authoritative for %s", server, identity)
    }
    identity
}

# HMAC-SHA256 of the bearer under a per-process random key. The session
# comparison is between two such digests, so where the compare exits
# early reveals byte positions of an unpredictable digest, not of the
# bearer -- which is the property a "constant-time compare" is after,
# obtained without one. The key keeps the digest useless outside this
# process.
.voice_tag <- function(state, bearer) {
    digest::hmac(state$key, bearer, algo = "sha256")
}

voice_session_new <- function(state, identity, bearer, room_id, expires_at_ms) {
    id <- voice_id(state)
    rec <- new.env(parent = emptyenv())
    rec$identity <- identity
    rec$bearer_tag <- .voice_tag(state, bearer)
    rec$room_id <- room_id
    rec$expires_at_ms <- expires_at_ms
    rec$turns <- new.env(parent = emptyenv())
    assign(id, rec, envir = state$sessions)
    id
}

# Authorise a Converse/ReportTurn call against its session. Unknown id,
# expired session, and wrong bearer are all PERMISSION_DENIED: telling
# them apart would confirm session ids to a caller who cannot use them.
voice_session_auth <- function(state, session_id, metadata) {
    cred <- voice_bearer(metadata)
    rec <- NULL
    if (is.character(session_id) && nzchar(session_id)) {
        rec <- state$sessions[[session_id]]
    }
    if (is.null(rec)) {
        voice_refuse("PERMISSION_DENIED", "no such voice session")
    }
    if (state$hooks$clock() >= rec$expires_at_ms) {
        rm(list = session_id, envir = state$sessions)
        voice_refuse("PERMISSION_DENIED", "no such voice session")
    }
    if (!identical(.voice_tag(state, cred$bearer), rec$bearer_tag)) {
        voice_refuse("PERMISSION_DENIED", "no such voice session")
    }
    rec
}
