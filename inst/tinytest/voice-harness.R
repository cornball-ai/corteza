# An AgentVoice server with the world faked out, for test_voice_wire.R.
#
#   r voice-harness.R <port-file> <out-dir>
#
# A real child process rather than an in-process server, for vientito's
# harness reason: a synchronous client and a caller-driven server loop
# both want the main thread, so one process cannot do both without the
# test becoming a scheduler -- and nothing should cross except bytes.
#
# Every hook is a fake: the federation answers for exactly one token,
# the allocator hands out one fixed grant, the "room turn" streams two
# deltas, and posting/editing write files the parent asserts on.

library(corteza)
args <- if (exists("argv")) unlist(argv) else commandArgs(trailingOnly = TRUE)
port_file <- args[[1L]]
out_dir <- args[[2L]]

USER <- "@ann:host.example"
TOKEN <- "tok-openid-1"

hooks <- list(
    http = function(method, url, body = NULL, headers = character()) {
        if (grepl(".well-known/matrix/server", url, fixed = TRUE)) {
            return(list(status = 404L, body = ""))
        }
        if (grepl("/_matrix/federation/v1/openid/userinfo", url,
                  fixed = TRUE)) {
            if (grepl(paste0("access_token=", TOKEN), url, fixed = TRUE)) {
                return(list(status = 200L,
                            body = sprintf('{"sub":"%s"}', USER)))
            }
            return(list(status = 401L, body = "{}"))
        }
        if (grepl("/v1/voice/allocations", url, fixed = TRUE)) {
            # Enforce the settled contract the way the real allocator
            # does, so the wire test proves corteza SENDS it: the
            # service credential unconditionally (401 without), and the
            # closed request schema with its protocol version (400
            # without) -- see gpu.ctl's voice-allocation contract.
            auth <- unname(headers[tolower(names(headers)) ==
                "authorization"])
            if (!length(auth) || !nzchar(auth[[1L]])) {
                # ABSENT credential is 401; PRESENT-but-wrong is 400.
                # The contract's documented coarseness (gpu.ctl fixed
                # their side of this seam in their PR #12) -- corteza
                # maps the two differently, so the fake must split them
                # the same way the real allocator does.
                return(list(status = 401L, body = '{"error":"no credential"}'))
            }
            if (!identical(auth[[1L]], "Bearer svc-tok-1")) {
                return(list(status = 400L, body = '{"error":"bad credential"}'))
            }
            req <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
                            error = function(e) NULL)
            if (!is.list(req) || !identical(req$v, "gpu-voice-alloc/1") ||
                !is.character(req$room_id) || !nzchar(req$room_id)) {
                return(list(status = 400L, body = '{"error":"bad request"}'))
            }
            expires <- format(as.numeric(Sys.time()) * 1000 + 600000,
                              scientific = FALSE)
            return(list(status = 200L, body = paste0(
                '{"ok":true,"v":"gpu-voice-alloc/1",',
                '"allocation_id":"va-harness-1",',
                '"room_id":"', req$room_id, '",',
                '"speech_to_text":',
                '{"host":"stt.tail","port":7871,"security":"insecure"},',
                '"text_to_speech":',
                '{"host":"tts.tail","port":7872,"security":"insecure"},',
                '"token":"media-tok","expires_at_unix_ms":', expires, "}")))
        }
        list(status = 500L, body = "")
    },
    run_turn = function(state, room_id, text, on_delta) {
        # A prompt carrying FAILPOST produces a reply the post hook
        # refuses, so the parent can drive the never-posted report path.
        if (grepl("FAILPOST", text, fixed = TRUE)) {
            on_delta("FAILPOST reply.")
            return("FAILPOST reply.")
        }
        on_delta("Hello ")
        on_delta("spoken world.")
        "Hello spoken world."
    },
    post = function(room_id, text) {
        if (grepl("FAILPOST", text, fixed = TRUE)) {
            stop("homeserver said no")
        }
        writeLines(c(room_id, text), file.path(out_dir, "posted"))
        "$evt-1"
    },
    edit = function(room_id, event_id, text) {
        writeLines(c(event_id, text), file.path(out_dir, "edited"))
        invisible(TRUE)
    },
    members = function(room_id) c(USER, "@corteza:bot.example"),
    history = function(room_id) list(),
    ready = function(port) writeLines(as.character(port), port_file)
)

voice_serve(config = list(voice = list(allocator = "http://alloc.fake",
                                       allocator_token = "svc-tok-1")),
            address = "127.0.0.1:0", hooks = hooks)
