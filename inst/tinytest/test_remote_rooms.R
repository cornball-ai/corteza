if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api unavailable")

local({
    root <- tempfile("remote-rooms-")
    dir.create(root)
    vars <- c("CORTEZA_REMOTE_DIR", "CORTEZA_STATE_DIR", "R_USER_DATA_DIR",
              "R_USER_CONFIG_DIR", "R_USER_CACHE_DIR")
    old <- Sys.getenv(vars, unset = NA)
    do.call(Sys.setenv, setNames(as.list(file.path(root, vars)), vars))
    on.exit({
        for (i in seq_along(vars)) if (is.na(old[[i]])) Sys.unsetenv(vars[[i]]) else
            do.call(Sys.setenv, setNames(list(old[[i]]), vars[[i]]))
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(user_id = "@courier:example", operators = "@owner:example",
                provider = "ollama", model = "fixture")
    cl <- chat.api::chat_loopback()
    sessions <- corteza:::bot_new_session_registry()
    corteza:::.remote_advertise(cfg)
    ctx <- new.env(parent = emptyenv())
    ctx$config <- list()
    ctx$cwd <- root
    expect_null(corteza:::.remote_choose(ctx, "")$room)
    expect_identical(corteza:::.remote_choose(ctx, "existing room")$room, "existing room")
    ctx$config$remote_control <- list(room = "configured")
    expect_identical(corteza:::.remote_choose(ctx, "")$room, "configured")
    expect_identical(corteza:::.remote_choose(ctx, "explicit")$room, "explicit")
    ctx$config <- list()

    created <- list()
    fail_after_create <- FALSE
    original_create <- chat.api::chat_channel_create
    on.exit(utils::assignInNamespace("chat_channel_create", original_create, "chat.api"), add = TRUE)
    # Match the real mx_room_create arguments, including immutable create
    # content. Loopback does not itself persist adapter-specific arguments.
    utils::assignInNamespace("chat_channel_create", function(client, name, topic,
        visibility, preset, invite, creation_content) {
        created[[length(created) + 1L]] <<- list(name = name, topic = topic,
            visibility = visibility, preset = preset, invite = invite)
        room <- original_create(client, name)
        chat.api::chat_set_state(client, room, "m.room.create", creation_content)
        if (fail_after_create) stop("connection lost after creation")
        room
    }, "chat.api")
    attach <- function(id, room = NULL) {
        dir <- corteza:::.remote_request(cfg$user_id, room, cfg$operators,
            session_id = id, cwd = root)
        corteza:::.remote_bot_pump(cfg, cl, sessions)
        list(dir = dir, accepted = corteza:::.remote_read_file(file.path(dir, "accepted.json")))
    }
    close <- function(x) corteza:::.remote_write(file.path(x$dir, "closed.json"), list(closed = TRUE))
    first <- attach("first_session")
    second <- attach("second_session")
    expect_identical(length(created), 2L)
    expect_true(first$accepted$managed)
    expect_identical(first$accepted$session_id, "first_session")
    expect_true(first$accepted$room != second$accepted$room)
    expect_identical(created[[1L]]$visibility, "private")
    expect_identical(created[[1L]]$preset, "private_chat")
    expect_identical(created[[1L]]$invite, character())
    expect_true(startsWith(created[[1L]]$topic, root))
    expect_identical(cl$env$invites[[1L]]$user, cfg$operators)
    expect_identical(length(cl$env$invites), 2L)
    duplicate <- attach("first_session")
    expect_true(grepl("already controls", duplicate$accepted$error))
    expect_identical(length(created), 2L)
    close(first)
    resumed <- attach("first_session")
    expect_identical(resumed$accepted$room, first$accepted$room)
    expect_identical(length(created), 2L)
    expect_identical(length(cl$env$invites), 2L)
    explicit <- attach("third_session", "existing-room")
    expect_identical(explicit$accepted$room, "existing-room")
    expect_false(explicit$accepted$managed)
    expect_identical(length(created), 2L)
    invalid <- attach(NULL)
    expect_true(grepl("durable CLI session ID", invalid$accepted$error))
    expect_identical(length(created), 2L)

    # A lost create response does not create a second room on retry.
    fail_after_create <- TRUE
    uncertain <- attach("uncertain_session")
    expect_true(grepl("connection lost", uncertain$accepted$error))
    fail_after_create <- FALSE
    recovered <- attach("uncertain_session")
    expect_identical(length(created), 3L)
    expect_identical(recovered$accepted$room, "Corteza: uncertain_session")

    # Offline session rooms and their threads never start a bot conversation.
    close(resumed)
    m <- list(id = "offline", channel = first$accepted$room,
        sender = cfg$operators, body = "continue", ts = Sys.time(), is_self = FALSE)
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    messages <- chat.api::chat_history(cl, m$channel)$messages
    expect_identical(length(messages), 1L)
    expect_true(grepl("disconnected", messages[[1L]]$body))
    m$thread <- "thread"
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    m$channel <- second$accepted$room
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    inbox <- corteza:::.remote_records(second$dir, "in")
    expect_identical(length(inbox), 1L)
    expect_identical(inbox[[1L]]$text, "continue")
    # Threads share the existing operator gate and approval/stop routing.
    m$id <- "thread-stranger"
    m$sender <- "@stranger:example"
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    expect_identical(length(corteza:::.remote_records(second$dir, "in")), 1L)
    m$sender <- cfg$operators
    approval <- corteza:::job_new_id()
    pending <- file.path(second$dir, "approvals", approval)
    corteza:::.remote_write(paste0(pending, ".request"), list(expires = as.numeric(Sys.time()) + 60))
    m$id <- "thread-approval"
    m$body <- paste("/approve", approval)
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    expect_true(corteza:::.remote_read_file(paste0(pending, ".answer"))$approved)
    m$id <- "thread-stop"
    m$body <- "/stop"
    expect_true(corteza:::.remote_bot_route(m, cfg, cl))
    expect_identical(corteza:::.remote_read_file(file.path(second$dir, "stop.json"))$id,
        "thread-stop")
    m$channel <- "existing-room"
    expect_false(corteza:::.remote_bot_route(m, cfg, cl))
    close(explicit)
    m$thread <- NULL
    expect_false(corteza:::.remote_bot_route(m, cfg, cl))

    # Exercise the real remote reader and common clear handler, including
    # durable session save/load and the subsequent destination selection.
    original_wait <- corteza:::.remote_wait
    on.exit(utils::assignInNamespace(".remote_wait", original_wait, "corteza"), add = TRUE)
    utils::assignInNamespace(".remote_wait", function(ctx, predicate, timeout = 60) {
        corteza:::.remote_bot_pump(cfg, cl, sessions)
        result <- predicate()
        corteza:::.remote_append(ctx$remote_dir, "in", list(id = "after-clear",
            text = "New task", sender = cfg$operators))
        result
    }, "corteza")
    ctx$session <- corteza::new_session("cli", provider = "ollama")
    ctx$session$history <- list(list(role = "user", content = "Previous task"))
    ctx$ws_enabled <- FALSE
    ctx$palette <- list(dim = "", reset = "")
    disk <- corteza:::session_new("ollama", "fixture", root)
    ctx$disk_session <- list(session = disk, sessionId = disk$sessionId)
    ctx$new_session_fn <- function() {
        s <- corteza:::session_new("ollama", "fixture", root)
        list(session = s, sessionId = s$sessionId)
    }
    original <- attach(disk$sessionId)
    ctx$remote_dir <- original$dir
    ctx$session$remote_dir <- original$dir
    ctx$remote_consumed <- character()
    corteza:::.remote_append(original$dir, "in", list(id = "clear",
        text = "/clear", sender = cfg$operators))
    out <- capture.output(input <- corteza:::.remote_input(ctx, "> "))
    expect_identical(input, "New task")
    expect_true(ctx$disk_session$sessionId != disk$sessionId)
    expect_identical(ctx$session$history, list())
    expect_true(file.exists(file.path(original$dir, "closed.json")))
    fresh_room <- corteza:::.remote_read_file(file.path(ctx$remote_dir, "accepted.json"))$room
    expect_true(fresh_room != original$accepted$room)
    old_output <- corteza:::.remote_records(original$dir, "out")
    expect_true(any(vapply(old_output, function(x) grepl(fresh_room, x$text, fixed = TRUE), TRUE)))
    expect_true(any(vapply(old_output, function(x) grepl("?via=example", x$text, fixed = TRUE), TRUE)))
    loaded <- corteza:::session_load(disk$sessionKey)
    expect_identical(loaded$sessionId, disk$sessionId)
    reattached <- attach(loaded$sessionId)
    expect_identical(reattached$accepted$room, original$accepted$room)
})

expect_identical(corteza:::.remote_room_link("!opaque", "@bot:example.org:8448"),
    "https://matrix.to/#/!opaque?via=example.org%3A8448")
expect_identical(corteza:::.remote_room_link("!opaque", "@bot:[2001:db8::1]:8448"),
    "https://matrix.to/#/!opaque?via=%5B2001%3Adb8%3A%3A1%5D%3A8448")

# A cold index discovers old bindings once. Ordinary bot traffic then reads
# no binding files; new rooms become visible without rebuilding that index.
local({
    root <- tempfile("remote-index-")
    old <- Sys.getenv("CORTEZA_REMOTE_DIR", unset = NA)
    Sys.setenv(CORTEZA_REMOTE_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_REMOTE_DIR") else Sys.setenv(CORTEZA_REMOTE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- list(user_id = "@bot:example", operators = "@owner:example")
    for (i in seq_len(40L)) {
        id <- paste0("session-", i)
        corteza:::.remote_write(corteza:::.remote_room_file(cfg$user_id, id),
            list(owner = cfg$user_id, session_id = id, room = paste0("!room-", i)))
    }
    original_read <- corteza:::.remote_read_file
    on.exit(utils::assignInNamespace(".remote_read_file", original_read, "corteza"), add = TRUE)
    reads <- 0L
    utils::assignInNamespace(".remote_read_file", function(path) {
        # Compare as the OS names them: Windows mixes the two slashes.
        norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
        if (identical(norm(dirname(path)), norm(file.path(root, "rooms")))) reads <<- reads + 1L
        original_read(path)
    }, "corteza")
    m <- list(channel = "!ordinary", id = "event")
    expect_false(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_identical(reads, 40L)
    reads <- 0L
    for (i in seq_len(5L)) expect_false(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_identical(reads, 0L)
    m$channel <- "!room-1"
    expect_true(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_identical(reads, 1L)
    fresh <- list(owner = cfg$user_id, session_id = "new", room = "!new", ready = TRUE)
    corteza:::.remote_room_write(corteza:::.remote_room_file(cfg$user_id, "new"), fresh)
    m$channel <- fresh$room
    expect_true(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_identical(reads, 2L)
    expect_false(corteza:::.remote_room_offline(m, list(user_id = "@other:example"), NULL))
    path <- corteza:::.remote_room_file(cfg$user_id, "new")
    unlink(path)
    expect_false(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_null(corteza:::.remote_room_index(cfg$user_id)[[m$channel]])
    corteza:::.remote_room_write(path, fresh)
    expect_true(corteza:::.remote_room_offline(m, cfg, NULL))
    # Simulate a reader failure after a successful write warmed the index.
    utils::assignInNamespace(".remote_read_file", function(file) {
        if (identical(file, path)) NULL else original_read(file)
    }, "corteza")
    expect_false(corteza:::.remote_room_offline(m, cfg, NULL))
    expect_null(corteza:::.remote_room_index(cfg$user_id)[[m$channel]])
    Sys.setenv(CORTEZA_REMOTE_DIR = file.path(root, "another-root"))
    expect_false(corteza:::.remote_room_offline(m, cfg, NULL))
})

# Test the real Matrix adapter's create -> encryption -> invite sequence,
# using only transport seams and synthetic credentials. No homeserver calls.
if (!requireNamespace("mx.client", quietly = TRUE)) exit_file("mx.client unavailable")
local({
    root <- tempfile("remote-encryption-")
    old <- Sys.getenv("CORTEZA_REMOTE_DIR", unset = NA)
    Sys.setenv(CORTEZA_REMOTE_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_REMOTE_DIR") else Sys.setenv(CORTEZA_REMOTE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    events <- character()
    fail_encryption <- TRUE
    cl <- chat.api::chat_matrix(mx = list(user_id = "@bot:example",
        server = "https://example.invalid", token = "fixture", device_id = "TEST"),
        e2ee = TRUE,
        .create = function(session, name, topic, visibility, preset, invite, creation_content) {
            expect_identical(session$server, "https://example.invalid")
            expect_identical(invite, character())
            expect_identical(visibility, "private")
            expect_identical(creation_content[["ai.cornball.corteza.remote"]]$session_id, "encrypted")
            events <<- c(events, "create")
            "!new:example"
        },
        .state = function(session, room_id, type, content, state_key) {
            expect_identical(type, "m.room.encryption")
            expect_identical(content$algorithm, "m.megolm.v1.aes-sha2")
            events <<- c(events, "encrypt")
            if (fail_encryption) stop("encryption unavailable")
            "$encryption"
        },
        .get_state = function(session, room_id, type, state_key) {
            expect_identical(type, "m.room.member")
            expect_identical(state_key, "@owner:example")
            NULL
        },
        .invite = function(session, room_id, user_id) {
            expect_identical(user_id, "@owner:example")
            events <<- c(events, "invite")
            invisible(TRUE)
        })
    request <- list(owner = "@bot:example", session_id = "encrypted", cwd = tempdir())
    expect_error(corteza:::.remote_room(request, list(e2ee = TRUE), cl, "@owner:example"),
        "encryption unavailable")
    expect_identical(events, c("create", "encrypt"))
    fail_encryption <- FALSE
    expect_identical(corteza:::.remote_room(request, list(e2ee = TRUE), cl, "@owner:example"),
        "!new:example")
    expect_identical(events, c("create", "encrypt", "encrypt", "invite"))
    expect_identical(corteza:::.remote_room(request, list(e2ee = TRUE), cl, "@owner:example"),
        "!new:example")
    expect_identical(events, c("create", "encrypt", "encrypt", "invite"))
    expect_error(corteza:::.remote_room(request, list(e2ee = TRUE), chat.api::chat_loopback(),
        "@owner:example"), "support encryption")
})

# A successful invite followed by a failed local write is recoverable even
# after the invitee joins. A 403 with no membership change remains an error.
local({
    root <- tempfile("remote-invite-")
    old <- Sys.getenv("CORTEZA_REMOTE_DIR", unset = NA)
    Sys.setenv(CORTEZA_REMOTE_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("CORTEZA_REMOTE_DIR") else Sys.setenv(CORTEZA_REMOTE_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    membership <- NULL
    reads <- invites <- creates <- 0L
    fail_record <- TRUE
    mode <- "join"
    cl <- chat.api::chat_matrix(mx = list(user_id = "@bot:example",
        server = "https://example.invalid", token = "fixture", device_id = "TEST"),
        .create = function(session, ...) {
            expect_identical(session$server, "https://example.invalid")
            creates <<- creates + 1L
            "!new:example"
        },
        .get_state = function(session, room_id, type, state_key) {
            expect_identical(room_id, "!new:example")
            expect_identical(type, "m.room.member")
            expect_identical(state_key, "@owner:example")
            reads <<- reads + 1L
            if (is.null(membership)) NULL else list(membership = membership)
        },
        .invite = function(session, room_id, user_id) {
            expect_identical(user_id, "@owner:example")
            invites <<- invites + 1L
            if (mode == "denied") stop("M_FORBIDDEN")
            membership <<- "join"
            if (mode == "race") stop("M_FORBIDDEN: already joined")
            invisible(TRUE)
        })
    original_write <- corteza:::.remote_write
    on.exit(utils::assignInNamespace(".remote_write", original_write, "corteza"), add = TRUE)
    utils::assignInNamespace(".remote_write", function(path, value) {
        if (fail_record && length(value$invited)) stop("disk full after invite")
        original_write(path, value)
    }, "corteza")
    request <- list(owner = "@bot:example", session_id = "invite", cwd = tempdir())
    expect_error(corteza:::.remote_room(request, list(), cl, "@owner:example"), "disk full after invite")
    expect_identical(membership, "join")
    expect_identical(c(creates, invites, reads), c(1L, 1L, 1L))
    fail_record <- FALSE
    expect_identical(corteza:::.remote_room(request, list(), cl, "@owner:example"), "!new:example")
    expect_identical(c(creates, invites, reads), c(1L, 1L, 2L))
    binding <- corteza:::.remote_read_file(corteza:::.remote_room_file(request$owner, request$session_id))
    expect_identical(unlist(binding$invited, use.names = FALSE), "@owner:example")
    membership <- "invite"
    corteza:::.remote_room_invite(cl, "!new:example", "@owner:example")
    expect_identical(invites, 1L)
    membership <- NULL
    mode <- "race"
    expect_silent(corteza:::.remote_room_invite(cl, "!new:example", "@owner:example"))
    expect_identical(invites, 2L)
    expect_identical(membership, "join")
    membership <- "ban"
    mode <- "denied"
    expect_error(corteza:::.remote_room_invite(cl, "!new:example", "@owner:example"), "M_FORBIDDEN")
    expect_identical(invites, 3L)
})
