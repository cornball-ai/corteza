local({
    if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api needed")
    f <- function(name) get(name, asNamespace("corteza"))
    root <- tempfile("room-media-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    path <- file.path(root, "plot.png")
    writeBin(as.raw(0:15), path)
    s <- corteza::new_session(channel = "matrix", provider = "anthropic",
                             approval_cb = function(...) TRUE)
    s$cwd <- root
    s$config <- list()
    s$room_id <- "fixture-room"
    s$context_archive_vault <- root
    s$context_conversation <- TRUE
    s$seen_event_ids <- character()
    s$history <- list(list(role = "user", content = "Post the PNG here."))
    f("journal_install")(s, file.path(root, "journal"))
    cfg <- list(operators = "human")
    chat <- chat.api::chat_loopback()
    current <- chat
    f(".room_media_bind")(s, cfg, function() current, "human", "fixture-room")
    schemas <- f(".room_media_tools")(list(), s)
    expect_length(schemas, 1L)
    expect_identical(names(schemas[[1]]$input_schema$properties), "path")
    expect_identical(jsonlite::fromJSON(jsonlite::toJSON(schemas[[1]]$input_schema,
        auto_unbox = TRUE))$required, "path")
    expect_true(grepl('"required":\\["path"\\]', jsonlite::toJSON(
        schemas[[1]]$input_schema, auto_unbox = TRUE)))
    # Mock the public generic. R 4.6 may hold S3 registrations as functions
    # after another adapter test, which assignInNamespace cannot remap by name.
    old_send <- chat.api::chat_send
    sent <- list()
    fail <- FALSE
    assignInNamespace("chat_send", function(client, channel, text,
        files = NULL, ...) {
        expect_identical(client, current)
        expect_identical(channel, "fixture-room")
        expect_identical(text, "")
        expect_identical(files, path)
        if (fail) return(NULL)
        sent[[length(sent) + 1L]] <<- files
        "$image"
    }, "chat.api")
    on.exit(assignInNamespace("chat_send", old_send, "chat.api"), add = TRUE)
    # Through the real policy/observer handler, never the arbitrary executor.
    events <- list()
    s$on_tool <- list(function(e) events[[length(events) + 1L]] <<- e)
    handler <- f(".make_tool_handler")(s, function(...) stop("external executor"))
    current <- chat.api::chat_loopback()
    result <- handler("send_room_file", list(path = path))
    expect_true(grepl("Sent plot.png", result, fixed = TRUE))
    expect_length(sent, 1L)
    expect_true("$image" %in% s$seen_event_ids)
    expect_identical(s$transcript[[1]]$event_id, "$image")
    expect_true(tail(events, 1)[[1]]$success)
    expect_identical(f("journal_load")(s$journal_dir)$live$history, s$history)

    s$config <- list(permissions = list(send_room_file = "deny"))
    expect_true(grepl("denied", handler("send_room_file", list(path = path))))
    expect_length(sent, 1L)
    s$config <- list()
    s$plan_mode <- TRUE
    expect_length(f(".room_media_tools")(schemas, s), 0L)
    expect_true(grepl("denied", handler("send_room_file", list(path = path))))
    s$plan_mode <- FALSE
    f(".room_media_bind")(s, cfg, function() current, "stranger", "fixture-room")
    expect_null(s$send_room_file)
    expect_length(f(".room_media_tools")(schemas, s), 0L)
    f(".room_media_bind")(s, cfg, function() current, "human", "fixture-room", "$thread")
    expect_true(s$send_room_file(path)$isError)
    expect_length(sent, 1L)
    f(".room_media_bind")(s, cfg, function() current, "human", "fixture-room")
    credentials <- file.path(root, ".env")
    writeLines("TOKEN=fixture", credentials)
    expect_true(s$send_room_file(credentials)$isError)
    if (.Platform$OS.type != "windows") {
        link <- file.path(root, "alias.png")
        stopifnot(file.symlink(credentials, link))
        expect_true(s$send_room_file(link)$isError)
    }
    expect_true(s$send_room_file(root)$isError)
    expect_true(s$send_room_file(file.path(root, "missing"))$isError)
    expect_length(sent, 1L)
    # Credential paths remain human-gated even under permissive policy.
    route <- f("supervisor_route")(list(tool = "send_room_file",
        args = list(path = credentials)), list(approval = "allow"), cwd = root)
    expect_identical(route$route, "human")
    expect_true(any(grepl("credential", route$flags)))
    fail <- TRUE
    before <- s$transcript
    expect_true(s$send_room_file(path)$isError)
    expect_identical(s$transcript, before)
})
