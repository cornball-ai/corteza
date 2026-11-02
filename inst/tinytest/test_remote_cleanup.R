if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api unavailable")

local({
    root <- tempfile("remote-cleanup-")
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
    cfg <- list(user_id = "@courier:example", operators = "@owner:example")
    cl <- chat.api::chat_loopback()
    corteza:::.remote_advertise(cfg)
    ns <- asNamespace("corteza")
    originals <- mget(c(".remote_wait", ".remote_emit", "run_repl_loop"), ns)
    on.exit(for (name in names(originals)) utils::assignInNamespace(name, originals[[name]], "corteza"), add = TRUE)
    attached_dir <- NULL
    emitted <- 0L
    utils::assignInNamespace(".remote_wait", function(ctx, predicate, timeout = 60) {
        attached_dir <<- ctx$remote_dir
        corteza:::.remote_bot_pump(cfg, cl)
        predicate()
    }, "corteza")
    utils::assignInNamespace("run_repl_loop", function(...) invisible(NULL), "corteza")
    utils::assignInNamespace(".remote_emit", function(ctx, text, audit = NULL) {
        if (grepl("disconnected", text, fixed = TRUE)) {
            emitted <<- emitted + 1L
            expect_true(file.exists(file.path(ctx$remote_dir, "closed.json")))
            stop("fixture final notice failed")
        }
        originals[[".remote_emit"]](ctx, text, audit)
    }, "corteza")
    ctx <- new.env(parent = emptyenv())
    ctx$config <- list()
    ctx$cwd <- root
    ctx$session <- corteza::new_session("cli", provider = "ollama")
    ctx$read_input <- function(...) character()
    ctx$render_reply <- function(...) invisible(NULL)
    expect_message(capture.output(corteza:::.remote_run(ctx, "fixture-room")), "fixture final notice failed")
    expect_identical(emitted, 1L)
    expect_null(ctx$remote_dir)
    expect_true(file.exists(file.path(attached_dir, "closed.json")))
    expect_identical(length(corteza:::.remote_sessions(cfg$user_id)), 0L)
    retry <- corteza:::.remote_request(cfg$user_id, "fixture-room", cfg$operators)
    corteza:::.remote_bot_pump(cfg, cl)
    accepted <- corteza:::.remote_read_file(file.path(retry, "accepted.json"))
    expect_null(accepted$error)
    expect_identical(accepted$room, "fixture-room")
})
