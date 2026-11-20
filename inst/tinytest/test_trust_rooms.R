# Project trust in a room: the command, what `status` adds, and the one
# notice a room gets when its session loaded nothing from a directory
# that holds something to load. test_trust.R covers the decision itself.

local({
    ns <- asNamespace("corteza")
    root <- tempfile("trust-rooms-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    env_names <- c("R_USER_CONFIG_DIR", "R_USER_DATA_DIR", "R_USER_CACHE_DIR")
    old_env <- Sys.getenv(env_names, unset = NA_character_)
    on.exit({
        rm(list = ls(ns$.bot_trust_told), envir = ns$.bot_trust_told)
        for (name in env_names) {
            if (is.na(old_env[[name]])) {
                Sys.unsetenv(name)
            } else {
                do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
            }
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    do.call(Sys.setenv, as.list(setNames(file.path(root, env_names), env_names)))
    untrust <- function() unlink(ns$trust_file())

    proj <- file.path(root, "proj")
    dir.create(proj)
    writeLines("# Rules of this project", file.path(proj, "AGENTS.md"))
    room <- function(id = "!room:example", trusted = FALSE, cwd = proj) {
        list(room_id = id, cwd = cwd, project_trusted = trusted)
    }
    cfg <- list(operators = "@troy:example")

    # ---- The command, as a room hears it -------------------------------
    parse <- ns$bot_parse_trust_command
    expect_equal(parse("/trust"), "")
    expect_equal(parse("trust"), "")
    expect_equal(parse("  /trust   YES "), "yes")
    expect_equal(parse("trust yes"), "yes")
    expect_equal(parse("@bot:example: /trust yes"), "yes")
    # With the slash, one other word is still the command's, for its
    # usage line.
    expect_equal(parse("/trust please"), "please")
    # Without it, anything but the two forms is a thing said to the bot.
    expect_null(parse("trust me"))
    expect_null(parse("trust yes please"))
    expect_null(parse("/trust yes please"))
    expect_null(parse("I trust you"))
    expect_null(parse("trustworthy"))
    expect_null(parse(""))
    expect_null(parse(NULL))

    # ---- Who may use it, and on what ------------------------------------
    command <- ns$bot_trust_command
    expect_true(grepl("Only an operator",
                      command(cfg, room(), "@guest:example", "yes")))
    expect_true(grepl("Only an operator",
                      command(cfg, room(), "@guest:example", "")))
    # A bot with no operators has nobody who may.
    expect_true(grepl("Only an operator",
                      command(list(), room(), "@troy:example", "yes")))
    expect_false(ns$project_trusted(proj))
    # An operator is shown what the directory holds, and nothing changes.
    said <- command(cfg, room(), "@troy:example", "")
    expect_true(grepl("Not trusted", said, fixed = TRUE))
    expect_true(grepl("AGENTS.md", said, fixed = TRUE))
    expect_true(grepl("/clear", said, fixed = TRUE))
    expect_false(ns$project_trusted(proj))
    expect_true(grepl("usage", command(cfg, room(), "@troy:example", "please"),
                      fixed = TRUE))
    expect_false(ns$project_trusted(proj))
    # "yes" trusts the room's directory, from the next conversation.
    said <- command(cfg, room(), "@troy:example", "yes")
    expect_true(ns$project_trusted(proj))
    expect_true(grepl("Trusted", said, fixed = TRUE))
    expect_true(grepl("next conversation", said, fixed = TRUE))
    untrust()
    # A room in the scratch folder has no project.
    scratch <- ns$bot_scratch_cwd()
    for (s in list(room(cwd = scratch), list(room_id = "!none:example"))) {
        expect_true(grepl("no project directory",
                          command(cfg, s, "@troy:example", "yes")))
    }
    expect_false(file.exists(ns$trust_file()))

    # ---- The scratch folder is never a project ----------------------------
    # A room's tools write there, so a file found in it was put there by
    # a model. Listing it, or the temp directory above it, makes the
    # project beside it trusted and leaves the scratch folder as it was.
    writeLines(jsonlite::toJSON(c(dirname(scratch), scratch)), ns$trust_file())
    expect_true(ns$project_trusted(proj))
    expect_false(ns$project_trusted(scratch))
    inside <- file.path(scratch, "made-by-a-tool")
    dir.create(file.path(inside, ".corteza"), recursive = TRUE)
    writeLines('{"provider": "planted"}',
               file.path(inside, ".corteza", "config.json"))
    planted <- file.path(scratch, ".corteza")
    made <- !dir.exists(planted)
    dir.create(planted, showWarnings = FALSE)
    writeLines('{"provider": "planted"}', file.path(planted, "config.json"))
    expect_false(ns$project_trusted(inside))
    expect_false(identical(ns$load_config(scratch)$provider, "planted"))
    expect_false(identical(ns$load_config(inside)$provider, "planted"))
    unlink(c(inside, if (made) planted else file.path(planted, "config.json")),
           recursive = TRUE)
    untrust()

    # ---- What `status` adds ------------------------------------------------
    status <- ns$bot_trust_status
    expect_equal(status(room(cwd = scratch)), character())
    expect_equal(status(list(room_id = "!none:example")), character())
    lines <- status(room())
    expect_equal(lines[[1L]], "trusted: no (/trust)")
    expect_true(any(grepl("AGENTS.md", lines, fixed = TRUE)))
    # Trusted since the session started: it says which of the two holds.
    ns$project_trust_add(proj)
    expect_true(grepl("no for this session, yes from the next",
                      status(room())[[1L]], fixed = TRUE))
    expect_equal(status(room(trusted = TRUE)), "trusted: yes")
    untrust()

    # ---- The notice ---------------------------------------------------------
    notice <- ns$bot_trust_notice
    note <- notice(room())
    expect_true(grepl("Not trusted", note, fixed = TRUE))
    expect_true(grepl("AGENTS.md", note, fixed = TRUE))
    expect_true(grepl("Only an operator", note, fixed = TRUE))
    expect_true(grepl("/clear", note, fixed = TRUE))
    # It is owed until it is delivered: asking again gives it again.
    expect_identical(notice(room()), note)
    ns$bot_trust_told(room())
    expect_null(notice(room()))
    # A thread and a rebuilt session are the same room; another room is not.
    expect_null(notice(room()))
    expect_identical(notice(room("!other:example")), note)
    # Nothing to say: a session that loaded the project, a scratch room,
    # and a directory with nothing in it to load.
    expect_null(notice(room("!third:example", trusted = TRUE)))
    expect_null(notice(room("!fourth:example", cwd = scratch)))
    expect_null(notice(list(room_id = "!none:example")))
    # A scratch room has no project whatever a tool left in its folder:
    # /trust could not trust it, so the room is not told to.
    left <- file.path(scratch, "AGENTS.md")
    if (!file.exists(left)) {
        writeLines("# left by a tool", left)
        expect_null(notice(room("!fourth:example", cwd = scratch)))
        expect_equal(status(room(cwd = scratch)), character())
        unlink(left)
    }
    empty <- file.path(root, "empty")
    dir.create(empty)
    if (!ns$trust_in_git(empty)) {
        expect_null(notice(room("!fifth:example", cwd = empty)))
    }

    # ---- Lists, as a room renders them ---------------------------------------
    # The terminal indents a list by 2 spaces; a room renders a message as
    # Markdown, where the indent shows as nothing and the items run
    # together, so a room gets a Markdown list, set off by blank lines.
    md <- ns$bot_markdown_list
    expect_identical(md(c("Left alone:", "  its config", "  AGENTS.md",
                          "/trust yes")),
                     c("Left alone:", "", "- its config", "- AGENTS.md", "",
                       "/trust yes"))
    expect_identical(md(c("a", "  x")), c("a", "", "- x"))
    expect_identical(md(c("a", "b")), c("a", "b"))
    expect_identical(md(character()), character())
    expect_false(grepl("\n  ", note, fixed = TRUE))
    expect_true(grepl("\n- [^\n]*AGENTS.md", note))
    expect_true(grepl("\n- [^\n]*AGENTS.md",
                      command(cfg, room(), "@troy:example", "")))
    expect_true(any(startsWith(status(room()), "- ")))
    # Rendered as the Matrix client renders it: a list.
    if (requireNamespace("mx.client", quietly = TRUE) &&
        exists("mx_markdown_to_html", envir = asNamespace("mx.client"))) {
        html <- get("mx_markdown_to_html", envir = asNamespace("mx.client"))(note)
        expect_true(grepl("<ul><li>", html, fixed = TRUE))
    }
})

# ---- Through the poll loop -------------------------------------------------
# The command and the notice as a room meets them: bot_poll() with the
# Matrix client and the model replaced. The guard sits at top level,
# since exit_file() ends the file between expressions.
if (!requireNamespace("chat.api", quietly = TRUE)) exit_file("chat.api needed")
local({
    ns <- asNamespace("corteza")
    root <- tempfile("trust-rooms-poll-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    env_names <- c("R_USER_CONFIG_DIR", "R_USER_DATA_DIR", "R_USER_CACHE_DIR",
                   "CORTEZA_STATE_DIR")
    old_env <- Sys.getenv(env_names, unset = NA_character_)
    saved <- list()
    patch <- function(name, fn, package = "corteza") {
        saved[[length(saved) + 1L]] <<- list(name = name, package = package,
            fn = get(name, asNamespace(package)))
        utils::assignInNamespace(name, fn, package)
    }
    on.exit({
        for (x in rev(saved)) {
            utils::assignInNamespace(x$name, x$fn, x$package)
        }
        rm(list = ls(ns$.bot_trust_told), envir = ns$.bot_trust_told)
        for (name in env_names) {
            if (is.na(old_env[[name]])) {
                Sys.unsetenv(name)
            } else {
                do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
            }
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    do.call(Sys.setenv, as.list(setNames(file.path(root, env_names), env_names)))

    proj <- file.path(root, "proj")
    dir.create(proj)
    writeLines("# Rules of this project", file.path(proj, "AGENTS.md"))

    cfg <- list(user_id = "fixture-bot", operators = "@human:example",
                provider = "ollama")
    chat <- chat.api::chat_loopback()
    sessions <- ns$bot_new_session_registry()
    s <- corteza::new_session("matrix", provider = "ollama", system = "fixture")
    s$config <- list()
    s$room_id <- s$job_key <- "fixture-room"
    s$job_owner <- "fixture-bot"
    s$cwd <- proj
    s$project_trusted <- FALSE
    s$rehydrate_checked <- TRUE
    s$seen_event_ids <- character()
    assign(s$room_id, s, sessions)

    incoming <- NULL
    patch("bot_load_config", function(...) cfg)
    patch("bot_chat_client", function(...) chat)
    patch("chat_whoami", function(...) chat.api::chat_identity(cfg$user_id),
          "chat.api")
    patch("chat_poll", function(...) list(first_run = FALSE,
        messages = list(incoming), reactions = list(), invites = list()),
        "chat.api")
    patch("bot_should_respond", function(...) TRUE)
    sent <- character()
    # Whether each was sent as Markdown, which a room renders as such.
    marked <- logical()
    refuse <- 0L
    patch("bot_reply_send", function(chat, channel, text, ...) {
        if (refuse > 0L) {
            refuse <<- refuse - 1L
            stop("the homeserver is away")
        }
        sent <<- c(sent, text)
        marked <<- c(marked, isTRUE(list(...)$markdown))
        paste0("$sent", length(sent))
    })
    turns <- 0L
    patch("bot_run_turn_in_cwd", function(prompt, session) {
        turns <<- turns + 1L
        session$history <- c(session$history,
                             list(list(role = "user", content = prompt)),
                             list(list(role = "assistant", content = "A reply.")))
        "A reply."
    })
    n <- 0L
    say <- function(body, sender = "@human:example") {
        n <<- n + 1L
        incoming <<- chat.api::chat_message(id = paste0("$in", n),
            channel = s$room_id, sender = sender, body = body, ts = Sys.time())
        corteza::bot_poll(sessions = sessions, timeout = 0)
    }
    in_history <- function(text) {
        any(vapply(s$history, function(h) {
            any(grepl(text, unlist(h$content), fixed = TRUE))
        }, NA))
    }

    # The notice could not be sent: the turn still runs, and the notice
    # is still owed.
    refuse <- 1L
    expect_equal(say("hello"), 1L)
    expect_equal(turns, 1L)
    expect_equal(sent, "A reply.")
    # Sent ahead of the next reply, once, and not handed to the model.
    expect_equal(say("hello again"), 1L)
    expect_equal(turns, 2L)
    expect_equal(length(sent), 3L)
    expect_true(grepl("Not trusted", sent[[2L]], fixed = TRUE))
    expect_true(grepl("AGENTS.md", sent[[2L]], fixed = TRUE))
    # What it left alone goes out as a Markdown list.
    expect_true(grepl("\n- [^\n]*AGENTS.md", sent[[2L]]))
    expect_true(marked[[2L]])
    expect_equal(sent[[3L]], "A reply.")
    expect_false(in_history("Not trusted"))
    expect_true(any(vapply(s$transcript, function(x) {
        grepl("Not trusted", x$content, fixed = TRUE)
    }, NA)))
    expect_equal(say("and again"), 1L)
    expect_equal(length(sent), 4L)
    expect_equal(sent[[4L]], "A reply.")

    # The command is answered without a turn. Somebody who is not an
    # operator changes nothing.
    expect_equal(say("/trust yes", sender = "@guest:example"), 1L)
    expect_equal(turns, 3L)
    expect_true(grepl("Only an operator", sent[[5L]], fixed = TRUE))
    expect_false(ns$project_trusted(proj))
    expect_equal(say("/trust"), 1L)
    expect_true(grepl("Not trusted", sent[[6L]], fixed = TRUE))
    expect_true(grepl("\n- [^\n]*AGENTS.md", sent[[6L]]))
    expect_true(marked[[6L]])
    expect_false(ns$project_trusted(proj))
    expect_equal(say("trust yes"), 1L)
    expect_true(grepl("Trusted", sent[[7L]], fixed = TRUE))
    expect_true(grepl("/clear", sent[[7L]], fixed = TRUE))
    expect_true(ns$project_trusted(proj))
    expect_equal(turns, 3L)
    expect_false(in_history("Trusted"))
    # This conversation is as it started, and `status` says so.
    expect_equal(say("status"), 1L)
    expect_true(grepl("trusted: no for this session, yes from the next",
                      sent[[8L]], fixed = TRUE))
    expect_true(grepl("\n- [^\n]*AGENTS.md", sent[[8L]]))
    expect_true(marked[[8L]])
    expect_equal(turns, 3L)
    # "trust me" is not the command.
    expect_equal(say("trust me"), 1L)
    expect_equal(turns, 4L)
    expect_equal(sent[[9L]], "A reply.")
})
