# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# Where a room works (R/room-binding.R; WP12, WP22a). An operator binds
# a room with /cd, inside one of the bot's project roots; a topic names
# no directory. The binding is in the room's state file, outlives a
# restart, and a move ends every conversation the room holds. Nothing
# here reaches a homeserver or a model: the sessions use ollama, which
# needs no key, and no turn runs.

ns <- asNamespace("corteza")
room <- "!room:example.invalid"
op <- "@human:example.invalid"
base_cfg <- function(...) {
    utils::modifyList(list(user = "bot", user_id = "@bot:example.invalid",
                           operators = op, provider = "ollama",
                           model = "fixture-model"),
                      list(...))
}
# Each block keeps the room states and jobs in a state folder of its
# own, and leaves no run roots or cwd set.
with_state <- function() {
    state <- tempfile("room-binding-state-")
    old <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    Sys.setenv(CORTEZA_STATE_DIR = state)
    function() {
        ns$bot_run_roots_set(NULL)
        ns$bot_run_cwd_set(NULL)
        if (is.na(old)) {
            Sys.unsetenv("CORTEZA_STATE_DIR")
        } else {
            Sys.setenv(CORTEZA_STATE_DIR = old)
        }
        unlink(state, recursive = TRUE)
    }
}
mkdir <- function(...) {
    d <- file.path(...)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    normalizePath(d, winslash = "/", mustWork = TRUE)
}
msg <- function(id, body, thread = NULL) {
    list(id = id, body = body, channel = room, thread = thread, ts = 1000)
}

# ---- The roots ---------------------------------------------------------------
local({
    top <- mkdir(tempfile("roots-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    a <- mkdir(top, "a")
    expect_null(ns$bot_check_roots(NULL))
    expect_null(ns$bot_check_roots(character()))
    expect_identical(ns$bot_check_roots(c(a, a)), a)
    absent <- file.path(top, "absent")
    expect_error(ns$bot_check_roots(absent), "does not create them")
    expect_false(file.exists(absent))
    expect_error(ns$bot_check_roots(c(a, NA)), "directory paths")
    expect_error(ns$bot_check_roots(3), "directory paths")

    # The 4 exported functions, and their deprecated names, take it last
    # and check it before anything asks for the transport.
    for (name in c("bot_configure", "bot_poll", "bot_run_init", "bot_run",
                   "matrix_configure", "matrix_poll", "matrix_run_init",
                   "matrix_run")) {
        f <- formals(getExportedValue("corteza", name))
        expect_identical(utils::tail(names(f), 1L), "project_roots", info = name)
        expect_null(f$project_roots, info = name)
    }
    expect_error(corteza::bot_poll(project_roots = absent), "does not create them")
    expect_error(corteza::bot_run_init(project_roots = absent),
                 "does not create them")
    expect_error(corteza::bot_configure("https://ex.invalid", "bot", "pw", "!r:ex",
                                        project_roots = absent),
                 "does not create them")
    expect_false(file.exists(absent))
})

local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("roots-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    saved <- mkdir(top, "saved")
    given <- mkdir(top, "given")
    gone <- file.path(top, "gone")
    # The run's roots stand over the saved ones.
    cfg <- base_cfg(project_roots = saved)
    expect_identical(ns$bot_project_roots(cfg), saved)
    ns$bot_run_roots_set(given)
    expect_identical(ns$bot_project_roots(cfg), given)
    ns$bot_run_roots_set(NULL)
    # A saved root that has gone is said once and left out.
    cfg <- base_cfg(project_roots = c(saved, gone))
    expect_message(got <- ns$bot_project_roots(cfg), "do not exist")
    expect_identical(got, saved)
    again <- tryCatch({
        ns$bot_project_roots(cfg)
        "quiet"
    }, message = function(m) conditionMessage(m))
    expect_identical(again, "quiet")
    expect_identical(ns$bot_project_roots(base_cfg()), character())
})

# ---- Inside a root, links resolved -------------------------------------------
local({
    top <- mkdir(tempfile("inside-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    proj <- mkdir(root, "proj")
    outside <- mkdir(top, "outside")
    roots <- ns$bot_check_roots(root)
    expect_identical(ns$bot_path_in_roots(proj, roots), proj)
    expect_identical(ns$bot_path_in_roots(root, roots), root)
    expect_null(ns$bot_path_in_roots(outside, roots))
    expect_null(ns$bot_path_in_roots(file.path(root, "absent"), roots))
    expect_null(ns$bot_path_in_roots(proj, character()))
    # A name that only starts like the root is not inside it.
    expect_null(ns$bot_path_in_roots(mkdir(top, "rootless"), roots))
    if (.Platform$OS.type != "windows") {
        # A link inside the root that leads out of it is out.
        link_out <- file.path(root, "escape")
        file.symlink(outside, link_out)
        expect_null(ns$bot_path_in_roots(link_out, roots))
        # A root that is itself a link holds what lies under its target.
        link_root <- file.path(top, "root-link")
        file.symlink(root, link_root)
        linked <- ns$bot_check_roots(link_root)
        expect_identical(linked, root)
        expect_identical(ns$bot_path_in_roots(file.path(link_root, "proj"), linked),
                         proj)
    }
    # A bare name is looked for under each root.
    other <- mkdir(top, "other")
    mkdir(other, "proj")
    expect_identical(ns$bot_root_path("proj", root)$path, file.path(root, "proj"))
    expect_true(grepl("more than one project root",
                      ns$bot_root_path("proj", c(root, other))$refusal, fixed = TRUE))
    expect_true(grepl("No directory nothing-here",
                      ns$bot_root_path("nothing-here", root)$refusal, fixed = TRUE))
})

# ---- What /cd asks for ---------------------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("binding-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    proj <- mkdir(root, "proj")
    outside <- mkdir(top, "outside")
    expect_identical(ns$bot_cd_binding(base_cfg(), "none"), list(state = "none"))
    expect_identical(ns$bot_cd_binding(base_cfg(), "DEFAULT"),
                     list(state = "default"))
    # No roots: refused, and the refusal names the key.
    expect_true(grepl("project_roots", ns$bot_cd_binding(base_cfg(), proj),
                      fixed = TRUE))
    cfg <- base_cfg(project_roots = root)
    expect_identical(ns$bot_cd_binding(cfg, proj), list(state = "bound", dir = proj))
    expect_identical(ns$bot_cd_binding(cfg, "proj"), list(state = "bound", dir = proj))
    expect_true(grepl("outside every project root", ns$bot_cd_binding(cfg, outside),
                      fixed = TRUE))
    absent <- file.path(root, "absent")
    expect_true(grepl("no existing directory", ns$bot_cd_binding(cfg, absent),
                      fixed = TRUE))
    expect_false(file.exists(absent))
    if (.Platform$OS.type != "windows") {
        link_out <- file.path(root, "escape")
        file.symlink(outside, link_out)
        expect_true(grepl("outside every project root",
                          ns$bot_cd_binding(cfg, link_out), fixed = TRUE))
    }
})

# ---- A room's directory, from its state ----------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("cwd-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    proj <- mkdir(root, "proj")
    bot_dir <- mkdir(top, "bot")
    cfg <- base_cfg(project_roots = root, cwd = bot_dir)
    bind <- function(binding) {
        state <- ns$bot_room_state_read(cfg, room)
        state$binding <- binding
        ns$bot_room_state_write(cfg, room, state)
    }
    expect_identical(ns$bot_room_cwd(cfg, room), bot_dir)
    bind(list(state = "bound", dir = proj))
    expect_identical(ns$bot_room_cwd(cfg, room), proj)
    bind(list(state = "none"))
    expect_true(ns$bot_is_scratch(ns$bot_room_cwd(cfg, room)))
    # A bound directory outside the roots in force now, or gone, is no
    # directory: the room reads and talks only until an operator acts.
    bind(list(state = "bound", dir = proj))
    expect_message(got <- ns$bot_room_cwd(base_cfg(cwd = bot_dir), room),
                   "outside every project root")
    expect_true(ns$bot_is_scratch(got))
    unlink(proj, recursive = TRUE)
    expect_message(got <- ns$bot_room_cwd(cfg, room), "gone")
    expect_true(ns$bot_is_scratch(got))
    # The on_clear hook's room takes the binding of the room it came from.
    bind(list(state = "none"))
    expect_true(ns$bot_room_binding_copy(cfg, room, "!filed:example.invalid"))
    expect_identical(ns$bot_room_binding(cfg, "!filed:example.invalid"),
                     list(state = "none"))
    expect_false(ns$bot_room_binding_copy(cfg, "!unbound:example.invalid",
                                          "!other:example.invalid"))
})

# ---- A state file that cannot be read ---------------------------------------
# What it held may have been `/cd none`, so the room takes no directory,
# not the bot's default, until an operator runs /cd, and that holds
# across reads and restarts (Codex, #292 review).
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("unreadable-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    proj <- mkdir(root, "proj")
    bot_dir <- mkdir(top, "bot")
    cfg <- base_cfg(project_roots = root, cwd = bot_dir)
    path <- ns$bot_room_state_path(cfg, room)
    bind <- function(binding) {
        state <- ns$bot_room_state_read(cfg, room)
        state$binding <- binding
        ns$bot_room_state_write(cfg, room, state)
    }
    asides <- function() {
        list.files(dirname(path), pattern = "unreadable", full.names = TRUE)
    }
    for (b in list(list(state = "none"), list(state = "bound", dir = proj))) {
        unlink(asides())
        bind(b)
        writeLines("{ not json", path)
        expect_message(got <- ns$bot_room_cwd(cfg, room), "until an operator runs /cd")
        expect_true(ns$bot_is_scratch(got), info = b$state)
        # Copied aside whole, and a marker in its place, so the next
        # read, or a restart, finds the same.
        expect_identical(readLines(asides()), "{ not json")
        expect_identical(ns$bot_room_state_read(cfg, room)$binding$state,
                         "unknown")
        expect_silent(got <- ns$bot_room_cwd(cfg, room))
        expect_true(ns$bot_is_scratch(got), info = b$state)
        expect_length(asides(), 1L)
        expect_true(grepl("binding is unknown", ns$bot_cd_state_text(cfg, room),
                          fixed = TRUE))
    }
    # A binding there but malformed is unknown as well; only a room with
    # none on record takes the default.
    bind(list(state = "bound", dir = 5))
    expect_identical(ns$bot_room_binding(cfg, room), list(state = "unknown"))
    expect_true(ns$bot_is_scratch(ns$bot_room_cwd(cfg, room)))
    bind(list(state = "elsewhere"))
    expect_identical(ns$bot_room_binding(cfg, room), list(state = "unknown"))
    # An operator's /cd repairs it: the move writes a binding over it.
    move <- function(binding) {
        ns$bot_room_move(cfg, room, binding, room, list(id = "$cd", ts = 1000))
    }
    expect_false(is.null(move(list(state = "default"))))
    expect_identical(ns$bot_room_cwd(cfg, room), bot_dir)
    # Where the marker cannot be written the unreadable file stays, and
    # reads the same way: the room never falls back to the default.
    # (A folder's write bit means nothing to root or on Windows.)
    if (.Platform$OS.type == "unix") {
        writeLines("{ not json", path)
        unlink(asides())
        # Before the state folder is removed, whatever stops this block.
        on.exit(Sys.chmod(dirname(path), "0755"), add = TRUE, after = FALSE)
        Sys.chmod(dirname(path), "0555")
        if (file.access(dirname(path), 2) == -1) {
            expect_message(got <- ns$bot_room_cwd(cfg, room), "could not copy aside")
            expect_true(ns$bot_is_scratch(got))
            expect_identical(readLines(path), "{ not json")
            expect_message(got <- ns$bot_room_cwd(cfg, room), "could not copy aside")
            expect_true(ns$bot_is_scratch(got))
        }
        Sys.chmod(dirname(path), "0755")
        expect_true(ns$bot_is_scratch(ns$bot_room_cwd(cfg, room)))
        expect_identical(ns$bot_room_state_read(cfg, room)$binding$state,
                         "unknown")
    }
})

# ---- /cd in a room ---------------------------------------------------------------
# A room's sessions, built for real, and a registry the way bot_poll()
# keeps it. replace() does what bot_poll() does after a move.
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("cd-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    proj <- mkdir(root, "proj")
    next_proj <- mkdir(root, "next")
    bot_dir <- mkdir(top, "bot")
    cfg <- base_cfg(project_roots = root, cwd = bot_dir)
    sessions <- ns$bot_new_session_registry()
    thread <- "$thread-root"
    tkey <- ns$bot_session_key(room, thread)
    main <- ns$bot_get_or_create_session(sessions, room, cfg)
    side <- ns$bot_get_or_create_session(sessions, tkey, cfg, room_id = room)
    expect_identical(main$cwd, bot_dir)
    before <- c(main$conversation_id, side$conversation_id)
    # A doer that worked here before the move, as the session notes it.
    ns$job_worker_note(main, "doer", dir = ns$job_worker_dir(main))
    expect_true(ns$job_worker_in_place(main, "doer"))
    cd <- function(body, sender = op, thread = NULL) {
        ns$bot_cd_command(msg(paste0("$", gsub("[^a-z]", "", body)), body,
                              thread), sender, sessions, cfg)
    }
    replace <- function(move) {
        for (key in move$keys) {
            ns$bot_reset_session(sessions, key, cfg, "$ack", move$ack,
                                 ns$bot_room_moved_record(move$state, key),
                                 room_id = room)
        }
    }

    # /cd alone says where the room works and the 3 forms.
    shown <- cd("/cd")$ack
    expect_true(grepl("bot's default", shown, fixed = TRUE))
    expect_true(grepl("`/cd none`", shown, fixed = TRUE))
    expect_true(grepl(root, shown, fixed = TRUE))
    # Only an operator moves a room; nothing changes otherwise.
    refused <- cd(paste("/cd", proj), sender = "@someone:example.invalid")
    expect_false(refused$moved)
    expect_true(grepl("Only an operator", refused$ack, fixed = TRUE))
    expect_identical(ns$bot_room_binding(cfg, room), list(state = "default"))
    # Outside the roots: refused.
    expect_false(cd(paste("/cd", top))$moved)

    # An operator binds. Each conversation, the thread's too, starts again
    # in the new directory, and a restart goes on there.
    move <- cd(paste("/cd", proj))
    expect_true(move$moved)
    expect_true(grepl(proj, move$ack, fixed = TRUE))
    expect_identical(sort(move$keys), sort(c(room, tkey)))
    expect_identical(ns$bot_room_binding(cfg, room), list(state = "bound", dir = proj))
    replace(move)
    main2 <- get(room, envir = sessions)
    side2 <- get(tkey, envir = sessions)
    expect_identical(main2$cwd, proj)
    expect_identical(side2$cwd, proj)
    expect_false(any(c(main2$conversation_id, side2$conversation_id) %in% before))
    expect_identical(main2$conversation_record$since, move$state$main$since)
    # The note moved with the room's job state and no longer matches, so
    # the next job starts a doer in the new directory
    # (job_worker_activate()).
    expect_false(ns$job_worker_in_place(main2, "doer"))
    # A room with a directory is a talker by default (WP20b).
    expect_true(isTRUE(main2$talker))
    restarted <- ns$bot_get_or_create_session(ns$bot_new_session_registry(), room, cfg)
    expect_identical(restarted$cwd, proj)
    expect_identical(restarted$conversation_id, main2$conversation_id)
    # A thread first seen after the move starts at it.
    late <- ns$bot_get_or_create_session(sessions, ns$bot_session_key(room, "$late"),
                                         cfg, room_id = room)
    expect_identical(late$conversation_record$since, move$state$moved$id)
    expect_identical(late$cwd, proj)

    # /cd none leaves the room with no directory, though the bot has a cwd.
    move <- cd("/cd none")
    expect_true(move$moved)
    replace(move)
    expect_true(ns$bot_is_scratch(get(room, envir = sessions)$cwd))
    expect_true(isTRUE(get(room, envir = sessions)$unbound))
    expect_true(ns$bot_is_scratch(ns$bot_get_or_create_session(
        ns$bot_new_session_registry(), room, cfg)$cwd))
    # /cd default goes back to the bot's cwd.
    move <- cd("/cd default")
    replace(move)
    expect_identical(get(room, envir = sessions)$cwd, bot_dir)
    expect_null(ns$bot_room_state_read(cfg, room)$binding)

    # An open job in any of the room's conversations stops a move, and
    # the refusal names it and its thread.
    job <- ns$job_create("fixture", workspace = bot_dir,
                         owner = ns$bot_job_owner(cfg),
                         origin = list(session_key = tkey, room = room))
    held <- cd(paste("/cd", next_proj))
    expect_false(held$moved)
    expect_true(grepl(job, held$ack, fixed = TRUE))
    expect_true(grepl(paste("thread", thread), held$ack, fixed = TRUE))
    expect_null(ns$bot_room_state_read(cfg, room)$binding)
    ns$job_settle(job, "cancelled", error = "fixture")

    # A conversation the archive cannot keep, or a state that cannot be
    # written: refused, and nothing changed.
    saved <- get("bot_archive_session", envir = ns)
    assignInNamespace("bot_archive_session", function(...) list(durable = FALSE),
                      "corteza")
    lost <- tryCatch(suppressMessages(cd(paste("/cd", next_proj))),
                     finally = assignInNamespace("bot_archive_session", saved,
                                                 "corteza"))
    expect_false(lost$moved)
    expect_true(grepl("could not be archived", lost$ack, fixed = TRUE))
    expect_null(ns$bot_room_state_read(cfg, room)$binding)
    saved <- get("bot_room_state_write", envir = ns)
    assignInNamespace("bot_room_state_write", function(...) stop("disk full"),
                      "corteza")
    unsaved <- tryCatch(suppressMessages(cd(paste("/cd", next_proj))),
                        finally = assignInNamespace("bot_room_state_write", saved,
                                                    "corteza"))
    expect_false(unsaved$moved)
    expect_true(grepl("could not be saved", unsaved$ack, fixed = TRUE))
    expect_null(ns$bot_room_state_read(cfg, room)$binding)
})

# ---- /ls ---------------------------------------------------------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    top <- mkdir(tempfile("ls-"))
    on.exit(unlink(top, recursive = TRUE), add = TRUE)
    root <- mkdir(top, "root")
    for (i in 1:55) {
        mkdir(root, sprintf("p%02d", i))
    }
    mkdir(root, ".hidden")
    writeLines("x", file.path(root, "a-file"))
    cfg <- base_cfg(project_roots = root)
    expect_true(grepl("Only an operator", ns$bot_ls_command(cfg, "@someone:ex"),
                      fixed = TRUE))
    expect_true(grepl("project_roots", ns$bot_ls_command(base_cfg(), op),
                      fixed = TRUE))
    expect_true(grepl(root, ns$bot_ls_command(cfg, op), fixed = TRUE))
    listed <- ns$bot_ls_command(cfg, op, root)
    lines <- strsplit(listed, "\n", fixed = TRUE)[[1L]]
    expect_true("- p01" %in% lines)
    expect_true("- p50" %in% lines)
    expect_false("- p51" %in% lines)
    expect_true("and 5 more" %in% lines)
    expect_false("- .hidden" %in% lines)
    expect_false("- a-file" %in% lines)
    expect_true(grepl("no directory inside a project root",
                      ns$bot_ls_command(cfg, op, top), fixed = TRUE))
    expect_true(grepl("has no directories", ns$bot_ls_command(cfg, op, "p01"),
                      fixed = TRUE))
    expect_identical(ns$bot_parse_ls_command("/ls p01")$arg, "p01")
    expect_identical(ns$bot_parse_cd_command("/CD  ~/x")$arg, "~/x")
    expect_null(ns$bot_parse_cd_command("cd ~/x"))
    expect_null(ns$bot_parse_cd_command("/cdx"))
})

# ---- A topic that starts with a directory is told once -----------------------------
local({
    done <- with_state()
    on.exit(done(), add = TRUE)
    cfg <- base_cfg()
    s <- new.env()
    s$room_id <- room
    s$cwd <- ns$bot_scratch_cwd()
    expect_null(ns$bot_topic_notice(cfg, s))
    s$topic_path <- "~/old-project"
    note <- ns$bot_topic_notice(cfg, s)
    expect_true(grepl("~/old-project", note, fixed = TRUE))
    expect_true(grepl("does not set where a room works", note, fixed = TRUE))
    expect_true(grepl("`/cd ~/old-project`", note, fixed = TRUE))
    expect_true(grepl("reads and talks only", note, fixed = TRUE))
    ns$bot_topic_told(cfg, s)
    expect_null(ns$bot_topic_notice(cfg, s))
    # Kept in the room's state: a session after a restart is not told.
    s2 <- new.env()
    s2$room_id <- room
    s2$topic_path <- "~/old-project"
    s2$cwd <- ns$bot_scratch_cwd()
    expect_null(ns$bot_topic_notice(cfg, s2))
    # Another path is news.
    s2$topic_path <- "~/another"
    expect_false(is.null(ns$bot_topic_notice(cfg, s2)))
})
