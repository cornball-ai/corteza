# Temporary user folders for this file, however it is run (isolate.R):
# a room's directory is read from its state file now.
source("isolate.R", local = TRUE)

# Where a room works: the directory an operator bound it to with /cd
# (test_room_binding.R), else the bot's `cwd`, else a scratch folder
# under the session's temp directory. A topic names no directory since
# 0.8.1 (WP12).
#
# Up to 0.8.0 the last step was ~/<localpart>, created on first use. A
# test config for "@bot:ex" then left ~/bot in the home directory of the
# machine running the checks, which is what got 0.8.0 archived.

same_dir <- function(a, b) {
    identical(normalizePath(a, winslash = "/", mustWork = FALSE),
              normalizePath(b, winslash = "/", mustWork = FALSE))
}

# A run's directory is process state. No block here may leave it set.
corteza:::bot_run_cwd_set(NULL)

# --- The order: the bot's cwd, then the scratch folder ---
local({
    topic_dir <- tempfile("topic")
    bot_dir <- tempfile("botdir")
    dir.create(topic_dir)
    dir.create(bot_dir)
    on.exit(unlink(c(topic_dir, bot_dir), recursive = TRUE))
    cfg <- list(user_id = "@bot:ex", cwd = bot_dir)

    # 1. A topic names no directory, even one that exists: up to 0.8.0
    # the room worked there. Its description is what follows the path,
    # and the path is kept only to tell the room (bot_topic_notice()).
    topic <- paste0(topic_dir, " | d")
    expect_null(corteza:::bot_parse_topic(topic)$cwd)
    expect_identical(corteza:::bot_parse_topic(topic)$description, "d")
    expect_identical(corteza:::bot_topic_path(topic), topic_dir)

    # 2. The bot's cwd, for a room no operator bound and for none.
    expect_identical(corteza:::bot_room_cwd(cfg, NULL), bot_dir)
    expect_identical(corteza:::bot_room_cwd(cfg, "!unbound-room:ex"), bot_dir)

    # 3. The scratch folder, for a bot that was given no directory.
    bare <- list(user_id = "@bot:ex")
    scratch <- corteza:::bot_room_cwd(bare, NULL)
    expect_true(dir.exists(scratch))
    expect_true(same_dir(dirname(scratch), tempdir()))
    expect_identical(basename(scratch), "corteza-unbound")
    expect_true(corteza:::bot_is_scratch(scratch))
    expect_false(corteza:::bot_is_scratch(bot_dir))
    expect_false(corteza:::bot_is_scratch(NULL))
    expect_identical(corteza:::bot_room_cwd(bare, "!unbound-room:ex"), scratch)

    # One folder, whatever the bot is called. These were ~/bot, ~/agent
    # and one more per account name in a test config.
    expect_identical(corteza:::bot_default_cwd(list(user_id = "")), scratch)
    expect_identical(corteza:::bot_default_cwd(list()), scratch)
    expect_identical(
        corteza:::bot_default_cwd(list(user_id = "@courier:example")),
        scratch)
})

# --- A saved cwd that has gone missing is not used, and not made ---
local({
    absent <- tempfile("gone")
    cfg <- list(user_id = "@bot:ex", cwd = absent)
    expect_message(got <- corteza:::bot_default_cwd(cfg), "does not exist")
    expect_true(corteza:::bot_is_scratch(got))
    expect_false(file.exists(absent))
    # Said once, not on every room and every step. expect_silent() lets
    # a message through, so the second call is caught by hand.
    again <- tryCatch({
        corteza:::bot_default_cwd(cfg)
        "quiet"
    }, message = function(m) conditionMessage(m))
    expect_identical(again, "quiet")
})

# A saved value that is not a path counts as none.
for (bad in list(NA_character_, c("a", "b"), 3, "", list("x"))) {
    expect_true(corteza:::bot_is_scratch(
        corteza:::bot_default_cwd(list(cwd = bad))))
}

# --- The argument check ---
expect_null(corteza:::bot_check_cwd(NULL))
for (bad in list(NA_character_, c("a", "b"), 3, "", TRUE)) {
    expect_error(corteza:::bot_check_cwd(bad), "one directory path")
}
local({
    absent <- tempfile("no-such-dir")
    expect_error(corteza:::bot_check_cwd(absent), "does not create it")
    expect_false(file.exists(absent))

    # A file is not a directory.
    f <- tempfile("a-file")
    writeLines("x", f)
    on.exit(unlink(f))
    expect_error(corteza:::bot_check_cwd(f), "existing directory")

    d <- tempfile("is-a-dir")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    expect_true(same_dir(corteza:::bot_check_cwd(d), d))

    # A relative path comes back absolute: the process may change its
    # working directory after the bot has started.
    old <- setwd(dirname(d))
    on.exit(setwd(old), add = TRUE)
    got <- corteza:::bot_check_cwd(basename(d))
    expect_true(same_dir(got, d))
    expect_true(grepl("^(/|[A-Za-z]:)", got))
})

# --- The saved value is checked when a run starts ---
local({
    absent <- tempfile("no-such-dir")
    expect_error(corteza:::bot_check_saved_cwd(list(cwd = absent)),
                 "does not create it")
    expect_false(file.exists(absent))
    expect_silent(corteza:::bot_check_saved_cwd(list()))
    expect_silent(corteza:::bot_check_saved_cwd(list(cwd = "")))
    expect_silent(corteza:::bot_check_saved_cwd(list(cwd = tempdir())))
})

# --- The 4 exported functions take `cwd` last and check it first ---
# The check comes before anything asks for the transport, so these need
# no chat.api and reach no server.
local({
    absent <- tempfile("no-such-dir")
    expect_error(corteza::bot_poll(cwd = absent), "does not create it")
    expect_error(corteza::bot_run_init(cwd = absent), "does not create it")
    expect_error(corteza::bot_run(cwd = absent), "does not create it")
    expect_error(
        corteza::bot_configure("https://ex.invalid", "bot", "pw", "!r:ex",
                               cwd = absent),
        "does not create it")
    expect_false(file.exists(absent))

    # After everything 0.8.0 had, so a call written by position against
    # 0.8.0 still matches. The deprecated names mirror the functions
    # they forward to (test_deprecated.R), so they take it there as
    # well. Only `operators` (test_rooms_operators.R) and
    # `project_roots` (test_room_binding.R) come after it.
    for (name in c("bot_configure", "bot_poll", "bot_run_init", "bot_run",
                   "matrix_configure", "matrix_poll", "matrix_run_init",
                   "matrix_run")) {
        f <- formals(getExportedValue("corteza", name))
        expect_identical(utils::tail(setdiff(names(f),
                                             c("operators", "project_roots")), 1L),
                         "cwd", info = name)
        expect_null(f$cwd, info = name)
    }
    # And they forward it: the same check answers.
    expect_error(suppressWarnings(corteza::matrix_poll(cwd = absent)),
                 "does not create it")
    expect_error(suppressWarnings(corteza::matrix_run_init(cwd = absent)),
                 "does not create it")
    expect_error(suppressWarnings(corteza::matrix_run(cwd = absent)),
                 "does not create it")
    expect_error(suppressWarnings(
        corteza::matrix_configure("https://ex.invalid", "bot", "pw", "!r:ex",
                                  cwd = absent)),
        "does not create it")
    expect_false(file.exists(absent))
})

# --- A run's cwd wins over the saved one ---
local({
    saved <- tempfile("saved")
    given <- tempfile("given")
    dir.create(saved)
    dir.create(given)
    on.exit({
        corteza:::bot_run_cwd_set(NULL)
        unlink(c(saved, given), recursive = TRUE)
    })
    cfg <- list(user_id = "@bot:ex", cwd = saved)
    expect_identical(corteza:::bot_default_cwd(cfg), saved)
    corteza:::bot_run_cwd_set(given)
    expect_identical(corteza:::bot_default_cwd(cfg), given)
    expect_identical(corteza:::bot_default_cwd(list(user_id = "@bot:ex")),
                     given)
    # A room no operator bound takes it too; a bound room's own comes
    # first (test_room_binding.R).
    expect_identical(corteza:::bot_room_cwd(cfg, "!unbound-room:ex"), given)
    corteza:::bot_run_cwd_set(NULL)
    expect_identical(corteza:::bot_default_cwd(cfg), saved)
})

# --- The room is told it has no directory, and how to get one ---
local({
    cfg <- list(user_id = "@bot:ex", user = "bot")
    scratch <- corteza:::bot_default_cwd(cfg)
    sys <- corteza:::bot_default_system(cfg, cwd = scratch)
    expect_true(grepl("no project directory", sys, fixed = TRUE))
    expect_true(grepl("it writes no files, runs no commands", sys, fixed = TRUE))
    expect_true(grepl(scratch, sys, fixed = TRUE))
    expect_true(grepl("/cd", sys, fixed = TRUE))
    expect_true(grepl("project roots", sys, fixed = TRUE))
    expect_false(grepl("room topic", sys, fixed = TRUE))
    expect_false(grepl("Use this as your scope", sys, fixed = TRUE))

    d <- tempfile("proj")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE))
    sys <- corteza:::bot_default_system(cfg, cwd = d)
    expect_false(grepl("no project directory", sys, fixed = TRUE))
    expect_true(grepl(paste("Working directory:", d), sys, fixed = TRUE))
    expect_true(grepl("Use this as your scope", sys, fixed = TRUE))

    # No directory passed at all: neither text.
    sys <- corteza:::bot_default_system(cfg)
    expect_false(grepl("Working directory", sys, fixed = TRUE))
    expect_false(grepl("no project directory", sys, fixed = TRUE))
})

# --- A scratch folder that was cleaned away is back before a turn ---
# It is under the temp directory, which a cleaner may empty while the bot
# runs. Without the folder the turn would run wherever the service was
# started.
local({
    scratch <- corteza:::bot_scratch_cwd()
    unlink(scratch, recursive = TRUE)
    expect_false(dir.exists(scratch))
    seen <- NULL
    orig <- corteza::turn
    assignInNamespace("turn", function(prompt, session) {
        seen <<- getwd()
        list(reply = "ok")
    }, ns = "corteza")
    on.exit(assignInNamespace("turn", orig, ns = "corteza"))
    s <- new.env()
    s$cwd <- scratch
    before <- getwd()
    expect_identical(corteza:::bot_run_turn_in_cwd("hi", s), "ok")
    expect_true(dir.exists(scratch))
    expect_true(same_dir(seen, scratch))
    expect_true(same_dir(getwd(), before))
})

# --- A turn that cannot enter its directory does not run ---
# Run anyway, it would work wherever the service was started.
local({
    ran <- 0L
    orig <- corteza::turn
    assignInNamespace("turn", function(prompt, session) {
        ran <<- ran + 1L
        list(reply = "ok")
    }, ns = "corteza")
    on.exit(assignInNamespace("turn", orig, ns = "corteza"), add = TRUE)
    before <- getwd()
    s <- new.env()

    # A directory a person bound, gone since.
    s$cwd <- tempfile("gone-")
    reply <- corteza:::bot_run_turn_in_cwd("hi", s)
    expect_identical(ran, 0L)
    expect_true(grepl("cannot enter the working directory", reply,
                      fixed = TRUE))
    expect_true(grepl(basename(s$cwd), reply, fixed = TRUE))
    expect_true(grepl("Nothing ran.", reply, fixed = TRUE))
    expect_true(same_dir(getwd(), before))

    # A file where the scratch folder should be: it cannot be made.
    scratch <- corteza:::bot_scratch_path()
    unlink(scratch, recursive = TRUE)
    writeLines("in the way", scratch)
    on.exit(unlink(scratch), add = TRUE)
    s$cwd <- scratch
    reply <- corteza:::bot_run_turn_in_cwd("hi", s)
    expect_identical(ran, 0L)
    expect_true(grepl("cannot enter the working directory", reply,
                      fixed = TRUE))
    expect_false(dir.exists(scratch))
    expect_true(same_dir(getwd(), before))
    unlink(scratch)

    # No directory at all.
    s$cwd <- NULL
    expect_identical(corteza:::bot_run_turn_in_cwd("hi", s),
                     "(agent error: this room has no working directory, so nothing ran.)")
    expect_identical(ran, 0L)

    # A directory setwd() refuses. Unix only, and not as root, who may
    # enter it anyway.
    locked <- tempfile("locked-")
    dir.create(locked)
    on.exit({
        Sys.chmod(locked, "0700")
        unlink(locked, recursive = TRUE)
    }, add = TRUE)
    Sys.chmod(locked, "0000")
    refused <- .Platform$OS.type == "unix" &&
        inherits(tryCatch(setwd(locked), error = function(e) e), "error")
    setwd(before)
    if (refused) {
        s$cwd <- locked
        reply <- corteza:::bot_run_turn_in_cwd("hi", s)
        expect_identical(ran, 0L)
        expect_true(grepl("cannot enter the working directory", reply,
                          fixed = TRUE))
        expect_true(same_dir(getwd(), before))
    }

    # The directory exists again: the turn runs there.
    s$cwd <- scratch
    expect_identical(corteza:::bot_run_turn_in_cwd("hi", s), "ok")
    expect_identical(ran, 1L)
    expect_true(dir.exists(scratch))
})

# --- The bot's own jobs settings ---
# From the project config of its cwd, over the global config. A bot with
# no directory has the global config alone: the scratch folder is not
# read for one, since a room's tools can write there.
local({
    d <- tempfile("botdir")
    dir.create(file.path(d, ".corteza"), recursive = TRUE)
    writeLines('{"jobs": {"max_workers": 3}}',
               file.path(d, ".corteza", "config.json"))
    scratch <- corteza:::bot_scratch_cwd()
    planted <- file.path(scratch, ".corteza")
    dir.create(planted, showWarnings = FALSE)
    writeLines('{"jobs": {"max_workers": 99}}',
               file.path(planted, "config.json"))
    # A project's config is read from a trusted directory only
    # (R/trust.R). The list of those is the user's, so this block gets a
    # config directory of its own before it adds the bot's directory.
    prev_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
    Sys.setenv(R_USER_CONFIG_DIR = tempfile("botdir-config-"))
    on.exit({
        unlink(c(d, planted, Sys.getenv("R_USER_CONFIG_DIR")), recursive = TRUE)
        if (is.na(prev_config)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = prev_config)
        }
    })

    # Untrusted, the bot's directory contributes nothing either.
    expect_null(corteza:::bot_jobs_config(list(cwd = d))$max_workers)
    corteza:::project_trust_add(d)
    expect_identical(corteza:::bot_jobs_config(list(cwd = d))$max_workers, 3L)
    global <- corteza:::load_config_file(
        corteza:::corteza_config_path("config.json"))$jobs
    expect_identical(corteza:::bot_jobs_config(list()), global)
    expect_false(identical(corteza:::bot_jobs_config(list())$max_workers,
                           99L))
})

# --- bot_run_init() keeps the directory for the loop ---
# No config at the explicit path, so no client is built and nothing is
# contacted. The state carries the directory for bot_run_step() to hand
# to bot_poll(), absolute.
if (requireNamespace("chat.api", quietly = TRUE) &&
    requireNamespace("mx.client", quietly = TRUE)) local({
    old_cfg <- Sys.getenv("CORTEZA_MATRIX_CONFIG", unset = NA)
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
    dir <- tempfile("corteza-cwd-")
    given <- file.path(dir, "given")
    dir.create(given, recursive = TRUE)
    Sys.setenv(CORTEZA_MATRIX_CONFIG = file.path(dir, "absent.json"),
               CORTEZA_STATE_DIR = dir)
    on.exit({
        corteza:::bot_run_cwd_set(NULL)
        if (is.na(old_cfg)) Sys.unsetenv("CORTEZA_MATRIX_CONFIG") else
            Sys.setenv(CORTEZA_MATRIX_CONFIG = old_cfg)
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(dir, recursive = TRUE)
    }, add = TRUE)
    state <- corteza::bot_run_init(cwd = given)
    expect_true(same_dir(state$opts$cwd, given))
    expect_true(same_dir(corteza:::bot_run_cwd(), given))
    expect_identical(corteza:::bot_default_cwd(list(user_id = "@bot:ex")),
                     state$opts$cwd)
    # The next run without one clears it.
    expect_null(corteza::bot_run_init()$opts$cwd)
    expect_null(corteza:::bot_run_cwd())
})

# --- Nothing appears in an empty home ---
# A fresh process, because R resolves "~" once and keeps the answer.
# Last in the file: where the home directory cannot be redirected, the
# rest of the file is skipped.
local({
    home <- tempfile("home")
    dir.create(home)
    on.exit(unlink(home, recursive = TRUE))
    probe <- function() {
        in_temp <- function(path) {
            identical(normalizePath(dirname(path), winslash = "/"),
                      normalizePath(tempdir(), winslash = "/"))
        }
        cfg <- list(user_id = "@bot:ex")
        dirs <- list(
            default = corteza:::bot_default_cwd(cfg),
            no_topic = corteza:::bot_room_cwd(cfg, NULL),
            described = corteza:::bot_room_cwd(cfg, "a description"),
            missing = suppressMessages(
                corteza:::bot_room_cwd(cfg, "/nonexistent/xyzzy | d")),
            nameless = corteza:::bot_default_cwd(list(user_id = "")),
            courier = corteza:::bot_default_cwd(
                list(user_id = "@courier:example")))
        list(home = path.expand("~"),
             in_temp = vapply(dirs, in_temp, logical(1)))
    }
    env <- c(callr::rcmd_safe_env(), HOME = home, R_USER = home)
    got <- callr::r(probe, env = env)
    if (!same_dir(got$home, home)) {
        exit_file("cannot redirect the home directory on this platform")
    }
    expect_identical(list.files(home, all.files = TRUE, no.. = TRUE),
                     character())
    expect_true(all(got$in_temp), info = paste(names(got$in_temp)[!got$in_temp],
                                               collapse = ", "))

    # A directory at ~/<localpart> is not the default any more, even on a
    # machine where an earlier version made one.
    dir.create(file.path(home, "bot"))
    again <- callr::r(probe, env = env)
    expect_true(all(again$in_temp))
    expect_identical(list.files(home, all.files = TRUE, no.. = TRUE), "bot")
    expect_identical(list.files(file.path(home, "bot"), all.files = TRUE,
                                no.. = TRUE), character())
})
