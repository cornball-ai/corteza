library(tinytest)

# Retention (R/retention.R): what goes once a session has gone unused,
# what stays, when the daily run is due, and what a person is told.
# Offline. Every fixture is under tempdir(): corteza's data and config
# directories are pointed there for the length of a block and put back
# after it, so nothing here can read or remove a real session.
#
# Time is passed in, not waited for. Fixtures are written now and the
# clock handed to the code is 100 days on, so everything is old unless a
# test dates one of its files a day before that clock. Only files are
# dated that way: not every platform sets the time of a folder.

DAY <- 24 * 60 * 60

# This process's own note of when it last looked at the stamp file.
clock_reset <- function() {
    env <- corteza:::.retention
    if (exists("looked", envir = env, inherits = FALSE)) {
        rm(list = "looked", envir = env)
    }
}

retention_fixture <- function() {
    root <- tempfile("corteza_retention_")
    dir.create(root)
    vars <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"))
    clock_reset()
    list(root = root, data = corteza:::corteza_data_dir(),
         config = corteza:::corteza_config_dir(),
         later = Sys.time() + 100 * DAY,
         restore = function() {
             for (v in vars) {
                 if (is.na(old[[v]])) {
                     Sys.unsetenv(v)
                 } else {
                     args <- list(old[[v]])
                     names(args) <- v
                     do.call(Sys.setenv, args)
                 }
             }
             clock_reset()
             unlink(root, recursive = TRUE)
         })
}

# One session's files in an agent's folder. `journal` names what goes in
# its journal folder; "entries" and "audit" become folders holding one
# file.
make_session <- function(fx, id, agent = "main",
                         journal = c("live.json", "segment-0001.json",
                                     "entries"),
                         trace = TRUE, transcript = TRUE) {
    dir <- file.path(fx$data, "agents", agent, "sessions")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    p <- corteza:::retention_paths(dir, id)
    if (length(journal)) {
        dir.create(p$journal)
    }
    for (name in journal) {
        if (name %in% c("entries", "audit")) {
            dir.create(file.path(p$journal, name))
            writeLines("{}", file.path(p$journal, name, "one.json"))
        } else {
            writeLines("{}", file.path(p$journal, name))
        }
    }
    if (trace) {
        writeLines("{}", p$trace)
    }
    if (transcript) {
        writeLines("{}", p$transcript)
    }
    p
}

# Everything under the data directory, as paths relative to it.
tree <- function(fx) {
    sort(list.files(fx$data, recursive = TRUE, all.files = TRUE,
                    include.dirs = TRUE, no.. = TRUE))
}

# Is each path one of `roots`, or under one?
under <- function(paths, roots) {
    vapply(paths, function(p) {
        any(p == roots | startsWith(p, paste0(roots, "/")))
    }, logical(1), USE.NAMES = FALSE)
}

used <- function(path, when) {
    stopifnot(isTRUE(Sys.setFileTime(path, when)))
}

set_config <- function(fx, json) {
    dir.create(fx$config, recursive = TRUE, showWarnings = FALSE)
    writeLines(json, file.path(fx$config, "config.json"))
}

# ---- What goes, what stays ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    # Its transcript was written yesterday: the trace beside it stays.
    talked <- make_session(fx, "talked")
    used(talked$transcript, fx$later - DAY)
    # Its live snapshot was written yesterday.
    journaled <- make_session(fx, "journaled")
    used(file.path(journaled$journal, "live.json"), fx$later - DAY)
    make_session(fx, "traced", journal = character())
    # A room's journal: no trace, no transcript beside it.
    room <- make_session(fx, "5f2c", agent = "matrix", trace = FALSE,
                         transcript = FALSE)
    # The holder of an archived turn.
    holder <- make_session(fx, "abc", agent = "subagent-abc", trace = FALSE)
    # Audit records live in the journal folder.
    audited <- make_session(fx, "audited",
                            journal = c("live.json", "segment-0001.json",
                                        "live.json.prev", "audit"))
    quiet <- make_session(fx, "quiet", journal = "audit", trace = FALSE)
    dir <- dirname(old$trace)
    writeLines("{}", file.path(dir, "sessions.json"))
    writeLines("x", file.path(dir, "old_workspace.rds"))
    writeLines("{}", file.path(dir, "old_workspace.json"))
    before <- tree(fx)

    # A dry run lists them and removes nothing.
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later)
    expect_identical(tree(fx), before)
    expect_false(any(found$removed))
    expect_identical(sort(paste(found$agent, found$session, found$kind)),
                     sort(c("main old journal", "main old trace",
                            "main traced trace", "matrix 5f2c journal",
                            "subagent-abc abc journal",
                            "main audited journal", "main audited trace")))
    expect_true(all(found$bytes > 0))
    expect_true(all(found$idle_days > 99 & found$idle_days < 101))

    # The real run removes exactly those.
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(nrow(found), 7L)
    expect_true(all(found$removed))
    gone <- c("agents/main/sessions/old.journal",
              "agents/main/sessions/old_trace.jsonl",
              "agents/main/sessions/traced_trace.jsonl",
              "agents/matrix/sessions/5f2c.journal",
              "agents/subagent-abc/sessions/abc.journal",
              "agents/main/sessions/audited.journal/live.json",
              "agents/main/sessions/audited.journal/live.json.prev",
              "agents/main/sessions/audited.journal/segment-0001.json",
              "agents/main/sessions/audited_trace.jsonl")
    expect_true(all(gone %in% before))
    expect_identical(tree(fx), before[!under(before, gone)])

    # Said once more by name. A transcript always stays, a holder's too,
    # and no agent's folder goes whole.
    expect_true(file.exists(old$transcript))
    expect_true(file.exists(holder$transcript))
    expect_true(dir.exists(file.path(fx$data, "agents", "matrix", "sessions")))
    # Audit records stay, and the folder that holds them.
    expect_true(file.exists(file.path(audited$journal, "audit", "one.json")))
    expect_true(file.exists(file.path(quiet$journal, "audit", "one.json")))
    # A session in use keeps everything.
    expect_true(file.exists(talked$trace))
    expect_true(file.exists(file.path(talked$journal, "segment-0001.json")))
    expect_true(file.exists(journaled$trace))

    # A folder that holds only audit records is no journal: the next run
    # has nothing to do.
    again <- corteza:::retention_prune(90, now = fx$later + DAY)
    expect_identical(nrow(again), 0L)
})

# ---- The number of days ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    # 100 days unused. 101 keeps it, 0 keeps everything.
    expect_identical(nrow(corteza:::retention_prune(101, now = fx$later)), 0L)
    expect_identical(nrow(corteza:::retention_prune(0, now = fx$later)), 0L)
    expect_true(file.exists(old$trace))
    expect_identical(nrow(corteza:::retention_prune(99, now = fx$later)), 2L)
    expect_false(file.exists(old$trace))
})

# ---- A session the caller has open ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    mine <- make_session(fx, "mine")
    other <- make_session(fx, "other")
    found <- corteza:::retention_prune(90, keep = "mine", now = fx$later)
    expect_identical(unique(found$session), "other")
    expect_true(file.exists(file.path(mine$journal, "live.json")))
    expect_true(file.exists(mine$trace))
    expect_false(dir.exists(other$journal))
})

# ---- A session written to between the scan and the removal ----
# Another process may hold it. The scan is replayed here as it was
# before the session was used again.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    stale <- corteza:::retention_scan(90, now = fx$later)
    expect_identical(nrow(stale), 2L)
    used(old$transcript, fx$later - DAY)
    real <- corteza:::retention_scan
    assignInNamespace("retention_scan", function(days, now = Sys.time()) stale,
                      ns = "corteza")
    on.exit(assignInNamespace("retention_scan", real, ns = "corteza"),
            add = TRUE, after = FALSE)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(nrow(found), 2L)
    expect_false(any(found$removed))
    expect_true(file.exists(file.path(old$journal, "live.json")))
    expect_true(file.exists(old$trace))
    expect_true(grepl("2 left in place.",
                      corteza:::retention_summary(found, 90, FALSE),
                      fixed = TRUE))
})

# ---- Links ----
# A link is never removed, and nothing behind one is.
if (.Platform$OS.type != "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    elsewhere <- file.path(fx$root, "elsewhere")
    dir.create(elsewhere)
    writeLines("{}", file.path(elsewhere, "live.json"))
    writeLines("{}", file.path(elsewhere, "trace.jsonl"))
    inner <- make_session(fx, "inner")
    dir <- dirname(inner$trace)
    file.symlink(elsewhere, file.path(dir, "linked.journal"))
    file.symlink(file.path(elsewhere, "trace.jsonl"),
                 file.path(dir, "linked_trace.jsonl"))
    # And a link inside a journal that does go.
    file.symlink(elsewhere, file.path(inner$journal, "entries", "out"))
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(unique(found$session), "inner")
    expect_true(all(found$removed))
    expect_false(dir.exists(inner$journal))
    expect_true(nzchar(Sys.readlink(file.path(dir, "linked.journal"))))
    expect_true(nzchar(Sys.readlink(file.path(dir, "linked_trace.jsonl"))))
    expect_identical(sort(list.files(elsewhere)),
                     c("live.json", "trace.jsonl"))
})

# ---- A name is a name ----
# unlink() reads its argument as a pattern unless told otherwise, and
# "a[12]" as a pattern is the session "a1".
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    odd <- make_session(fx, "a[12]")
    a1 <- make_session(fx, "a1")
    used(a1$transcript, fx$later - DAY)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(unique(found$session), "a[12]")
    expect_true(all(found$removed))
    expect_false(dir.exists(odd$journal))
    expect_false(file.exists(odd$trace))
    expect_true(file.exists(file.path(a1$journal, "live.json")))
    expect_true(file.exists(a1$trace))
})

# ---- sessions_prune() ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    # Nothing there: it says so, returns the empty table and makes nothing.
    expect_message(found <- corteza::sessions_prune(dry_run = TRUE, days = 90),
                   "No session has gone unused for 90 days.", fixed = TRUE)
    expect_identical(names(found), c("agent", "session", "kind", "path",
                                     "last_use", "idle_days", "bytes",
                                     "removed"))
    expect_identical(nrow(found), 0L)
    expect_false(dir.exists(fx$data))
    expect_message(corteza::sessions_prune(days = 0), "Retention is off")
    expect_message(corteza::sessions_prune(days = 1), "unused for 1 day.",
                   fixed = TRUE)
    # `dry_run` that is neither TRUE nor FALSE must not come out as a
    # removal.
    expect_error(corteza::sessions_prune(dry_run = "yes"), "TRUE or FALSE")
    expect_error(corteza::sessions_prune(dry_run = NA), "TRUE or FALSE")
    expect_error(corteza::sessions_prune(dry_run = c(TRUE, TRUE)),
                 "TRUE or FALSE")
    expect_error(corteza::sessions_prune(days = -1), "non-negative")
    expect_error(corteza::sessions_prune(days = "90"), "non-negative")
    expect_error(corteza::sessions_prune(days = c(1, 2)), "non-negative")
    expect_error(corteza::sessions_prune(days = NA_real_), "non-negative")
    # With no `days` the user's config decides.
    set_config(fx, '{"retention": {"session_days": 0}}')
    expect_message(corteza::sessions_prune(), "Retention is off")
    set_config(fx, '{"retention": {"session_days": 30}}')
    expect_message(corteza::sessions_prune(), "unused for 30 days.",
                   fixed = TRUE)
})

# Against the real clock, with files dated 100 days back. A platform that
# cannot date a folder skips the block; a Unix that cannot fails.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    new <- make_session(fx, "new")
    paths <- c(old$journal, old$trace, old$transcript,
               list.files(old$journal, full.names = TRUE, recursive = TRUE,
                          include.dirs = TRUE))
    dated <- all(Sys.setFileTime(paths, Sys.time() - 100 * DAY)) &&
        all(file.info(paths)$mtime < Sys.time() - 99 * DAY)
    expect_true(dated || .Platform$OS.type == "windows")
    if (dated) {
        expect_message(found <- corteza::sessions_prune(dry_run = TRUE,
                                                        days = 90),
                       "Would remove 1 journal and 1 trace of sessions unused for 90 days or more",
                       fixed = TRUE)
        expect_false(any(found$removed))
        expect_true(file.exists(old$trace))
        expect_message(found <- corteza::sessions_prune(days = 90),
                       "Removed 1 journal and 1 trace of sessions unused for 90 days or more",
                       fixed = TRUE)
        expect_true(all(found$removed))
        expect_false(dir.exists(old$journal))
        expect_false(file.exists(old$trace))
        expect_true(file.exists(old$transcript))
        expect_true(file.exists(file.path(new$journal, "live.json")))
        expect_true(file.exists(new$trace))
    }
})

# ---- The setting ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    days <- function() corteza:::retention_config()$session_days
    expect_equal(days(), 90)
    set_config(fx, "{}")
    expect_equal(days(), 90)
    set_config(fx, '{"retention": {}}')
    expect_equal(days(), 90)
    set_config(fx, '{"retention": {"session_days": null}}')
    expect_equal(days(), 90)
    set_config(fx, '{"retention": {"session_days": 30}}')
    expect_equal(days(), 30)
    set_config(fx, '{"retention": {"session_days": 0.5}}')
    expect_equal(days(), 0.5)
    # What cannot be read turns retention off. It is never read as a
    # shorter time than the person wrote.
    for (bad in c('{"retention": {"session_days": "soon"}}',
                  '{"retention": {"session_days": -5}}',
                  '{"retention": {"session_days": [30, 60]}}',
                  '{"retention": {"session_days": true}}',
                  '{"retention": 30}',
                  '{"retention": {"session_days": 0},,}',
                  "[1, 2]")) {
        set_config(fx, bad)
        expect_equal(days(), 0, info = bad)
    }
    # A project's config cannot set it. The merged config carries the
    # value, and retention does not read the merged config.
    set_config(fx, "{}")
    project <- file.path(fx$root, "project")
    dir.create(file.path(project, ".corteza"), recursive = TRUE)
    writeLines('{"retention": {"session_days": 1}}',
               file.path(project, ".corteza", "config.json"))
    expect_equal(corteza:::load_config(project)$retention$session_days, 1)
    owd <- setwd(project)
    on.exit(setwd(owd), add = TRUE, after = FALSE)
    expect_equal(days(), 90)
})

# ---- When a run is due ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    dir.create(fx$data, recursive = TRUE)
    stamp <- corteza:::retention_stamp_path()
    now <- Sys.time()
    expect_true(corteza:::retention_run_due(now))
    writeLines("x", stamp)
    used(stamp, now)
    expect_false(corteza:::retention_run_due(now + 60))
    expect_false(corteza:::retention_run_due(now + 23 * 60 * 60))
    expect_true(corteza:::retention_run_due(now + 25 * 60 * 60))
    # A stamp dated ahead, by a clock that was wrong once, does not put
    # retention off until that date.
    used(stamp, now + 10 * DAY)
    expect_true(corteza:::retention_run_due(now))
    # A process looks at the stamp once an hour.
    clock_reset()
    expect_true(corteza:::retention_look_due(now))
    expect_false(corteza:::retention_look_due(now + 59 * 60))
    expect_true(corteza:::retention_look_due(now + 61 * 60))
})

# ---- The daily run ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    # No data directory: nothing to remove, and none is made.
    expect_null(corteza:::retention_tick(now = fx$later))
    expect_false(dir.exists(fx$data))

    old <- make_session(fx, "old")
    mine <- make_session(fx, "mine")
    stamp <- corteza:::retention_stamp_path()
    clock_reset()
    line <- corteza:::retention_tick(keep = "mine", now = fx$later)
    expect_true(grepl("^Removed 1 journal and 1 trace of sessions unused for 90 days or more",
                      line))
    expect_false(dir.exists(old$journal))
    expect_true(file.exists(mine$trace))
    expect_true(file.exists(stamp))

    # Within the hour this process does not look, whatever is there.
    unlink(stamp)
    expect_null(corteza:::retention_tick(now = fx$later + 60))
    expect_false(file.exists(stamp))
    expect_true(file.exists(mine$trace))
    # After it, it does, and with no stamp a run is due.
    line <- corteza:::retention_tick(now = fx$later + 2 * 60 * 60)
    expect_true(grepl("^Removed 1 journal and 1 trace", line))
    expect_false(file.exists(mine$trace))
    expect_true(file.exists(stamp))
    # A stamp a minute old: no run, though a session is waiting to go.
    late <- make_session(fx, "late")
    used(stamp, fx$later - 60)
    clock_reset()
    expect_null(corteza:::retention_tick(now = fx$later))
    expect_true(file.exists(late$trace))
})

# It never raises, and a run that fails waits a day like any other.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    real <- corteza:::retention_prune
    assignInNamespace("retention_prune", function(...) stop("disk on fire"),
                      ns = "corteza")
    on.exit(assignInNamespace("retention_prune", real, ns = "corteza"),
            add = TRUE, after = FALSE)
    expect_null(corteza:::retention_tick(now = fx$later))
    expect_true(file.exists(corteza:::retention_stamp_path()))
    expect_true(file.exists(old$trace))
})

# Off in the config: no run, and no stamp either.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    set_config(fx, '{"retention": {"session_days": 0}}')
    expect_null(corteza:::retention_tick(now = fx$later))
    expect_false(file.exists(corteza:::retention_stamp_path()))
    expect_true(file.exists(old$trace))
})

# ---- A pruned session still resumes, and says what is gone ----
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    s <- corteza:::session_new(provider = "anthropic", model = "m",
                               cwd = tempdir(), session_key = "kept")
    corteza:::transcript_append(s, "user", "what is a fold?")
    corteza:::transcript_append(s, "assistant", "A reduction.")
    corteza:::trace_add(s$sessionId, "run_r", list(code = "1"), "[1] 1",
                        TRUE, 5)
    p <- corteza:::retention_paths(corteza:::sessions_dir(), s$sessionId)
    dir.create(p$journal)
    writeLines("{}", file.path(p$journal, "live.json"))
    expect_true(file.exists(p$trace))
    # Nothing to say while its journal and trace are there.
    expect_null(corteza:::retention_resume_note(s$sessionId, now = fx$later))

    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(sort(found$kind), c("journal", "trace"))
    expect_true(all(found$removed))

    back <- corteza:::session_load("kept")
    expect_identical(back$sessionId, s$sessionId)
    expect_identical(length(back$messages), 2L)
    expect_identical(back$messages[[1L]]$role, "user")
    note <- corteza:::retention_resume_note(s$sessionId, now = fx$later)
    expect_true(grepl("^Last used (99|100) days ago\\. Its journal and tool trace were kept for 90 days and are gone; the conversation is intact\\.$",
                      note))

    # Not for a session in recent use, an unknown one, or with
    # retention off.
    expect_null(corteza:::retention_resume_note(s$sessionId,
                                                now = Sys.time() + DAY))
    expect_null(corteza:::retention_resume_note("nobody", now = fx$later))
    set_config(fx, '{"retention": {"session_days": 0}}')
    expect_null(corteza:::retention_resume_note(s$sessionId, now = fx$later))
})

# ---- The terminal loop ----
# It runs retention where it starts, says what went, and leaves the
# session it has open. The loop reads the clock itself, so the files are
# dated back; the session has no journal of its own here, so only being
# named keeps its files.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    old <- make_session(fx, "old")
    mine <- make_session(fx, "mine")
    paths <- unlist(lapply(list(old, mine), function(p) {
        c(p$journal, p$trace, p$transcript,
          list.files(p$journal, full.names = TRUE, recursive = TRUE,
                     include.dirs = TRUE))
    }))
    dated <- all(Sys.setFileTime(paths, Sys.time() - 100 * DAY)) &&
        all(file.info(paths)$mtime < Sys.time() - 99 * DAY)
    expect_true(dated || .Platform$OS.type == "windows")
    if (dated) {
        ctx <- new.env(parent = emptyenv())
        ctx$ws_enabled <- FALSE
        ctx$palette <- list(dim = "<", reset = ">", cyan = "", bold = "",
                            yellow = "", green = "", bright_magenta = "",
                            red = "", magenta = "")
        ctx$read_input <- function(prompt_str) character(0)
        ctx$session <- new.env(parent = emptyenv())
        ctx$session$sessionId <- "mine"
        ctx$session$journal_enabled <- FALSE
        out <- capture.output(corteza:::run_repl_loop(ctx))
        expect_true(any(grepl("^<Removed 1 journal and 1 trace of sessions unused for 90 days or more .*>$",
                              out)))
        expect_false(dir.exists(old$journal))
        expect_false(file.exists(old$trace))
        expect_true(file.exists(file.path(mine$journal, "live.json")))
        expect_true(file.exists(mine$trace))
    }
})
