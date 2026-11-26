# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

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

# The run also prunes corteza's state and remote-control folders
# (R/retention-state.R), and the clock here is 100 days on. Each is put
# where the data folder puts it, so a value set where these tests run (a
# bot's service environment) is never read.
retention_fixture <- function() {
    root <- tempfile("corteza_retention_")
    dir.create(root)
    vars <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "CORTEZA_STATE_DIR",
              "CORTEZA_REMOTE_DIR")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"))
    data <- corteza:::corteza_data_dir()
    Sys.setenv(CORTEZA_STATE_DIR = file.path(data, "state"),
               CORTEZA_REMOTE_DIR = file.path(data, "remote-control"))
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

# A symbolic link stands at `path`. Sys.readlink() gives NA for a path
# that does not exist, and nzchar(NA) is TRUE, so nzchar() alone passes
# for a link that was removed.
is_link <- function(path) {
    target <- Sys.readlink(path)
    !is.na(target) && nzchar(target)
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
    # A room's conversation waiting for the host (R/archive.R) stays,
    # however old, until the host takes it.
    pending <- file.path(fx$data, "state", "archive", "5f2c.pending.json")
    dir.create(dirname(pending), recursive = TRUE)
    writeLines("{}", pending)
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
    expect_true(file.exists(pending))
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

# ---- A room's kept attachments ----
# A chat room's session keeps the pictures people send it beside its
# journal, in `<id>.attachments/` (R/media.R). They go with its journal
# and trace once the session has gone unused, and only the files
# corteza named there.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    room <- make_session(fx, "room", agent = "matrix", transcript = FALSE)
    att <- sub("[.]journal$", ".attachments", room$journal)
    dir.create(att)
    kept <- file.path(att, "0123456789ab-cornball2.png")
    writeBin(as.raw(1:64), kept)
    mine <- file.path(att, "notes.txt")
    writeLines("a person's own file", mine)
    found <- corteza:::retention_scan(90, now = fx$later)
    expect_identical(sort(found$kind), c("attachments", "journal", "trace"))
    expect_equal(found$bytes[found$kind == "attachments"], 64)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_true(all(found$removed))
    expect_false(file.exists(kept))
    expect_true(file.exists(mine))
    expect_true(dir.exists(att))
    expect_true(grepl(paste("^Removed 1 journal, 1 trace and 1 folder of",
                            "attachments of sessions unused for 90 days"),
                      corteza:::retention_summary(found, 90, FALSE)))
})

# A picture kept the day before is a use of the session: nothing goes.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    room <- make_session(fx, "room", agent = "matrix", transcript = FALSE)
    att <- sub("[.]journal$", ".attachments", room$journal)
    dir.create(att)
    kept <- file.path(att, "0123456789ab-new.png")
    writeBin(as.raw(1:8), kept)
    used(kept, fx$later - DAY)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(nrow(found), 0L)
    expect_true(file.exists(kept))
    expect_true(file.exists(file.path(room$journal, "live.json")))
})

# A session with kept pictures and no journal, as with `journal_enabled:
# false`, still goes by their age, and its emptied folder with it.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    att <- file.path(fx$data, "agents", "matrix", "sessions",
                     "nojournal.attachments")
    dir.create(att, recursive = TRUE)
    kept <- file.path(att, "0123456789ab-a.png")
    writeBin(as.raw(1:8), kept)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(found$kind, "attachments")
    expect_true(found$removed)
    expect_false(dir.exists(att))
})

# A kept picture or a journal file that is a link was not made by
# corteza: it stays, and so does what it points at, wherever it is in
# the folder. The real files beside it go, each folder stays for its
# link, and no target is counted. Run twice, the second time as Windows
# sees links, where only the real path tells.
if (.Platform$OS.type != "windows") local({
    for (windows_like in c(FALSE, TRUE)) local({
        fx <- retention_fixture()
        on.exit(fx$restore(), add = TRUE)
        if (windows_like) {
            orig_link <- corteza:::retention_is_link
            assignInNamespace("retention_is_link", function(path) FALSE,
                              ns = "corteza")
            on.exit(assignInNamespace("retention_is_link", orig_link,
                                      ns = "corteza"), add = TRUE)
        }
        why <- if (windows_like) "as on Windows" else "with Sys.readlink()"
        room <- make_session(fx, "room", agent = "matrix", transcript = FALSE)
        att <- sub("[.]journal$", ".attachments", room$journal)
        dir.create(att)
        kept <- file.path(att, "0123456789ab-kept.png")
        writeBin(as.raw(1:8), kept)
        away <- file.path(fx$root, "away")
        dir.create(away)
        writeBin(as.raw(1:8), file.path(away, "picture.png"))
        writeLines("{}", file.path(away, "live.json"))
        link <- file.path(att, "0123456789ab-link.png")
        expect_true(file.symlink(file.path(away, "picture.png"), link),
                    info = why)
        live <- file.path(room$journal, "live.json")
        unlink(live)
        expect_true(file.symlink(file.path(away, "live.json"), live),
                    info = why)
        # One among the shared entries, the way an entry is named.
        writeBin(as.raw(seq_len(100) %% 256), file.path(away, "entry.json"))
        entry <- file.path(room$journal, "entries", "0123abcd.json")
        expect_true(file.symlink(file.path(away, "entry.json"), entry),
                    info = why)
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(sort(found$kind),
                         c("attachments", "journal", "trace"), info = why)
        # No link's target is counted: 8 bytes of the kept picture, and
        # 3 each of the segment and the real entry.
        expect_equal(found$bytes[found$kind == "attachments"], 8,
                     info = why)
        expect_equal(found$bytes[found$kind == "journal"], 6, info = why)
        expect_true(all(found$removed), info = why)
        expect_false(file.exists(kept), info = why)
        expect_false(file.exists(file.path(room$journal, "segment-0001.json")),
                     info = why)
        expect_false(file.exists(file.path(room$journal, "entries", "one.json")),
                     info = why)
        expect_true(is_link(link), info = why)
        expect_true(is_link(live), info = why)
        expect_true(is_link(entry), info = why)
        expect_identical(sort(list.files(away)),
                         c("entry.json", "live.json", "picture.png"),
                         info = why)
        expect_true(dir.exists(att), info = why)
        expect_true(dir.exists(file.path(room$journal, "entries")), info = why)
        expect_true(dir.exists(room$journal), info = why)
    })
})

# A folder whose only file of such a name is a link holds nothing of
# corteza's: retention neither counts it nor touches it.
if (.Platform$OS.type != "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    room <- make_session(fx, "room", agent = "matrix", transcript = FALSE)
    att <- sub("[.]journal$", ".attachments", room$journal)
    dir.create(att)
    target <- file.path(fx$root, "picture.png")
    writeBin(as.raw(1:8), target)
    link <- file.path(att, "0123456789ab-link.png")
    expect_true(file.symlink(target, link))
    expect_false("attachments" %in%
                 corteza:::retention_scan(90, now = fx$later)$kind)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(sort(found$kind), c("journal", "trace"))
    expect_true(is_link(link))
    expect_true(file.exists(target))
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
    # And a link inside the entries of a journal that does go: the
    # journal's own files go, and the link stays, with the folders it is
    # in.
    out <- file.path(inner$journal, "entries", "out")
    file.symlink(elsewhere, out)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(unique(found$session), "inner")
    expect_true(all(found$removed))
    expect_false(file.exists(file.path(inner$journal, "live.json")))
    expect_false(file.exists(file.path(inner$journal, "entries", "one.json")))
    expect_true(is_link(out))
    expect_true(is_link(file.path(dir, "linked.journal")))
    expect_true(is_link(file.path(dir, "linked_trace.jsonl")))
    expect_identical(sort(list.files(elsewhere)),
                     c("live.json", "trace.jsonl"))
})

# A link on the way: an agent's folder, or its sessions folder. Nothing
# behind it is listed or removed, and a real session beside it still
# goes.
if (.Platform$OS.type != "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    # Sessions kept outside the data directory, as make_session() would
    # lay them out under another one.
    far <- make_session(list(data = file.path(fx$root, "away")), "far",
                        agent = "a")
    far2 <- make_session(list(data = file.path(fx$root, "away2")), "far2",
                         agent = "b")
    near <- make_session(fx, "near")
    agents <- file.path(fx$data, "agents")
    expect_true(file.symlink(file.path(fx$root, "away", "agents", "a"),
                             file.path(agents, "linked")))
    dir.create(file.path(agents, "b"))
    expect_true(file.symlink(dirname(far2$trace),
                             file.path(agents, "b", "sessions")))
    outside <- c(far$trace, far$transcript, far2$trace, far2$transcript,
                 file.path(far$journal, "live.json"),
                 file.path(far2$journal, "live.json"))

    # A link that points back inside: the same sessions under a second
    # name. They are reached once, by their real folder.
    expect_true(file.symlink(file.path(agents, "main"),
                             file.path(agents, "alias")))

    # The scan does not go into a folder behind a link.
    read <- character()
    orig <- corteza:::retention_scan_dir
    assignInNamespace("retention_scan_dir", function(dir, ...) {
        read <<- c(read, dir)
        orig(dir, ...)
    }, ns = "corteza")
    found <- corteza:::retention_scan(90, now = fx$later)
    assignInNamespace("retention_scan_dir", orig, ns = "corteza")
    expect_identical(read, file.path(agents, "main", "sessions"))
    expect_identical(unique(found$session), "near")
    expect_identical(unique(found$agent), "main")

    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(unique(found$session), "near")
    expect_true(all(found$removed))
    expect_false(dir.exists(near$journal))
    expect_true(all(file.exists(outside)))
    # Asked about one of them directly, removal still refuses.
    via_link <- file.path(agents, "linked", "sessions", "far_trace.jsonl")
    expect_true(file.exists(via_link))
    expect_false(corteza:::retention_remove(via_link, "trace"))
    expect_true(file.exists(far$trace))
})

# A link whose target stays inside the data directory: an agent's folder
# pointing at a backup kept there, and a sessions folder pointing at
# another. Nothing behind either is listed or removed. Run twice, the
# second time as Windows sees links: Sys.readlink() reports none there,
# so the real path alone has to catch them.
if (.Platform$OS.type != "windows") local({
    for (windows_like in c(FALSE, TRUE)) local({
        fx <- retention_fixture()
        on.exit(fx$restore(), add = TRUE)
        if (windows_like) {
            orig_link <- corteza:::retention_is_link
            assignInNamespace("retention_is_link", function(path) FALSE,
                              ns = "corteza")
            on.exit(assignInNamespace("retention_is_link", orig_link,
                                      ns = "corteza"), add = TRUE)
        }
        why <- if (windows_like) "as on Windows" else "with Sys.readlink()"
        near <- make_session(fx, "near")
        kept <- make_session(list(data = file.path(fx$data, "backup")),
                             "kept", agent = "a")
        kept2 <- make_session(list(data = file.path(fx$data, "backup2")),
                              "kept2", agent = "b")
        agents <- file.path(fx$data, "agents")
        expect_true(file.symlink(file.path(fx$data, "backup", "agents", "a"),
                                 file.path(agents, "linked")), info = why)
        dir.create(file.path(agents, "c"))
        expect_true(file.symlink(dirname(kept2$trace),
                                 file.path(agents, "c", "sessions")),
                    info = why)
        inside <- c(kept$trace, kept$transcript, kept2$trace,
                    kept2$transcript, file.path(kept$journal, "live.json"),
                    file.path(kept2$journal, "live.json"))

        read <- character()
        orig_dir <- corteza:::retention_scan_dir
        assignInNamespace("retention_scan_dir", function(dir, ...) {
            read <<- c(read, dir)
            orig_dir(dir, ...)
        }, ns = "corteza")
        found <- corteza:::retention_scan(90, now = fx$later)
        assignInNamespace("retention_scan_dir", orig_dir, ns = "corteza")
        expect_identical(read, file.path(agents, "main", "sessions"),
                         info = why)
        expect_identical(unique(found$session), "near", info = why)

        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(unique(found$session), "near", info = why)
        expect_false(dir.exists(near$journal), info = why)
        expect_true(all(file.exists(inside)), info = why)
        via_link <- file.path(agents, "linked", "sessions",
                              basename(kept$trace))
        expect_true(file.exists(via_link), info = why)
        expect_false(corteza:::retention_within(via_link), info = why)
        expect_false(corteza:::retention_remove(via_link, "trace"),
                     info = why)
        expect_true(file.exists(kept$trace), info = why)
        # The same file by its own path is under the data directory with
        # no link on the way.
        expect_true(corteza:::retention_within(kept$trace), info = why)
    })
})

# A link to itself resolves nowhere, so its real path cannot give it
# away. Sys.readlink() does.
if (.Platform$OS.type != "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    make_session(fx, "near")
    loop <- file.path(fx$data, "agents", "loop")
    expect_true(file.symlink("loop", loop))
    expect_false(corteza:::retention_within(file.path(loop, "sessions",
                                                      "x_trace.jsonl")))
    found <- corteza:::retention_scan(90, now = fx$later)
    expect_identical(unique(found$session), "near")
})

# On Windows, a junction to a folder inside the data directory, which
# Sys.readlink() does not see and no one needs a privilege to make.
if (.Platform$OS.type == "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    near <- make_session(fx, "near")
    kept <- make_session(list(data = file.path(fx$data, "backup")), "kept",
                         agent = "a")
    link <- file.path(fx$data, "agents", "linked")
    if (isTRUE(Sys.junction(file.path(fx$data, "backup", "agents", "a"),
                            link))) {
        # R for Windows cannot remove a link (unlink() and file.remove()
        # both fail on one), and the link has to go before its folders.
        on.exit({
            win <- gsub("/", "\\\\", normalizePath(dirname(link),
                                                   winslash = "/"))
            system2("cmd", c("/c", "rmdir",
                             shQuote(paste0(win, "\\linked"), type = "cmd")),
                    stdout = FALSE, stderr = FALSE)
        }, add = TRUE, after = FALSE)
        via_link <- file.path(link, "sessions", basename(kept$trace))
        expect_true(file.exists(via_link))
        expect_identical(Sys.readlink(link), "")
        expect_false(corteza:::retention_within(via_link))
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(unique(found$session), "near")
        expect_true(file.exists(kept$trace))
        expect_true(file.exists(file.path(kept$journal, "live.json")))
    }
})

# The real path decides as well, for a link Sys.readlink() cannot see
# (Windows). Here ".." stands in for one: no part of the path is a link,
# and it still leads out of the data directory.
local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    make_session(fx, "here")
    beside <- file.path(dirname(fx$data), "beside")
    dir.create(beside)
    writeLines("{}", file.path(beside, "x_trace.jsonl"))
    out <- file.path(fx$data, "agents", "..", "..", "beside",
                     "x_trace.jsonl")
    expect_true(file.exists(out))
    expect_false(corteza:::retention_within(out))
    expect_false(corteza:::retention_remove(out, "trace"))
    expect_true(file.exists(file.path(beside, "x_trace.jsonl")))
    expect_true(corteza:::retention_within(file.path(fx$data, "agents",
                                                     "main", "sessions",
                                                     "here_trace.jsonl")))
    # Not under the data directory by name: refused before anything is
    # read.
    expect_false(corteza:::retention_within(file.path(fx$root, "x")))
})

# On Windows one path may hold both separators, and case does not count:
# tempfile() and tools::R_user_dir() give backslashes, and dirname() and
# file.path() forward slashes. A store's folder named through dirname()
# is still under the data directory, so the job, worker and remote
# stores are pruned there too (test_retention_state.R).
if (.Platform$OS.type == "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    make_session(fx, "here")
    trace <- file.path(fx$data, "agents", "main", "sessions",
                       "here_trace.jsonl")
    expect_true(corteza:::retention_within(trace))
    expect_true(corteza:::retention_within(gsub("/", "\\", trace,
                                                fixed = TRUE)))
    expect_true(corteza:::retention_within(trace, dirname(dirname(trace))))
    expect_true(corteza:::retention_within(toupper(trace)))
    expect_false(corteza:::retention_within(file.path(fx$root, "x")))
})

# The data directory itself may be a link, as when a person moves it to
# another disk: what lies under it is corteza's.
if (.Platform$OS.type != "windows") local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    real <- file.path(fx$root, "realdata")
    dir.create(real)
    expect_true(file.symlink(real, file.path(fx$root, "linkdata")))
    Sys.setenv(R_USER_DATA_DIR = file.path(fx$root, "linkdata"))
    moved <- fx
    moved$data <- corteza:::corteza_data_dir()
    old <- make_session(moved, "old")
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(unique(found$session), "old")
    expect_true(all(found$removed))
    expect_false(dir.exists(old$journal))
    expect_true(file.exists(old$transcript))
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
                                     "incomplete", "removed"))
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
    # A project's config cannot set it, a trusted project's included.
    # The merged config carries the value, and retention does not read
    # the merged config.
    set_config(fx, "{}")
    project <- file.path(fx$root, "project")
    dir.create(file.path(project, ".corteza"), recursive = TRUE)
    writeLines('{"retention": {"session_days": 1}}',
               file.path(project, ".corteza", "config.json"))
    expect_null(corteza:::load_config(project)$retention$session_days)
    corteza:::project_trust_add(project)
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

local({
    fx <- retention_fixture()
    on.exit(fx$restore(), add = TRUE)
    ns <- asNamespace("corteza")
    limits <- '{"retention":{"session_days":0,"audit_payload_days":7,"journal_segment_days":7,"workspace_snapshot_days":7}}'
    set_config(fx, limits)
    dir <- file.path(fx$data, "agents", "main", "sessions")
    dir.create(dir, recursive = TRUE)
    writeLines("{}", file.path(dir, "fixture.jsonl"))
    snap <- file.path(dir, "fixture_workspace.rds")
    saveRDS(list(x = 1), snap)
    metadata <- file.path(dir, "fixture_workspace.json")
    writeLines('{"turn":1,"objects":{}}', metadata)
    unrelated <- file.path(dir, "other_workspace.rds")
    file.copy(snap, unrelated)
    j <- file.path(dir, "fixture.journal")
    s <- corteza::new_session()
    ns$journal_install(s, j)
    old <- Sys.time() - 10 * DAY
    for (i in 1:3) {
        # Simulate a fresh bucket on each day.
        s$.journal$entry_day <- NULL
        after <- list(list(role = "user", content = paste("day", i)))
        ns$journal_rewrite(s, s$history, after, "context_edit", edits = list())
        s$history <- after
    }
    a <- ns$.audit_turn_begin(s)
    one <- ns$.audit_payload_put(a, "first", "tool_full")
    pinned <- ns$.audit_payload_put(a, "second", "tool_full")
    fresh <- ns$.audit_payload_put(a, "third", "tool_full")
    # These records intentionally survive the prune. On Windows their long
    # names must be removed before the fixture's ordinary recursive cleanup.
    on.exit({
        kept <- vapply(list(one, pinned, fresh), ns$.audit_payload_path, "")
        unlink(c(kept, paste0(kept, ".pin")), expand = FALSE)
    }, add = TRUE, after = FALSE)
    corteza::audit_payload_pin(pinned)
    for (path in c(ns$.journal_segment_path(j, 1), snap, metadata, unrelated,
                   ns$.audit_payload_path(one), ns$.audit_payload_path(pinned))) {
        Sys.setFileTime(path, old)
    }
    set_config(fx, '{"retention":{"session_days":0}}')
    expect_equal(nrow(suppressMessages(corteza::sessions_prune())), 0L)
    expect_true(all(file.exists(c(ns$.audit_payload_path(one), snap, ns$.journal_segment_path(j, 1)))))
    set_config(fx, limits)
    preview <- suppressMessages(corteza::sessions_prune(dry_run = TRUE))
    expect_identical(sort(preview$kind),
                     sort(c("segment", "entry", "payload", rep("workspace snapshot", 2))))
    overlapping <- ns$retention_history(ns$retention_config(), TRUE,
        idle = data.frame(kind = "journal", path = j))
    expect_false(any(overlapping$kind %in% c("segment", "entry")))
    expect_true("payload" %in% overlapping$kind)
    expect_false(any(preview$removed))
    done <- suppressMessages(corteza::sessions_prune())
    expect_true(all(done$removed))
    expect_identical(sort(done$path), sort(preview$path))
    expect_false(any(file.exists(done$path)))
    expect_true(file.exists(unrelated))
    expect_identical(corteza::audit_payload_read(pinned), "second")
    expect_identical(corteza::audit_payload_read(fresh), "third")
    expect_identical(ns$journal_load(j)$live$history, s$history)
    mark <- ns$journal_mark(s)
    ns$journal_rewrite(s, s$history, list(), "test")
    s$history <- list()
    expect_identical(ns$journal_entries_since(s, mark), list())
    restarted <- corteza::new_session()
    ns$journal_install(restarted, j)
    expect_identical(vapply(ns$journal_load(j)$segments, function(seg) seg$seq, 1L), 2:5)
})

# Expiry in another process while a turn waits. Both marks must still
# read their entire suffix, including entries in segments already removed.
local({
    fx <- retention_fixture()
    on.exit(fx$restore())
    ns <- asNamespace("corteza")
    set_config(fx, '{"retention":{"session_days":0,"journal_segment_days":0.1}}')
    expect_equal(ns$retention_config()$journal_segment_days, 1)
    expect_message(corteza::sessions_prune(dry_run = TRUE), "Nothing is old enough")
    dir <- file.path(fx$data, "agents", "main", "sessions", "waiting.journal")
    s <- corteza::new_session()
    ns$journal_install(s, dir)
    first <- ns$journal_mark(s)
    entries <- lapply(1:4, function(i) list(role = "assistant", content = as.character(i)))
    for (i in 1:4) {
        s$history <- c(s$history, entries[i])
        if (i == 2) second <- ns$journal_mark(s)
        if (i < 4) {
            ns$journal_rewrite(s, s$history, s$history, "context_index")
            Sys.setFileTime(ns$.journal_segment_path(dir, i), Sys.time() - 2 * DAY)
        }
    }
    gone <- callr::r(function() suppressMessages(corteza::sessions_prune()),
                     user_profile = FALSE)
    expect_equal(sum(gone$kind == "segment" & gone$removed), 2L)
    expect_false(file.exists(ns$.journal_segment_path(dir, 1)))
    expect_identical(ns$journal_turn_slice(s, first, 0L), entries)
    expect_identical(ns$journal_entries_since(s, second), entries[3:4])
    expect_false(ns$journal_rewritten_since(s, first, ignore = "context_index"))
})
