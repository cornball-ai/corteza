# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

library(tinytest)

# Retention, part 2 (R/retention-state.R): job chains, worker folders and
# remote-control sessions. Offline. corteza's data, config, state and
# remote-control folders are pointed under tempdir() for the length of a
# block and put back after it, so nothing here can read or remove a real
# record.
#
# Time is passed in, not waited for, as in test_retention.R: fixtures
# are written now and the clock handed to the code is 100 days on, so
# everything is old unless a test dates one of its files a day before
# that clock.

DAY <- 24 * 60 * 60

state_fixture <- function(state = NULL) {
    root <- tempfile("corteza_retention_state_")
    dir.create(root)
    vars <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "CORTEZA_STATE_DIR",
              "CORTEZA_REMOTE_DIR")
    old <- Sys.getenv(vars, unset = NA)
    Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
               R_USER_CONFIG_DIR = file.path(root, "config"))
    # Where corteza puts them by default, unless the test says otherwise.
    data <- corteza:::corteza_data_dir()
    if (is.null(state)) {
        state <- file.path(data, "state")
    }
    remote <- file.path(data, "remote-control")
    Sys.setenv(CORTEZA_STATE_DIR = state, CORTEZA_REMOTE_DIR = remote)
    env <- corteza:::.retention
    if (exists("looked", envir = env, inherits = FALSE)) {
        rm(list = "looked", envir = env)
    }
    list(root = root, data = data, state = state, remote = remote,
         later = Sys.time() + 100 * DAY,
         restore = function() {
             for (v in vars) {
                 if (is.na(old[[v]])) {
                     Sys.unsetenv(v)
                 } else {
                     do.call(Sys.setenv, stats::setNames(list(old[[v]]), v))
                 }
             }
             if (exists("looked", envir = env, inherits = FALSE)) {
                 rm(list = "looked", envir = env)
             }
             unlink(root, recursive = TRUE)
         })
}

jid <- function(n) sprintf("20260101T%06d-%08x", n, n)

write_record <- function(path, x) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    corteza:::job_write_file(path, x)
}

# A job's folder as corteza writes it. `status` NULL leaves it queued,
# "running" dispatched, anything else ended so.
make_job <- function(fx, id, status = "done", resume_of = NULL,
                     review_of = NULL, parent = NULL, audit = FALSE) {
    dir <- file.path(fx$state, "jobs", id)
    write_record(file.path(dir, "intent.json"),
                 list(id = id, task = "t", resume_of = resume_of,
                      review_of = review_of, parent = parent))
    if (!is.null(status)) {
        write_record(file.path(dir, "dispatch.json"), list(at = "then"))
    }
    if (!is.null(status) && status != "running") {
        write_record(file.path(dir, "progress", "00000001.json"),
                     list(text = "p"))
        write_record(file.path(dir, "outcome.json"), list(status = status))
    }
    if (identical(status, "incomplete")) {
        write_record(file.path(dir, "resume", "ready.json"), list(job = id))
        writeLines("x", file.path(dir, "resume", "workspace.RData"))
    }
    if (audit) {
        write_record(file.path(dir, "audit", "one.json"), list(a = 1))
    }
    dir
}

make_worker <- function(fx, name, files = "journal/live.json") {
    dir <- file.path(fx$state, "workers", name)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    for (f in files) {
        dir.create(dirname(file.path(dir, f)), recursive = TRUE,
                   showWarnings = FALSE)
        writeLines("{}", file.path(dir, f))
    }
    dir
}

make_remote <- function(fx, id, request = NULL, closed = FALSE) {
    if (is.null(request)) {
        request <- list(id = id, pid = Sys.getpid(),
                        host = Sys.info()[["nodename"]])
    }
    dir <- file.path(fx$remote, "sessions", id)
    write_record(file.path(dir, "request.json"), request)
    write_record(file.path(dir, "out", "000000000001.json"),
                 list(type = "text", text = "hi"))
    write_record(file.path(dir, "seen", "abc"), list(id = "x"))
    if (closed) {
        write_record(file.path(dir, "closed.json"), list(closed = TRUE))
    }
    dir
}

# Everything under the fixture, as paths relative to it.
tree <- function(fx) {
    sort(list.files(fx$root, recursive = TRUE, all.files = TRUE,
                    include.dirs = TRUE, no.. = TRUE))
}

rel <- function(fx, path) substring(path, nchar(fx$root) + 2L)

under <- function(paths, roots) {
    vapply(paths, function(p) {
        any(p == roots | startsWith(p, paste0(roots, "/")))
    }, logical(1), USE.NAMES = FALSE)
}

used <- function(path, when) {
    stopifnot(isTRUE(Sys.setFileTime(path, when)))
}

# ---- Job chains: what goes, what stays ----
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    done <- make_job(fx, jid(1))
    # An incomplete job whose resume is still queued: the chain stays.
    make_job(fx, jid(2), "incomplete")
    make_job(fx, jid(3), NULL, resume_of = jid(2))
    # Ended, but its progress changed yesterday.
    touched <- make_job(fx, jid(4))
    used(file.path(touched, "progress", "00000001.json"), fx$later - DAY)
    make_job(fx, jid(5), "running")
    # Ended, with its review still open.
    make_job(fx, jid(6))
    make_job(fx, jid(7), NULL, review_of = jid(6))
    # Ended, with a job it delegated still running.
    make_job(fx, jid(8))
    make_job(fx, jid(9), "running", parent = jid(8))
    # A consultation keeps its audit records in its folder.
    consult <- make_job(fx, jid(10), audit = TRUE)
    before <- tree(fx)

    # A dry run lists them and removes nothing.
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later)
    expect_identical(tree(fx), before)
    expect_identical(sort(paste(found$kind, found$session)),
                     paste("job", jid(c(1, 10))))
    expect_true(all(is.na(found$agent)))
    expect_false(any(found$incomplete))
    expect_true(all(found$bytes > 0))
    expect_true(all(found$idle_days > 99 & found$idle_days < 101))

    found <- corteza:::retention_prune(90, now = fx$later)
    expect_true(all(found$removed))
    gone <- c(rel(fx, done),
              rel(fx, file.path(consult, c("intent.json", "dispatch.json",
                                           "progress", "outcome.json"))))
    expect_true(all(gone %in% before))
    expect_identical(tree(fx), before[!under(before, gone)])
    expect_identical(list.files(consult, recursive = TRUE, all.files = TRUE),
                     "audit/one.json")
    # The ledger reads the rest as it did.
    ids <- vapply(corteza:::job_list(), `[[`, "", "id")
    expect_identical(sort(ids), jid(2:9))
    expect_identical(corteza:::job_read(jid(3))$status, "queued")
    # A folder that holds only audit records is no job, and nothing
    # unknown either: the next run has nothing to do.
    again <- corteza:::retention_prune(90, now = fx$later + DAY)
    expect_identical(nrow(again), 0L)
    # 0 days keeps everything.
    expect_identical(nrow(corteza:::retention_prune(0, now = fx$later)), 0L)
})

# ---- A chain that ended incomplete and nothing resumed ----
# It holds the snapshot a resume starts from, so it waits longer.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    waiting <- make_job(fx, jid(1), "incomplete")
    # Resumed, and the resume ended: an ordinary chain, named by its
    # newest job.
    make_job(fx, jid(2), "incomplete")
    make_job(fx, jid(3), resume_of = jid(2))
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later)
    expect_identical(found$session, rep(jid(3), 2L))
    expect_false(any(found$incomplete))

    at200 <- Sys.time() + 200 * DAY
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = at200)
    expect_identical(sort(found$session), c(jid(1), jid(3), jid(3)))
    expect_identical(found$session[found$incomplete], jid(1))
    expect_identical(corteza:::retention_summary(found, 90, TRUE),
                     paste0("Would remove 2 job chains unchanged for 90 days ",
                            "or more (0.0 MB). Incomplete and never resumed: ",
                            jid(1), "."))
    # 0 keeps it for good, and a longer limit for everything else wins.
    later <- Sys.time() + 1000 * DAY
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = later,
                                       incomplete_days = 0)
    expect_false(jid(1) %in% found$session)
    expect_identical(nrow(corteza:::retention_prune(365, dry_run = TRUE,
                                                    now = at200)), 0L)
    found <- corteza:::retention_prune(90, dry_run = TRUE, now = at200,
                                       incomplete_days = 150)
    expect_true(jid(1) %in% found$session)

    found <- corteza:::retention_prune(90, now = at200)
    expect_true(all(found$removed))
    expect_false(dir.exists(waiting))
    expect_identical(corteza:::retention_summary(found, 90, FALSE),
                     paste0("Removed 2 job chains unchanged for 90 days ",
                            "or more (0.0 MB). Incomplete and never resumed: ",
                            jid(1), "."))
})

# ---- Folders that do not read as a job's ----
# Left whole. One as old as the limit is listed, so a person can look.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    jobs <- file.path(fx$state, "jobs")
    # A resume's staging folder, from a process that ended mid-way.
    write_record(file.path(jobs, ".resume-job-1eb8ca699bba22", "intent.json"),
                 list(id = jid(1)))
    write_record(file.path(jobs, "20260101T000001-zzzzzzzz", "intent.json"),
                 list(id = "20260101T000001-zzzzzzzz"))
    write_record(file.path(jobs, jid(2), "outcome.json"), list(status = "done"))
    write_record(file.path(jobs, jid(3), "intent.json"), list(id = jid(4)))
    dir.create(file.path(jobs, jid(5)))
    writeLines("{not json", file.path(jobs, jid(5), "intent.json"))
    # Written yesterday: not listed yet.
    write_record(file.path(jobs, jid(6), "notes.json"), list(a = 1))
    used(file.path(jobs, jid(6), "notes.json"), fx$later - DAY)
    # A file is no folder: left, and not listed.
    writeLines("x", file.path(jobs, "stray.txt"))
    before <- tree(fx)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(tree(fx), before)
    expect_identical(sort(found$session),
                     sort(c(".resume-job-1eb8ca699bba22",
                            "20260101T000001-zzzzzzzz", jid(c(2, 3, 5)))))
    expect_true(all(found$kind == "unknown"))
    expect_false(any(found$removed))
    expect_identical(corteza:::retention_summary(found, 90, FALSE),
                     paste("No session has gone unused for 90 days. 5 folders",
                           "that do not read as corteza's stay;",
                           "sessions_prune(dry_run = TRUE) lists them."))
    one <- found[found$session %in% jid(2),, drop = FALSE]
    expect_identical(corteza:::retention_summary(one, 90, TRUE),
                     paste("No session has gone unused for 90 days. 1 folder",
                           "that does not read as corteza's stays;",
                           "sessions_prune(dry_run = TRUE) lists it."))
})

# ---- A removal that stops partway ----
# The intent goes first, so what is left never reads as a queued job.
# Skipped where a folder's mode does not stop a removal (Windows, root).
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    stuck <- make_job(fx, jid(1))
    progress <- file.path(stuck, "progress")
    Sys.chmod(progress, "0500")
    on.exit(Sys.chmod(progress, "0700"), add = TRUE, after = FALSE)
    if (.Platform$OS.type != "windows" && file.access(progress, 2L) != 0L) {
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_false(found$removed)
        expect_false(file.exists(file.path(stuck, "intent.json")))
        expect_true(file.exists(file.path(progress, "00000001.json")))
        expect_null(corteza:::job_read(jid(1)))
        expect_identical(length(corteza:::job_list()), 0L)
        expect_true(grepl("Removed 0 job chains .* 1 left in place[.]$",
                          corteza:::retention_summary(found, 90, FALSE)))
        # What is left no longer reads as a job's.
        again <- corteza:::retention_prune(90, now = fx$later + DAY)
        expect_identical(again$kind, "unknown")
    }
})

# A job's intent that cannot be removed: nothing else goes, so its
# outcome stays and it never reads as queued. The same for a remote
# session's request. The removal is stood in for.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    job <- make_job(fx, jid(1))
    remote <- make_remote(fx, jid(2), closed = TRUE)
    real <- corteza:::retention_unlink
    refused <- character()
    assignInNamespace("retention_unlink", function(paths) {
        held <- basename(paths) %in% c("intent.json", "request.json")
        refused <<- c(refused, paths[held])
        real(paths[!held])
        !any(held)
    }, ns = "corteza")
    on.exit(assignInNamespace("retention_unlink", real, ns = "corteza"),
            add = TRUE, after = FALSE)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_false(any(found$removed))
    expect_identical(sort(basename(refused)), c("intent.json", "request.json"))
    expect_identical(corteza:::job_read(jid(1))$status, "done")
    expect_true(file.exists(file.path(job, "progress", "00000001.json")))
    expect_true(file.exists(file.path(remote, "closed.json")))
    expect_true(file.exists(file.path(remote, "out", "000000000001.json")))
})

# ---- A chain written to between the scan and the removal ----
# Another process may hold it. The scan is replayed here as it was
# before the first job of the chain changed.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    first <- make_job(fx, jid(1), "incomplete")
    second <- make_job(fx, jid(2), resume_of = jid(1))
    stale <- corteza:::retention_state_scan(90, 180, now = fx$later)
    expect_identical(nrow(stale), 2L)
    used(file.path(first, "progress", "00000001.json"), fx$later - DAY)
    real <- corteza:::retention_state_scan
    assignInNamespace("retention_state_scan",
                      function(days, incomplete_days, now = Sys.time(),
                               keep = character()) stale,
                      ns = "corteza")
    on.exit(assignInNamespace("retention_state_scan", real, ns = "corteza"),
            add = TRUE, after = FALSE)
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_false(any(found$removed))
    expect_true(file.exists(file.path(first, "intent.json")))
    expect_true(file.exists(file.path(second, "intent.json")))
    expect_identical(corteza:::retention_summary(found, 90, FALSE),
                     paste("Removed 0 job chains unchanged for 90 days or more",
                           "(0.0 MB). 1 left in place."))
})

# ---- Worker folders ----
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    # A monitor's: a journal, its audit records, and the temporary files
    # of writes that never finished.
    monitor <- make_worker(fx, strrep("a", 24),
                           c("journal/live.json", "journal/live.json.prev",
                             "journal/segment-0001.json",
                             "journal/entries/one.json",
                             "journal/audit/one.json", "history.1eb8ca9e673d0",
                             "checkpoint.json.1eb8caf6f4fc",
                             "workspace.RData.tmp-123"))
    plain <- make_worker(fx, strrep("b", 24))
    empty <- make_worker(fx, strrep("e", 24), character())
    # A checkpoint that names no identity is not of the form corteza
    # writes (part 2b, below, for one that does): listed, and left.
    doer <- make_worker(fx, strrep("c", 24),
                        c("checkpoint.json", "workspace.RData", "history.rds",
                          "journal/live.json"))
    busy <- make_worker(fx, strrep("d", 24))
    used(file.path(busy, "journal", "live.json"), fx$later - DAY)
    # Not a worker's: a file a worker does not write, a name it does not
    # give.
    stranger <- make_worker(fx, strrep("f", 24),
                            c("journal/live.json", "notes.txt"))
    named <- make_worker(fx, "not-a-digest")
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(sort(paste(found$kind, found$session)),
                     sort(c(paste("worker", strrep(c("a", "b", "e"), 24)),
                            paste("unknown", c(strrep(c("c", "f"), 24),
                                               "not-a-digest")))))
    expect_true(all(found$removed[found$kind == "worker"]))
    expect_identical(list.files(monitor, recursive = TRUE, all.files = TRUE),
                     "journal/audit/one.json")
    expect_false(dir.exists(plain))
    expect_false(dir.exists(empty))
    expect_identical(sort(list.files(doer, recursive = TRUE)),
                     c("checkpoint.json", "history.rds", "journal/live.json",
                       "workspace.RData"))
    expect_true(file.exists(file.path(busy, "journal", "live.json")))
    expect_true(file.exists(file.path(stranger, "notes.txt")))
    expect_true(file.exists(file.path(stranger, "journal", "live.json")))
    expect_true(file.exists(file.path(named, "journal", "live.json")))
    again <- corteza:::retention_prune(90, now = fx$later + DAY)
    expect_identical(again$kind, c("unknown", "unknown", "unknown"))

    # A worker that writes a checkpoint after the scan is left whole.
    late <- make_worker(fx, strrep("9", 24))
    scan <- corteza:::retention_state_scan(90, 180, now = fx$later)
    expect_identical(scan$session[scan$kind == "worker"], strrep("9", 24))
    writeLines("{}", file.path(late, "checkpoint.json"))
    expect_false(corteza:::retention_state_remove(late, "worker"))
    expect_true(file.exists(file.path(late, "journal", "live.json")))
})

# ---- Worker folders with a checkpoint (part 2b) ----
# A checkpoint's folder goes once it and the place its identity names
# have both gone unused for the limit: a terminal's conversation, or a
# room's or thread's session, by its journal.

ident <- function(owner, key, workspace, role = "doer") {
    list(owner = owner, key = key, workspace = workspace, role = role)
}
# A worker's folder as it writes it: named for the identity, with the
# checkpoint that records it, and, given `worker`, its process record.
make_checkpoint <- function(fx, identity, worker = NULL) {
    name <- basename(corteza:::job_worker_state_dir(identity))
    dir <- make_worker(fx, name, c("workspace.RData", "history.rds",
                                   "journal/live.json",
                                   "journal/audit/one.json"))
    write_record(file.path(dir, "checkpoint.json"),
                 list(identity = identity, job = jid(1), at = "then",
                      objects = "x", conversation = "c"))
    if (!is.null(worker)) {
        write_record(file.path(dir, "worker.json"), worker)
    }
    dir
}
owner <- "@bot:example.invalid"
room_journal <- function(key) {
    corteza:::bot_journal_dir(list(user_id = owner), key)
}
here <- Sys.info()[["nodename"]]

local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    ws <- normalizePath(fx$root, winslash = "/")
    sessions <- file.path(fx$data, "agents", "main", "sessions")
    dir.create(sessions, recursive = TRUE)
    # A terminal's conversation used yesterday, and one long unused.
    fresh_term <- make_checkpoint(fx, ident("local:host", "repl:fresh", ws))
    writeLines("{}", file.path(sessions, "fresh.jsonl"))
    used(file.path(sessions, "fresh.jsonl"), fx$later - DAY)
    stale_term <- make_checkpoint(fx, ident("local:host", "repl:stale", ws))
    writeLines("{}", file.path(sessions, "stale.jsonl"))
    # A room whose conversation record changed yesterday, a thread past a
    # sidebar (its epoch on the key) whose journal did, and a room long
    # unused.
    fresh_room <- make_checkpoint(fx, ident(owner, "!fresh:example.invalid", ws))
    record <- file.path(room_journal("!fresh:example.invalid"),
                        "conversation-1.jsonl")
    write_record(record, list(kind = "conversation"))
    used(record, fx$later - DAY)
    thread <- "!side:example.invalid\r$root"
    sidebar <- make_checkpoint(fx, ident(owner,
                                         paste0(thread, ":after-sidebar:3"), ws))
    write_record(file.path(room_journal(thread), "live.json"), list())
    used(file.path(room_journal(thread), "live.json"), fx$later - DAY)
    stale_room <- make_checkpoint(fx, ident(owner, "!stale:example.invalid", ws))
    write_record(file.path(room_journal("!stale:example.invalid"), "live.json"),
                 list())
    # No place left: the folder's own age decides.
    orphan <- make_checkpoint(fx, ident(owner, "!gone:example.invalid", ws))
    recent <- make_checkpoint(fx, ident(owner, "!gone2:example.invalid", ws))
    used(file.path(recent, "checkpoint.json"), fx$later - DAY)

    dry <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later)
    expect_identical(sort(dry$path[dry$kind == "checkpoint"]),
                     sort(c(stale_term, stale_room, orphan)))
    found <- corteza:::retention_prune(90, now = fx$later)
    cp <- found[found$kind == "checkpoint",, drop = FALSE]
    expect_identical(sort(cp$path), sort(c(stale_term, stale_room, orphan)))
    expect_true(all(cp$removed))
    # What the worker wrote goes; the journal's audit records stay.
    for (dir in cp$path) {
        expect_identical(list.files(dir, recursive = TRUE, all.files = TRUE),
                         "journal/audit/one.json", info = dir)
    }
    for (dir in c(fresh_term, fresh_room, sidebar, recent)) {
        expect_identical(sort(list.files(dir, recursive = TRUE)),
                         c("checkpoint.json", "history.rds",
                           "journal/audit/one.json", "journal/live.json",
                           "workspace.RData"), info = dir)
    }
    # A transcript is never removed.
    expect_true(file.exists(file.path(sessions, "stale.jsonl")))
    expect_true(grepl("3 worker checkpoints",
                      corteza:::retention_state_line(found, 90, FALSE),
                      fixed = TRUE))
})

# Held, whatever its age: a session the caller has open, a live worker,
# a job open in its workspace or running under its identity. A
# checkpoint that cannot be read, or names another folder's identity,
# is listed as unknown and left.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    ws <- normalizePath(fx$root, winslash = "/")
    open_term <- make_checkpoint(fx, ident("local:host", "repl:open", ws))
    open_room <- make_checkpoint(fx, ident(owner, "!open:example.invalid", ws))
    room_id <- sub("[.]journal$", "",
                   basename(room_journal("!open:example.invalid")))
    live <- make_checkpoint(fx, ident(owner, "!live:example.invalid", ws),
                            worker = list(pid = Sys.getpid(), host = here))
    p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
                               c("--vanilla", "-e", "invisible(0)"))
    p$wait()
    dead <- make_checkpoint(fx, ident(owner, "!dead:example.invalid", ws),
                            worker = list(pid = p$get_pid(), host = here))
    far <- make_checkpoint(fx, ident(owner, "!far:example.invalid", ws),
                           worker = list(pid = 1L, host = "elsewhere.invalid"))
    # Another directory, where a job is queued.
    project <- normalizePath(file.path(fx$root, "data"), winslash = "/")
    queued <- make_checkpoint(fx, ident(owner, "!queued:example.invalid",
                                        project))
    write_record(file.path(fx$state, "jobs", jid(7), "intent.json"),
                 list(id = jid(7), task = "t", workspace = project))
    # A plain worker folder (no checkpoint) whose job runs elsewhere, by
    # the identity its dispatch names, and one whose worker is alive.
    runner <- ident(owner, "!run:example.invalid", "/nowhere/else")
    running <- make_worker(fx, basename(corteza:::job_worker_state_dir(runner)))
    write_record(file.path(fx$state, "jobs", jid(8), "intent.json"),
                 list(id = jid(8), task = "t", workspace = "/nowhere/else"))
    write_record(file.path(fx$state, "jobs", jid(8), "dispatch.json"),
                 list(at = "then", worker = list(identity = runner)))
    monitor <- make_worker(fx, strrep("6", 24))
    write_record(file.path(monitor, "worker.json"),
                 list(pid = Sys.getpid(), host = here))
    unreadable <- make_worker(fx, strrep("7", 24))
    writeLines("{ not json", file.path(unreadable, "checkpoint.json"))
    elsewhere <- make_worker(fx, strrep("8", 24))
    write_record(file.path(elsewhere, "checkpoint.json"),
                 list(identity = ident(owner, "!other:example.invalid", ws)))

    found <- corteza:::retention_prune(90, keep = c("open", room_id),
                                       now = fx$later)
    gone <- if (.Platform$OS.type == "windows") character() else dead
    expect_identical(found$path[found$kind == "checkpoint"], gone)
    expect_identical(found$path[found$kind == "worker"], character())
    expect_identical(sort(found$path[found$kind == "unknown"]),
                     sort(c(unreadable, elsewhere)))
    for (dir in c(open_term, open_room, live, far, queued, unreadable,
                  elsewhere)) {
        expect_true(file.exists(file.path(dir, "checkpoint.json")), info = dir)
    }
    expect_true(file.exists(file.path(running, "journal", "live.json")))
    expect_true(file.exists(file.path(monitor, "journal", "live.json")))
    if (length(gone)) {
        expect_identical(list.files(dead, recursive = TRUE),
                         "journal/audit/one.json")
    }
    # Without the caller's sessions, theirs go too.
    again <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later)
    expect_identical(sort(again$path[again$kind == "checkpoint"]),
                     sort(c(open_term, open_room)))
    # A bot passes its registry's rooms, by the id their journals have.
    registry <- corteza:::bot_new_session_registry()
    s <- new.env()
    s$job_owner <- owner
    assign("!open:example.invalid", s, envir = registry)
    assign("!other:example.invalid\r$root", 1, envir = registry)
    expect_identical(corteza:::bot_retention_keep(registry), room_id)
    held <- corteza:::retention_prune(90, dry_run = TRUE, now = fx$later,
                                      keep = corteza:::bot_retention_keep(registry))
    expect_identical(held$path[held$kind == "checkpoint"], open_term)
})

# Read again just before it goes: a place used since the scan, a
# session opened or a worker started since, leaves the folder.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    ws <- normalizePath(fx$root, winslash = "/")
    sessions <- file.path(fx$data, "agents", "main", "sessions")
    dir.create(sessions, recursive = TRUE)
    cp <- make_checkpoint(fx, ident("local:host", "repl:later", ws))
    transcript <- file.path(sessions, "later.jsonl")
    writeLines("{}", transcript)
    other <- make_checkpoint(fx, ident("local:host", "repl:other", ws))
    scan <- corteza:::retention_state_scan(90, 180, now = fx$later)
    row <- scan[scan$kind == "checkpoint" & scan$path == cp,, drop = FALSE]
    expect_equal(nrow(row), 1L)
    expect_false(corteza:::retention_checkpoint_moved(cp, row$last_use))
    used(transcript, fx$later - DAY)
    expect_true(corteza:::retention_checkpoint_moved(cp, row$last_use))
    row <- scan[scan$kind == "checkpoint" & scan$path == other,, drop = FALSE]
    expect_false(corteza:::retention_checkpoint_moved(other, row$last_use))
    expect_true(corteza:::retention_checkpoint_moved(other, row$last_use,
                                                     keep = "other"))
    write_record(file.path(other, "worker.json"),
                 list(pid = Sys.getpid(), host = here))
    expect_true(corteza:::retention_checkpoint_moved(other, row$last_use))
    cp <- other
    # The checkpoint goes first, so a removal that stops partway leaves
    # a folder judged by its own age.
    own <- corteza:::retention_state_own(cp, normalizePath(cp, winslash = "/"),
                                         "checkpoint")
    expect_identical(own$first, file.path(cp, "checkpoint.json"))
    expect_true(file.path(cp, "worker.json") %in% own$files)
})

# ---- Remote-control sessions ----
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    host <- Sys.info()[["nodename"]]
    closed <- make_remote(fx, jid(1), closed = TRUE)
    # Open, and its process is this one.
    alive <- make_remote(fx, jid(2))
    # Another host's, and one with no process id: nothing says they ended.
    away <- make_remote(fx, jid(3), list(id = jid(3), pid = 1,
                                         host = paste0(host, "-elsewhere")))
    nopid <- make_remote(fx, jid(4), list(id = jid(4), host = host))
    # Closed, but written to yesterday.
    fresh <- make_remote(fx, jid(5), closed = TRUE)
    used(file.path(fresh, "out", "000000000001.json"), fx$later - DAY)
    # Not a session's record.
    garbled <- file.path(fx$remote, "sessions", jid(6))
    dir.create(garbled, recursive = TRUE)
    writeLines("{not json", file.path(garbled, "request.json"))
    misnamed <- make_remote(fx, jid(7), list(id = jid(8), pid = 1, host = host))
    # Its process ran here and ended.
    ended <- NULL
    if (.Platform$OS.type != "windows") {
        p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
                                   c("--vanilla", "-e", "invisible(0)"))
        p$wait()
        ended <- make_remote(fx, jid(9), list(id = jid(9), pid = p$get_pid(),
                                              host = host))
    }
    # The rooms and bots files beside the sessions are not this store's.
    write_record(file.path(fx$remote, "rooms", "abc.json"), list(room = "!r"))
    write_record(file.path(fx$remote, "bots", "abc.json"), list(pid = 1))
    found <- corteza:::retention_prune(90, now = fx$later)
    removed <- c(jid(1), if (!is.null(ended)) jid(9))
    expect_identical(sort(found$session[found$kind == "remote"]), removed)
    expect_true(all(found$removed[found$kind == "remote"]))
    expect_identical(sort(found$session[found$kind == "unknown"]),
                     jid(c(6, 7)))
    expect_false(dir.exists(closed))
    if (!is.null(ended)) {
        expect_false(dir.exists(ended))
    }
    for (dir in c(alive, away, nopid, fresh, garbled, misnamed)) {
        expect_true(file.exists(file.path(dir, "request.json")), info = dir)
    }
    expect_true(file.exists(file.path(fx$remote, "rooms", "abc.json")))
    expect_true(file.exists(file.path(fx$remote, "bots", "abc.json")))
})

# ---- Links ----
# A link is never removed, nor anything behind one, and a folder holding
# one stays. A directory junction works without symlink privileges on Windows.
make_dir_link <- function(target, link) {
    if (.Platform$OS.type == "windows") isTRUE(Sys.junction(target, link)) else
        isTRUE(file.symlink(target, link))
}
drop_dir_link <- function(link) {
    if (.Platform$OS.type == "windows") {
        path <- file.path(normalizePath(dirname(link), winslash = "/", mustWork = TRUE),
                          basename(link))
        status <- system2("cmd", c("/c", "rmdir", shQuote(chartr("/", "\\", path), type = "cmd")))
        stopifnot(status == 0L)
    } else unlink(link)
}
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    away <- file.path(fx$root, "away")
    dir.create(away)
    writeLines("keep", file.path(away, "precious.txt"))
    job <- make_job(fx, jid(1))
    linked <- file.path(job, "linked")
    if (make_dir_link(away, linked)) {
        on.exit(if (dir.exists(linked)) drop_dir_link(linked), add = TRUE, after = FALSE)
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_true(found$removed)
        expect_identical(list.files(job, all.files = TRUE, no.. = TRUE),
                         "linked")
        expect_true(file.exists(file.path(away, "precious.txt")))
        # What is left is no job's record, and is listed so.
        found <- corteza:::retention_prune(90, now = fx$later + DAY)
        expect_identical(paste(found$kind, found$session),
                         paste("unknown", jid(1)))
        expect_true(file.exists(file.path(away, "precious.txt")))
        drop_dir_link(linked)
    }
    # An entry of the ledger that is a link, to a job's folder elsewhere:
    # not listed, and nothing behind it touched.
    outside <- file.path(fx$root, "outside", jid(3))
    write_record(file.path(outside, "intent.json"), list(id = jid(3)))
    write_record(file.path(outside, "outcome.json"), list(status = "done"))
    entry <- file.path(fx$state, "jobs", jid(3))
    if (make_dir_link(outside, entry)) {
        on.exit(if (dir.exists(entry)) drop_dir_link(entry), add = TRUE, after = FALSE)
        found <- corteza:::retention_prune(90, now = fx$later + DAY)
        expect_false(jid(3) %in% found$session)
        expect_true(file.exists(file.path(outside, "intent.json")))
        expect_true(file.exists(file.path(outside, "outcome.json")))
        drop_dir_link(entry)
    }
    # A store reached through a link is not read at all.
    elsewhere <- file.path(fx$root, "elsewhere")
    write_record(file.path(elsewhere, jid(2), "intent.json"), list(id = jid(2)))
    write_record(file.path(elsewhere, jid(2), "outcome.json"),
                 list(status = "done"))
    jobs <- file.path(fx$state, "jobs")
    unlink(jobs, recursive = TRUE)
    if (make_dir_link(elsewhere, jobs)) {
        on.exit(if (dir.exists(jobs)) drop_dir_link(jobs), add = TRUE, after = FALSE)
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(nrow(found), 0L)
        expect_true(file.exists(file.path(elsewhere, jid(2), "intent.json")))
    }
    if (dir.exists(jobs)) drop_dir_link(jobs)
    workers <- file.path(fx$state, "workers")
    named <- file.path(elsewhere, strrep("a", 24), "journal")
    dir.create(named, recursive = TRUE)
    writeLines("{}", file.path(named, "live.json"))
    if (make_dir_link(elsewhere, workers)) {
        on.exit(drop_dir_link(workers), add = TRUE, after = FALSE)
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(nrow(found), 0L)
        expect_true(file.exists(file.path(named, "live.json")))
    }
})

# A record that is a link is not read: a job whose intent is a link does
# not read as a job, and nothing of it goes, so a finished job never
# reads as queued. A removal refuses a folder whose intent is a link by
# the time it gets there. An outcome that is a link counts as none, and
# a remote session's request or closed record likewise.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    away <- file.path(fx$root, "away")
    dir.create(away)
    relink <- function(record, target) {
        write_record(target, corteza:::job_read_file(record))
        unlink(record)
        made <- isTRUE(suppressWarnings(file.symlink(target, record)))
        if (made) links <<- c(links, record)
        made
    }
    links <- character()
    remove_file_link <- function(path) {
        if (.Platform$OS.type == "windows") {
            win <- file.path(normalizePath(dirname(path), winslash = "/"), basename(path))
            status <- system2("cmd", c("/c", "del", "/q", shQuote(chartr("/", "\\", win), type = "cmd")))
            stopifnot(status == 0L)
        } else unlink(path)
    }
    on.exit(for (p in links) {
        if (file.exists(p) && !dir.exists(p)) remove_file_link(p)
    }, add = TRUE, after = FALSE)

    job <- make_job(fx, jid(1))
    intent <- file.path(job, "intent.json")
    if (relink(intent, file.path(away, "intent-1.json"))) {
        expect_identical(corteza:::job_read(jid(1))$status, "done")
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(paste(found$kind, found$session),
                         paste("unknown", jid(1)))
        expect_true(file.exists(file.path(job, "outcome.json")))
        expect_true(file.exists(file.path(job, "dispatch.json")))
        expect_identical(corteza:::job_read(jid(1))$status, "done")
        # The removal itself refuses it, whatever a scan said.
        expect_false(corteza:::retention_state_remove(job, "job"))
        expect_true(file.exists(file.path(job, "outcome.json")))
        expect_true(file.exists(file.path(job, "progress", "00000001.json")))
        expect_true(file.exists(file.path(away, "intent-1.json")))
        expect_identical(corteza:::job_read(jid(1))$status, "done")
        # So does a folder in place of the intent.
        remove_file_link(intent)
        dir.create(intent)
        writeLines("{}", file.path(intent, "x.json"))
        expect_false(corteza:::retention_state_remove(job, "job"))
        expect_true(file.exists(file.path(job, "outcome.json")))
    }

    # An outcome reached through a link counts as none: the job is open,
    # and its chain stays.
    open <- make_job(fx, jid(2))
    if (relink(file.path(open, "outcome.json"),
               file.path(away, "outcome-2.json"))) {
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_false(jid(2) %in% found$session)
        expect_true(file.exists(file.path(open, "intent.json")))
        expect_true(file.exists(file.path(open, "dispatch.json")))
    }

    # A remote session whose request is a link is no session of
    # corteza's; one whose closed record is a link is not closed (its
    # process is this one, still running).
    linked <- make_remote(fx, jid(3), closed = TRUE)
    if (relink(file.path(linked, "request.json"),
               file.path(away, "request-3.json"))) {
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_identical(found$kind[found$session %in% jid(3)], "unknown")
        expect_true(file.exists(file.path(linked, "closed.json")))
        expect_true(file.exists(file.path(linked, "out", "000000000001.json")))
    }
    running <- make_remote(fx, jid(4), closed = TRUE)
    if (relink(file.path(running, "closed.json"),
               file.path(away, "closed-4.json"))) {
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_false(jid(4) %in% found$session)
        expect_true(file.exists(file.path(running, "request.json")))
    }
})

# The state folder itself may be a link, as the data folder may: it is
# read from where it points.
local({
    root <- tempfile("corteza_state_link_")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    real_state <- file.path(root, "real-state")
    dir.create(real_state)
    link <- file.path(root, "state")
    if (make_dir_link(real_state, link)) {
        on.exit(drop_dir_link(link), add = TRUE, after = FALSE)
        fx <- state_fixture(state = link)
        on.exit(fx$restore(), add = TRUE)
        job <- make_job(fx, jid(1))
        found <- corteza:::retention_prune(90, now = fx$later)
        expect_true(found$removed)
        expect_false(dir.exists(file.path(real_state, "jobs", jid(1))))
    }
})

# ---- A state folder outside the data folder ----
local({
    root <- tempfile("corteza_state_elsewhere_")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    fx <- state_fixture(state = file.path(root, "state"))
    on.exit(fx$restore(), add = TRUE)
    job <- make_job(fx, jid(1))
    worker <- make_worker(fx, strrep("a", 24))
    found <- corteza:::retention_prune(90, now = fx$later)
    expect_identical(sort(found$kind), c("job", "worker"))
    expect_true(all(found$removed))
    expect_false(dir.exists(job))
    expect_false(dir.exists(worker))
})

# ---- The setting, the daily run and sessions_prune() ----
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    set_config <- function(json) {
        path <- corteza:::corteza_config_path("config.json")
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeLines(json, path)
    }
    limits <- function() corteza:::retention_config()[c("session_days", "incomplete_days")]
    expect_identical(limits(), list(session_days = 90, incomplete_days = 180))
    set_config('{"retention": {"incomplete_days": 30}}')
    expect_identical(limits(),
                     list(session_days = 90, incomplete_days = 30))
    # A value it cannot read keeps what it governs.
    set_config('{"retention": {"incomplete_days": "soon"}}')
    expect_identical(limits(),
                     list(session_days = 90, incomplete_days = 0))
    set_config('{"retention": {"incomplete_days": -1, "session_days": 30}}')
    expect_identical(limits(),
                     list(session_days = 30, incomplete_days = 0))

    # The daily run reads it: an incomplete chain stays at 200 days.
    make_job(fx, jid(1), "incomplete")
    make_job(fx, jid(2))
    make_worker(fx, strrep("a", 24))
    make_remote(fx, jid(3), closed = TRUE)
    at200 <- Sys.time() + 200 * DAY
    line <- corteza:::retention_tick(now = at200)
    expect_identical(line,
                     paste("Removed 1 job chain, 1 worker folder and 1",
                           "remote-control session unchanged for 30 days or",
                           "more (0.0 MB)."))
    expect_true(file.exists(file.path(fx$state, "jobs", jid(1),
                                      "intent.json")))
})

# Against the real clock, with files and folders dated 100 days back. A
# platform that cannot date a folder skips the block; a Unix that cannot
# fails.
local({
    fx <- state_fixture()
    on.exit(fx$restore(), add = TRUE)
    job <- make_job(fx, jid(1))
    remote <- make_remote(fx, jid(2), closed = TRUE)
    paths <- c(job, remote,
               list.files(c(job, remote), full.names = TRUE, recursive = TRUE,
                          include.dirs = TRUE, all.files = TRUE))
    dated <- all(Sys.setFileTime(paths, Sys.time() - 100 * DAY)) &&
    all(file.info(paths)$mtime < Sys.time() - 99 * DAY)
    expect_true(dated || .Platform$OS.type == "windows")
    if (dated) {
        expect_message(found <- corteza::sessions_prune(dry_run = TRUE,
                                                        days = 90),
                       paste("Would remove 1 job chain and 1 remote-control",
                             "session unchanged for 90 days or more"),
                       fixed = TRUE)
        expect_true(dir.exists(job))
        expect_message(found <- corteza::sessions_prune(days = 90),
                       paste("Removed 1 job chain and 1 remote-control",
                             "session unchanged for 90 days or more"),
                       fixed = TRUE)
        expect_true(all(found$removed))
        expect_false(dir.exists(job))
        expect_false(dir.exists(remote))
    }
})
