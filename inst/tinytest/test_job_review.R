library(tinytest)

# Review of a doer's job: the reviewer's limits, the lock handed from
# doer to review with no gap, queue order around a review, and what the
# surfaces say. The workers' init and run functions are replaced, so no
# provider is called.

state <- tempfile("job-review-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

plain <- normalizePath({
    d <- tempfile("review-checkout")
    dir.create(d)
    d
})
holder <- corteza:::job_lock_holder
ts <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS%z")
corteza::ensure_skills()

# --- Verdict parsing ---
verdict <- corteza:::job_review_verdict
expect_identical(verdict("VERDICT: approve\nLooks right."), "approve")
expect_identical(verdict("verdict: Changes requested\n- a.R:3"),
                 "changes requested")
expect_identical(verdict("**VERDICT: approve**"), "approve")
expect_identical(verdict("\n\nVERDICT: approve"), "approve")
# No verdict in the asked-for form is no verdict, not a guess.
expect_true(is.na(verdict("Looks fine to me.")))
expect_true(is.na(verdict("VERDICT: maybe")))
expect_true(is.na(verdict(NULL)))
# A verdict quoted deep in the findings is not the reviewer's own.
expect_true(is.na(verdict(paste(c(rep("finding", 8),
                                  "VERDICT: approve"), collapse = "\n"))))

# --- The reviewer's limits ---
s0 <- corteza::new_session("matrix", provider = "anthropic",
                           model_map = list(cloud = "claude-opus-5-5"))
s0$cwd <- plain
s0$config <- list()
rv <- corteza:::job_worker_spec(s0, "reviewer")
expect_identical(rv$tools, corteza:::JOB_REVIEWER_TOOLS)
expect_false(any(c("bash", "cmd", "run_r", "run_r_script", "write_file",
                   "replace_in_file", "web_search", "fetch_url",
                   "spawn_subagent", "delegate") %in% rv$tools))
expect_false(rv$web_search)
expect_identical(rv$allowed_paths, plain)
expect_identical(rv$system, corteza:::JOB_REVIEWER_SYSTEM)
# Same model as the doer unless the config names another.
expect_identical(rv$model, "claude-opus-5-5")
# The doer is unchanged: no confinement, default web search.
dv <- corteza:::job_worker_spec(s0, "doer")
expect_null(dv$web_search)
expect_null(dv$allowed_paths)
expect_true("bash" %in% dv$tools)
# A reviewer on another provider does not inherit the doer's model name.
s0$config <- list(jobs = list(reviewer = list(provider = "openai")))
rv2 <- corteza:::job_worker_spec(s0, "reviewer")
expect_identical(rv2$provider, "openai")
expect_null(rv2$model)
s0$config <- list(jobs = list(reviewer = list(provider = "openai",
                                              model = "gpt-6")))
expect_identical(corteza:::job_worker_spec(s0, "reviewer")$model, "gpt-6")
s0$config <- list(jobs = list(reviewer = list(model = "claude-sonnet-5-5")))
rv3 <- corteza:::job_worker_spec(s0, "reviewer")
expect_identical(rv3$provider, "anthropic")
expect_identical(rv3$model, "claude-sonnet-5-5")
expect_error(corteza:::job_worker_spec(s0, "auditor"), "unknown job role")
expect_true(corteza:::job_role_locks("reviewer"))
expect_false(corteza:::job_role_writes("reviewer"))

# --- The git base a review diffs against ---
if (nzchar(Sys.which("git"))) {
    repo <- tempfile("review-repo")
    dir.create(repo)
    git <- function(...) {
        system2("git", c("-C", shQuote(repo), "-c", "user.name=t", "-c",
                         "user.email=t@example.com", ...),
                stdout = TRUE, stderr = TRUE)
    }
    snap <- corteza:::job_git_snapshot
    git("init", "-q")
    # Before the first commit there is nothing to diff against.
    expect_null(snap(repo))
    writeLines("one", file.path(repo, "a.txt"))
    writeLines("ignored.txt", file.path(repo, ".gitignore"))
    git("add", "a.txt", ".gitignore")
    git("commit", "-q", "-m", "first")
    head <- git("rev-parse", "HEAD")

    # The starting state a job inherits: an uncommitted edit to a
    # tracked file, a file that was never committed, an ignored file.
    writeLines("two", file.path(repo, "a.txt"))
    writeLines("draft v1", file.path(repo, "notes.txt"))
    writeLines("secret", file.path(repo, "ignored.txt"))
    index_path <- file.path(repo, ".git", "index")
    # `git status` refreshes the index's cached stats itself, so the
    # checksum is taken after it, not before.
    before <- list(status = git("status", "--porcelain"),
                   refs = git("for-each-ref"),
                   stash = git("stash", "list"))
    before$index <- unname(tools::md5sum(index_path))

    base <- snap(repo)
    expect_identical(base$head, head)
    expect_true(grepl("^[0-9a-f]{40,64}$", base$snapshot))
    expect_false(identical(base$snapshot, head))
    expect_true(base$untracked$included)
    expect_identical(base$untracked$n, 1L)
    expect_identical(unlist(base$untracked$files), "notes.txt")

    # Taking it touched nothing: working tree, real index, HEAD, refs,
    # and stash are as they were.
    expect_identical(readLines(file.path(repo, "a.txt")), "two")
    expect_identical(unname(tools::md5sum(index_path)), before$index)
    expect_identical(readLines(file.path(repo, "notes.txt")), "draft v1")
    expect_identical(git("status", "--porcelain"), before$status)
    expect_identical(git("for-each-ref"), before$refs)
    expect_identical(git("stash", "list"), before$stash)
    expect_identical(git("rev-parse", "HEAD"), head)
    expect_identical(length(list.files(tempdir(),
                                       pattern = "^corteza-index-")), 0L)

    # The job: edits the tracked file, edits the never-committed file,
    # adds a new file, touches the ignored one.
    writeLines("three", file.path(repo, "a.txt"))
    writeLines("draft v2", file.path(repo, "notes.txt"))
    writeLines("brand new", file.path(repo, "added.txt"))
    writeLines("secret 2", file.path(repo, "ignored.txt"))
    end <- snap(repo, base = base)
    d <- git("diff", paste0(base$snapshot, "..", end$snapshot))
    # Only the job's changes: not the edit that predated it.
    expect_true(any(d == "-two"))
    expect_true(any(d == "+three"))
    expect_false(any(d == "-one"))
    # The never-committed file shows as an edit, not as a whole new
    # file and not as nothing.
    expect_true(any(d == "-draft v1"))
    expect_true(any(d == "+draft v2"))
    # The new file shows as added; the ignored one not at all.
    expect_true(any(d == "+brand new"))
    expect_false(any(grepl("secret", d)))
    # The reviewer's own tool reads that range.
    via_tool <- corteza:::call_skill(
        "git_diff", list(ref = paste0(base$snapshot, "..", end$snapshot),
                         path = repo), ctx = list())
    expect_false(isTRUE(via_tool$isError))
    expect_true(grepl("+draft v2", via_tool$content[[1L]]$text, fixed = TRUE))
    # Two snapshots of an unchanged tree differ in nothing.
    expect_identical(length(git("diff", paste0(end$snapshot, "..",
                                               snap(repo, base = end)$snapshot))),
                     0L)

    job <- list(id = "20261001T000000-aaaaaaaa", task = "fix a.txt",
                dispatch = list(base = base))
    task <- corteza:::job_review_task(job, "Changed a.txt; tests pass.", end)
    expect_true(grepl("fix a.txt", task, fixed = TRUE))
    expect_true(grepl("Changed a.txt; tests pass.", task, fixed = TRUE))
    expect_true(grepl(paste0(base$snapshot, "..", end$snapshot), task,
                      fixed = TRUE))
    expect_true(grepl("untracked files included", task))
    expect_true(grepl(".gitignore are not covered", task, fixed = TRUE))
    expect_true(grepl("VERDICT: approve", task, fixed = TRUE))

    # --- The two ends cover the same files ---
    # A file in one snapshot and not the other reads as added or deleted
    # when nothing touched it, so the end follows the start file by file.
    diff_of <- function(b, e) {
        git("diff", paste0(b$snapshot, "..", e$snapshot))
    }
    # The base record as the review reads it back from the ledger.
    stored <- function(b) {
        f <- tempfile(fileext = ".json")
        on.exit(unlink(f))
        corteza:::job_write_file(f, list(base = b))
        corteza:::job_read_file(f)$base
    }

    # Too much untracked at the start: those files are left out of both
    # ends, and the reviewer is told which and why.
    small_base <- snap(repo, max_untracked_bytes = 0)
    expect_false(small_base$untracked$included)
    expect_identical(small_base$untracked$n, 2L)
    expect_true(corteza:::job_is_sha(small_base$untracked$names))
    small_base <- stored(small_base)
    writeLines("draft v3", file.path(repo, "notes.txt"))
    writeLines("four", file.path(repo, "a.txt"))
    writeLines("made by the job", file.path(repo, "created.txt"))
    # The job stages a file that was untracked, and so not snapshotted,
    # at the start. Its content is no more comparable for being staged.
    git("add", "notes.txt")
    small_end <- snap(repo, base = small_base)
    d2 <- diff_of(small_base, small_end)
    expect_true(any(d2 == "+four"))
    expect_false(any(grepl("draft", d2)))
    expect_false(any(grepl("brand new", d2)))
    # What the job created is new at both ends' reckoning: shown.
    expect_true(any(d2 == "+made by the job"))
    expect_true(small_end$untracked$included)
    git("reset", "-q", "--", "notes.txt")
    job$dispatch$base <- small_base
    limited <- corteza:::job_review_task(job, "x", small_end)
    expect_true(grepl("2 file(s)", limited, fixed = TRUE))
    expect_true(grepl("NOT in that diff at either end", limited, fixed = TRUE))
    expect_true(grepl("notes.txt", limited, fixed = TRUE))
    expect_true(grepl("do not attribute", limited, fixed = TRUE))
    expect_true(grepl("Files this job created are in the diff", limited,
                      fixed = TRUE))
    expect_false(grepl("untracked files included", limited))
    # Still too much at the end: the job's new file is left out too, and
    # that is said separately.
    none_end <- snap(repo, max_untracked_bytes = 0, base = small_base)
    expect_false(none_end$untracked$included)
    expect_identical(unlist(none_end$untracked$files), "created.txt")
    expect_false(any(grepl("made by the job", diff_of(small_base, none_end))))
    neither <- corteza:::job_review_task(job, "x", none_end)
    expect_true(grepl("left 1 new untracked file(s)", neither, fixed = TRUE))
    expect_true(grepl("created.txt", neither, fixed = TRUE))
    expect_false(grepl("Files this job created are in the diff", neither,
                       fixed = TRUE))

    # Within the limit at the start, over it at the end. The files the
    # start covered stay covered: unchanged ones do not show as deleted,
    # and an edit to one shows as an edit. Only the new file is left out.
    full_base <- stored(snap(repo))
    expect_true(full_base$untracked$included)
    expect_null(full_base$untracked$names)
    writeLines(strrep("z", 5000), file.path(repo, "big-new.txt"))
    writeLines("draft v4", file.path(repo, "notes.txt"))
    over_end <- snap(repo, max_untracked_bytes = 1000, base = full_base)
    expect_false(over_end$untracked$included)
    expect_identical(unlist(over_end$untracked$files), "big-new.txt")
    d3 <- diff_of(full_base, over_end)
    expect_false(any(grepl("^deleted file", d3)))
    expect_true(any(d3 == "-draft v3"))
    expect_true(any(d3 == "+draft v4"))
    expect_false(any(grepl("big-new.txt", d3, fixed = TRUE)))
    job$dispatch$base <- full_base
    over <- corteza:::job_review_task(job, "x", over_end)
    expect_true(grepl("left 1 new untracked file(s)", over, fixed = TRUE))
    expect_true(grepl("big-new.txt", over, fixed = TRUE))
    expect_true(grepl("already untracked when the job", over, fixed = TRUE))
    expect_false(grepl("at either end", over, fixed = TRUE))
    unlink(file.path(repo, "big-new.txt"))

    # A file the start covered stays in the end when the job makes git
    # ignore it, instead of showing as deleted.
    writeLines(c("ignored.txt", "added.txt"), file.path(repo, ".gitignore"))
    d4 <- diff_of(full_base, snap(repo, base = full_base))
    expect_false(any(grepl("^deleted file", d4)))
    expect_true(any(d4 == "+added.txt"))
    writeLines("ignored.txt", file.path(repo, ".gitignore"))
    # One it removed does show as deleted.
    unlink(file.path(repo, "created.txt"))
    d5 <- diff_of(full_base, snap(repo, base = full_base))
    expect_identical(sum(grepl("^deleted file", d5)), 1L)
    expect_true(any(d5 == "-made by the job"))
    # So does a tracked file it removed.
    unlink(file.path(repo, "a.txt"))
    d5b <- diff_of(full_base, snap(repo, base = full_base))
    expect_identical(sum(grepl("^deleted file", d5b)), 2L)
    expect_true(any(d5b == "--- a/a.txt"))
    writeLines("four", file.path(repo, "a.txt"))

    if (.Platform$OS.type != "windows") {
        # Names git would quote in its default listing: counted at their
        # real size, snapshotted, and matched between the two ends.
        odd <- c("tab\there.txt", "new\nline.txt", "quo\"te.txt",
                 "café.txt", ":(top)a.txt", "-u")
        for (f in odd) {
            writeLines(strrep("x", 100), file.path(repo, f))
        }
        capped <- snap(repo, max_untracked_bytes = 300)
        expect_false(capped$untracked$included)
        expect_true(capped$untracked$bytes >= 600)
        odd_base <- stored(snap(repo))
        expect_true(odd_base$untracked$included)
        writeLines("edited", file.path(repo, odd[1L]))
        d6 <- diff_of(odd_base, snap(repo, base = odd_base))
        expect_identical(sum(grepl("^diff --git", d6)), 1L)
        expect_true(any(d6 == "+edited"))
        # Left out at the start, they stay out at the end by name, also
        # once staged.
        capped <- stored(capped)
        writeLines("edited again", file.path(repo, odd[2L]))
        git("add", "--", shQuote(odd[1L]), shQuote(odd[5L]))
        d7 <- diff_of(capped, snap(repo, base = capped))
        expect_identical(length(d7), 0L)
        git("reset", "-q")
        unlink(file.path(repo, odd))
    }

    # Covered at the start but not at the end is said.
    job$dispatch$base <- base
    expect_true(grepl("NOT in that diff",
                      corteza:::job_review_task(job, "x", none_end),
                      fixed = TRUE))
    # The limit comes from the config, in megabytes.
    expect_identical(corteza:::job_snapshot_max_bytes(
        list(config = list(jobs = list(review_snapshot_max_mb = 1)))),
        1024^2)
    expect_identical(corteza:::job_snapshot_max_bytes(list(config = list())),
                     corteza:::JOB_SNAPSHOT_MAX_BYTES)

    # A snapshot that could not be taken is said, not hidden.
    job$dispatch$base$snapshot <- NULL
    failed <- corteza:::job_review_task(job, "x", end)
    expect_true(grepl("could not be snapshotted", failed, fixed = TRUE))
    expect_true(grepl("Do not attribute", failed, fixed = TRUE))
    job$dispatch$base <- base
    expect_true(grepl("could not be snapshotted",
                      corteza:::job_review_task(job, "x", NULL),
                      fixed = TRUE))
    unlink(repo, recursive = TRUE)
}
expect_null(corteza:::job_git_snapshot(plain))
no_git <- corteza:::job_review_task(
    list(id = "20261001T000000-aaaaaaaa", task = "t", dispatch = list()), "")
expect_true(grepl("not a git repository", no_git, fixed = TRUE))
expect_true(grepl("returned no report", no_git, fixed = TRUE))

# --- Handing the lock over ---
local({
    mk <- function(task) {
        id <- corteza:::job_create(task, workspace = plain)
        corteza:::job_mark_dispatched(id)
        id
    }
    a <- mk("doer")
    r <- corteza:::job_create("review", role = "reviewer", workspace = plain,
                              review_of = a)
    w <- mk("another writer")
    # Only the current holder can hand over.
    expect_false(corteza:::job_lock_transfer(plain, a, r))
    expect_true(corteza:::job_lock_acquire(plain, a)$ok)
    expect_false(corteza:::job_lock_transfer(plain, w, r))
    expect_true(corteza:::job_lock_transfer(plain, a, r, owner = "@c:ex"))
    h <- holder(plain)
    expect_identical(h$job, r)
    expect_identical(h$transferred_from, a)
    # The doer's job ending, and its release, free nothing: the queued
    # review holds the checkout.
    corteza:::job_settle(a, "done")
    corteza:::job_lock_release(plain, a)
    expect_identical(holder(plain)$job, r)
    expect_false(corteza:::job_lock_acquire(plain, w)$ok)
    # A released lock cannot be handed over.
    corteza:::job_lock_release(plain, r)
    expect_false(corteza:::job_lock_transfer(plain, r, w))
    expect_true(corteza:::job_lock_acquire(plain, w)$ok)
    corteza:::job_lock_release(plain, w)
    for (id in c(r, w)) corteza:::job_settle(id, "done")
})

# --- A review does not run on a checkout someone else held since ---
# The lock handed to a queued review goes stale if its owning process
# dies. Another bot's job can then take the checkout and change it, and
# the review is still queued when its owner comes back.
review_fn <- function(task) list(reply = "VERDICT: approve")
restart_case <- function(intruder) {
    dir <- normalizePath({
        d <- tempfile("restart-checkout")
        dir.create(d)
        d
    })
    key <- paste0("!restart-", basename(dir), ":ex")
    d_id <- corteza:::job_create("doer work", workspace = dir,
                                 owner = "@claude:ex",
                                 origin = list(session_key = key))
    corteza:::job_mark_dispatched(d_id)
    corteza:::job_lock_acquire(dir, d_id, owner = "@claude:ex")
    r_id <- corteza:::job_create("Review the work", role = "reviewer",
                                 workspace = dir, owner = "@claude:ex",
                                 origin = list(session_key = key),
                                 review_of = d_id)
    stopifnot(corteza:::job_lock_transfer(dir, d_id, r_id, "@claude:ex"))
    corteza:::job_settle(d_id, "done", result = "did it")
    # The owning process dies: the review's reservation now names a
    # process that is gone.
    cur <- corteza:::job_lock_current(dir)
    hp <- file.path(corteza:::job_lock_gen_dir(corteza:::job_lock_root(dir),
                                               cur$gen), "holder.json")
    h <- jsonlite::fromJSON(hp)
    p <- processx::process$new("true")
    p$wait()
    h$pid <- p$get_pid()
    corteza:::job_write_file(hp, h)
    w_id <- NULL
    if (intruder) {
        # Another bot's job takes the stale lock, edits, and finishes.
        w_id <- corteza:::job_create("other bot's edit", workspace = dir,
                                     owner = "@codex:ex",
                                     origin = list(session_key = "!x:ex"))
        corteza:::job_mark_dispatched(w_id)
        stopifnot(corteza:::job_lock_acquire(dir, w_id, "@codex:ex")$ok)
        corteza:::job_lock_release(dir, w_id)
        corteza:::job_settle(w_id, "done")
    }
    # The owner restarts and pumps its room.
    s <- corteza::new_session("cli")
    s$cwd <- dir
    s$config <- list()
    s$job_key <- key
    s$job_owner <- "@claude:ex"
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = review_fn)
    events <- list()
    deadline <- Sys.time() + 30
    repeat {
        events <- c(events, corteza:::job_pump(s))
        if (corteza:::job_read(r_id)$status %in%
            corteza:::JOB_STATUSES_FINAL || Sys.time() > deadline) {
            break
        }
        Sys.sleep(0.1)
    }
    corteza:::job_worker_close_all(s)
    list(review = corteza:::job_read(r_id), intruder = w_id, dir = dir,
         doer = d_id, events = events)
}
if (.Platform$OS.type != "windows") {
    broken <- restart_case(intruder = TRUE)
    expect_identical(broken$review$status, "cancelled")
    expect_true(grepl("not held continuously", broken$review$outcome$reason))
    expect_true(grepl(broken$intruder, broken$review$outcome$reason,
                      fixed = TRUE))
    # It never reached a worker, and it does not keep the checkout.
    expect_null(broken$review$dispatch)
    expect_null(holder(broken$dir))
    # The room is told, as for any ended job.
    told <- Filter(function(e) identical(e$type, "settled"), broken$events)
    expect_identical(length(told), 1L)
    expect_true(grepl("Ask for the review again",
                      corteza:::bot_job_result_text(told[[1L]]$job)))
    # A restart with nobody in between is not a break: the review takes
    # the checkout again and runs.
    intact <- restart_case(intruder = FALSE)
    expect_identical(intact$review$status, "done")
    expect_identical(intact$review$outcome$verdict, "approve")
}
# The rule itself, on the lock history.
local({
    dir <- normalizePath({
        d <- tempfile("unbroken")
        dir.create(d)
        d
    })
    mk <- function(task) {
        id <- corteza:::job_create(task, workspace = dir)
        corteza:::job_mark_dispatched(id)
        id
    }
    unbroken <- corteza:::job_review_unbroken
    a <- mk("doer")
    r <- mk("review")
    w <- mk("writer")
    # The doer never held the checkout: nothing shows the tree is its.
    expect_false(unbroken(dir, a, r)$ok)
    corteza:::job_lock_acquire(dir, a)
    # The review holds nothing yet.
    expect_false(unbroken(dir, a, r)$ok)
    corteza:::job_lock_transfer(dir, a, r)
    expect_true(unbroken(dir, a, r)$ok)
    # A hand-off that failed is fine too, as long as the review was next.
    b <- mk("doer 2")
    r2 <- mk("review 2")
    corteza:::job_lock_release(dir, r)
    corteza:::job_lock_acquire(dir, b)
    corteza:::job_settle(b, "done")
    corteza:::job_lock_acquire(dir, r2)
    expect_true(unbroken(dir, b, r2)$ok)
    # Someone in between breaks it, and is named.
    corteza:::job_lock_release(dir, r2)
    c2 <- mk("doer 3")
    r3 <- mk("review 3")
    corteza:::job_lock_acquire(dir, c2)
    corteza:::job_settle(c2, "done")
    corteza:::job_lock_acquire(dir, w)
    corteza:::job_lock_release(dir, w)
    corteza:::job_lock_acquire(dir, r3)
    res <- unbroken(dir, c2, r3)
    expect_false(res$ok)
    expect_identical(res$by, w)
})
# A hand-off that failed is not described as a locked checkout.
unlocked <- list(status = "done",
                 outcome = list(review_job = "20261001T000000-bbbbbbbb",
                                review_locked = FALSE))
note <- corteza:::job_outcome_notes(unlocked)
expect_true(grepl("could not be handed", note))
expect_false(grepl("stays locked", note))

# --- End to end: doer, review, and the writers waiting behind them ---
run_fn <- function(task) {
    if (startsWith(task, "Review the work")) {
        Sys.sleep(1.5)
        return(list(reply = paste("VERDICT: changes requested",
                                  "R/a.R:3 off by one", sep = "\n")))
    }
    if (identical(task, "boom")) {
        stop("doer failed")
    }
    n <- get0("jobs_seen", envir = globalenv(), ifnotfound = 0) + 1
    assign("jobs_seen", n, envir = globalenv())
    Sys.sleep(as.numeric(task))
    list(reply = sprintf("did it #%d", n))
}
make_session <- function(key, owner) {
    s <- corteza::new_session("cli")
    s$cwd <- plain
    s$config <- list()
    s$job_key <- key
    s$job_owner <- owner
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = run_fn)
    s
}
claude <- make_session("!viento:ex", "@claude:ex")
codex <- make_session("!vientox:ex", "@codex:ex")
final <- function(id) {
    corteza:::job_read(id)$status %in% corteza:::JOB_STATUSES_FINAL
}

d <- corteza:::job_submit(claude, "1", review = TRUE)
expect_true(corteza:::job_read(d)$review)
# A second job from the same room, and a writer from another bot, both
# queued while the doer runs.
q <- corteza:::job_submit(claude, "0")
w <- corteza:::job_submit(codex, "0")
seen <- list()
deadline <- Sys.time() + 60
r <- NULL
repeat {
    seen <- c(seen, corteza:::job_pump(claude), corteza:::job_pump(codex))
    r <- corteza:::job_read(d)$outcome$review_job
    if ((!is.null(r) && final(r) && final(q) && final(w)) ||
        Sys.time() > deadline) {
        break
    }
    Sys.sleep(0.1)
}
dj <- corteza:::job_read(d)
rj <- corteza:::job_read(r)
qj <- corteza:::job_read(q)
wj <- corteza:::job_read(w)

expect_identical(dj$status, "done")
# The lock went to the review directly.
expect_true(isTRUE(dj$outcome$review_locked))
expect_identical(rj$status, "done")
expect_identical(rj$role, "reviewer")
expect_identical(rj$review_of, d)
expect_identical(rj$owner, "@claude:ex")
expect_identical(rj$origin$session_key, "!viento:ex")
expect_identical(rj$outcome$verdict, "changes requested")
expect_true(grepl("did it #1", rj$task, fixed = TRUE))
# A review is not itself reviewed.
expect_false(isTRUE(rj$review))
expect_null(rj$outcome$review_job)

# Nothing wrote to the checkout between the doer finishing and the
# review ending: both waiting writers started after the review ended.
expect_identical(qj$status, "done")
expect_identical(wj$status, "done")
review_end <- ts(rj$outcome$finished_at)
expect_true(ts(qj$dispatch$dispatched_at) >= review_end)
expect_true(ts(wj$dispatch$dispatched_at) >= review_end)
# The review ran ahead of the older queued job it was blocking.
expect_true(ts(rj$dispatch$dispatched_at) <= ts(qj$dispatch$dispatched_at))

# The doer and the reviewer are separate processes, and the doer's
# survived the review: the next doer job ran in the same process with
# its state intact, and nothing had to be restored.
expect_false(identical(rj$dispatch$worker$pid, dj$dispatch$worker$pid))
expect_identical(qj$dispatch$worker$pid, dj$dispatch$worker$pid)
expect_identical(qj$outcome$result, "did it #2")
types <- vapply(seen, function(e) e$type, "")
expect_false("restored" %in% types)
# Each waiting writer was told it was blocked.
expect_true(sum(types == "blocked") >= 2L)
expect_null(holder(plain))

# --- What the surfaces say ---
expect_identical(corteza:::job_title(rj), sprintf("review of job %s", d))
expect_true(any(grepl(r, corteza:::job_outcome_notes(dj), fixed = TRUE)))
expect_true(any(grepl("stays locked", corteza:::job_outcome_notes(dj))))
expect_identical(corteza:::job_outcome_notes(rj),
                 "Verdict: changes requested.")
room_text <- corteza:::bot_job_result_text(rj)
expect_true(grepl(sprintf("review of job %s", d), room_text, fixed = TRUE))
expect_true(grepl("R/a.R:3 off by one", room_text, fixed = TRUE))
expect_true(grepl("Verdict: changes requested.", room_text, fixed = TRUE))
# The review's long instruction text is not used as its title.
expect_false(grepl("How to see the change", room_text, fixed = TRUE))
expect_true(grepl("Review queued as job",
                  corteza:::bot_job_result_text(dj), fixed = TRUE))
expect_true(grepl("Review queued as job",
                  corteza:::.repl_job_result_text(dj), fixed = TRUE))
expect_true(grepl("review of job", corteza:::format_job(rj), fixed = TRUE))
# A job with no review says nothing about one.
expect_identical(length(corteza:::job_outcome_notes(qj)), 0L)
# A review that gave no verdict says so.
none <- rj
none$outcome$verdict <- NULL
expect_identical(corteza:::job_outcome_notes(none), "Verdict: none given.")

# --- No review of work that did not finish ---
f <- corteza:::job_submit(claude, "boom", review = TRUE)
deadline <- Sys.time() + 30
while (!final(f) && Sys.time() < deadline) {
    corteza:::job_pump(claude)
    Sys.sleep(0.1)
}
fj <- corteza:::job_read(f)
expect_identical(fj$status, "failed")
expect_null(fj$outcome$review_job)
expect_identical(length(Filter(function(j) identical(j$review_of, f),
                               corteza:::job_list())), 0L)
expect_null(holder(plain))
# Only a job that writes can be reviewed.
rr <- corteza:::job_create("x", role = "reviewer", workspace = plain,
                           origin = list(session_key = "none"))
expect_false(corteza:::job_read(rr)$review)
s_tmp <- make_session("!none:ex", "@claude:ex")
expect_false(corteza:::job_read(
    corteza:::job_submit(s_tmp, "Review the work", role = "reviewer",
                         review = TRUE))$review)
corteza:::job_cancel(s_tmp, corteza:::job_list(origin_key = "!none:ex")[[1L]]$id)
corteza:::job_pump(s_tmp)

# --- The real init applies the reviewer's limits in the child ---
probe <- make_session("!probe:ex", "@claude:ex")
# The reasoning settings in the spec reach the child's session too.
probe$reasoning_effort <- "high"
probe$job_worker_spec <- list(run_fn = function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    list(reply = paste(
        identical(st$session$tools_filter, corteza:::JOB_REVIEWER_TOOLS),
        isFALSE(st$session$web_search),
        identical(getOption("corteza.allowed_paths"), getwd()),
        identical(st$session$reasoning_effort, "high"),
        sep = " "))
})
pr <- corteza:::job_create("probe", role = "reviewer", workspace = plain,
                           owner = "@claude:ex",
                           origin = list(session_key = "!probe:ex"))
deadline <- Sys.time() + 30
while (!final(pr) && Sys.time() < deadline) {
    corteza:::job_pump(probe)
    Sys.sleep(0.1)
}
expect_identical(corteza:::job_read(pr)$outcome$result, "TRUE TRUE TRUE TRUE")

# --- delegate(review = ...) ---
corteza::ensure_skills()
tools <- corteza:::skills_as_api_tools("delegate")
expect_identical(tools[[1L]]$input_schema$properties$review$type, "boolean")
expect_false("review" %in% tools[[1L]]$input_schema$required)
t <- make_session("!tool:ex", "@claude:ex")
t$job_worker_spec$run_fn <- function(task) {
    Sys.sleep(30)
    list(reply = "late")
}
job_of <- function(res) {
    corteza:::job_read(regmatches(res$content[[1L]]$text,
        regexpr("[0-9]{8}T[0-9]{6}-[0-9a-f]{8}", res$content[[1L]]$text)))
}
res <- corteza:::tool_delegate("x", review = TRUE, ctx = list(session = t))
expect_true(job_of(res)$review)
expect_true(grepl("review of the work will follow", res$content[[1L]]$text))
plain_res <- corteza:::tool_delegate("x", ctx = list(session = t))
expect_false(job_of(plain_res)$review)
expect_false(grepl("review", plain_res$content[[1L]]$text))
# The config sets the default; an explicit FALSE still wins.
t$config <- list(jobs = list(review = TRUE))
expect_true(job_of(corteza:::tool_delegate("x",
                                           ctx = list(session = t)))$review)
expect_false(job_of(corteza:::tool_delegate("x", review = FALSE,
                                            ctx = list(session = t)))$review)

for (s in list(claude, codex, s_tmp, probe, t)) {
    corteza:::job_worker_close_all(s)
}
if (is.na(old_state)) {
    Sys.unsetenv("CORTEZA_STATE_DIR")
} else {
    Sys.setenv(CORTEZA_STATE_DIR = old_state)
}
unlink(c(state, plain), recursive = TRUE)
