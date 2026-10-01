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
    git("init", "-q")
    # stash create needs an identity; set it on the repo, not globally.
    git("config", "user.name", "t")
    git("config", "user.email", "t@example.com")
    # Before the first commit there is nothing to diff against.
    expect_null(corteza:::job_git_base(repo))
    writeLines("one", file.path(repo, "a.txt"))
    git("add", "a.txt")
    git("commit", "-q", "-m", "first")
    head <- git("rev-parse", "HEAD")

    clean <- corteza:::job_git_base(repo)
    expect_identical(clean$head, head)
    expect_identical(clean$snapshot, head)
    expect_false(clean$dirty)
    expect_true(clean$exact)
    # An untracked file is not a tracked change.
    writeLines("new", file.path(repo, "untracked.txt"))
    expect_false(corteza:::job_git_base(repo)$dirty)

    writeLines("two", file.path(repo, "a.txt"))
    dirty <- corteza:::job_git_base(repo)
    expect_true(dirty$dirty)
    expect_true(dirty$exact)
    expect_false(identical(dirty$snapshot, head))
    # Taking the snapshot touched nothing: the edit is still in the
    # working tree, HEAD has not moved, and no stash entry was made.
    expect_identical(readLines(file.path(repo, "a.txt")), "two")
    expect_identical(git("rev-parse", "HEAD"), head)
    expect_identical(length(git("stash", "list")), 0L)
    # The snapshot holds the earlier edit, so a later diff against it
    # shows only what came after.
    writeLines("three", file.path(repo, "a.txt"))
    d <- git("diff", dirty$snapshot, "--", "a.txt")
    expect_true(any(d == "-two"))
    expect_true(any(d == "+three"))
    expect_false(any(d == "-one"))

    job <- list(id = "20261001T000000-aaaaaaaa", task = "fix a.txt",
                dispatch = list(base = dirty))
    task <- corteza:::job_review_task(job, "Changed a.txt; tests pass.")
    expect_true(grepl("fix a.txt", task, fixed = TRUE))
    expect_true(grepl("Changed a.txt; tests pass.", task, fixed = TRUE))
    expect_true(grepl(dirty$snapshot, task, fixed = TRUE))
    expect_true(grepl("this job's work only", task, fixed = TRUE))
    expect_true(grepl("VERDICT: approve", task, fixed = TRUE))
    # A snapshot that could not be taken is said, not hidden.
    job$dispatch$base$exact <- FALSE
    expect_true(grepl("could not be separated",
                      corteza:::job_review_task(job, "x"), fixed = TRUE))
    unlink(repo, recursive = TRUE)
}
expect_null(corteza:::job_git_base(plain))
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
probe$job_worker_spec <- list(run_fn = function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    list(reply = paste(
        identical(st$session$tools_filter, corteza:::JOB_REVIEWER_TOOLS),
        isFALSE(st$session$web_search),
        identical(getOption("corteza.allowed_paths"), getwd()),
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
expect_identical(corteza:::job_read(pr)$outcome$result, "TRUE TRUE TRUE")

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
