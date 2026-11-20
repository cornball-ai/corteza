# A worker obeys its conversation's plan mode, approval mode and dry run
# (R/job-authority.R, WP20a rule 7). Offline: the model is replaced in
# the worker's process, and no config of the user's is read.
source("isolate.R", local = TRUE)
Sys.setenv(R_USER_CONFIG_DIR = tempfile("corteza_config_"))

ns <- asNamespace("corteza")

# ---- The record ----------------------------------------------------------
local({
    root <- tempfile("authority-unit-")
    dir.create(root)
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA_character_)
    Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"))
    on.exit({
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    s <- corteza::new_session("cli", provider = "anthropic")
    s$config <- list()
    s$job_key <- "unit"
    s$sessionId <- "conv-1"
    id <- ns$job_create("fixture", workspace = root,
                        origin = list(session_key = "unit"))

    # What the session allows, with dry run read from either place.
    expect_identical(ns$job_authority_now(s),
                     list(plan_mode = FALSE, approval_mode = "ask", dry_run = FALSE))
    s$config$dry_run <- TRUE
    expect_true(ns$job_authority_now(s)$dry_run)
    s$config$dry_run <- NULL
    s$dry_run <- TRUE
    expect_true(ns$job_authority_now(s)$dry_run)
    s$dry_run <- NULL

    # Each write takes the next number.
    expect_identical(ns$job_authority_write(s, id), 1L)
    rec <- ns$job_authority_read(id, "unit")
    expect_true(rec$ok)
    expect_equal(rec$seq, 1)
    expect_false(rec$plan_mode)

    # The running job's record follows the session, and only when a
    # setting changed.
    s$.job_current <- id
    expect_false(ns$job_authority_sync(s))
    s$plan_mode <- TRUE
    expect_true(ns$job_authority_sync(s))
    rec <- ns$job_authority_read(id, "unit")
    expect_equal(rec$seq, 2)
    expect_true(rec$plan_mode)
    expect_false(ns$job_authority_sync(s))

    # Only the conversation that started the job writes it: after the
    # terminal's /clear (a new session id), or in a room's replacement
    # session, which does not carry the last write, the record stays.
    s$sessionId <- "conv-2"
    s$plan_mode <- FALSE
    expect_false(ns$job_authority_sync(s))
    expect_true(ns$job_authority_read(id, "unit")$plan_mode)
    fresh <- corteza::new_session("cli", provider = "anthropic")
    fresh$config <- list()
    fresh$job_key <- "unit"
    fresh$.job_current <- id
    expect_false(ns$job_authority_sync(fresh))
    expect_true(ns$job_authority_read(id, "unit")$plan_mode)

    # A write that fails leaves no record, and the next one still takes a
    # number above any before it.
    s$sessionId <- "conv-1"
    real_write <- ns$job_write_file
    assignInNamespace("job_write_file", function(path, x) stop("disk full"),
                      ns = "corteza")
    failed <- tryCatch(ns$job_authority_write(s, id), error = conditionMessage)
    assignInNamespace("job_write_file", real_write, ns = "corteza")
    expect_identical(failed, "disk full")
    expect_false(file.exists(ns$job_authority_path(id)))
    expect_identical(ns$job_authority_read(id, "unit")$reason,
                     "this job has no authority record")
    expect_identical(ns$job_authority_write(s, id), 3L)

    # What a worker refuses to run under.
    path <- ns$job_authority_path(id)
    good <- ns$job_read_file(path)
    put <- function(x) ns$job_write_file(path, x)
    expect_identical(ns$job_authority_read(NULL, "unit")$reason,
                     "no job is running")
    expect_identical(ns$job_authority_read(id, "other")$reason,
                     "this job's authority record names another conversation")
    expect_identical(ns$job_authority_read(id, NULL)$reason,
                     "this job's authority record names another conversation")
    put(utils::modifyList(good, list(job = "20260101T000000-00000000")))
    expect_identical(ns$job_authority_read(id, "unit")$reason,
                     "this job's authority record names another job")
    put(utils::modifyList(good, list(approval_mode = "whatever")))
    expect_identical(ns$job_authority_read(id, "unit")$reason,
                     "this job's authority record could not be read")
    put(utils::modifyList(good, list(plan_mode = "yes")))
    expect_identical(ns$job_authority_read(id, "unit")$reason,
                     "this job's authority record could not be read")
    writeLines("{not json", path)
    expect_identical(ns$job_authority_read(id, "unit")$reason,
                     "this job's authority record could not be read")
    put(good)
    expect_true(ns$job_authority_read(id, "unit")$ok)
})

# ---- turn()'s handler -----------------------------------------------------
# A session with an `authority` reads it before each call and again just
# before the call runs.
local({
    s <- corteza::new_session("cli", provider = "anthropic")
    s$config <- list()
    state <- list(ok = TRUE, seq = 1, plan_mode = FALSE,
                  approval_mode = "allow", dry_run = FALSE)
    s$authority <- function() state
    s$plan_mode_locked <- TRUE
    ran <- character()
    h <- ns$.make_tool_handler(s, tool_executor = function(name, args) {
        ran <<- c(ran, name)
        list(content = list(list(type = "text", text = "ran")))
    })
    write <- function() h("write_file", list(path = "notes.md", content = "x"))

    expect_identical(write(), "ran")
    expect_identical(s$config$approval_mode, "allow")

    # Plan mode from the record: a write is refused, a read runs. The
    # refusal does not send the worker to exit_plan_mode.
    state$plan_mode <- TRUE
    out <- write()
    expect_true(grepl("corteza policy denied", out, fixed = TRUE))
    expect_true(grepl("only the person can end plan mode", out, fixed = TRUE))
    expect_false(grepl("via exit_plan_mode", out, fixed = TRUE))
    expect_identical(h("read_file", list(path = "notes.md")), "ran")

    # The worker cannot end plan mode, whatever its tool list holds.
    out <- h("exit_plan_mode", list(plan = "p"))
    expect_true(grepl("only the person can end it", out, fixed = TRUE))
    expect_true(isTRUE(s$plan_mode))

    # Approval mode from the record: "deny" makes the write's ask a deny.
    state$plan_mode <- FALSE
    state$approval_mode <- "deny"
    expect_true(grepl("corteza policy denied", write(), fixed = TRUE))

    # Dry run from the record reaches both places the handler reads it.
    # (This executor stands in for the tool; the worker's own executor
    # previews instead of writing, which the worker test below shows.)
    state$approval_mode <- "allow"
    state$dry_run <- TRUE
    write()
    expect_true(isTRUE(s$dry_run))
    expect_true(isTRUE(s$config$dry_run))
    state$dry_run <- FALSE

    # A record it cannot use refuses everything, before anything runs.
    state <- list(ok = FALSE, reason = "this job has no authority record")
    before <- ran
    out <- h("read_file", list(path = "notes.md"))
    expect_identical(out, paste("[job policy denied: this job has no",
                                "authority record; nothing ran]"))
    expect_identical(ran, before)

    # A change while the call waited for an approval: it does not run.
    state <- list(ok = TRUE, seq = 5, plan_mode = FALSE,
                  approval_mode = "ask", dry_run = FALSE)
    s$approval_cb <- function(call, decision, ...) {
        state <<- utils::modifyList(state, list(seq = 6, plan_mode = TRUE))
        TRUE
    }
    before <- ran
    out <- write()
    expect_true(grepl("settings changed while this call waited", out,
                      fixed = TRUE))
    expect_identical(ran, before)
    # Unchanged, the approved call runs.
    state$plan_mode <- FALSE
    s$approval_cb <- function(call, decision, ...) TRUE
    expect_identical(write(), "ran")
    # A record that went missing during the wait refuses too.
    s$approval_cb <- function(call, decision, ...) {
        state <<- list(ok = FALSE, reason = "this job has no authority record")
        TRUE
    }
    before <- ran
    out <- write()
    expect_true(grepl("this job has no authority record; it did not run",
                      out, fixed = TRUE))
    expect_identical(ran, before)
})

# ---- Review from the record (WP20c) ---------------------------------------
# A doer at the terminal or in chat() holds a review gate, which the
# record turns on before each call while its conversation is in review.
# A room's worker keeps its room's gate whatever the record says, and
# the monitor holds neither.
local({
    s <- corteza::new_session("cli", provider = "anthropic")
    s$config <- list()
    state <- list(ok = TRUE, seq = 1, plan_mode = FALSE,
                  approval_mode = "ask", dry_run = FALSE)
    s$authority <- function() state
    s$plan_mode_locked <- TRUE
    ns$.job_worker_child_supervise(s, list(role = "doer"))
    expect_null(s$auto_gate)
    expect_true(is.function(s$.job_review_gate))
    reviewed <- 0L
    s$.job_review_gate <- function(call, decision) {
        reviewed <<- reviewed + 1L
        list(action = "proceed", reason = "")
    }
    prompted <- 0L
    s$approval_cb <- function(call, decision, ...) {
        prompted <<- prompted + 1L
        TRUE
    }
    h <- ns$.make_tool_handler(s, tool_executor = function(name, args) {
        list(content = list(list(type = "text", text = "ran")))
    })
    write <- function() h("write_file", list(path = "notes.md", content = "x"))
    expect_identical(write(), "ran")
    expect_identical(c(prompted, reviewed), c(1L, 0L))
    state$approval_mode <- "review"
    expect_identical(write(), "ran")
    expect_identical(c(prompted, reviewed), c(1L, 1L))
    state$approval_mode <- "ask"
    expect_identical(write(), "ran")
    expect_identical(c(prompted, reviewed), c(2L, 1L))
    expect_null(s$auto_gate)

    room <- list(role = "doer", supervise = list(mode = "human"))
    ns$.job_worker_child_supervise(s, room)
    expect_null(s$.job_review_gate)
    gate <- s$auto_gate
    expect_true(is.function(gate))
    for (mode in c("review", "ask")) {
        state$approval_mode <- mode
        ns$.authority_apply(s)
        expect_identical(s$auto_gate, gate)
    }
    ns$.job_worker_child_supervise(s, list(role = "monitor"))
    expect_null(s$auto_gate)
    expect_null(s$.job_review_gate)
})

# The review gate itself: a call policy() allowed runs and one it
# refused is refused, unread by the rules; of a call that would prompt,
# what the rules leave open goes to the reviewer and what they catch to
# the person.
local({
    orig <- ns$.job_worker_child_supervised_ask
    asked <- character()
    assignInNamespace(".job_worker_child_supervised_ask",
                      function(route, call, decision, notes) {
        asked <<- c(asked, route)
        list(approved = TRUE, by = "monitor", reason = "fits the task")
    }, "corteza")
    on.exit(assignInNamespace(".job_worker_child_supervised_ask", orig,
                              "corteza"), add = TRUE)
    cwd <- tempfile("review-gate-")
    dir.create(cwd)
    cwd <- normalizePath(cwd)
    on.exit(unlink(cwd, recursive = TRUE), add = TRUE)
    gate <- ns$.job_worker_child_review_gate(cwd)
    w <- function(path) {
        list(tool = "write_file", class = "write",
             args = list(path = path, content = "x"))
    }
    # Outside the project and every scratch folder the rules allow
    # (scan_new()). Nothing here writes it.
    outside <- file.path(normalizePath("/", winslash = "/", mustWork = FALSE),
                         "corteza-review-gate-elsewhere.R")
    expect_identical(gate(w(outside), list(approval = "allow"))$action,
                     "proceed")
    expect_identical(gate(w(outside), list(approval = "deny",
                                           reason = "no"))$action, "deny")
    expect_identical(asked, character())
    expect_identical(gate(w(file.path(cwd, "x.R")),
                          list(approval = "ask", reason = "a write"))$action,
                     "proceed")
    gate(w(outside), list(approval = "ask", reason = "a write"))
    expect_identical(asked, c("monitor", "human"))
})

# ---- Plan mode in a worker's request --------------------------------------
local({
    tool <- function(n) list(name = n, description = n, input_schema = list())
    tools <- list(tool("read_file"), tool("exit_plan_mode"))
    names_of <- function(x) vapply(x, function(t) t$name, "")
    expect_identical(names_of(ns$.plan_mode_filter_tools(tools, TRUE)),
                     c("read_file", "exit_plan_mode"))
    expect_identical(names_of(ns$.plan_mode_filter_tools(tools, TRUE,
                                                         offer_exit = FALSE)),
                     "read_file")
    expect_true(grepl("Only the person can end plan mode",
                      ns$.plan_mode_note(TRUE, locked = TRUE), fixed = TRUE))
    expect_false(grepl("exit_plan_mode", ns$.plan_mode_note(TRUE, locked = TRUE),
                       fixed = TRUE))
    expect_false(grepl("Only the person", ns$.plan_mode_note(TRUE), fixed = TRUE))
})

# ---- A warm worker, through real turns ------------------------------------
# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}
# One worker process runs every job below. Its model is a script: the
# task names the calls to make, in order, and its last reply reports the
# model it was asked for, whether exit_plan_mode was offered, whether
# the plan-mode text was in its prompt, and the tool results it saw. A
# step `write <file> <gate>` waits for the file <gate> before calling.
local({
    root <- tempfile("authority-jobs-")
    dir.create(root)
    root <- normalizePath(root, winslash = "/")
    old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA_character_)
    Sys.setenv(CORTEZA_STATE_DIR = file.path(root, "state"))
    on.exit({
        if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else
            Sys.setenv(CORTEZA_STATE_DIR = old_state)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    writeLines("fixture", file.path(root, "marker.txt"))
    s <- corteza::new_session("cli", provider = "anthropic", web_search = FALSE,
                              model_map = list(cloud = "model-a", local = NULL))
    s$cwd <- root
    s$config <- list(approval_mode = "allow", jobs = list(max_turns = 6L))
    s$job_key <- "authority-fixture"
    s$job_worker_spec <- list(
        tools = c("read_file", "write_file", "exit_plan_mode"),
        init_fn = function(spec) {
            Sys.setenv(ANTHROPIC_API_KEY = "offline-fixture")
            corteza::subagent_turn_init(provider = "anthropic", model = "fixture",
                tools_filter = spec$tools, system = "fixture", max_turns = 6L,
                web_search = FALSE)
            session <- get(".subagent_state", asNamespace("corteza"))$session
            session$config <- list(context_edit = list(enabled = FALSE),
                                   context_compact_pct = 100)
            utils::assignInNamespace(".agent_anthropic", function(messages, tools,
                                                                  system, model, ...) {
                text_of <- function(m) {
                    b <- m$content
                    if (is.character(b)) return(paste(b, collapse = " "))
                    paste(unlist(lapply(b, function(x) {
                        inner <- x$content
                        if (is.list(inner)) {
                            inner <- vapply(inner, function(y) {
                                if (is.null(y$text)) "" else y$text
                            }, "")
                        }
                        c(x$text, inner)
                    })), collapse = " ")
                }
                # The worker keeps its history between jobs, so this job's
                # task is the newest user message that holds a script.
                is_task <- vapply(messages, function(m) {
                    identical(m$role, "user") &&
                        grepl("script:", text_of(m), fixed = TRUE)
                }, logical(1))
                at <- max(which(is_task))
                task <- text_of(messages[[at]])
                script <- sub("^script: *", "",
                              regmatches(task, regexpr("script:[^\n]*", task)))
                steps <- strsplit(script, " ; ", fixed = TRUE)[[1L]]
                steps <- steps[nzchar(steps)]
                after <- messages[-seq_len(at)]
                step <- length(after) %/% 2L + 1L
                usage <- list(input_tokens = 1L, output_tokens = 1L)
                if (step > length(steps)) {
                    users <- Filter(function(m) identical(m$role, "user"), after)
                    seen <- paste(vapply(users, text_of, ""), collapse = " | ")
                    offered <- vapply(tools, function(t) {
                        if (is.null(t$name)) "" else t$name
                    }, "")
                    prompt <- paste(unlist(system), collapse = " ")
                    asked <- paste(vapply(Filter(function(m) identical(m$role, "user"),
                                                 messages), text_of, ""),
                                   collapse = " ")
                    # Plan mode reaches a worker as a note in its job's
                    # prompt; the system text stays as it is (WP14).
                    budget <- list(...)$max_tokens
                    text <- sprintf("model=%s exit_offered=%s plan_note=%s plan_system=%s results=%s max_tokens=%s",
                                    model, "exit_plan_mode" %in% offered,
                                    grepl("[Plan mode is on", asked, fixed = TRUE),
                                    grepl("Plan mode", prompt, fixed = TRUE),
                                    seen, if (is.null(budget)) "none" else budget)
                    return(list(text = text, tool_calls = list(), usage = usage,
                                assistant_message = list(role = "assistant",
                                    content = list(list(type = "text", text = text)))))
                }
                words <- strsplit(steps[[step]], " ", fixed = TRUE)[[1L]]
                if (length(words) >= 3L) {
                    writeLines("waiting", paste0(words[[3L]], ".waiting"))
                    deadline <- Sys.time() + 30
                    while (!file.exists(words[[3L]]) && Sys.time() < deadline) {
                        Sys.sleep(0.05)
                    }
                }
                name <- switch(words[[1L]], write = "write_file",
                               read = "read_file", exit = "exit_plan_mode")
                args <- switch(words[[1L]],
                               write = list(path = file.path(getwd(), words[[2L]]),
                                            content = "x"),
                               read = list(path = file.path(getwd(), "marker.txt")),
                               exit = list(plan = "fixture plan"))
                id <- paste0("call-", length(messages))
                list(text = "", usage = usage,
                     tool_calls = list(list(id = id, name = name, arguments = args)),
                     assistant_message = list(role = "assistant", content = list(
                         list(type = "tool_use", id = id, name = name, input = args))))
            }, "llm.api")
        })
    on.exit(corteza:::job_worker_close_all(s), add = TRUE)
    at <- function(x) file.path(root, x)
    unasked <- 0L
    pump_until <- function(done, on_event = function(ev) NULL) {
        deadline <- Sys.time() + 60
        repeat {
            for (ev in ns$job_pump(s)) on_event(ev)
            if (isTRUE(done())) return(invisible(TRUE))
            if (Sys.time() > deadline) stop("fixture job timed out")
            Sys.sleep(0.05)
        }
    }
    run <- function(task, gate = NULL, while_waiting = NULL, on_event = NULL) {
        id <- ns$job_submit(s, task)
        if (!is.null(gate)) {
            pump_until(function() file.exists(at(paste0(gate, ".waiting"))))
            while_waiting(id)
            ns$job_pump(s)
            writeLines("go", at(gate))
        }
        final <- function() ns$job_read(id)$status %in% ns$JOB_STATUSES_FINAL
        # Only the approval test expects a question. Any other is
        # declined, so the job ends, and counted, so the test fails.
        if (is.null(on_event)) {
            on_event <- function(ev) {
                if (identical(ev$type, "approval")) {
                    unasked <<- unasked + 1L
                    ns$job_answer(s, ev$job$id, ev$request$id, FALSE)
                }
            }
        }
        pump_until(final, on_event)
        ns$job_read(id)
    }

    # A first job writes, outside plan mode.
    j <- run("script: write a1")
    expect_identical(j$status, "done")
    expect_true(file.exists(at("a1")))
    expect_true(grepl("model=model-a", j$outcome$result, fixed = TRUE))
    pid <- s$.job_worker$get_pid()

    # Codex's case: a job, then /plan, then a second job in the same
    # worker. The second is refused its write and told it is in plan
    # mode; it is not offered exit_plan_mode.
    s$plan_mode <- TRUE
    j <- run("script: write b1")
    expect_identical(s$.job_worker$get_pid(), pid)
    expect_false(file.exists(at("b1")))
    expect_true(grepl("plan mode", j$outcome$result, fixed = TRUE))
    expect_true(grepl("exit_offered=FALSE plan_note=TRUE plan_system=FALSE",
                      j$outcome$result, fixed = TRUE))

    # exit_plan_mode, called anyway, is refused, and plan mode holds.
    j <- run("script: exit ; write e1")
    expect_true(grepl("only the person can end it", j$outcome$result,
                      fixed = TRUE))
    expect_false(file.exists(at("e1")))
    s$plan_mode <- FALSE

    # /plan, /permissions deny and /dryrun typed while a job runs: its
    # next call meets them.
    j <- run("script: write c1 go-c1", gate = "go-c1",
             while_waiting = function(id) s$plan_mode <- TRUE)
    expect_false(file.exists(at("c1")))
    expect_true(grepl("plan mode", j$outcome$result, fixed = TRUE))
    s$plan_mode <- FALSE
    j <- run("script: write c2 go-c2", gate = "go-c2",
             while_waiting = function(id) s$config$approval_mode <- "deny")
    expect_false(file.exists(at("c2")))
    expect_true(grepl("corteza policy denied", j$outcome$result, fixed = TRUE))
    s$config$approval_mode <- "allow"
    j <- run("script: write c3 go-c3", gate = "go-c3",
             while_waiting = function(id) s$config$dry_run <- TRUE)
    expect_false(file.exists(at("c3")))
    s$config$dry_run <- NULL

    # A record gone, or naming another job, refuses the call.
    j <- run("script: write d1 go-d1", gate = "go-d1",
             while_waiting = function(id) unlink(ns$job_authority_path(id)))
    expect_false(file.exists(at("d1")))
    expect_true(grepl("this job has no authority record", j$outcome$result,
                      fixed = TRUE))
    j <- run("script: write d2 go-d2", gate = "go-d2",
             while_waiting = function(id) {
        rec <- ns$job_read_file(ns$job_authority_path(id))
        rec$job <- "20260101T000000-00000000"
        ns$job_write_file(ns$job_authority_path(id), rec)
    })
    expect_false(file.exists(at("d2")))
    expect_true(grepl("names another job", j$outcome$result, fixed = TRUE))

    # /plan typed while the call waits for an approval: the person's yes
    # does not run it.
    s$config$approval_mode <- "ask"
    asked <- 0L
    j <- run("script: write f1", on_event = function(ev) {
        if (identical(ev$type, "approval")) {
            asked <<- asked + 1L
            s$plan_mode <- TRUE
            ns$job_pump(s)
            ns$job_answer(s, ev$job$id, ev$request$id, TRUE)
        }
    })
    expect_identical(asked, 1L)
    expect_false(file.exists(at("f1")))
    expect_true(grepl("settings changed while this call waited",
                      j$outcome$result, fixed = TRUE))
    s$plan_mode <- FALSE
    s$config$approval_mode <- "allow"

    # /permissions review typed while a job runs (WP20c): its next call
    # that would prompt asks the reviewer (route "monitor"), and the
    # answer holds. Typed off again while a job runs, a call asks the
    # person, with no route.
    routes <- character()
    answer <- function(ev) {
        if (identical(ev$type, "approval")) {
            routes <<- c(routes, ev$request$route %||% "none")
            ns$job_answer(s, ev$job$id, ev$request$id, TRUE)
        }
    }
    s$config$approval_mode <- "ask"
    j <- run("script: write r1 go-r1", gate = "go-r1",
             while_waiting = function(id) s$config$approval_mode <- "review",
             on_event = answer)
    expect_true(file.exists(at("r1")))
    j <- run("script: write r2 go-r2", gate = "go-r2",
             while_waiting = function(id) s$config$approval_mode <- "ask",
             on_event = answer)
    expect_true(file.exists(at("r2")))
    expect_identical(routes, c("monitor", "none"))
    s$config$approval_mode <- "allow"

    # /model between 2 jobs: the second runs on the new model, in the
    # same process.
    s$model_map$cloud <- "model-b"
    j <- run("script: read")
    expect_true(grepl("model=model-b", j$outcome$result, fixed = TRUE))
    expect_identical(s$.job_worker$get_pid(), pid)
    # /max-tokens in a talker session sets the doer's budget, which the
    # next job's requests carry (WP20c); without one, the worker's config
    # decides.
    expect_true(grepl("max_tokens=none", j$outcome$result, fixed = TRUE))
    s$doer_max_tokens <- 4000L
    j <- run("script: read")
    expect_true(grepl("max_tokens=4000", j$outcome$result, fixed = TRUE))
    s$doer_max_tokens <- NULL
    j <- run("script: read")
    expect_true(grepl("max_tokens=none", j$outcome$result, fixed = TRUE))
    # No job but the approval test asked anyone anything.
    expect_identical(unasked, 0L)
})
