library(tinytest)

# The monitor: a third worker process a session owns, asked to rule on
# tool calls. Its model is replaced by a function that answers from the
# text of the request, so nothing here reaches a provider.

state <- tempfile("job-monitor-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)
# The monitor's settings come from the user's config; give the tests an
# empty one of their own.
cfg_home <- tempfile("job-monitor-config")
dir.create(file.path(cfg_home, "R", "corteza"), recursive = TRUE)
old_cfg <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
Sys.setenv(R_USER_CONFIG_DIR = cfg_home)

proj <- tempfile("job-monitor-project")
dir.create(file.path(proj, ".git"), recursive = TRUE)
proj <- normalizePath(proj)

# One stand-in for every worker of a session. A monitor's request is
# answered by what its arguments say; anything else is a doer's job,
# which asks about its task the way a supervised tool call would.
fake <- function(task) {
    if (grepl("A tool call is waiting for your ruling", task, fixed = TRUE)) {
        id <- sub(".*REQUEST-ID: ([A-Za-z0-9_-]+).*", "\\1", task)
        scope <- get(".job_worker_state",
                     envir = asNamespace("corteza"))$monitor_scope
        answer <- function(verdict, reason, as = id) {
            list(reply = sprintf("REQUEST: %s\nVERDICT: %s\nREASON: %s", as,
                                 verdict, reason))
        }
        if (grepl("refuse-me", task)) return(answer("refuse", "do it another way"))
        if (grepl("escalate-me", task)) return(answer("escalate", "a person should see this"))
        if (grepl("wrong-id", task)) return(answer("approve", "fine", as = "zzz"))
        if (grepl("garbage", task)) return(list(reply = "Sure, go ahead."))
        if (grepl("boom", task)) stop("model down")
        if (grepl("slow", task)) Sys.sleep(60)
        return(answer("approve", paste("scope", scope)))
    }
    ask <- get(".job_worker_child_supervised_ask", envir = asNamespace("corteza"))
    a <- ask("monitor", list(tool = "bash", args = list(command = task)),
             list(reason = "config: bash requires approval"),
             "installs a package into the default R library")
    list(reply = paste(a$approved, a$by, a$reason, sep = " | "))
}

make_session <- function(key) {
    s <- corteza::new_session("matrix")
    s$cwd <- proj
    s$config <- list()
    s$job_key <- key
    s$job_worker_spec <- list(init_fn = function(spec) invisible(TRUE),
                              run_fn = fake)
    s
}
question <- function(cmd, id = "r000001-abcdef", scope = "job-1") {
    list(scope = scope, request_id = id, goal = "tidy the package",
         tool = "bash", args = list(command = cmd),
         reason = "config: bash requires approval",
         notes = "names a path outside the project: /opt/x",
         earlier = "bash: ls -> approved (by you)")
}
answer <- function(s, timeout = 20, limit = 120) {
    deadline <- Sys.time() + timeout
    repeat {
        v <- corteza:::job_monitor_poll(s, timeout = limit)
        if (!is.null(v) || Sys.time() > deadline) {
            return(v)
        }
        Sys.sleep(0.05)
    }
}

# --- What the monitor is started with ---
s <- make_session("room-monitor")
spec <- corteza:::job_worker_spec(s, "monitor")
expect_identical(spec$tools, corteza:::SUBAGENT_PRESETS$monitor)
expect_false(any(c("bash", "write_file", "run_r", "web_search") %in% spec$tools))
expect_true("read_handle" %in% spec$tools)
expect_identical(spec$web_search, FALSE)
expect_identical(spec$allowed_paths, proj)
expect_identical(spec$project_context, "instructions")
expect_identical(spec$system, corteza:::JOB_MONITOR_SYSTEM)
expect_identical(spec$max_turns, 6L)
# The monitor is not itself supervised, whatever the session says.
s$job_supervise <- list(mode = "monitor")
expect_null(corteza:::job_worker_spec(s, "monitor")$supervise)
doer <- corteza:::job_worker_spec(s, "doer")
expect_identical(doer$supervise$mode, "monitor")
expect_identical(doer$supervise$write_roots, character())
s$job_supervise <- NULL
expect_null(corteza:::job_worker_spec(s, "doer")$supervise)
# Its model is the doer's unless the user's config names another, and a
# project's config has no say.
s$provider <- "anthropic"
s$model_map$cloud <- "big-model"
s$reasoning_effort <- "high"
s$config <- list(supervisor = list(model = "from-the-project"),
                 jobs = list())
spec <- corteza:::job_worker_spec(s, "monitor")
expect_identical(spec$model, "big-model")
expect_identical(spec$reasoning_effort, "high")
writeLines(paste0('{"supervisor": {"provider": "openai", "model": "small-model", ',
                  '"max_turns": 4, "write_roots": ["~/tasks"]}}'),
           corteza:::corteza_config_path("config.json"))
spec <- corteza:::job_worker_spec(s, "monitor")
expect_identical(spec$provider, "openai")
expect_identical(spec$model, "small-model")
expect_identical(spec$max_turns, 4L)
# Effort tuned for the doer's model does not follow to another.
expect_null(spec$reasoning_effort)
s$job_supervise <- list(mode = "human")
expect_identical(corteza:::job_worker_spec(s, "doer")$supervise$write_roots, "~/tasks")
s$job_supervise <- NULL
unlink(corteza:::corteza_config_path("config.json"))
s$config <- list()

# --- The request carries what the monitor needs to rule ---
txt <- corteza:::job_monitor_question(question("git add R/x.R"))
for (part in c("REQUEST-ID: r000001-abcdef", "tidy the package", "TOOL: bash",
               "git add R/x.R", "names a path outside the project",
               "bash: ls -> approved (by you)", "REQUEST: r000001-abcdef")) {
    expect_true(grepl(part, txt, fixed = TRUE), info = part)
}

# --- Asking starts the process; the answer is collected later ---
expect_false(corteza:::job_monitor_alive(s))
corteza:::job_monitor_ask(s, question("git add R/x.R"))
expect_true(corteza:::job_monitor_alive(s))
pid <- s$.job_monitor$get_pid()
live <- corteza:::job_workers_live(list(s))
expect_identical(length(live), 1L)
expect_identical(live[[1L]]$role, "monitor")
expect_true(live[[1L]]$busy)
# One call at a time.
expect_error(corteza:::job_monitor_ask(s, question("ls")), "busy")
v <- answer(s)
expect_identical(v$verdict, "approve")
expect_identical(v$reason, "scope job-1")
expect_identical(v$q$request_id, "r000001-abcdef")
expect_null(s$.job_monitor_pending)
expect_false(corteza:::job_workers_live(list(s))[[1L]]$busy)
# Nothing pending, nothing to collect.
expect_null(corteza:::job_monitor_poll(s))

# --- The same process answers the next call, and knows the job ---
corteza:::job_monitor_ask(s, question("refuse-me", id = "r000002-abcdef"))
v <- answer(s)
expect_identical(v$verdict, "refuse")
expect_identical(v$reason, "do it another way")
expect_identical(s$.job_monitor$get_pid(), pid)
corteza:::job_monitor_ask(s, question("ls", id = "r000003-abcdef",
                                      scope = "job-2"))
expect_identical(answer(s)$reason, "scope job-2")

# --- Anything short of a clean ruling is passed on ---
corteza:::job_monitor_ask(s, question("escalate-me", id = "r000004-abcdef"))
v <- answer(s)
expect_identical(v$verdict, "escalate")
expect_identical(v$reason, "a person should see this")
corteza:::job_monitor_ask(s, question("garbage", id = "r000005-abcdef"))
v <- answer(s)
expect_identical(v$verdict, "escalate")
expect_true(grepl("request id|VERDICT", v$reason))
# An answer for some other request approves nothing.
corteza:::job_monitor_ask(s, question("wrong-id", id = "r000006-abcdef"))
v <- answer(s)
expect_identical(v$verdict, "escalate")
expect_true(grepl("wrong request id", v$reason))
corteza:::job_monitor_ask(s, question("boom", id = "r000007-abcdef"))
v <- answer(s)
expect_identical(v$verdict, "escalate")
expect_true(grepl("model down", v$reason))
expect_true(corteza:::job_monitor_alive(s))

# --- A monitor that does not answer in time is stopped ---
corteza:::job_monitor_ask(s, question("slow", id = "r000008-abcdef"))
Sys.sleep(1.2)
v <- corteza:::job_monitor_poll(s, timeout = 1)
expect_identical(v$verdict, "escalate")
expect_true(grepl("did not answer within", v$reason))
expect_false(corteza:::job_monitor_alive(s))
expect_null(s$.job_monitor_pending)

# --- One that died is replaced by the next call ---
corteza:::job_monitor_ask(s, question("slow", id = "r000009-abcdef"))
s$.job_monitor$kill()
v <- answer(s)
expect_identical(v$verdict, "escalate")
expect_true(grepl("exited", v$reason))
v <- corteza:::job_monitor_ask_wait(s, question("ls", id = "r000010-abcdef"))
expect_identical(v$verdict, "approve")
expect_true(corteza:::job_monitor_alive(s))
expect_false(identical(s$.job_monitor$get_pid(), pid))

# --- A monitor that cannot start approves nothing ---
broken <- make_session("room-broken-monitor")
broken$job_worker_spec$init_fn <- function(spec) stop("no provider key")
v <- corteza:::job_monitor_ask_wait(broken, question("ls"))
expect_identical(v$verdict, "escalate")
expect_true(grepl("could not be asked", v$reason))
expect_false(corteza:::job_monitor_alive(broken))

# --- It moves with a replaced session and is retired like a worker ---
replacement <- make_session("room-monitor")
corteza:::job_state_move(s, replacement)
expect_true(corteza:::job_monitor_alive(replacement))
expect_null(s$.job_monitor)
# Busy: never closed, however old or however far over the limit.
corteza:::job_monitor_ask(replacement, question("slow", id = "r000011-abcdef"))
expect_identical(corteza:::job_workers_retire(list(replacement), idle_minutes = 1,
                                              max_workers = 0,
                                              now = Sys.time() + 86400), 0L)
expect_true(corteza:::job_monitor_alive(replacement))
corteza:::job_monitor_poll(replacement, timeout = 0)
expect_false(corteza:::job_monitor_alive(replacement))
# Idle: closed when its time is up.
corteza:::job_monitor_ask_wait(replacement, question("ls", id = "r000012-abcdef"))
expect_identical(corteza:::job_workers_retire(list(replacement), idle_minutes = 30,
                                              max_workers = 8), 0L)
expect_identical(corteza:::job_workers_retire(list(replacement), idle_minutes = 30,
                                              max_workers = 8,
                                              now = Sys.time() + 31 * 60), 1L)
expect_false(corteza:::job_monitor_alive(replacement))
# Shutdown closes it with the others.
corteza:::job_monitor_ask_wait(replacement, question("ls", id = "r000013-abcdef"))
corteza:::job_worker_close_all(replacement)
expect_false(corteza:::job_monitor_alive(replacement))

# --- A supervised worker's request says who should answer it ---
pump_for <- function(s, id, type, timeout = 20) {
    deadline <- Sys.time() + timeout
    repeat {
        ev <- Filter(function(e) identical(e$type, type), corteza:::job_pump(s))
        if (length(ev) || Sys.time() > deadline ||
            corteza:::job_read(id)$status %in% corteza:::JOB_STATUSES_FINAL) {
            return(ev)
        }
        Sys.sleep(0.1)
    }
}
settle <- function(s, id, timeout = 20) {
    deadline <- Sys.time() + timeout
    while (!corteza:::job_read(id)$status %in% corteza:::JOB_STATUSES_FINAL &&
        Sys.time() < deadline) {
        corteza:::job_pump(s)
        Sys.sleep(0.1)
    }
    corteza:::job_read(id)
}
w <- make_session("room-supervised")
a <- corteza:::job_submit(w, "R CMD INSTALL .")
ev <- pump_for(w, a, "approval")
expect_identical(length(ev), 1L)
req <- ev[[1L]]$request
expect_identical(req$route, "monitor")
expect_identical(unlist(req$notes), "installs a package into the default R library")
# The monitor's refusal reaches the worker with its reason.
expect_true(corteza:::job_answer(w, a, req$id, FALSE, by = "monitor",
                                 reason = "use a scratch library"))
expect_identical(settle(w, a)$outcome$result,
                 "FALSE | monitor | use a scratch library")
# A person's approval names the person.
b <- corteza:::job_submit(w, "git add R/x.R")
req <- pump_for(w, b, "approval")[[1L]]$request
corteza:::job_answer(w, b, req$id, TRUE, by = "@troy:ex")
expect_true(grepl("^TRUE \\| @troy:ex", settle(w, b)$outcome$result))
# Nobody answering is a no, and says so.
w$config <- list(jobs = list(approval_timeout_sec = 1))
corteza:::job_worker_close(w)
c_id <- corteza:::job_submit(w, "ls")
expect_identical(settle(w, c_id)$outcome$result,
                 "FALSE | timeout | nobody answered in time")
corteza:::job_worker_close_all(w)

# --- The real init installs the gate, and the rules run in the worker ---
# No provider is called: init builds the session, and the run function
# makes one tool call through the session's own handler.
gated <- make_session("room-gated")
gated$job_worker_spec <- list(run_fn = function(task) {
    st <- get(".subagent_state", envir = asNamespace("corteza"))
    handler <- get(".make_tool_handler", envir = asNamespace("corteza"))(
        st$session, tool_executor = function(name, args) {
            list(content = list(list(type = "text", text = "it ran")))
        })
    list(reply = paste(is.function(st$session$auto_gate),
                       handler("bash", list(command = task)), sep = " | "))
})
gated$job_supervise <- list(mode = "monitor")
g1 <- corteza:::job_submit(gated, "git push origin main")
req <- pump_for(gated, g1, "approval")[[1L]]$request
# A rule caught it: the request is for a person, with what was caught.
expect_identical(req$route, "human")
expect_true(grepl("pushes commits", req$reason))
corteza:::job_answer(gated, g1, req$id, FALSE, by = "@troy:ex")
out <- settle(gated, g1)$outcome$result
expect_true(grepl("^TRUE \\| ", out))
expect_true(grepl("[user declined: pushes commits (git push)]", out, fixed = TRUE))
# What the rules leave open is for the monitor, and an approval runs it.
g2 <- corteza:::job_submit(gated, "git status --short")
req <- pump_for(gated, g2, "approval")[[1L]]$request
expect_identical(req$route, "monitor")
corteza:::job_answer(gated, g2, req$id, TRUE, by = "monitor", reason = "fine")
expect_true(grepl("it ran", settle(gated, g2)$outcome$result, fixed = TRUE))
# Both calls are on the job's record, and the job's result counts them.
log1 <- corteza:::supervisor_log_read(g1)
expect_identical(length(log1), 1L)
expect_identical(log1[[1L]]$action, "declined")
expect_identical(log1[[1L]]$route, "human")
expect_true(any(grepl("approved by the monitor",
                      corteza:::job_outcome_notes(corteza:::job_read(g2)))))
# A room that asks sends the ordinary call to a person as well.
corteza:::job_worker_close(gated)
gated$job_supervise <- list(mode = "human")
g3 <- corteza:::job_submit(gated, "git status --short")
req <- pump_for(gated, g3, "approval")[[1L]]$request
expect_identical(req$route, "human")
corteza:::job_answer(gated, g3, req$id, TRUE, by = "@troy:ex")
expect_true(grepl("it ran", settle(gated, g3)$outcome$result, fixed = TRUE))
corteza:::job_worker_close_all(gated)

for (sess in list(s, broken, replacement)) {
    corteza:::job_worker_close_all(sess)
}
if (is.na(old_cfg)) Sys.unsetenv("R_USER_CONFIG_DIR") else Sys.setenv(R_USER_CONFIG_DIR = old_cfg)
if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old_state)
unlink(c(state, cfg_home, proj), recursive = TRUE)
