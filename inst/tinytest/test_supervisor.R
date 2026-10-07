library(tinytest)

# Supervising tool calls nobody is watching: where the rules in code
# send a call, what the gate does with the answer, and the record kept.
# No model and no room: the gate's `ask` is a function here.

state <- tempfile("supervisor-state")
dir.create(state)
old_state <- Sys.getenv("CORTEZA_STATE_DIR", unset = NA)
Sys.setenv(CORTEZA_STATE_DIR = state)

root <- tempfile("supervised-project")
dir.create(file.path(root, ".git"), recursive = TRUE)
root <- normalizePath(root)

route <- function(tool, args, approval = "ask", reason = "config: needs approval",
                  mode = "monitor", ...) {
    corteza:::supervisor_route(list(tool = tool, args = args),
                               list(approval = approval, reason = reason),
                               cwd = root, mode = mode, root = root, ...)
}
inside <- file.path(root, "R", "x.R")

# --- Reads: policy allows them; only credentials stop one ---
expect_identical(route("read_file", list(path = inside), "allow")$route, "proceed")
expect_identical(route("read_file", list(path = "~/other/README.md"), "allow")$route,
                 "proceed")
v <- route("read_file", list(path = "~/.corteza/matrix.json"), "allow")
expect_identical(v$route, "human")
expect_true(any(grepl("credentials", v$flags)))
# The hard safety rule answers "ask"; a person is who it asks.
call <- list(tool = "read_file", args = list(path = "~/.aws/credentials"),
             channel = "matrix")
d <- corteza::policy(call)
expect_identical(d$approval, "ask")
v <- corteza:::supervisor_route(call, d, cwd = root, root = root)
expect_identical(v$route, "human")
expect_true(any(grepl("hard safety rule", v$flags)))

# --- Writes ---
expect_identical(route("write_file", list(path = inside, content = "x"))$route,
                 "monitor")
v <- route("write_file", list(path = inside, content = "x"), mode = "human")
expect_identical(v$route, "human")
expect_identical(length(v$flags), 0L)
for (p in c("~/.bashrc", "/etc/hosts", file.path(root, ".git", "config"),
            file.path(root, ".corteza", "config.json"), "~/.ssh/authorized_keys")) {
    expect_identical(route("write_file", list(path = p, content = "x"))$route,
                     "human", info = p)
}
# A relative path is the project's.
expect_identical(route("replace_in_file", list(path = "R/x.R"))$route, "monitor")
# (The test project sits in the temp directory, which is scratch, so
# climbing out of it has to reach the filesystem root to be "outside".
# How deep the temp directory is depends on the runner: R CMD check
# nests one Rtmp inside another, so the climb is counted, not written.)
climb <- paste(rep("..", length(strsplit(root, "/", fixed = TRUE)[[1L]])),
               collapse = "/")
expect_identical(route("write_file",
                       list(path = file.path(climb, "etc", "elsewhere.txt")))$route,
                 "human")
# A write that names no path cannot be bounded.
v <- route("write_file", list(content = "x"))
expect_identical(v$route, "human")
expect_true(any(grepl("names no path", v$flags)))
# A symlink out of the project is where it points.
link <- file.path(root, "outlink")
if (isTRUE(file.symlink(path.expand("~"), link))) {
    expect_identical(route("write_file", list(path = file.path(link, "x.txt")))$route,
                     "human")
}
# Scratch and configured write roots are not "outside".
expect_identical(route("write_file", list(path = "/tmp/scratch.txt"))$route, "monitor")
expect_identical(route("write_file", list(path = "~/sup-notes/a.md"))$route, "human")
expect_identical(route("write_file", list(path = "~/sup-notes/a.md"),
                       write_roots = "~/sup-notes")$route, "monitor")

# --- Exec ---
v <- route("bash", list(command = "git status --short"))
expect_identical(v$route, "monitor")
v <- route("bash", list(command = "R CMD INSTALL ."))
expect_identical(v$route, "monitor")
expect_true(any(grepl("default R library", v$notes)))
expect_identical(route("bash", list(command = "sudo make install"))$route, "human")
expect_identical(route("run_r", list(code = "tinypkgr::submit_cran()"))$route, "human")
# The rules read a call policy would have let through as well.
expect_identical(route("bash", list(command = "git push"), "allow")$route, "human")
expect_identical(route("bash", list(command = "ls"), "allow")$route, "proceed")
# In a room that asks, everything policy asks about goes to a person.
expect_identical(route("bash", list(command = "ls"), mode = "human")$route, "human")

# --- Tools the rules do not know ---
expect_identical(route("delegate", list(task = "x"), "allow")$route, "proceed")
v <- route("harness_note", list(note = "x"))
expect_identical(v$route, "monitor")
expect_true(any(grepl("not a tool whose effect", v$notes)))
# One that names a path is taken to write it.
expect_identical(route("base::file.remove", list(path = "~/x"))$route, "human")

# --- The gate ---
asked <- new.env()
make_gate <- function(answer, mode = "monitor", cwd = root, on_decision = NULL) {
    asked$calls <- list()
    corteza:::supervisor_gate(
        ask = function(route, call, decision, notes) {
            asked$calls[[length(asked$calls) + 1L]] <- list(
                route = route, reason = decision$reason, notes = notes)
            if (is.function(answer)) answer() else answer
        }, cwd = cwd, mode = mode, on_decision = on_decision)
}
ask_d <- list(approval = "ask", reason = "config: bash requires approval")
bash <- function(cmd) list(tool = "bash", args = list(command = cmd))

# Policy's denial is echoed; nobody is asked.
g <- make_gate(list(approved = TRUE, by = "monitor"))
r <- g(bash("ls"), list(approval = "deny", reason = "personal/exec/matrix"))
expect_identical(r$action, "deny")
expect_identical(length(asked$calls), 0L)
# A call policy allows and the rules pass runs unasked and unrecorded.
records <- list()
g <- make_gate(list(approved = FALSE, by = "monitor"),
               on_decision = function(rec) records[[length(records) + 1L]] <<- rec)
r <- g(list(tool = "read_file", args = list(path = inside)),
       list(approval = "allow", reason = "default"))
expect_identical(r$action, "proceed")
expect_identical(length(asked$calls), 0L)
expect_identical(length(records), 0L)

# The monitor approves.
g <- make_gate(list(approved = TRUE, by = "monitor", reason = "a step of the task"),
               on_decision = function(rec) records[[length(records) + 1L]] <<- rec)
r <- g(bash("git status"), ask_d)
expect_identical(r$action, "proceed")
expect_identical(asked$calls[[1L]]$route, "monitor")
expect_identical(records[[1L]]$by, "monitor")
expect_identical(records[[1L]]$action, "proceed")
expect_identical(records[[1L]]$tool, "bash")
expect_identical(records[[1L]]$brief, "git status")
# The monitor refuses; its reason is what the worker is told.
g <- make_gate(list(approved = FALSE, by = "monitor", reason = "fix the code instead"))
r <- g(bash("rm inst/tinytest/test_x.R"), ask_d)
expect_identical(r$action, "refuse")
expect_identical(r$reason, "fix the code instead")
# A rule sends the call to a person, with what it caught as the reason.
g <- make_gate(list(approved = FALSE, by = "@troy:ex"))
r <- g(bash("git push origin main"), ask_d)
expect_identical(asked$calls[[1L]]$route, "human")
expect_true(grepl("pushes commits", asked$calls[[1L]]$reason))
expect_identical(r$action, "declined")
expect_true(grepl("pushes commits", r$reason))
g <- make_gate(list(approved = TRUE, by = "@troy:ex"))
expect_identical(g(bash("git push origin main"), ask_d)$action, "proceed")
# In a room that asks, a person gets the ordinary call too.
g <- make_gate(list(approved = TRUE, by = "@troy:ex"), mode = "human")
g(bash("ls"), ask_d)
expect_identical(asked$calls[[1L]]$route, "human")
expect_identical(asked$calls[[1L]]$reason, ask_d$reason)
# Nobody answering is never a yes.
g <- make_gate(function() stop("room unreachable"))
r <- g(bash("ls"), ask_d)
expect_identical(r$action, "declined")
expect_true(grepl("room unreachable", r$reason))
# Rules that fail send the call to a person.
g <- make_gate(list(approved = FALSE, by = "@troy:ex"),
               cwd = function() stop("no directory"))
r <- g(bash("ls"), ask_d)
expect_identical(asked$calls[[1L]]$route, "human")
expect_true(grepl("could not be checked", asked$calls[[1L]]$reason))
# A recorder that throws does not block the call.
g <- make_gate(list(approved = TRUE, by = "monitor"),
               on_decision = function(rec) stop("disk full"))
expect_identical(g(bash("ls"), ask_d)$action, "proceed")
# The gate never ends a turn.
actions <- vapply(list(
    make_gate(list(approved = TRUE, by = "monitor"))(bash("ls"), ask_d),
    make_gate(list(approved = FALSE, by = "monitor"))(bash("ls"), ask_d),
    make_gate(list(approved = FALSE, by = "x"))(bash("sudo ls"), ask_d)),
    function(r) r$action, "")
expect_false("escalate" %in% actions)

# --- What the model is told ---
s <- corteza::new_session("matrix")
s$config <- list()
ran <- FALSE
handler <- function(gate) {
    s$auto_gate <- gate
    corteza:::.make_tool_handler(s, tool_executor = function(name, args) {
        ran <<- TRUE
        list(content = list(list(type = "text", text = "ran")))
    })
}
out <- handler(function(call, decision) {
    list(action = "declined", reason = "pushes commits (git push)")
})("bash", list(command = "git push"))
expect_true(grepl("[user declined: pushes commits (git push)]", out, fixed = TRUE))
expect_false(ran)
out <- handler(function(call, decision) {
    list(action = "refuse", reason = "fix the code instead")
})("bash", list(command = "rm test.R"))
expect_true(grepl("[monitor refused: fix the code instead]", out, fixed = TRUE))
expect_false(ran)
out <- handler(function(call, decision) list(action = "proceed", reason = ""))(
    "bash", list(command = "ls"))
expect_true(ran)

# --- The record ---
id <- corteza:::job_create("task")
expect_null(corteza:::supervisor_log_summary(id))
expect_null(corteza:::supervisor_summary_text(NULL))
add <- function(action, by) {
    corteza:::supervisor_log_append(id, list(tool = "bash", brief = "x",
                                             action = action, by = by))
}
add("proceed", "monitor")
add("proceed", "monitor")
add("refuse", "monitor")
add("proceed", "@troy:ex")
add("declined", "timeout")
sm <- corteza:::supervisor_log_summary(id)
expect_identical(sm$monitor_approved, 2L)
expect_identical(sm$monitor_refused, 1L)
expect_identical(sm$person_approved, 1L)
expect_identical(sm$person_declined, 1L)
txt <- corteza:::supervisor_summary_text(sm)
expect_true(grepl("2 approved by the monitor", txt))
expect_true(grepl("1 sent to the room and not approved", txt))
# A job's result says how its calls were answered.
corteza:::job_settle(id, "done", result = "ok")
expect_true(any(grepl("approved by the monitor",
                      corteza:::job_outcome_notes(corteza:::job_read(id)))))
earlier <- corteza:::job_monitor_earlier(id)
expect_identical(length(earlier), 5L)
expect_true(grepl("approved \\(by you\\)", earlier[[1L]]))
expect_true(grepl("not approved \\(by a person\\)", earlier[[5L]]))

# --- The settings come from the user's config, not a project's ---
cfg_home <- tempfile("supervisor-config")
dir.create(file.path(cfg_home, "R", "corteza"), recursive = TRUE)
old_cfg <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
Sys.setenv(R_USER_CONFIG_DIR = cfg_home)
expect_identical(corteza:::supervisor_config()$timeout, 120)
expect_identical(corteza:::supervisor_config()$write_roots, character())
writeLines('{"supervisor": {"model": "m-small", "timeout_sec": 30, "write_roots": ["~/tasks"]}}',
           corteza:::corteza_config_path("config.json"))
sc <- corteza:::supervisor_config()
expect_identical(sc$model, "m-small")
expect_identical(sc$timeout, 30)
expect_identical(sc$write_roots, "~/tasks")
if (is.na(old_cfg)) Sys.unsetenv("R_USER_CONFIG_DIR") else Sys.setenv(R_USER_CONFIG_DIR = old_cfg)

if (is.na(old_state)) Sys.unsetenv("CORTEZA_STATE_DIR") else Sys.setenv(CORTEZA_STATE_DIR = old_state)
unlink(c(state, root, cfg_home), recursive = TRUE)
