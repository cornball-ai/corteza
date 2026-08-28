library(tinytest)

# ---- parse_monitor_verdict: well-formed ----

v <- corteza:::parse_monitor_verdict("VERDICT: continue\nREASON: on track")
expect_equal(v$verdict, "continue")
expect_equal(v$reason, "on track")

v <- corteza:::parse_monitor_verdict("VERDICT: stop\nREASON: goal met")
expect_equal(v$verdict, "stop")

v <- corteza:::parse_monitor_verdict("VERDICT: escalate\nREASON: needs a human")
expect_equal(v$verdict, "escalate")

# Case-insensitive, and tolerant of the markdown a chat model reaches for.
expect_equal(corteza:::parse_monitor_verdict("verdict: continue")$verdict,
             "continue")
expect_equal(corteza:::parse_monitor_verdict("**VERDICT: stop**")$verdict,
             "stop")
expect_equal(corteza:::parse_monitor_verdict("> VERDICT: continue")$verdict,
             "continue")
expect_equal(corteza:::parse_monitor_verdict("VERDICT:continue")$verdict,
             "continue")

# A missing REASON is not fatal -- the verdict is the load-bearing half.
v <- corteza:::parse_monitor_verdict("VERDICT: continue")
expect_equal(v$verdict, "continue")
expect_equal(v$reason, "")

# Preamble before the contract still parses.
v <- corteza:::parse_monitor_verdict(
    "I checked the diff and the tests.\n\nVERDICT: stop\nREASON: done")
expect_equal(v$verdict, "stop")

# ---- parse_monitor_verdict: everything unparseable is "escalate" ----
#
# The whole point. A supervisor whose garbled output reads as approval is
# worse than no supervisor, so each of these must come back escalate --
# never continue.

expect_equal(corteza:::parse_monitor_verdict("")$verdict, "escalate")
expect_equal(corteza:::parse_monitor_verdict("   \n  ")$verdict, "escalate")
expect_equal(corteza:::parse_monitor_verdict(NULL)$verdict, "escalate")
expect_equal(corteza:::parse_monitor_verdict(NA_character_)$verdict, "escalate")
expect_equal(corteza:::parse_monitor_verdict(character(0))$verdict, "escalate")
expect_equal(corteza:::parse_monitor_verdict(c("VERDICT: continue", "x"))$verdict,
             "escalate")

# Prose with no verdict line at all.
expect_equal(
    corteza:::parse_monitor_verdict("Looks fine to me, go ahead.")$verdict,
    "escalate")

# A verdict value outside the contract.
expect_equal(corteza:::parse_monitor_verdict("VERDICT: maybe")$verdict,
             "escalate")
expect_equal(corteza:::parse_monitor_verdict("VERDICT: yes")$verdict,
             "escalate")

# Self-contradiction is escalation: there is no defensible way to pick
# one of two verdicts, so we don't pick.
v <- corteza:::parse_monitor_verdict(
    "VERDICT: continue\nOn reflection:\nVERDICT: stop")
expect_equal(v$verdict, "escalate")
expect_true(grepl("conflicting", v$reason))

# Repeating the same verdict is not a conflict.
expect_equal(
    corteza:::parse_monitor_verdict("VERDICT: stop\nVERDICT: stop")$verdict,
    "stop")

# ---- lexical path normalization ----

expect_equal(corteza:::.normalize_lexical("/a/b/../c"), "/a/c")
expect_equal(corteza:::.normalize_lexical("/a/./b"), "/a/b")
expect_equal(corteza:::.normalize_lexical("/a/b/../../c"), "/c")
expect_equal(corteza:::.normalize_lexical("/a//b"), "/a/b")
# Walking above the root stays at the root rather than wrapping around.
expect_equal(corteza:::.normalize_lexical("/../../etc"), "/etc")
# Relative paths keep a leading .. so containment below fails closed.
expect_equal(corteza:::.normalize_lexical("../x"), "../x")

expect_true(corteza:::.is_absolute_path("/etc"))
expect_false(corteza:::.is_absolute_path("R/foo.R"))

expect_equal(corteza:::.resolve_against("R/foo.R", "/home/u/proj"),
             "/home/u/proj/R/foo.R")
expect_equal(corteza:::.resolve_against("../secret", "/home/u/proj"),
             "/home/u/secret")
expect_equal(corteza:::.resolve_against("/etc/passwd", "/home/u/proj"),
             "/etc/passwd")

expect_true(corteza:::.path_within("/home/u/proj/R/x.R", "/home/u/proj"))
expect_true(corteza:::.path_within("/home/u/proj", "/home/u/proj"))
expect_false(corteza:::.path_within("/home/u/secret", "/home/u/proj"))
# A sibling directory sharing a name prefix is not inside.
expect_false(corteza:::.path_within("/home/u/proj-evil/x", "/home/u/proj"))

# ---- get_auto_config ----

d <- corteza:::get_auto_config(list())
expect_equal(d$max_loops, 10L)
expect_true(d$allow_exec)
expect_equal(d$never_broker, character())

d <- corteza:::get_auto_config(list(auto = list(max_loops = 3,
                                                allow_exec = FALSE,
                                                never_broker = "write_file")))
expect_equal(d$max_loops, 3L)
expect_false(d$allow_exec)
expect_equal(d$never_broker, "write_file")

# ---- monitor_in_envelope ----

root <- "/home/u/proj"
allow <- list(approval = "ask", reason = "default: code/write/console")

env_of <- function(tool, args, decision = allow, config = list()) {
    corteza:::monitor_in_envelope(
        list(tool = tool, args = args, paths = corteza:::resolve_paths(
            list(args = args))),
        decision, config, root)
}

# In-project write is exactly what the monitor is for.
expect_true(env_of("write_file", list(path = "R/new.R"))$ok)
expect_true(env_of("replace_in_file", list(path = "DESCRIPTION"))$ok)
expect_true(env_of("read_file", list(path = "R/turn.R"))$ok)

# A hard safety verdict is never brokered, whatever the tool.
e <- env_of("read_file", list(path = "~/.ssh/id_rsa"),
            decision = list(approval = "ask",
                            reason = "safety: ~/.ssh/id_rsa is a credential path"))
expect_false(e$ok)
expect_true(grepl("hard safety", e$reason))

# Path escapes the project.
e <- env_of("write_file", list(path = "../../etc/hosts"))
expect_false(e$ok)
expect_true(grepl("outside the project", e$reason))

e <- env_of("write_file", list(path = "/etc/hosts"))
expect_false(e$ok)

# Configured denied path, even when it sits inside the project root.
e <- env_of("read_file", list(path = "vault/keys.txt"),
            config = list(denied_paths = file.path(root, "vault")))
expect_false(e$ok)
expect_true(grepl("denied path", e$reason))

# never_broker pins a tool to always stop for a human.
e <- env_of("write_file", list(path = "R/new.R"),
            config = list(auto = list(never_broker = "write_file")))
expect_false(e$ok)
expect_true(grepl("never_broker", e$reason))

# Exec: brokered by default, refused when allow_exec is off.
expect_true(env_of("bash", list(command = "R CMD check ."))$ok)
e <- env_of("bash", list(command = "R CMD check ."),
            config = list(auto = list(allow_exec = FALSE)))
expect_false(e$ok)
expect_true(grepl("allow_exec", e$reason))

e <- env_of("run_r", list(code = "1 + 1"),
            config = list(auto = list(allow_exec = FALSE)))
expect_false(e$ok)

# dangerous_tools is deliberately NOT an envelope input: that list is
# bash/run_r/write_file/replace_in_file, i.e. the calls the monitor
# exists to broker. Treating it as a stop-list would make auto mode
# inert, so a default config must still let an in-project write through.
expect_true(env_of("write_file", list(path = "R/new.R"),
                   config = list(dangerous_tools =
                                     corteza:::default_dangerous_tools()))$ok)

# ---- envelope: unknown tools and unresolvable mutations fail closed ----

# A package tool (base::file.remove and friends are registered by the
# default skill_packages config) is not one of classify_op()'s known
# read/write/exec tools, so its effect can't be bounded.
e <- env_of("base::file.remove", list(x = "R/turn.R"))
expect_false(e$ok)
expect_true(grepl("not a recognized", e$reason))

e <- env_of("some_new_tool", list(thing = "x"))
expect_false(e$ok)

# A write tool whose target resolve_paths() cannot see must not pass by
# having nothing to check.
e <- env_of("write_file", list(destination_file = "R/x.R"))
expect_false(e$ok)
expect_true(grepl("no target path", e$reason))

# ---- envelope: symlink containment ----

if (.Platform$OS.type == "unix") {
    tmp_root <- file.path(tempdir(), "corteza-monitor-symlink")
    dir.create(file.path(tmp_root, "proj"), recursive = TRUE,
               showWarnings = FALSE)
    dir.create(file.path(tmp_root, "outside"), recursive = TRUE,
               showWarnings = FALSE)
    link <- file.path(tmp_root, "proj", "escape")
    if (!file.exists(link)) {
        file.symlink(file.path(tmp_root, "outside"), link)
    }
    proj <- file.path(tmp_root, "proj")

    # Lexically "escape/x.R" is inside the project. It is not.
    e <- corteza:::monitor_in_envelope(
        list(tool = "write_file", args = list(path = "escape/x.R"),
             paths = "escape/x.R"), allow, list(), proj)
    expect_false(e$ok)
    expect_true(grepl("outside the project", e$reason))

    # A genuinely in-project path still passes, including one that does
    # not exist yet (the common case for a new file).
    e <- corteza:::monitor_in_envelope(
        list(tool = "write_file", args = list(path = "R/brand-new.R"),
             paths = "R/brand-new.R"), allow, list(), proj)
    expect_true(e$ok)

    unlink(tmp_root, recursive = TRUE)
}

# ---- envelope: project config can tighten but never widen ----

cfg_root <- file.path(tempdir(), "corteza-monitor-cfg")
dir.create(file.path(cfg_root, ".corteza"), recursive = TRUE,
           showWarnings = FALSE)

# A project that tries to grant itself exec brokering does not get it.
writeLines('{"auto": {"allow_exec": true, "never_broker": []}}',
           file.path(cfg_root, ".corteza", "config.json"))
resolved <- corteza:::auto_envelope_config(cfg_root, allow_exec = FALSE)
expect_false(corteza:::get_auto_config(resolved)$allow_exec)

# A project that vetoes exec is honored even when the call site granted it.
writeLines('{"auto": {"allow_exec": false}}',
           file.path(cfg_root, ".corteza", "config.json"))
resolved <- corteza:::auto_envelope_config(cfg_root, allow_exec = TRUE)
expect_false(corteza:::get_auto_config(resolved)$allow_exec)

# never_broker unions across layers; a project can add but not clear.
writeLines('{"auto": {"never_broker": ["write_file"]}}',
           file.path(cfg_root, ".corteza", "config.json"))
resolved <- corteza:::auto_envelope_config(cfg_root, allow_exec = TRUE)
expect_true("write_file" %in% corteza:::get_auto_config(resolved)$never_broker)

unlink(cfg_root, recursive = TRUE)

# ---- verdict vocabularies are disjoint ----

# An approval answered in the progress vocabulary is not an approval.
v <- corteza:::parse_monitor_verdict(
    "VERDICT: continue", allowed = corteza:::.MONITOR_VERDICTS_APPROVAL)
expect_equal(v$verdict, "escalate")
expect_true(grepl("not a verdict for this question", v$reason))

# And the reverse.
v <- corteza:::parse_monitor_verdict(
    "VERDICT: approve", allowed = corteza:::.MONITOR_VERDICTS_PROGRESS)
expect_equal(v$verdict, "escalate")

v <- corteza:::parse_monitor_verdict(
    "VERDICT: approve", allowed = corteza:::.MONITOR_VERDICTS_APPROVAL)
expect_equal(v$verdict, "approve")

# ---- echoed request id ----

expect_equal(
    corteza:::parse_monitor_verdict("REQUEST: a1\nVERDICT: continue",
                                    request_id = "a1")$verdict,
    "continue")

# A reply that echoes nothing may be answering a different question.
v <- corteza:::parse_monitor_verdict("VERDICT: continue", request_id = "a1")
expect_equal(v$verdict, "escalate")
expect_true(grepl("did not echo", v$reason))

# A reply echoing the wrong id is stale or crossed.
v <- corteza:::parse_monitor_verdict("REQUEST: a7\nVERDICT: continue",
                                     request_id = "a1")
expect_equal(v$verdict, "escalate")
expect_true(grepl("wrong request id", v$reason))

# ---- prompt rendering helpers ----

expect_equal(corteza:::.monitor_truncate("", 10L), "(nothing)")
expect_equal(corteza:::.monitor_truncate("abc", 10L), "abc")
expect_true(grepl("truncated",
                  corteza:::.monitor_truncate(strrep("x", 50L), 10L)))

expect_equal(corteza:::.monitor_render_args(list()), "  (none)")
expect_true(grepl("path: R/x.R",
                  corteza:::.monitor_render_args(list(path = "R/x.R"))))

# ---- monitor_auto_gate wiring ----
#
# No network: monitor_in_envelope rejects before any query is attempted,
# so the gate must escalate without a live subagent.

seen <- list()
gate <- corteza:::monitor_auto_gate(
    "no-such-monitor", config = list(), cwd = root,
    on_verdict = function(call, action, reason) {
        seen[[length(seen) + 1L]] <<- list(action = action, reason = reason)
    })

g <- gate(list(tool = "write_file", args = list(path = "../../etc/hosts"),
               paths = "../../etc/hosts"), allow)
expect_equal(g$action, "escalate")
expect_true(grepl("outside the project", g$reason))

# An in-envelope call with an unreachable monitor escalates rather than
# proceeding. Absence of a verdict is never a yes.
g <- gate(list(tool = "write_file", args = list(path = "R/new.R"),
               paths = "R/new.R"), allow)
expect_equal(g$action, "escalate")
expect_true(grepl("unreachable", g$reason))
expect_equal(length(seen), 2L)

# ---- the gate sees "allow" decisions, not just "ask" ----
#
# This is the whole reason the gate is not an approval_cb. The default
# tensor resolves the `random` data class to "allow" for writes on every
# channel, and classify_data() only returns "code" under ~/projects or
# ~/src -- so an ordinary repo elsewhere gets "allow" and would never
# reach an approval callback. Assert that reading of policy directly, so
# this test fails if the tensor is ever retuned.
d <- corteza:::default_policy(list(tool = "write_file", channel = "console",
                                   paths = "/srv/work/repo/R/x.R"))
expect_equal(d$approval, "allow")

# And assert the gate is consulted for exactly that call anyway.
g <- gate(list(tool = "write_file", args = list(path = "R/new.R"),
               paths = "R/new.R"),
          list(approval = "allow", reason = "default: random/write/console"))
expect_equal(g$action, "escalate")

# ---- the gate inside the real tool handler ----
#
# Everything above tests the gate in isolation. This drives
# .make_tool_handler() itself -- the actual dispatch path turn() uses --
# so the wiring is verified rather than assumed.

executed <- character()
fake_exec <- function(name, args) {
    executed <<- c(executed, name)
    list(content = list(list(type = "text", text = "did it")))
}

gated_session <- function(action, reason = "because") {
    s <- corteza::new_session("console")
    s$auto_gate <- function(call, decision) {
        list(action = action, reason = reason)
    }
    s
}

# proceed: the tool runs.
executed <- character()
h <- corteza:::.make_tool_handler(gated_session("proceed"), fake_exec)
out <- h("read_file", list(path = "DESCRIPTION"))
expect_equal(executed, "read_file")
expect_true(grepl("did it", out))

# refuse: the model is told no and the tool does NOT run.
executed <- character()
h <- corteza:::.make_tool_handler(gated_session("refuse", "off goal"), fake_exec)
out <- h("write_file", list(path = "R/x.R", content = "x"))
expect_equal(executed, character())
expect_true(grepl("monitor refused", out))
expect_true(grepl("off goal", out))

# escalate: propagates as a turn-aborting condition, tool does not run.
executed <- character()
h <- corteza:::.make_tool_handler(gated_session("escalate", "needs a human"),
                                  fake_exec)
got <- tryCatch(h("write_file", list(path = "R/x.R", content = "x")),
                corteza_auto_escalate = function(c) c$reason)
expect_equal(got, "needs a human")
expect_equal(executed, character())

# A gate that throws is a gate that approved nothing.
executed <- character()
s <- corteza::new_session("console")
s$auto_gate <- function(call, decision) stop("gate exploded")
h <- corteza:::.make_tool_handler(s, fake_exec)
got <- tryCatch(h("write_file", list(path = "R/x.R", content = "x")),
                corteza_auto_escalate = function(c) c$reason)
expect_true(grepl("gate failed", got))
expect_equal(executed, character())

# A gate returning a malformed answer fails closed too.
executed <- character()
s <- corteza::new_session("console")
s$auto_gate <- function(call, decision) list(nonsense = TRUE)
h <- corteza:::.make_tool_handler(s, fake_exec)
got <- tryCatch(h("write_file", list(path = "R/x.R", content = "x")),
                corteza_auto_escalate = function(c) "escalated")
expect_equal(got, "escalated")
expect_equal(executed, character())

# The gate stands in for the human: an "ask" call it approved must not
# then also hit approval_cb.
executed <- character()
asked <- 0L
s <- corteza::new_session("console")
s$auto_gate <- function(call, decision) list(action = "proceed", reason = "")
s$approval_cb <- function(call, decision) {
    asked <<- asked + 1L
    FALSE
}
s$config <- list(permissions = list(read_file = "ask"))
h <- corteza:::.make_tool_handler(s, fake_exec)
out <- h("read_file", list(path = "DESCRIPTION"))
expect_equal(asked, 0L)
expect_equal(executed, "read_file")

# With no gate set, the ordinary approval path is untouched.
executed <- character()
asked <- 0L
s <- corteza::new_session("console")
s$approval_cb <- function(call, decision) {
    asked <<- asked + 1L
    FALSE
}
s$config <- list(permissions = list(read_file = "ask"))
h <- corteza:::.make_tool_handler(s, fake_exec)
out <- h("read_file", list(path = "DESCRIPTION"))
expect_equal(asked, 1L)
expect_equal(executed, character())
expect_true(grepl("user declined", out))

# ---- escalation condition aborts rather than declining ----

cond <- corteza:::auto_escalate_condition("bad thing", "write_file")
expect_true(inherits(cond, "corteza_auto_escalate"))
# Not an "error": the defensive tryCatch(error=) wrappers inside
# .make_tool_handler must not be able to swallow an escalation.
expect_false(inherits(cond, "error"))
expect_true(inherits(cond, "interrupt"))
expect_true(grepl("write_file", conditionMessage(cond)))

caught <- tryCatch({
    stop(corteza:::auto_escalate_condition("nope", "bash"))
    "not reached"
}, corteza_auto_escalate = function(c) c$reason)
expect_equal(caught, "nope")

# The specific failure this guards: an escalation raised inside a handler
# wrapped in tryCatch(error = ...) must still propagate.
survived <- tryCatch({
    tryCatch(stop(corteza:::auto_escalate_condition("deep", "bash")),
             error = function(e) "swallowed")
}, corteza_auto_escalate = function(c) "propagated")
expect_equal(survived, "propagated")

# ---- monitor confinement: read-only tools are not a sandbox ----
#
# tool_config() overlays the process-level corteza.allowed_paths option,
# which subagent_turn_init() sets for confined presets. Verified here on
# the option rather than by spawning a child, so it runs without an API
# key; the spawn path is covered by the at_home test below.

expect_true(isTRUE(corteza:::PRESET_CONFINED[["monitor"]]))
expect_equal(corteza:::PRESET_WEB_SEARCH[["monitor"]], FALSE)
expect_null(corteza:::PRESET_CONFINED[["work"]])
expect_null(corteza:::PRESET_WEB_SEARCH[["investigate"]])

conf_root <- file.path(tempdir(), "corteza-confine")
dir.create(conf_root, recursive = TRUE, showWarnings = FALSE)
old_opt <- getOption("corteza.allowed_paths")
options(corteza.allowed_paths = conf_root)
expect_equal(corteza:::tool_config()$allowed_paths, conf_root)

# In-root reads validate; outside-root reads do not.
expect_true(corteza:::tool_check_path(file.path(conf_root, "a.txt"),
                                      "read")$ok)
expect_false(corteza:::tool_check_path("/etc/hosts", "read")$ok)

options(corteza.allowed_paths = old_opt)
# Unset restores the historical unrestricted behavior.
expect_true(corteza:::tool_check_path("/etc/hosts", "read")$ok)
unlink(conf_root, recursive = TRUE)

# The monitor preset resolves to a read-only tool list.
mon_tools <- corteza:::resolve_subagent_tools(preset = "monitor")
expect_true(all(c("read_file", "grep_files", "git_diff") %in% mon_tools))
for (forbidden in c("write_file", "replace_in_file", "bash", "run_r",
                    "web_search", "fetch_url")) {
    expect_false(forbidden %in% mon_tools)
}
