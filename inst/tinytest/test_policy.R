library(tinytest)

# ---- classify_op: the class a tool declared ----

expect_equal(corteza:::classify_op("read_file"), "read")
expect_equal(corteza:::classify_op("list_files"), "read")
expect_equal(corteza:::classify_op("grep_files"), "read")
expect_equal(corteza:::classify_op("git_log"), "read")
expect_equal(corteza:::classify_op("write_file"), "write")
expect_equal(corteza:::classify_op("replace_in_file"), "write")
expect_equal(corteza:::classify_op("bash"), "exec")
expect_equal(corteza:::classify_op("run_r"), "exec")
expect_equal(corteza:::classify_op("run_r_script"), "exec")
expect_equal(corteza:::classify_op("spawn_subagent"), "control")
expect_equal(corteza:::classify_op("query_subagent"), "continue")
expect_equal(corteza:::classify_op("harness_note"), "write")
# A host's room tools are classified though no registry holds them.
expect_equal(corteza:::classify_op("send_room_file"), "write")
expect_equal(corteza:::classify_op("delegate_to"), "continue")
expect_equal(corteza:::classify_op("ask_room"), "continue")
# A tool nobody registered, or one registered with no class.
expect_equal(corteza:::classify_op("no_such_tool"), "unknown")
expect_equal(corteza:::classify_op(""), "unknown")
expect_equal(corteza:::classify_op(NULL), "unknown")

# A class is one of 5 words; anything else is refused where the tool is
# registered, so the gate never meets it.
expect_equal(corteza:::tool_class_check(NULL, "x"), "unknown")
expect_equal(corteza:::tool_class_check("unknown", "x"), "unknown")
expect_equal(corteza:::tool_class_check("exec", "x"), "exec")
expect_error(corteza:::tool_class_check("reed", "my_tool"), "my_tool: class")
expect_error(corteza:::tool_class_check(c("read", "write"), "x"), "class")
expect_error(corteza:::skill_spec("x", "d", handler = function(a, c) NULL,
                                  class = "admin"), "class")

# ---- resolve_paths / resolve_urls ----

# A tool corteza wrote: its paths by argument name.
expect_equal(
    corteza:::resolve_paths(list(tool = "write_file",
                                 args = list(path = "foo", content = "~/.ssh/x"))),
    "foo"
)
expect_equal(
    corteza:::resolve_paths(list(tool = "read_file",
                                 args = list(from = "a", to = "b"))),
    c("a", "b")
)
expect_equal(corteza:::resolve_paths(list(tool = "bash",
                                          args = list(command = "cat ~/.ssh/x"))),
             character())
expect_equal(corteza:::resolve_paths(list(args = list())), character())

# Any other tool: every string argument, at any depth, whatever its name.
expect_equal(
    corteza:::resolve_paths(list(tool = "base::unlink",
                                 args = list(x = "~/.ssh/id_rsa", recursive = TRUE))),
    "~/.ssh/id_rsa"
)
expect_equal(
    corteza:::resolve_paths(list(args = list(from = "a", to = "b"))),
    c("a", "b")
)
expect_equal(
    corteza:::resolve_paths(list(tool = "my_skill",
                                 args = list(opts = list(out = "x.csv", n = 2L),
                                             sep = "", keep = NA_character_))),
    "x.csv"
)
expect_equal(corteza:::resolve_paths(list(tool = "my_skill",
                                          args = list(n = 2L))),
             character())

expect_equal(
    corteza:::resolve_urls(list(args = list(url = "https://ex.invalid"))),
    "https://ex.invalid"
)
expect_equal(corteza:::resolve_urls(list(args = list(path = "x"))),
             character())

# ---- classify_data: defaults (no user config) ----

local({
    # Override to known values so the test is deterministic.
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.code_paths = c("~/projects")
    )
    on.exit(options(op), add = TRUE)

    expect_equal(
        corteza:::classify_data(list(paths = "~/Documents/notes.md")),
        "personal"
    )
    expect_equal(
        corteza:::classify_data(list(paths = "~/projects/mypkg/R/foo.R")),
        "code"
    )
    expect_equal(
        corteza:::classify_data(list(paths = "/tmp/scratch.txt")),
        "random"
    )
    expect_equal(
        corteza:::classify_data(list(paths = character())),
        "random"
    )
})

# ---- classify_data: sticky via context ----

expect_equal(
    corteza:::classify_data(
        list(paths = "/tmp/out.txt"),
        context = list(recent_classes = c("personal"))
    ),
    "personal"
)
expect_equal(
    corteza:::classify_data(
        list(paths = character()),
        context = list(recent_classes = c("code"))
    ),
    "code"
)

# ---- check_safety ----

expect_null(corteza:::check_safety(list(paths = "/tmp/x")))

safety <- corteza:::check_safety(list(paths = "~/.ssh/id_ed25519"))
expect_equal(safety$model, "local")
expect_equal(safety$approval, "ask")
expect_true(grepl("credential", safety$reason))
# Marked, so that no saved "always" answers it.
expect_identical(safety$rule, "credential")

# A package tool's key under any argument name reaches the rule.
d <- corteza::policy(list(tool = "base::unlink",
                          args = list(x = "~/.ssh/id_rsa"), channel = "cli"))
expect_equal(d$approval, "ask")
expect_identical(d$rule, "credential")

# ---- policy: safety overrides everything ----

local({
    op <- options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "bypass")
    })
    on.exit(options(op), add = TRUE)

    d <- corteza::policy(list(
        tool = "read_file",
        args = list(path = "~/.ssh/id_rsa"),
        channel = "cli"
    ))
    expect_equal(d$model, "local")
    expect_equal(d$approval, "ask")
})

# ---- policy: default tensor lookup ----

local({
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.code_paths = c("~/projects"),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    d <- corteza::policy(list(
        tool = "read_file",
        args = list(path = "~/projects/foo/R/bar.R"),
        channel = "cli"
    ))
    expect_equal(d$model, "cloud")
    expect_equal(d$approval, "allow")

    d <- corteza::policy(list(
        tool = "write_file",
        args = list(path = "~/Documents/taxes.md"),
        channel = "matrix"
    ))
    expect_equal(d$model, "local")
    expect_equal(d$approval, "deny")

    # A command asks by its class, on every channel, with no config.
    d <- corteza::policy(list(
        tool = "bash",
        args = list(command = "ls /tmp"),
        channel = "matrix"
    ))
    expect_equal(d$model, "cloud")
    expect_equal(d$approval, "ask")

    # A read under a personal path is allowed (approval-engine note,
    # decision 2); a write there asks at a prompt and is refused in a
    # room.
    d <- corteza::policy(list(tool = "read_file",
                              args = list(path = "~/Documents/taxes.md"),
                              channel = "matrix"))
    expect_equal(d$model, "local")
    expect_equal(d$approval, "allow")
    d <- corteza::policy(list(tool = "write_file",
                              args = list(path = "~/Documents/taxes.md"),
                              channel = "cli"))
    expect_equal(d$approval, "ask")
    d <- corteza::policy(list(tool = "bash", args = list(command = "ls"),
                              channel = "matrix",
                              context = list(recent_classes = "personal")))
    expect_equal(d$approval, "deny")
})

# ---- policy: user fn takes precedence (when not overridden by safety) ----

local({
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.code_paths = c("~/projects"),
        corteza.policy = function(call) {
            if (identical(call$channel, "matrix") &&
                identical(corteza:::classify_op(call$tool), "exec")) {
                return(list(model = "cloud", approval = "ask",
                            reason = "matrix exec: always ask"))
            }
            NULL
        }
    )
    on.exit(options(op), add = TRUE)

    d <- corteza::policy(list(
        tool = "bash",
        args = list(command = "ls"),
        channel = "matrix"
    ))
    expect_equal(d$approval, "ask")
    expect_equal(d$reason, "matrix exec: always ask")

    # User fn returning NULL falls through to default.
    d <- corteza::policy(list(
        tool = "read_file",
        args = list(path = "/tmp/x"),
        channel = "cli"
    ))
    expect_equal(d$approval, "allow")
})

# ---- policy: a tool with no class asks; control and continue run ----

d <- corteza::policy(list(
    tool = "no_such_tool",
    args = list(task = "demo"),
    channel = "cli"
))
expect_equal(d$approval, "ask")
for (tool in c("spawn_subagent", "kill_subagent", "job_cancel", "bg_kill",
               "query_subagent", "delegate", "job_resume")) {
    expect_equal(corteza::policy(list(tool = tool, channel = "matrix"))$approval,
                 "allow", info = tool)
}

# ---- the config: relaxes only what a person wrote ----

local({
    op <- options(corteza.policy = NULL, corteza.personal_paths = "~/Documents")
    on.exit(options(op), add = TRUE)
    p <- function(tool, config, args = list(), channel = "cli") {
        corteza::policy(list(tool = tool, args = args, channel = channel),
                        config)
    }
    # No name is allowed for being absent from dangerous_tools: a tool
    # with no class asks, and so does a write left off the list.
    expect_equal(p("no_such_tool", list())$approval, "ask")
    expect_equal(p("write_file", list(dangerous_tools = "bash"))$approval, "ask")
    expect_equal(p("base::file.copy", list(dangerous_tools = character()),
                   list(from = "a", to = "b"))$approval, "ask")
    # A name on the list asks whatever its class.
    expect_equal(p("read_file", list(dangerous_tools = "read_file"))$approval,
                 "ask")
    expect_equal(p("read_file", list())$approval, "allow")
    # A permissions entry replaces the verdict for its tool.
    expect_equal(p("bash", list(permissions = list(bash = "allow")))$approval,
                 "allow")
    expect_equal(p("read_file", list(permissions = list(read_file = "ask")))$approval,
                 "ask")
    expect_equal(p("read_file", list(permissions = list(read_file = "deny")))$approval,
                 "deny")
    expect_equal(p("no_such_tool", list(permissions = list(no_such_tool = "allow")))$approval,
                 "allow")
    # approval_mode says what "ask" means for a tool with no entry.
    expect_equal(p("bash", list(approval_mode = "allow"))$approval, "allow")
    expect_equal(p("no_such_tool", list(approval_mode = "deny"))$approval, "deny")
    expect_equal(p("read_file", list(approval_mode = "deny"))$approval, "allow")
    expect_equal(p("bash", list(approval_mode = "allow",
                                permissions = list(bash = "ask")))$approval, "ask")
    expect_equal(p("bash", list(approval_mode = "sometimes"))$approval, "ask")
    # Nothing in the config lifts a deny or the credential rule.
    room <- function(config) {
        p("write_file", config, list(path = "~/Documents/x.md"), "matrix")$approval
    }
    expect_equal(room(list(approval_mode = "allow")), "deny")
    expect_equal(room(list(permissions = list(write_file = "allow"))), "deny")
    key <- p("read_file", list(approval_mode = "allow",
                               permissions = list(read_file = "allow")),
             list(path = "~/.ssh/id_rsa"))
    expect_equal(key$approval, "ask")
    expect_identical(key$rule, "credential")
})

# ---- the mode comes first ----

local({
    plan <- function(tool, args = list(), config = NULL) {
        corteza::policy(list(tool = tool, args = args, channel = "cli",
                             context = list(plan_mode = TRUE)), config)
    }
    # A write under a credential path is refused in plan mode, where it
    # used to ask, and a yes to that prompt performed it.
    d <- plan("write_file", list(path = "~/.ssh/authorized_keys"))
    expect_equal(d$approval, "deny")
    expect_identical(d$rule, "plan_mode")
    # A read there still asks, as the credential rule's.
    expect_identical(plan("read_file", list(path = "~/.ssh/id_rsa"))$rule,
                     "credential")
    # The user's policy function and the config come after the mode.
    op <- options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow", reason = "anything goes")
    })
    on.exit(options(op), add = TRUE)
    expect_equal(plan("bash", list(command = "ls"))$approval, "deny")
    expect_equal(plan("no_such_tool", config = list(approval_mode = "allow",
                      permissions = list(no_such_tool = "allow")))$approval,
                 "deny")
    expect_equal(plan("read_file", list(path = "a"))$reason, "anything goes")
})
