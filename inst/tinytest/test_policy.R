library(tinytest)

# ---- classify_op ----

expect_equal(corteza:::classify_op("read_file"), "read")
expect_equal(corteza:::classify_op("list_files"), "read")
expect_equal(corteza:::classify_op("grep_files"), "read")
expect_equal(corteza:::classify_op("git_log"), "read")
expect_equal(corteza:::classify_op("write_file"), "write")
expect_equal(corteza:::classify_op("replace_in_file"), "write")
expect_equal(corteza:::classify_op("bash"), "exec")
expect_equal(corteza:::classify_op("run_r"), "exec")
expect_equal(corteza:::classify_op("run_r_script"), "exec")
expect_equal(corteza:::classify_op("spawn_subagent"), "unknown")

# ---- resolve_paths / resolve_urls ----

expect_equal(
    corteza:::resolve_paths(list(args = list(path = "foo"))),
    "foo"
)
expect_equal(
    corteza:::resolve_paths(list(args = list(from = "a", to = "b"))),
    c("a", "b")
)
expect_equal(corteza:::resolve_paths(list(args = list())), character())

expect_equal(
    corteza:::resolve_urls(list(args = list(url = "https://example"))),
    "https://example"
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

    d <- corteza::policy(list(
        tool = "bash",
        args = list(command = "ls /tmp"),
        channel = "matrix"
    ))
    expect_equal(d$model, "cloud")
    expect_equal(d$approval, "allow")
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

# ---- policy: unknown op defaults to ask ----

d <- corteza::policy(list(
    tool = "spawn_subagent",
    args = list(task = "demo"),
    channel = "cli"
))
expect_equal(d$approval, "ask")
