library(tinytest)

# ---- new_session ----

s <- corteza::new_session("cli")
expect_true(is.environment(s))
expect_equal(s$channel, "cli")
expect_equal(s$recent_classes, character())
expect_equal(s$max_turns, 10L)

s <- corteza::new_session("matrix", history = list(list(role = "user",
                                                       content = "hi")))
expect_equal(length(s$history), 1L)

# Invalid channel rejected
expect_error(corteza::new_session("bogus"))

# ---- .flatten_mcp_result ----

expect_equal(
    corteza:::.flatten_mcp_result(
        list(content = list(list(type = "text", text = "hello")))
    ),
    "hello"
)
expect_equal(
    corteza:::.flatten_mcp_result(list(
        isError = TRUE,
        content = list(list(type = "text", text = "bad path"))
    )),
    "Error: bad path"
)
expect_equal(
    corteza:::.flatten_mcp_result(
        list(content = list(list(type = "text", text = "a"),
                            list(type = "text", text = "b")))
    ),
    "a\nb"
)
expect_equal(corteza:::.flatten_mcp_result("plain string"), "plain string")

# ---- tool handler: policy gating ----

# Deny path: tool_handler returns a denial message, skill is not called.
local({
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- corteza::new_session("matrix")
    h <- corteza:::.make_tool_handler(s)

    # matrix + personal + write = deny
    out <- h("write_file", list(path = "~/Documents/notes.md",
                                content = "x"))
    expect_true(grepl("denied", out))
    # Note: sticky context still updates even on deny, because we classified
    # the data touched. That is the desired behavior: the LLM trying to
    # write personal data means personal data is in play this turn.
    expect_true("personal" %in% s$recent_classes)
})

# Ask path: approval_cb FALSE -> declined.
local({
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    called <- FALSE
    s <- corteza::new_session(
        "cli",
        approval_cb = function(call, decision) {
            called <<- TRUE
            FALSE
        }
    )
    h <- corteza:::.make_tool_handler(s)

    # cli + personal + read = ask
    out <- h("read_file", list(path = "~/Documents/private.md"))
    expect_true(called)
    expect_true(grepl("declined", out))
})

# Ask path: approval_cb TRUE -> dispatches to the real skill. We use
# list_files against a real temp dir so the test stays offline.
local({
    tmp <- tempfile("turn-")
    dir.create(tmp)
    file.create(file.path(tmp, "a.txt"), file.path(tmp, "b.txt"))
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    op <- options(
        corteza.code_paths = c(tmp),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- corteza::new_session(
        "matrix",
        approval_cb = function(call, decision) TRUE
    )
    h <- corteza:::.make_tool_handler(s)
    # matrix + code + read = allow
    out <- h("list_files", list(path = tmp))
    expect_true(grepl("a\\.txt", out) || grepl("a.txt", out))
    expect_true("code" %in% s$recent_classes)
})

# ---- /permissions contract: config-driven approval gate ----
# Codex found that chat() was silently approving write_file /
# replace_in_file when the target path classified as `random` (the
# tensor cell random/write/console = "allow"). The CLI separately
# enforced approval_mode + dangerous_tools, so the two surfaces
# disagreed about what required approval. policy() now overlays
# session$config so both honor /permissions.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    default_cfg <- list(
        approval_mode = "ask",
        dangerous_tools = corteza:::default_dangerous_tools()
    )

    # write_file in console: even though the path doesn't fall under
    # `code_paths`, the dangerous-tools config must force "ask".
    called_write <- FALSE
    s_write <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            called_write <<- TRUE
            FALSE
        }
    )
    s_write$config <- default_cfg
    h_write <- corteza:::.make_tool_handler(s_write)
    out_write <- h_write("write_file",
                         list(path = "/tmp/corteza-test-write.txt",
                              content = "x"))
    expect_true(called_write)
    expect_true(grepl("declined", out_write))

    # replace_in_file in console: same — must hit approval_cb.
    called_replace <- FALSE
    s_replace <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            called_replace <<- TRUE
            FALSE
        }
    )
    s_replace$config <- default_cfg
    h_replace <- corteza:::.make_tool_handler(s_replace)
    out_replace <- h_replace("replace_in_file",
                             list(path = "/tmp/corteza-test-replace.txt",
                                  old_text = "a", new_text = "b"))
    expect_true(called_replace)
    expect_true(grepl("declined", out_replace))

    # bash in console: also in dangerous_tools by default.
    called_bash <- FALSE
    s_bash <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            called_bash <<- TRUE
            FALSE
        }
    )
    s_bash$config <- default_cfg
    h_bash <- corteza:::.make_tool_handler(s_bash)
    out_bash <- h_bash("bash", list(command = "ls /tmp"))
    expect_true(called_bash)

    # Sanity: without config, the historical contract holds — a
    # write_file in console that classifies as random falls into the
    # tensor allow cell and approval_cb does not fire. Use a fake
    # tool_executor so the test doesn't actually touch the
    # filesystem when the negative-case write goes through.
    called_none <- FALSE
    fake_executor <- function(name, args) {
        list(content = list(list(type = "text", text = "stub")))
    }
    s_none <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            called_none <<- TRUE
            FALSE
        }
    )
    # no s_none$config — policy() sees config = NULL.
    h_none <- corteza:::.make_tool_handler(s_none,
                                           tool_executor = fake_executor)
    sanity_path <- tempfile("corteza-test-no-cfg-")
    on.exit(unlink(sanity_path, force = TRUE), add = TRUE)
    h_none("write_file", list(path = sanity_path, content = "x"))
    expect_false(called_none)
    expect_false(file.exists(sanity_path))
})

# Per-tool permissions override approval_mode: setting permissions =
# list(bash = "deny") in config should make the handler refuse the
# call regardless of the default tensor.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) TRUE
    )
    s$config <- list(approval_mode = "ask",
                     dangerous_tools = c("bash"),
                     permissions = list(bash = "deny"))
    h <- corteza:::.make_tool_handler(s)
    out <- h("bash", list(command = "echo no"))
    expect_true(grepl("denied|deny", out, ignore.case = TRUE))
})

# Per-tool permissions = "allow" should downgrade a tensor-driven
# "ask" so the tool runs without prompting. Mirrors the CLI's
# requires_approval() semantics: a tool the user has explicitly
# marked allow skips approval regardless of how the data classifies.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    called <- FALSE
    fake_executor <- function(name, args) {
        list(content = list(list(type = "text", text = "ran")))
    }
    s <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            called <<- TRUE
            FALSE
        }
    )
    s$config <- list(approval_mode = "ask",
                     dangerous_tools = c("write_file"),
                     permissions = list(write_file = "allow"))
    h <- corteza:::.make_tool_handler(s, tool_executor = fake_executor)
    sandbox <- tempfile("corteza-test-allow-")
    on.exit(unlink(sandbox, force = TRUE), add = TRUE)
    out <- h("write_file", list(path = sandbox, content = "x"))
    expect_false(called)
    expect_true(grepl("ran", out))
    expect_false(file.exists(sandbox))
})

# User policy via options("corteza.policy") is the final word — config
# overlay must NOT downgrade or tighten a verdict that came from the
# user fn. Codex caught the regression where project config could
# override a process-level user policy.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.personal_paths = character(),
        corteza.policy = function(call) {
            if (identical(call$tool %||% "", "bash")) {
                list(model = "cloud", approval = "ask",
                     reason = "user fn: always ask for bash")
            } else {
                NULL
            }
        }
    )
    on.exit(options(op), add = TRUE)

    # Config tries to allow bash via per-tool permissions. User policy
    # says ask. User policy wins.
    cfg <- list(approval_mode = "ask",
                dangerous_tools = c("bash"),
                permissions = list(bash = "allow"))

    d <- corteza::policy(list(tool = "bash",
                              args = list(command = "ls"),
                              channel = "console"),
                         config = cfg)
    expect_equal(d$approval, "ask")
    expect_equal(d$reason, "user fn: always ask for bash")

    # Config also can't tighten when user fn said allow.
    options(corteza.policy = function(call) {
        list(model = "cloud", approval = "allow",
             reason = "user fn: trust everything")
    })
    cfg_tight <- list(approval_mode = "ask",
                      dangerous_tools = c("bash"),
                      permissions = list(bash = "deny"))
    d2 <- corteza::policy(list(tool = "bash",
                               args = list(command = "rm -rf /"),
                               channel = "console"),
                          config = cfg_tight)
    expect_equal(d2$approval, "allow")
    expect_equal(d2$reason, "user fn: trust everything")
})

# ---- turn(): smoke test that session is still usable ----

s <- corteza::new_session("cli")
expect_equal(s$channel, "cli")
# turn() itself requires an LLM call so we don't run it offline; the
# pieces it composes are exercised above.
