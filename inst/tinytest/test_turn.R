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

# CLI channel honors config$permissions through the unified approval
# path. Two cases codex flagged on the split-brain CLI:
#   1) permissions = list(read_file = "ask") should make the CLI
#      prompt for read_file even though it isn't in dangerous_tools
#      by default.
#   2) permissions = list(bash = "allow") should make the CLI skip
#      prompting for bash even though bash IS in dangerous_tools by
#      default.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    fake_executor <- function(name, args) {
        list(content = list(list(type = "text", text = "ran")))
    }

    # Case 1: permissions = list(read_file = "ask") in cli channel.
    called_read <- FALSE
    s_read <- corteza::new_session(
        "cli",
        approval_cb = function(call, decision) {
            called_read <<- TRUE
            FALSE
        }
    )
    s_read$config <- list(approval_mode = "ask",
                          dangerous_tools = corteza:::default_dangerous_tools(),
                          permissions = list(read_file = "ask"))
    h_read <- corteza:::.make_tool_handler(s_read,
                                           tool_executor = fake_executor)
    h_read("read_file", list(path = tempfile("cli-test-read-")))
    expect_true(called_read)

    # Case 2: permissions = list(bash = "allow") in cli channel —
    # tensor would say "ask" (code/exec/cli), config "allow" wins.
    called_bash <- FALSE
    s_bash <- corteza::new_session(
        "cli",
        approval_cb = function(call, decision) {
            called_bash <<- TRUE
            FALSE
        }
    )
    s_bash$config <- list(approval_mode = "ask",
                          dangerous_tools = corteza:::default_dangerous_tools(),
                          permissions = list(bash = "allow"))
    h_bash <- corteza:::.make_tool_handler(s_bash,
                                           tool_executor = fake_executor)
    out <- h_bash("bash", list(command = "echo hi"))
    expect_false(called_bash)
    expect_true(grepl("ran", out))
})

# Default executor (no tool_executor passed) must honor session
# dry_run too — otherwise the short-circuit silently lets the tool
# run for real. Codex reproduced this: session$dry_run = TRUE with
# the default handler, then write_file created the file with no
# prompt. Regression coverage uses a tempfile to verify the file is
# NOT created when dry_run is on, AND is created when dry_run is off.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    target <- tempfile("corteza-dryrun-default-")
    on.exit(unlink(target, force = TRUE), add = TRUE)

    cb_called <- FALSE
    s <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            cb_called <<- TRUE
            FALSE
        }
    )
    s$dry_run <- TRUE
    h <- corteza:::.make_tool_handler(s) # default executor
    out <- h("write_file", list(path = target, content = "x"))
    expect_false(cb_called)
    expect_false(file.exists(target))
    expect_true(grepl("DRY RUN|dry.run|preview", out, ignore.case = TRUE))

    # chat()'s /dryrun toggles config$dry_run, not session$dry_run.
    # The handler reads both, so this path must also short-circuit.
    s2 <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) {
            cb_called <<- TRUE
            FALSE
        }
    )
    s2$config <- list(dry_run = TRUE)
    cb_called <- FALSE
    h2 <- corteza:::.make_tool_handler(s2)
    target2 <- tempfile("corteza-dryrun-config-")
    on.exit(unlink(target2, force = TRUE), add = TRUE)
    out2 <- h2("write_file", list(path = target2, content = "x"))
    expect_false(cb_called)
    expect_false(file.exists(target2))

    # Sanity: turning dry_run off makes the same call execute (and
    # since approval_cb declines, the tool DOESN'T run for a
    # different reason: declined, not previewed). Either way the
    # file must not exist.
    target3 <- tempfile("corteza-dryrun-off-")
    on.exit(unlink(target3, force = TRUE), add = TRUE)
    s3 <- corteza::new_session(
        "console",
        approval_cb = function(call, decision) FALSE
    )
    s3$config <- list(approval_mode = "ask",
                     dangerous_tools = corteza:::default_dangerous_tools())
    h3 <- corteza:::.make_tool_handler(s3)
    h3("write_file", list(path = target3, content = "x"))
    expect_false(file.exists(target3))
})

# Dry-run mode short-circuits before policy/approval so a preview
# doesn't prompt or get blocked by a config "deny". Codex caught the
# regression where the unified approval path turned dry-run into a
# guarded action.
local({
    op <- options(
        corteza.code_paths = character(),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    cb_called <- FALSE
    fake_executor <- function(name, args) {
        list(content = list(list(type = "text",
                                 text = "[DRY RUN] preview")))
    }
    s <- corteza::new_session(
        "cli",
        approval_cb = function(call, decision) {
            cb_called <<- TRUE
            FALSE
        }
    )
    # Config would normally tighten this call to ask/deny, but dry-run
    # must override.
    s$config <- list(approval_mode = "ask",
                     dangerous_tools = corteza:::default_dangerous_tools(),
                     permissions = list(bash = "deny"))
    s$dry_run <- TRUE
    h <- corteza:::.make_tool_handler(s, tool_executor = fake_executor)
    out <- h("bash", list(command = "rm -rf /"))
    expect_false(cb_called)
    expect_true(grepl("DRY RUN", out, fixed = TRUE))

    # Same call without dry_run goes through approval — sanity check
    # that the short-circuit isn't unconditional.
    cb_called <- FALSE
    s$dry_run <- FALSE
    h2 <- corteza:::.make_tool_handler(s, tool_executor = fake_executor)
    out2 <- h2("bash", list(command = "echo hi"))
    expect_false(cb_called) # never reaches cb because config says deny
    expect_true(grepl("denied|deny", out2, ignore.case = TRUE))
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

# ---- default tool_executor passes the parent session in ctx ----
# Regression: spawn_subagent silently defaulted to anthropic when chat()
# ran a non-Anthropic provider because the default executor called
# call_skill() with no ctx, so tool_spawn_subagent() saw ctx$session =
# NULL and subagent_spawn() fell through to getOption("corteza.provider").
local({
    op <- options(
        corteza.policy = function(call) {
            list(model = "cloud", approval = "allow", reason = "test allow")
        }
    )
    on.exit(options(op), add = TRUE)

    captured <- NULL
    orig <- corteza:::call_skill
    assignInNamespace(
        "call_skill",
        function(name, args, ctx = list(), ...) {
            captured <<- ctx
            list(content = list(list(type = "text", text = "ok")))
        },
        ns = "corteza"
    )
    on.exit(assignInNamespace("call_skill", orig, ns = "corteza"), add = TRUE)

    s <- corteza::new_session("cli", provider = "ollama",
                              approval_cb = function(call, decision) TRUE)
    h <- corteza:::.make_tool_handler(s) # default executor
    h("spawn_subagent", list(task = "test"))

    expect_false(is.null(captured))
    expect_identical(captured$session, s)
    expect_identical(captured$session$provider, "ollama")
})

# ---- turn(): smoke test that session is still usable ----

s <- corteza::new_session("cli")
expect_equal(s$channel, "cli")
# turn() itself requires an LLM call so we don't run it offline; the
# pieces it composes are exercised above.
