library(tinytest)

# ---- new_session ----

s <- llamaR::new_session("cli")
expect_true(is.environment(s))
expect_equal(s$channel, "cli")
expect_equal(s$recent_classes, character())
expect_equal(s$max_turns, 10L)

s <- llamaR::new_session("matrix", history = list(list(role = "user",
                                                       content = "hi")))
expect_equal(length(s$history), 1L)

# Invalid channel rejected
expect_error(llamaR::new_session("bogus"))

# ---- .flatten_mcp_result ----

expect_equal(
    llamaR:::.flatten_mcp_result(
        list(content = list(list(type = "text", text = "hello")))
    ),
    "hello"
)
expect_equal(
    llamaR:::.flatten_mcp_result(list(
        isError = TRUE,
        content = list(list(type = "text", text = "bad path"))
    )),
    "Error: bad path"
)
expect_equal(
    llamaR:::.flatten_mcp_result(
        list(content = list(list(type = "text", text = "a"),
                            list(type = "text", text = "b")))
    ),
    "a\nb"
)
expect_equal(llamaR:::.flatten_mcp_result("plain string"), "plain string")

# ---- tool handler: policy gating ----

# Deny path: tool_handler returns a denial message, skill is not called.
local({
    op <- options(
        llamaR.personal_paths = c("~/Documents"),
        llamaR.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- llamaR::new_session("matrix")
    h <- llamaR:::.make_tool_handler(s)

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
        llamaR.personal_paths = c("~/Documents"),
        llamaR.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    called <- FALSE
    s <- llamaR::new_session(
        "cli",
        approval_cb = function(call, decision) {
            called <<- TRUE
            FALSE
        }
    )
    h <- llamaR:::.make_tool_handler(s)

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
        llamaR.code_paths = c(tmp),
        llamaR.personal_paths = character(),
        llamaR.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- llamaR::new_session(
        "matrix",
        approval_cb = function(call, decision) TRUE
    )
    h <- llamaR:::.make_tool_handler(s)
    # matrix + code + read = allow
    out <- h("list_files", list(path = tmp))
    expect_true(grepl("a\\.txt", out) || grepl("a.txt", out))
    expect_true("code" %in% s$recent_classes)
})

# ---- turn(): smoke test that session is still usable ----

s <- llamaR::new_session("cli")
expect_equal(s$channel, "cli")
# turn() itself requires an LLM call so we don't run it offline; the
# pieces it composes are exercised above.
