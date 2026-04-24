library(tinytest)

expect_true(is.function(corteza::add_observer))
expect_true(is.function(corteza::observer_progress))

# observer_progress prints a tool-specific hint next to the tool name.
local({
    obs <- corteza::observer_progress()
    out <- capture.output(obs(list(
        call = list(tool = "read_file", args = list(path = "/x/y.R")),
        outcome = "ran", result = "a\nb\n", success = TRUE
    )))
    expect_true(any(grepl("[read_file] /x/y.R", out, fixed = TRUE)))
    expect_true(any(grepl("(2 lines)", out, fixed = TRUE)))

    out2 <- capture.output(obs(list(
        call = list(tool = "grep_files", args = list(pattern = "foo")),
        outcome = "ran", result = "", success = TRUE
    )))
    expect_true(any(grepl("/foo/", out2, fixed = TRUE)))

    out3 <- capture.output(obs(list(
        call = list(tool = "list_files", args = list(path = ".")),
        outcome = "deny", result = "[denied]", success = FALSE
    )))
    expect_true(any(grepl("[list_files] . denied", out3, fixed = TRUE)))
})

# Observer receives event for allow+success.
local({
    seen <- list()
    s <- corteza::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    corteza::add_observer(s, function(event) {
        seen[[length(seen) + 1L]] <<- event
    })

    tmp <- tempfile("obs-")
    dir.create(tmp)
    file.create(file.path(tmp, "x.txt"))
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    op <- options(
        corteza.code_paths = c(tmp),
        corteza.personal_paths = character(),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    h <- corteza:::.make_tool_handler(s)
    h("list_files", list(path = tmp))

    expect_equal(length(seen), 1L)
    expect_equal(seen[[1]]$outcome, "ran")
    expect_true(seen[[1]]$success)
    expect_equal(seen[[1]]$turn_number, 1L)
    expect_true(is.numeric(seen[[1]]$elapsed_ms))
})

# Observer fires on deny.
local({
    seen <- list()
    op <- options(
        corteza.personal_paths = c("~/Documents"),
        corteza.policy = NULL
    )
    on.exit(options(op), add = TRUE)

    s <- corteza::new_session("matrix")
    corteza::add_observer(s, function(event) {
        seen[[length(seen) + 1L]] <<- event
    })

    h <- corteza:::.make_tool_handler(s)
    h("write_file", list(path = "~/Documents/secret.md", content = "x"))

    expect_equal(length(seen), 1L)
    expect_equal(seen[[1]]$outcome, "deny")
    expect_false(seen[[1]]$success)
})

# Observer error doesn't break tool dispatch.
local({
    s <- corteza::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    corteza::add_observer(s, function(event) {
        stop("boom")
    })

    tmp <- tempfile("obs-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    op <- options(corteza.personal_paths = character(),
                  corteza.policy = NULL)
    on.exit(options(op), add = TRUE)

    h <- corteza:::.make_tool_handler(s)
    # Should not raise.
    out <- h("list_files", list(path = tmp))
    expect_true(is.character(out))
})

# Multiple observers run in order.
local({
    order <- character()
    s <- corteza::new_session("cli",
                             approval_cb = function(call, decision) TRUE)
    corteza::add_observer(s, function(event) order <<- c(order, "a"))
    corteza::add_observer(s, function(event) order <<- c(order, "b"))

    tmp <- tempfile("obs-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    op <- options(corteza.personal_paths = character(),
                  corteza.policy = NULL)
    on.exit(options(op), add = TRUE)

    h <- corteza:::.make_tool_handler(s)
    h("list_files", list(path = tmp))
    expect_equal(order, c("a", "b"))
})
