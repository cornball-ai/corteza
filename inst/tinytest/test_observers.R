library(tinytest)

expect_true(is.function(corteza::add_observer))
expect_true(is.function(corteza::observer_progress))

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
