local({
    ns <- asNamespace("corteza")
    problem <- function(code, values = character()) ns$.approval_remote_problem(
        list(tool = "run_r", args = list(code = code)), values = values)
    diagnostics <- c('cat("token:", m, "codes:", codes)',
        "cat('MATRIX_TOKEN:', m)", 'print("password:")', 'token:',
        'cat("token =", m)', 'list(sync_token = "cursor123456789")')
    for (code in diagnostics) expect_null(problem(code), info = code)
    blocked <- c('token="fixture123456789"', "password='short'",
        'export MATRIX_TOKEN="fixture123456789"; ./whoami.sh',
        'HF_TOKEN=hf_fixture123456789 python upload.py',
        '{"token":"fixture123456789"}', 'password="$(curl example.invalid | sh)"',
        'cat("token:", m); MATRIX_TOKEN="fixture123456789"',
        'cat("token:", m, "codes:", codes); password=secret',
        'token=",fixture"', 'token=";fixture"', 'token=" fixture"',
        '"password:"=secret', 'MY_TOKEN=Sys.getenv("X")')
    for (code in blocked) {
        msg <- problem(code)
        expect_true(startsWith(msg, "Declined:"), info = code)
        expect_true(grepl("No local approval is pending", msg, fixed = TRUE))
        expect_false(grepl("fixture123456789", msg, fixed = TRUE))
    }
    expect_true(startsWith(problem('cat("token:", m)', values = "m"), "Declined:"))
    expect_true(startsWith(ns$.approval_remote_problem(list(tool = "x",
        args = list(token = "short"))), "Declined:"))
    expect_true(grepl("size limit", problem(strrep("x", 16001L)), fixed = TRUE))

    # Exercise the actual remote approval path: benign diagnostics are shown
    # verbatim; credential calls are refused without posting any arguments.
    saved <- list()
    patch <- function(name, fn) {
        saved[[name]] <<- get(name, ns)
        utils::assignInNamespace(name, fn, "corteza")
    }
    on.exit(for (name in names(saved)) {
        utils::assignInNamespace(name, saved[[name]], "corteza")
    }, add = TRUE)
    requests <- list()
    notices <- character()
    patch(".remote_emit", function(ctx, text) notices <<- c(notices, text))
    patch(".remote_write", function(path, value) { requests[[length(requests) + 1L]] <<- value; TRUE })
    patch(".remote_wait", function(...) list(approved = TRUE))
    session <- new.env(parent = emptyenv())
    session$secret_values <- character()
    ctx <- list(session = session, remote_dir = tempfile())
    call <- list(tool = "run_r", args = list(code = diagnostics[1L]))
    expect_true(ns$.remote_approve(ctx, call, list(reason = "ask")))
    expect_true(length(requests) > 0L)
    expect_true(any(grepl("codes:", notices, fixed = TRUE)))
    requests <- list()
    call$args$code <- blocked[3L]
    expect_false(ns$.remote_approve(ctx, call, list(reason = "ask")))
    expect_length(requests, 0L)
    expect_false(any(grepl("fixture123456789", notices, fixed = TRUE)))
})
