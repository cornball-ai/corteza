local({
    root <- tempfile("private-records-")
    old <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
    Sys.setenv(R_USER_DATA_DIR = root)
    on.exit({
        if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    mask <- Sys.umask("0000")
    on.exit(Sys.umask(mask), add = TRUE)
    s <- list(sessionId = "fixture-private", cwd = tempdir())
    path <- corteza:::transcript_write_header(s$sessionId, s$cwd)
    if (.Platform$OS.type != "windows") {
        expect_equal(as.integer(file.info(path)$mode), strtoi("600", 8L))
        expect_equal(as.integer(file.info(dirname(path))$mode), strtoi("700", 8L))
    }
    expect_equal(as.integer(Sys.umask()), 0)
    corteza:::transcript_append(s, "user", "exact fixture secret")
    expect_identical(corteza:::transcript_load(s$sessionId)[[1L]]$content[[1L]]$text, "exact fixture secret")
    Sys.chmod(path, "0644")
    corteza:::transcript_append(s, "assistant", "reply", usage = NULL)
    if (.Platform$OS.type != "windows") {
        expect_equal(as.integer(file.info(path)$mode), strtoi("600", 8L))
    }
    long <- paste(rep("x", 800L), collapse = "")
    trace <- corteza:::trace_add(s$sessionId, "fixture",
        args = list(code = long, nested = list(values = c("short", long, NA_character_))),
        result = c(long, "short"), success = TRUE, elapsed_ms = 1,
        turn_id = "turn1", call_id = "call1")
    rows <- corteza:::trace_load(s$sessionId)
    expect_length(rows, 1L)
    expect_true(rows[[1L]]$truncated)
    expect_true(rows[[1L]]$truncation$args)
    expect_true(rows[[1L]]$truncation$result)
    expect_equal(nchar(rows[[1L]]$args$code), 200L)
    expect_equal(nchar(rows[[1L]]$result[[1L]]), 500L)
    expect_identical(rows[[1L]]$call_id, "call1")
    expect_identical(rows[[1L]]$turn_id, "turn1")
    if (.Platform$OS.type != "windows") {
        expect_equal(as.integer(file.info(trace)$mode), strtoi("600", 8L))
    }
    corteza:::trace_add(s$sessionId, "fixture", list(code = "small"), "done", TRUE, 1)
    expect_false(tail(corteza:::trace_load(s$sessionId), 1L)[[1L]]$truncated)
    # Failed writes restore the caller's umask as well.
    blocker <- file.path(root, "blocker")
    writeLines("file", blocker)
    expect_error(suppressWarnings(corteza:::.private_record_write(file.path(blocker, "child"), "x")))
    expect_equal(as.integer(Sys.umask()), 0)
})
