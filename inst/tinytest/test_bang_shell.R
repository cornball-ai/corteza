# Tests for `! <cmd>` shell-line helper. Covers cwd routing, output
# capture, and the staged-output truncation cap. Runs only on unix
# (the helper has a separate cmd.exe path on Windows that needs its
# own coverage in a Windows-only block, not added in v0).

library(tinytest)

run_bang <- corteza:::run_bang_shell

if (.Platform$OS.type == "unix") {

    # 1. Basic: stdout captured, returned as $text.
    res <- run_bang("echo hello", cwd = tempdir())
    expect_equal(res$text, "hello")
    expect_equal(res$staged, "hello")

    # 2. cwd routing: the shell line runs *in* the requested cwd, and
    # the caller's working directory is restored on exit.
    tmp <- normalizePath(tempdir(), mustWork = TRUE)
    pre <- getwd()
    res <- run_bang("pwd", cwd = tmp)
    expect_equal(normalizePath(res$text, mustWork = FALSE), tmp)
    expect_equal(getwd(), pre)

    # 3. stderr is folded into stdout so the user sees error output.
    # (suppressWarnings inside the helper swallows the system2
    # warning about non-zero exit status.)
    res <- run_bang("echo out; echo err 1>&2", cwd = tempdir())
    expect_true(grepl("out", res$text, fixed = TRUE))
    expect_true(grepl("err", res$text, fixed = TRUE))

    # 4. Non-zero exit status doesn't abort R or raise an error.
    res <- run_bang("false", cwd = tempdir())
    expect_true(is.character(res$text))

    # 5. Empty output -> empty strings (used to confirm we don't
    # crash on commands that produce nothing).
    res <- run_bang("true", cwd = tempdir())
    expect_equal(res$text, "")
    expect_equal(res$staged, "")

    # 6. Multi-line output is preserved on screen, single string in
    # the staged version.
    res <- run_bang("printf 'a\\nb\\nc\\n'", cwd = tempdir())
    expect_equal(res$text, "a\nb\nc")

    # 7. Staged-output cap. Generate output longer than the 4000-char
    # cap and assert the staged version is truncated while the
    # on-screen $text is full.
    big <- run_bang("seq 1 2000", cwd = tempdir())
    expect_true(nchar(big$text) >= 4000L)
    expect_true(nchar(big$staged) < nchar(big$text))
    expect_true(grepl("truncated", big$staged, fixed = TRUE))

    # 8. Errors caught by the tryCatch return a string, not a
    # condition. Use a bogus shell path via a clearly-invalid command
    # to force the error pathway -- here we patch by running a
    # command that errors during execution; system2 doesn't error,
    # so use a non-existent shell binary indirectly via a syntax
    # error that bash propagates.
    res <- run_bang("if; then", cwd = tempdir())
    expect_true(is.character(res$text))
}

# --- run_r_eval -----------------------------------------------------

run_r <- corteza:::run_r_eval

# Visible result prints into $text and $staged.
res <- run_r("1 + 1")
expect_true(grepl("^\\[1\\] 2", res$text))
expect_equal(res$staged, res$text)

# Invisible result yields empty output.
res <- run_r("invisible(42)")
expect_equal(res$text, "")
expect_equal(res$staged, "")

# Errors return a string starting with "Error:", not a condition.
res <- run_r("stop('boom')")
expect_true(grepl("^Error: boom", res$text))
expect_equal(res$staged, res$text)

# Oversized printed output -> staged version replaced by str().
# data.frame with 5000 rows easily blows past the 4000-char cap when
# printed in full, and str() of a data.frame is small.
res <- run_r("data.frame(x = 1:5000, y = letters[(1:5000 %% 26) + 1])")
expect_true(nchar(res$text) > 4000L)
expect_true(nchar(res$staged) < nchar(res$text))
expect_true(grepl("truncated", res$staged, fixed = TRUE))
expect_true(grepl("str()", res$staged, fixed = TRUE))
expect_true(grepl("'data.frame'", res$staged, fixed = TRUE))
