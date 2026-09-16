library(tinytest)

# run_r returns what the code writes on the way (cat, print, message,
# warnings) followed by the value of the last expression, in both modes.
# Before this, only the final value's print came back and everything the
# model narrated with cat() vanished.

e <- new.env(parent = globalenv())
text_of <- function(res) res$content[[1L]]$text

expect_equal(text_of(corteza::tool_run_r("cat('hello\\n'); 42", envir = e)),
             "hello\n[1] 42")
expect_equal(text_of(corteza::tool_run_r("print('p'); invisible(1)", envir = e)),
             "[1] \"p\"")
expect_equal(text_of(corteza::tool_run_r("cat('no newline'); 1", envir = e)),
             "no newline\n[1] 1")
expect_equal(text_of(corteza::tool_run_r("invisible(1)", envir = e)), "")
expect_equal(text_of(corteza::tool_run_r("x <- 41L; x + 1L", envir = e)), "[1] 42")

# messages and warnings are reported, not lost, and do not escape.
res <- corteza::tool_run_r("message('note'); 1", envir = e)
expect_equal(text_of(res), "note\n[1] 1")
res <- corteza::tool_run_r("warning('careful'); 2", envir = e)
expect_equal(text_of(res), "Warning: careful\n[1] 2")

# streams interleave in emission order, not stdout-then-messages.
res <- corteza::tool_run_r("cat('a\\n'); message('b'); cat('c\\n'); 9", envir = e)
expect_equal(text_of(res), "a\nb\nc\n[1] 9")
res <- corteza::tool_run_r("message('first'); cat('second\\n'); invisible()", envir = e)
expect_equal(text_of(res), "first\nsecond")

# options(warn = 2) turns a warning into an error: code after it does not
# run, and the result reports the error rather than swallowing it.
res <- corteza::tool_run_r(
    "old <- options(warn = 2); on.exit(options(old)); cat('before\\n'); warning('w'); cat('after\\n')",
    envir = e)
expect_true(grepl("before", text_of(res), fixed = TRUE))
expect_false(grepl("after", text_of(res), fixed = TRUE))
expect_true(grepl("Error", text_of(res)))
expect_true(isTRUE(res$r_error))
expect_equal(getOption("warn"), 0L)

# r_error distinguishes a failed evaluation from a successful one, while
# the model-facing isError stays FALSE for both (an R error is a normal
# tool result, not a transport failure).
ok_res <- corteza::tool_run_r("1 + 1", envir = e)
expect_false(isTRUE(ok_res$r_error))
expect_false(isTRUE(ok_res$isError))
err_res <- corteza::tool_run_r("stop('boom')", envir = e)
expect_true(isTRUE(err_res$r_error))
expect_false(isTRUE(err_res$isError))

# output written before an error survives, and the error is still reported.
res <- corteza::tool_run_r("cat('partial\\n'); stop('boom')", envir = e)
expect_equal(text_of(res), "partial\nError: boom")
expect_false(isTRUE(res$isError))

# a parse error reads as before
expect_true(grepl("^Error: ", text_of(corteza::tool_run_r("1 +", envir = e))))

# sinks are balanced afterwards
expect_equal(sink.number(), 0L)

# streamed output goes through the same cap as any other result
local({
    on.exit(corteza:::clear_handles(), add = TRUE)
    res <- corteza::tool_run_r("for (i in 1:200) cat('line', i, '\\n'); invisible()",
                               envir = e)
    expect_true(grepl("[tool output truncated]", text_of(res), fixed = TRUE))
    expect_true(grepl("200 lines", text_of(res)))
})

# the supervised worker inherits the behavior
corteza::ensure_skills()
s <- corteza::new_session("cli")
s$cwd <- tempdir()
s$config <- list(run_r_mode = "worker", skill_timeout = 30,
                 skill_timeout_max = 30)
res <- corteza:::call_skill("run_r",
                            list(code = "cat('from worker\\n'); message('m'); 3"),
                            ctx = list(session = s, cwd = s$cwd))
expect_false(isTRUE(res$isError))
expect_equal(text_of(res), "from worker\nm\n[1] 3")
corteza:::.run_r_worker_close(s)
