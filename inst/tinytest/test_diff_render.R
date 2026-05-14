library(tinytest)

# Identical inputs return NULL so the display layer can skip silently.
expect_null(corteza:::compute_unified_diff("a\nb\n", "a\nb\n", "x.R"))
expect_null(corteza:::compute_unified_diff("", "", "x.R"))

# New file: empty old, non-empty new. Every content line should appear
# as an addition.
new_file <- corteza:::compute_unified_diff("", "hello\nworld\n", "new.R")
expect_false(is.null(new_file))
expect_identical(new_file$path, "new.R")
expect_true(any(grepl("^\\+hello$", new_file$lines)))
expect_true(any(grepl("^\\+world$", new_file$lines)))
# Counts in summary reflect two additions, zero removals.
expect_true(grepl("Added 2 lines", new_file$summary, fixed = TRUE))
expect_false(grepl("removed", new_file$summary, fixed = TRUE))

# File emptied: all removals.
emptied <- corteza:::compute_unified_diff("a\nb\n", "", "empty.R")
expect_false(is.null(emptied))
expect_true(grepl("Removed 2 lines", emptied$summary, fixed = TRUE))
expect_true(any(grepl("^-a$", emptied$lines)))
expect_true(any(grepl("^-b$", emptied$lines)))

# Single-line change inside a longer file.
edited <- corteza:::compute_unified_diff(
                                         "a\nb\nc\nd\n",
                                         "a\nB\nc\nd\n",
                                         "x.R"
)
expect_false(is.null(edited))
expect_true(grepl("Added 1 line", edited$summary, fixed = TRUE))
expect_true(grepl("removed 1 line", edited$summary, fixed = TRUE))
expect_true(any(grepl("^@@", edited$lines)))
expect_true(any(grepl("^-b$", edited$lines)))
expect_true(any(grepl("^\\+B$", edited$lines)))

# Headers carry the path we passed in, not the temp-file paths.
expect_true(any(grepl("^--- ", edited$lines)))
expect_true(any(grepl("^\\+\\+\\+ ", edited$lines)))
expect_true(any(grepl("x.R", edited$lines, fixed = TRUE)))

# Missing trailing newline doesn't crash the diff; we get a payload back.
no_newline <- corteza:::compute_unified_diff("a\nb", "a\nB", "x.R")
expect_false(is.null(no_newline))
expect_true(length(no_newline$lines) > 0L)

# Fallback when `diff` is not on PATH: poison the binary cache so
# .diff_binary() returns "" without touching the package namespace
# (locked bindings prevent shimming the function itself). The result
# should still be a non-NULL payload, just with empty lines and a
# fallback flag.
cache <- corteza:::.diff_binary_cache
saved <- if (exists("value", envir = cache, inherits = FALSE)) {
    get("value", envir = cache, inherits = FALSE)
} else NULL
on.exit({
    if (is.null(saved)) {
        suppressWarnings(rm(list = "value", envir = cache))
    } else {
        assign("value", saved, envir = cache)
    }
}, add = TRUE)
assign("value", "", envir = cache)

fb <- corteza:::compute_unified_diff("a\nb\nc\n", "a\nB\nc\n", "x.R")
expect_false(is.null(fb))
expect_true(isTRUE(fb$fallback))
expect_identical(fb$lines, character(0L))
expect_true(nzchar(fb$summary))
