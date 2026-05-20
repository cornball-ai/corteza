# Tests for the RStudio-addin routing logic. The addin's rstudioapi
# calls are hard to mock; this exercises the pure
# `.corteza_route(code, ext, in_chat)` decision function instead.

library(tinytest)

route <- corteza:::.corteza_route

# --- in_chat = TRUE: route by extension to the console ------------

# R script -> /r prefix on console.
expect_equal(route("1 + 1", "r", TRUE),
             list(target = "console", text = "/r 1 + 1"))
expect_equal(route("1 + 1", "R", TRUE)$text, "/r 1 + 1")

# Shell script -> ! prefix on console (chat()'s slash-dispatch
# intercepts ! cmd and stages the output for the LLM).
expect_equal(route("ls -la", "sh", TRUE),
             list(target = "console", text = "! ls -la"))
expect_equal(route("ls -la", "bash", TRUE),
             list(target = "console", text = "! ls -la"))
expect_equal(route("ls -la", "SH", TRUE)$text, "! ls -la")

# Other extensions -> plain on console (becomes LLM input in chat).
expect_equal(route("hi", "py", TRUE),
             list(target = "console", text = "hi"))
# Empty extension (unsaved buffer) -> treat as R, prepend /r.
# Codex caught this 2026-05-20: previously routed unsaved buffers
# as "other", so Ctrl+Enter in chat() sent the line as raw LLM
# input instead of /r-evaluated R.
expect_equal(route("1 + 1", "", TRUE),
             list(target = "console", text = "/r 1 + 1"))

# --- in_chat = FALSE: addin behaves like default execute-line ----

# R script -> plain on console (RStudio's default Ctrl+Enter).
expect_equal(route("1 + 1", "r", FALSE),
             list(target = "console", text = "1 + 1"))
expect_equal(route("1 + 1", "R", FALSE)$text, "1 + 1")

# Other extensions -> plain on console.
expect_equal(route("hi", "py", FALSE),
             list(target = "console", text = "hi"))
# Empty extension outside chat -> also defaults to R (RStudio's
# built-in Ctrl+Enter does the same on untitled buffers).
expect_equal(route("1 + 1", "", FALSE),
             list(target = "console", text = "1 + 1"))

# Shell script with no chat -> Terminal pane (not console). This
# is where shell lines actually belong; sending to console would
# try to eval as R syntax.
expect_equal(route("ls -la", "sh", FALSE),
             list(target = "terminal", text = "ls -la"))
expect_equal(route("ls -la", "bash", FALSE)$target, "terminal")
expect_equal(route("ls -la", "SH", FALSE)$target, "terminal")

# --- .next_code_row: skip blank lines and comments ----------------

next_row <- corteza:::.next_code_row

# Start past the end with no following code -> past-end sentinel.
expect_equal(next_row(c("a", "b"), 3L), 3L)

# Blank line is skipped.
expect_equal(next_row(c("a <- 1", "", "b <- 2"), 2L), 3L)

# Comment line is skipped.
expect_equal(next_row(c("a <- 1", "# comment", "b <- 2"), 2L), 3L)

# Comment with leading whitespace is skipped.
expect_equal(next_row(c("a <- 1", "    # indented", "b <- 2"), 2L), 3L)

# Multiple blanks + comments in a row.
expect_equal(next_row(c("a <- 1", "", "# c1", "  ", "# c2", "z"), 2L), 6L)

# Inline comments after code are NOT skipped -- the line still has
# executable content before the #.
expect_equal(next_row(c("a <- 1", "b <- 2 # tail", "c"), 2L), 2L)

# No more code lines below -> past-end sentinel (n+1).
expect_equal(next_row(c("a <- 1", "# c1", "# c2"), 2L), 4L)

# --- .corteza_statement_range: full multi-line expression --------

stmt <- corteza:::.corteza_statement_range

# Single-line top-level expression -> just that line.
expect_equal(stmt(c("x <- 1", "y <- 2"), 1L), c(1L, 1L))
expect_equal(stmt(c("x <- 1", "y <- 2"), 2L), c(2L, 2L))

# Multi-line expression: cursor on first line -> full range.
buf <- c("x <- 1",
         "lm(y ~ x,",
         "   data = df)",
         "z <- 2")
expect_equal(stmt(buf, 2L), c(2L, 3L))
# Cursor on the continuation line -> still full range.
expect_equal(stmt(buf, 3L), c(2L, 3L))
# Cursor on neighboring single-line stmts -> just those lines.
expect_equal(stmt(buf, 1L), c(1L, 1L))
expect_equal(stmt(buf, 4L), c(4L, 4L))

# Comment line outside any expression -> single-line fallback.
buf2 <- c("x <- 1", "# a comment", "y <- 2")
expect_equal(stmt(buf2, 2L), c(2L, 2L))

# Blank line inside an expression is part of the expression.
buf3 <- c("foo(",
          "",
          "  a = 1)")
expect_equal(stmt(buf3, 2L), c(1L, 3L))

# Unparseable buffer -> single-line fallback.
expect_equal(stmt(c("not valid R %%%"), 1L), c(1L, 1L))

# Out-of-range line -> single-line fallback (the caller handles).
expect_equal(stmt(c("x <- 1"), 0L), c(0L, 0L))
expect_equal(stmt(c("x <- 1"), 5L), c(5L, 5L))
