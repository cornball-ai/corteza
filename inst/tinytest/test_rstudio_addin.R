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
expect_equal(route("hi", "", TRUE),
             list(target = "console", text = "hi"))

# --- in_chat = FALSE: addin behaves like default execute-line ----

# R script -> plain on console (RStudio's default Ctrl+Enter).
expect_equal(route("1 + 1", "r", FALSE),
             list(target = "console", text = "1 + 1"))
expect_equal(route("1 + 1", "R", FALSE)$text, "1 + 1")

# Other extensions -> plain on console.
expect_equal(route("hi", "py", FALSE),
             list(target = "console", text = "hi"))

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
