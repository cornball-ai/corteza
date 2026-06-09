library(tinytest)

html <- corteza:::matrix_markdown_to_html(paste(c(
    "## Brief",
    "",
    "1. `one`",
    "2. **two**",
    "",
    "```",
    "a < b & c",
    "```"
), collapse = "\n"))

expect_true(grepl("<h2>Brief</h2>", html, fixed = TRUE))
expect_true(grepl("<ol><li><code>one</code></li><li><strong>two</strong></li></ol>", html, fixed = TRUE))
expect_true(grepl("a &lt; b &amp; c", html, fixed = TRUE))
