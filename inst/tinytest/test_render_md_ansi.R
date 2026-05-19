# Tests for render_md_ansi(). Uses a forced palette so escape sequences
# are always emitted regardless of the runner's TTY state, plus a
# disabled-palette test that round-trips raw markdown through.

render <- corteza:::render_md_ansi
inline <- corteza:::render_md_inline
on_palette <- corteza:::ansi_colors

# Forced palette: ANSI on. We construct it directly so the tests don't
# depend on the runner's tty / env vars.
ansi <- list(reset = "\033[0m", bold = "\033[1m", dim = "\033[2m",
             red = "\033[31m", green = "\033[32m", yellow = "\033[33m",
             blue = "\033[34m", magenta = "\033[35m", cyan = "\033[36m",
             white = "\033[37m",
             bright_red = "\033[91m", bright_green = "\033[92m",
             bright_yellow = "\033[93m", bright_blue = "\033[94m",
             bright_magenta = "\033[95m", bright_cyan = "\033[96m")
off <- stats::setNames(as.list(rep("", length(ansi))), names(ansi))

# --- ANSI-off short-circuit: raw markdown passes through unchanged ---

expect_equal(render("**bold** and *italic*", palette = off),
             "**bold** and *italic*")
expect_equal(render("# Heading\n\nbody", palette = off),
             "# Heading\n\nbody")

# --- options(corteza.markdown = FALSE) opt-out ---

withr_local_options <- function(opts, code) {
    old <- options(opts)
    on.exit(options(old), add = TRUE)
    force(code)
}
old <- options(corteza.markdown = FALSE)
expect_equal(render("**bold**", palette = ansi), "**bold**")
options(old)

# --- Inline transforms ---

expect_equal(inline("plain text", ansi), "plain text")

expect_equal(inline("a **bold** word", ansi),
             "a \033[1mbold\033[22m word")

expect_equal(inline("a *italic* word", ansi),
             "a \033[3mitalic\033[23m word")

expect_equal(inline("a _italic_ word", ansi),
             "a \033[3mitalic\033[23m word")

# snake_case identifiers must NOT get italicized.
expect_equal(inline("call my_var_name to verify", ansi),
             "call my_var_name to verify")

# Math-like a*b*c also stays literal (asterisks not at word boundaries).
expect_equal(inline("compute a*b*c carefully", ansi),
             "compute a*b*c carefully")

# Inline code is bright_cyan, and its content isn't re-interpreted.
res <- inline("set `**not bold**` literally", ansi)
expect_true(grepl("\033\\[96m\\*\\*not bold\\*\\*\033\\[0m", res))
# And there should be NO bold escapes outside the code span.
expect_false(grepl("\033\\[1m", res))

# Bold and inline code in the same line.
res <- inline("**bold** then `code`", ansi)
expect_true(grepl("\033\\[1mbold\033\\[22m", res))
expect_true(grepl("\033\\[96mcode\033\\[0m", res))

# Markdown link: [text](url) -> bright_blue text, dim (url).
res <- inline("see [docs](https://example.com) for more", ansi)
expect_true(grepl("\033\\[94mdocs\033\\[0m \033\\[2m\\(https://example\\.com\\)\033\\[0m", res))

# --- Block-level transforms ---

# H1 - bold + bright_magenta
res <- render("# Heading One", palette = ansi)
expect_true(grepl("\033\\[1m\033\\[95mHeading One", res))

# H2 - bold + bright_blue
res <- render("## Heading Two", palette = ansi)
expect_true(grepl("\033\\[1m\033\\[94mHeading Two", res))

# H3 - bright_blue, no bold
res <- render("### Heading Three", palette = ansi)
expect_true(grepl("\033\\[94mHeading Three", res))
expect_false(grepl("\033\\[1m", res))

# Inline transforms apply inside headings.
res <- render("## A **bold** heading", palette = ansi)
expect_true(grepl("\033\\[1mbold\033\\[22m", res))

# Heading style resumes after an inline reset (inline code's
# bright_cyan ends with a full \033[0m). The trailing " API" must
# still be styled with the H2 bold + bright_blue prefix.
res <- render("## The `corteza::chat()` API", palette = ansi)
# Find the "API" substring and confirm it's preceded by the heading
# resume (bold + bright_blue), not stranded after a bare reset.
expect_true(grepl("\033\\[0m\033\\[1m\033\\[94m API", res))

# Same for an inline link inside a heading.
res <- render("## See [docs](https://x) for setup", palette = ansi)
expect_true(grepl("\033\\[0m\033\\[1m\033\\[94m for setup", res))

# Blockquote
res <- render("> quoted text", palette = ansi)
expect_true(grepl("\033\\[2m\\|\033\\[0m quoted text", res))

# Bulleted list - dash and asterisk both get the same treatment.
res <- render("- first\n* second", palette = ansi)
expect_true(grepl("\033\\[32m•\033\\[0m first", res))
expect_true(grepl("\033\\[32m•\033\\[0m second", res))

# Indented sub-bullets preserve indent.
res <- render("- top\n  - nested", palette = ansi)
expect_true(grepl("^  \033\\[32m•\033\\[0m nested$",
                  strsplit(res, "\n")[[1]][2]))

# Fenced code block - fence lines are hidden, body is dim+indented,
# inline regex is bypassed inside the block.
src <- "```r\nx <- **not bold**\n```"
res <- render(src, palette = ansi)
expect_false(grepl("```", res))
expect_true(grepl("  \033\\[2mx <- \\*\\*not bold\\*\\*\033\\[0m", res))
# A single-line body should be the only line in the output.
expect_equal(length(strsplit(res, "\n", fixed = TRUE)[[1]]), 1L)

# Surrounding blank lines and language tag are handled cleanly.
src <- "before\n\n```bash\necho hello\n```\n\nafter"
res <- render(src, palette = ansi)
expect_false(grepl("```", res))
expect_false(grepl("bash", res))
expect_true(grepl("  \033\\[2mecho hello\033\\[0m", res))

# --- Edge cases ---

# Empty string returns unchanged.
expect_equal(render("", palette = ansi), "")

# Non-character / multi-element passes through (defensive).
expect_equal(render(NULL, palette = ansi), NULL)
expect_equal(render(c("a", "b"), palette = ansi), c("a", "b"))

# Mixed content end-to-end.
md <- paste(
    "# Title",
    "",
    "Some **bold** and *italic* and `code`.",
    "",
    "- bullet one",
    "- bullet two",
    "",
    "> a quote",
    "",
    "```",
    "raw code",
    "```",
    sep = "\n"
)
res <- render(md, palette = ansi)
expect_true(grepl("Title", res))
expect_true(grepl("\033\\[1mbold\033\\[22m", res))
expect_true(grepl("\033\\[3mitalic\033\\[23m", res))
expect_true(grepl("\033\\[32m•\033\\[0m bullet", res))
expect_true(grepl("\\|\033\\[0m a quote", res))
expect_true(grepl("raw code", res))
expect_false(grepl("```", res))
