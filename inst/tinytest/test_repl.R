# run_repl_loop(): minimal /help-then-EOF drive of the shared loop.
# Exercises the loop's input/dispatch/help/EOF path without touching
# the LLM, turn(), or any session machinery -- only the injected hooks.

# Scripted input: "/help" first call, EOF (character(0)) second so the
# loop hits the help branch and then the clean exit.
calls <- 0L
read_input <- function(prompt_str) {
    calls <<- calls + 1L
    if (calls == 1L) {
        "/help"
    } else {
        character(0)
    }
}

# Track that the help hook fired.
help_hit <- FALSE
help_text <- function() {
    help_hit <<- TRUE
    "HELP"
}

empty_palette <- list(dim = "", reset = "", cyan = "", bold = "",
                      yellow = "", green = "", bright_magenta = "")

ctx <- new.env(parent = emptyenv())
ctx$ws_enabled <- FALSE
ctx$palette <- empty_palette
ctx$help_text <- help_text
ctx$read_input <- read_input

# run_repl_loop returns invisible(NULL); the /help-then-EOF path must
# not error and must hit the help hook exactly once.
res <- corteza:::run_repl_loop(ctx)

expect_null(res)
expect_true(help_hit)
expect_equal(calls, 2L)
