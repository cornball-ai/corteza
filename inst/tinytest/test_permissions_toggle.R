library(tinytest)

# ---- permissions_command: display ----

cfg <- list(approval_mode = "ask")
res <- corteza:::permissions_command(cfg, NULL, cwd = tempdir())
expect_false(res$changed)
expect_null(res$mode)
expect_true(grepl("Approval mode: ask", res$text))
# The display names the toggle, so it is discoverable from the command
# itself rather than only from /help.
expect_true(grepl("/permissions allow\\|ask\\|deny", res$text))

# ---- permissions_command: switching ----

res <- corteza:::permissions_command(cfg, "allow", cwd = tempdir())
expect_true(res$changed)
expect_equal(res$mode, "allow")
expect_true(grepl("was ask", res$text))
# Relaxing approvals must say what it did not relax.
expect_true(grepl("Credential paths still prompt", res$text))
# And where to make it stick.
expect_true(grepl("Permanent", res$text))

res <- corteza:::permissions_command(list(approval_mode = "allow"), "deny")
expect_true(res$changed)
expect_equal(res$mode, "deny")
expect_true(grepl("was allow", res$text))

# Setting the mode it already has is honest about being a no-op.
res <- corteza:::permissions_command(list(approval_mode = "ask"), "ask")
expect_true(res$changed)
expect_true(grepl("unchanged", res$text))

# Case and whitespace tolerated.
expect_equal(corteza:::permissions_command(cfg, "  ALLOW ")$mode, "allow")

# ---- permissions_command: rejects nonsense ----

for (bad in c("yes", "on", "off", "true", "always", "-1")) {
    res <- corteza:::permissions_command(cfg, bad)
    expect_false(res$changed)
    expect_null(res$mode)
    expect_true(grepl("Unknown approval mode", res$text))
}

# Empty / NA behave as "just show me", not as a mode.
expect_false(corteza:::permissions_command(cfg, "")$changed)
expect_false(corteza:::permissions_command(cfg, NA)$changed)
expect_false(corteza:::permissions_command(cfg, NULL)$changed)

# ---- the toggle actually changes what policy() decides ----
#
# The point of the command. A toggle that updates a config field but
# leaves dispatch unchanged would pass every assertion above and still
# prompt on every call, so assert the decision itself rather than the
# field. write_file is in default_dangerous_tools(), which is what makes
# it prompt under "ask".

call <- list(tool = "write_file", channel = "console",
             args = list(path = "R/x.R"), paths = "/srv/work/repo/R/x.R")

ask_cfg <- list(approval_mode = "ask",
                dangerous_tools = corteza:::default_dangerous_tools())
expect_equal(corteza::policy(call, config = ask_cfg)$approval, "ask")

toggled <- corteza:::permissions_command(ask_cfg, "allow")
ask_cfg$approval_mode <- toggled$mode
expect_equal(corteza::policy(call, config = ask_cfg)$approval, "allow")

toggled <- corteza:::permissions_command(ask_cfg, "deny")
ask_cfg$approval_mode <- toggled$mode
expect_equal(corteza::policy(call, config = ask_cfg)$approval, "deny")

# ---- driven through the real REPL loop ----
#
# Everything above tests the helper. This drives run_repl_loop() itself,
# so the wiring is verified rather than assumed: a helper that works and
# a command that never reaches it look identical from the helper's side.

scripted_input <- function(lines) {
    i <- 0L
    function(prompt_str) {
        i <<- i + 1L
        if (i <= length(lines)) {
            lines[[i]]
        } else {
            character(0)
        }
    }
}

toggle_ctx <- function(lines, mode = "ask") {
    ctx <- new.env(parent = emptyenv())
    ctx$ws_enabled <- FALSE
    ctx$palette <- list(dim = "", reset = "", cyan = "", bold = "",
                        yellow = "", green = "", bright_magenta = "",
                        red = "", magenta = "")
    ctx$read_input <- scripted_input(lines)
    ctx$help_text <- function() "HELP"
    ctx$handle_copy <- function(x) invisible(NULL)
    ctx$format_tools <- function(s) "TOOLS"
    ctx$pending_r_context <- character(0)
    ctx$last_assistant_response <- ""
    ctx$cwd <- tempdir()
    ctx$config <- list(approval_mode = mode)
    ctx$session <- new.env(parent = emptyenv())
    ctx$session$config <- list(approval_mode = mode)
    ctx
}

# Both copies move together. policy() reads session$config at call time
# while the display commands render ctx$config; updating one and not the
# other is how a toggle ends up lying about itself.
ctx <- toggle_ctx(c("/permissions allow"))
out <- capture.output(corteza:::run_repl_loop(ctx))
expect_equal(ctx$config$approval_mode, "allow")
expect_equal(ctx$session$config$approval_mode, "allow")
expect_true(any(grepl("was ask", out)))

# Bare /permissions still renders and changes nothing.
ctx <- toggle_ctx(c("/permissions"))
out <- capture.output(corteza:::run_repl_loop(ctx))
expect_equal(ctx$config$approval_mode, "ask")
expect_true(any(grepl("Approval mode: ask", out)))

# A bad argument leaves the mode alone rather than falling through to
# some default.
ctx <- toggle_ctx(c("/permissions banana"))
out <- capture.output(corteza:::run_repl_loop(ctx))
expect_equal(ctx$config$approval_mode, "ask")
expect_equal(ctx$session$config$approval_mode, "ask")
expect_true(any(grepl("Unknown approval mode", out)))

# Round trip: relax then tighten again within one session.
ctx <- toggle_ctx(c("/permissions allow", "/permissions deny"))
out <- capture.output(corteza:::run_repl_loop(ctx))
expect_equal(ctx$config$approval_mode, "deny")
expect_equal(ctx$session$config$approval_mode, "deny")

# ---- what the toggle cannot waive ----
#
# check_safety() short-circuits before the config overlay, so a
# credential path keeps asking under every mode. This is the claim the
# command prints at the user, so it gets an assertion rather than a
# comment.

secret_call <- list(tool = "read_file", channel = "console",
                    args = list(path = "~/.ssh/id_rsa"),
                    paths = "~/.ssh/id_rsa")
for (mode in c("allow", "ask", "deny")) {
    d <- corteza::policy(secret_call,
                         config = list(approval_mode = mode,
                                       dangerous_tools =
                                           corteza:::default_dangerous_tools()))
    expect_equal(d$approval, "ask")
    expect_true(grepl("^safety:", d$reason))
}
