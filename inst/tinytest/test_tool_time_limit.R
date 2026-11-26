# Slow. Skipped on CRAN for check time; CI sets NOT_CRAN.
if (!tinytest::at_home() && !identical(Sys.getenv("NOT_CRAN"), "true")) {
    exit_file("slow; runs at home and in CI")
}

# Temporary user folders for this file, however it is run (isolate.R).
source("isolate.R", local = TRUE)

library(tinytest)

# A tool's time limit stays inside the tool (R/skill.R), and grep_files
# reads text within bounds (R/tool-impl.R). Nothing here reaches a model.

ns <- asNamespace("corteza")

# ---- The limit fires where skill_run() catches it --------------------------
# R runs a calling handler outside the tool with the tool's own handlers
# out of reach. A bytecode loop asks for the limit every 1024 turns, so a
# handler that loops 5000 times is where an armed limit fires. skill_run()
# holds the tool's warnings and messages until the limit is off.
local({
    # The limit passes while the tool sleeps, where R does not look.
    slow <- list(name = "slow_warner", inputSchema = list(),
                 handler = function(args, ctx) {
        Sys.sleep(1.3)
        warning("late warning")
        message("late message")
        corteza:::ok("done")
    })
    busy <- function() for (i in seq_len(5000L)) NULL
    out <- tryCatch(withCallingHandlers(
        ns$skill_run(slow, list(), timeout = 1L),
        warning = function(w) {
            busy()
            invokeRestart("muffleWarning")
        },
        message = function(m) {
            busy()
            invokeRestart("muffleMessage")
        }), error = function(e) paste("escaped:", conditionMessage(e)))
    # The tool ends in skill_run(): done, or timed out, never an error
    # that leaves it.
    expect_true(is.list(out), info = paste(out, collapse = " "))

    # The same for a tool that fails of its own accord after the limit
    # passed: the limit is off before its error is reported and before
    # what it said goes out.
    failing <- list(name = "late_failure", inputSchema = list(),
                    handler = function(args, ctx) {
        message("early message")
        Sys.sleep(1.3)
        stop("broke")
    })
    out <- tryCatch(withCallingHandlers(
        ns$skill_run(failing, list(), timeout = 1L),
        message = function(m) {
            busy()
            invokeRestart("muffleMessage")
        }), error = function(e) paste("escaped:", conditionMessage(e)))
    expect_true(is.list(out), info = paste(out, collapse = " "))
    expect_true(is.list(out) && isTRUE(out$isError))
})

local({
    # The handlers outside see what the tool said, in order, once the
    # tool has finished.
    said <- list(name = "said", inputSchema = list(),
                 handler = function(args, ctx) {
        warning("a warning")
        message("a message")
        order <<- c(order, "tool done")
        corteza:::ok("done")
    })
    order <- character()
    seen <- character()
    out <- withCallingHandlers(ns$skill_run(said, list(), timeout = 5L),
                               warning = function(w) {
        order <<- c(order, "handler")
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
    }, message = function(m) {
        order <<- c(order, "handler")
        seen <<- c(seen, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    expect_false(isTRUE(out$isError))
    expect_identical(seen, c("a warning", "a message\n"))
    expect_identical(order, c("tool done", "handler", "handler"))
})

local({
    # A tool that fails after a warning: its error is the result, and the
    # warning still reaches the handlers outside.
    failing <- list(name = "failing", inputSchema = list(),
                    handler = function(args, ctx) {
        warning("first")
        stop("broke")
    })
    seen <- character()
    out <- withCallingHandlers(ns$skill_run(failing, list(), timeout = 5L),
                               warning = function(w) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    expect_true(isTRUE(out$isError))
    expect_true(grepl("Skill error: broke", out$content[[1L]]$text))
    expect_identical(seen, "first")

    # The first 100 are passed on; the rest are counted in the log.
    noisy <- list(name = "noisy", inputSchema = list(),
                  handler = function(args, ctx) {
        for (i in 1:150) warning(sprintf("w%d", i))
        corteza:::ok("done")
    })
    seen <- character()
    out <- withCallingHandlers(ns$skill_run(noisy, list(), timeout = 5L),
                               warning = function(w) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    expect_false(isTRUE(out$isError))
    expect_identical(seen, sprintf("w%d", 1:100))

    # A tool without a limit is left as it was: its conditions go out as
    # it raises them.
    seen <- character()
    order <- character()
    plain <- list(name = "plain", inputSchema = list(),
                  handler = function(args, ctx) {
        warning("now")
        order <<- c(order, "after the warning")
        corteza:::ok("done")
    })
    withCallingHandlers(ns$skill_run(plain, list(), timeout = NULL),
                        warning = function(w) {
        order <<- c(order, "handler")
        invokeRestart("muffleWarning")
    })
    expect_identical(order, c("handler", "after the warning"))
})

# ---- Warnings that are errors ----------------------------------------------
# Codex's finding on #289: with options(warn = 2), the held warning let
# the tool run on past it, and its release raised the error outside
# skill_run(). The tool stops at the warning, as R stops it, and the
# error is the tool's result.
local({
    op <- options(warn = 2L)
    on.exit(options(op), add = TRUE)
    ran <- FALSE
    tool <- list(name = "warning_probe", inputSchema = list(),
                 handler = function(args, ctx) {
        warning("stop here")
        ran <<- TRUE
        corteza:::ok("executed after warning")
    })
    out <- tryCatch(ns$skill_run(tool, list(), timeout = 5L),
                    error = function(e) paste("escaped:", conditionMessage(e)))
    expect_false(ran)
    expect_true(is.list(out) && isTRUE(out$isError),
                info = paste(unlist(out), collapse = " "))
    text <- ""
    if (is.list(out) && length(out$content)) {
        text <- out$content[[1L]]$text
    }
    expect_identical(text, "Skill error: (converted from warning) stop here")
})
# A tool that makes warnings errors after one: that warning passes on as
# the warning it was raised as, and nothing escapes.
local({
    op <- options(warn = 1L)
    on.exit(options(op), add = TRUE)
    tool <- list(name = "late_strict", inputSchema = list(),
                 handler = function(args, ctx) {
        warning("raised under warn 1")
        options(warn = 2L)
        corteza:::ok("done")
    })
    out <- NULL
    said <- utils::capture.output(type = "message",
        out <- tryCatch(ns$skill_run(tool, list(), timeout = 5L),
                        error = function(e) paste("escaped:",
                                                  conditionMessage(e))))
    options(op)
    expect_true(is.list(out) && !isTRUE(out$isError),
                info = paste(unlist(out), collapse = " "))
    expect_true(any(grepl("raised under warn 1", said, fixed = TRUE)))
})

# ---- grep_files reads text within bounds -----------------------------------
local({
    dir <- tempfile("grep_bounds_")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    writeLines(c("one needle", "two", "three needle"), file.path(dir, "a.txt"))
    # Binary: a NUL byte in its head, and the word too.
    writeBin(c(charToRaw("needle"), as.raw(c(0, 1, 2, 10)),
               charToRaw("needle\n")), file.path(dir, "b.bin"))
    # Latin-1 text: grep() would warn for the line and pass it by.
    writeBin(c(charToRaw("caf"), as.raw(0xe9), charToRaw(" needle\n")),
               file.path(dir, "c.txt"))
    writeLines(strrep("x", 2000), file.path(dir, "d.txt"))

    grep_in <- function(limits = list(), ...) {
        real <- get(".grep_limits", envir = ns)
        on.exit(assignInNamespace(".grep_limits", real, ns = "corteza"),
                add = TRUE)
        assignInNamespace(".grep_limits", utils::modifyList(real, limits),
                          ns = "corteza")
        # A warning counts as a failure, and a failure is a line of the
        # result, so the assertions below say what went wrong.
        text <- tryCatch(withCallingHandlers(
            ns$tool_grep_files("needle", path = dir, ...)$content[[1L]]$text,
            warning = function(w) stop("grep_files warned: ", conditionMessage(w))),
            error = function(e) paste("failed:", conditionMessage(e)))
        strsplit(text, "\n", fixed = TRUE)[[1L]]
    }

    # Paths are compared by their end: a temporary folder can resolve
    # elsewhere (macOS: /var is /private/var).
    has <- function(lines, end) any(endsWith(lines, end))
    lines <- grep_in(list(file_bytes = 1000), file_pattern = "*")
    expect_true(has(lines, "a.txt:1: one needle"))
    expect_true(has(lines, "a.txt:3: three needle"))
    # The Latin-1 line is found and shown as UTF-8.
    expect_true(has(lines, "c.txt:1: café needle"))
    expect_true(all(validUTF8(lines)))
    # The binary file is passed by, and so is the one over the size.
    expect_false(any(grepl("b.bin", lines, fixed = TRUE)))
    expect_identical(tail(lines, 1L),
                     "Skipped 2 files: 1 binary, 1 over 1000 bytes.")

    # At the match limit the search stops and says so.
    lines <- grep_in(list(matches = 2L), file_pattern = "*.txt")
    expect_identical(length(lines), 3L)
    expect_identical(lines[3L], "Stopped at 2 matches.")

    # A long matching line is cut.
    writeLines(paste("needle", strrep("y", 100)), file.path(dir, "e.txt"))
    lines <- grep_in(list(line_chars = 20L), file_pattern = "e.txt")
    expect_identical(length(lines), 1L)
    expect_true(has(lines, "e.txt:1: needle yyyyyyyyyyyyy..."))

    # Out of time, it says how many files it did not search.
    lines <- grep_in(list(seconds = 0L), file_pattern = "*.txt")
    expect_identical(lines, c("No matches found",
                              "Stopped after 0 s, 4 files not searched."))

    # Nothing found, nothing skipped: as before.
    out <- ns$tool_grep_files("absent", path = dir, file_pattern = "a.txt")
    expect_identical(out$content[[1L]]$text, "No matches found")
    # The default limits.
    expect_identical(ns$.grep_limits,
                     list(file_bytes = 10 * 1024^2, matches = 500L,
                          line_chars = 500L, seconds = 20L))
    expect_identical(ns$.grep_skipped_note(c(binary = 0L, large = 1L,
                                             unreadable = 0L), 10 * 1024^2),
                     "Skipped 1 file: 1 over 10 MB.")
})
