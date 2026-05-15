library(tinytest)

# format_worked_for: pure formatter. Build POSIXct values explicitly
# so the test never depends on real-time clocks.
ts <- function(t) as.POSIXct(t, origin = "1970-01-01", tz = "UTC")

# Sub-second elapses round to "<1s".
expect_identical(corteza:::format_worked_for(ts(0), ts(0)),
                 "Worked for <1s")
expect_identical(corteza:::format_worked_for(ts(0), ts(0.4)),
                 "Worked for <1s")

# Sub-minute: integer seconds.
expect_identical(corteza:::format_worked_for(ts(0), ts(1)),
                 "Worked for 1s")
expect_identical(corteza:::format_worked_for(ts(0), ts(4)),
                 "Worked for 4s")
expect_identical(corteza:::format_worked_for(ts(0), ts(59)),
                 "Worked for 59s")

# Minutes: includes the seconds component, even when it's 0.
expect_identical(corteza:::format_worked_for(ts(0), ts(60)),
                 "Worked for 1m 0s")
expect_identical(corteza:::format_worked_for(ts(0), ts(198)),
                 "Worked for 3m 18s")

# Hours: includes minutes and seconds.
expect_identical(corteza:::format_worked_for(ts(0), ts(3600)),
                 "Worked for 1h 0m 0s")
expect_identical(corteza:::format_worked_for(ts(0), ts(3725)),
                 "Worked for 1h 2m 5s")

# Defensive: end before start, or NaN end, treated as <1s.
expect_identical(corteza:::format_worked_for(ts(10), ts(5)),
                 "Worked for <1s")

# turn_footer_line: wraps with dim ANSI and pads with ─ to width.
no_color <- stats::setNames(as.list(rep("", 16L)),
                            c("reset", "bold", "dim", "red", "green",
                              "yellow", "blue", "magenta", "cyan", "white",
                              "bright_red", "bright_green", "bright_yellow",
                              "bright_blue", "bright_magenta", "bright_cyan"))

line <- corteza:::turn_footer_line(ts(0), ts(198),
                                   palette = no_color, width = 40L)
expect_true(grepl("Worked for 3m 18s", line, fixed = TRUE))
expect_true(grepl("^─ Worked for 3m 18s ─+$", line))
# Padded to at least the requested width (matches dashes count).
expect_true(nchar(line, type = "chars") >= 40L)

# ANSI palette: dim escape wraps the line.
ansi <- list(reset = "\033[0m", dim = "\033[2m")
ansi_line <- corteza:::turn_footer_line(ts(0), ts(60),
                                        palette = ansi, width = 30L)
expect_true(startsWith(ansi_line, "\033[2m"))
expect_true(endsWith(ansi_line, "\033[0m"))

# Terminal-width detection: COLUMNS env var wins.
local({
    old <- Sys.getenv("COLUMNS")
    on.exit(Sys.setenv(COLUMNS = old), add = TRUE)
    Sys.setenv(COLUMNS = "100")
    expect_identical(corteza:::detect_terminal_width(), 100L)
    Sys.unsetenv("COLUMNS")
    # Falls back to options("width").
    op <- options(width = 72L)
    on.exit(options(op), add = TRUE)
    expect_identical(corteza:::detect_terminal_width(), 72L)
})

# When width = NULL, turn_footer_line picks up the detected width.
local({
    old <- Sys.getenv("COLUMNS")
    on.exit(Sys.setenv(COLUMNS = old), add = TRUE)
    Sys.setenv(COLUMNS = "120")
    line <- corteza:::turn_footer_line(ts(0), ts(5),
                                       palette = no_color, width = NULL)
    expect_true(nchar(line, type = "chars") >= 120L)
})
