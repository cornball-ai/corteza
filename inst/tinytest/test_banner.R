# Tests for the corteza startup banner renderer. Visual tests don't
# fit tinytest, so this exercises the substitution and ANSI-wrapping
# logic rather than the final look.

library(tinytest)

banner <- corteza:::corteza_startup_banner

# Forced palettes so tests don't depend on the runner's TTY (which
# is FALSE under tinytest, making ansi_colors() return all empty
# strings).
key_names <- c("reset", "bold", "dim", "red", "green", "yellow",
               "blue", "magenta", "cyan", "white", "bright_red",
               "bright_green", "bright_yellow", "bright_blue",
               "bright_magenta", "bright_cyan")
ansi <- stats::setNames(
                        as.list(c("\033[0m", "\033[1m", "\033[2m", "\033[31m",
                                  "\033[32m", "\033[33m", "\033[34m", "\033[35m",
                                  "\033[36m", "\033[37m", "\033[91m", "\033[92m",
                                  "\033[93m", "\033[94m", "\033[95m", "\033[96m")),
                        key_names
)
off <- stats::setNames(as.list(rep("", length(key_names))), key_names)

# --- substitution ------------------------------------------------

out <- banner(version = "9.9.9", model = "modA", provider = "provB")
expect_true(grepl("v9.9.9", out, fixed = TRUE))
expect_true(grepl("modA", out, fixed = TRUE))
expect_true(grepl("provB", out, fixed = TRUE))
expect_true(grepl("corteza", out, fixed = TRUE))
expect_true(grepl("/help", out, fixed = TRUE))
expect_true(grepl("/quit", out, fixed = TRUE))
# tools count is no longer displayed in the banner.
expect_false(grepl("tools", out, fixed = TRUE))

# 4th-component dev marker is dropped for display.
out2 <- banner(version = "9.9.9.42", model = "m", provider = "p")
expect_true(grepl("v9.9.9", out2, fixed = TRUE))
expect_false(grepl("v9.9.9.42", out2, fixed = TRUE))

# Banner is 9 terminal rows tall (one per pixel row, no compaction).
expect_equal(length(strsplit(out, "\n", fixed = TRUE)[[1]]), 9L)

# --- truncation --------------------------------------------------

# A long model name is truncated rather than blowing out the row.
out <- banner(version = "0.0.0", model = strrep("X", 30L), provider = "p")
# Truncation cap on model is 9 (right-padded slot); long name becomes
# "XXXXXX..." (6 X + "..." = 9 chars).
expect_true(grepl(paste0(strrep("X", 6L), "..."), out, fixed = TRUE))
expect_false(grepl(strrep("X", 10L), out, fixed = TRUE))

# --- no-ANSI fallback --------------------------------------------

# Banner uses the yellow-square emoji (U+1F7E8) as kernels. No ANSI
# escapes -- the emoji is its own color, supported across modern
# terminals. Confirm the emoji appears and Y placeholders are gone.
out <- banner(version = "0.0.0", model = "m", provider = "p")
expect_false(grepl("\033", out, fixed = TRUE))
expect_true(grepl("\U0001F7E8", out, fixed = TRUE))
expect_false(grepl("Y", out, fixed = TRUE))

# Banner is the same regardless of palette argument (palette is now
# ignored; kept for backward-compat with old callers).
out2 <- banner(version = "0.0.0", model = "m", provider = "p",
               palette = ansi)
expect_equal(out, out2)
