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
out <- banner(version = "0.0.0", model = strrep("X", 30L),
              provider = "p", tools_count = 1L, palette = off)
# Truncation cap on model is 10; long name becomes "XXXXXXX..." (7 X + "...").
expect_true(grepl(paste0(strrep("X", 7L), "..."), out, fixed = TRUE))
expect_false(grepl(strrep("X", 11L), out, fixed = TRUE))

# --- no-ANSI fallback --------------------------------------------

# When the palette has no ANSI, Y characters render as '*' (a
# placeholder so the silhouette is still visually distinct in plain
# text). Confirms the no-color branch fires.
out <- banner(version = "0.0.0", model = "m", provider = "p",
              tools_count = 1L, palette = off)
expect_false(grepl("\033", out, fixed = TRUE))
expect_true(grepl("*", out, fixed = TRUE))

# --- ANSI on -----------------------------------------------------

out <- banner(version = "0.0.0", model = "m", provider = "p",
              tools_count = 1L, palette = ansi)
# Color escape (256-color index 220) appears.
expect_true(grepl("\033\\[38;5;220m", out))
# Reset escape appears.
expect_true(grepl("\033\\[0m", out))
# Block character appears (the Y -> block conversion).
expect_true(grepl("█", out))
# No literal 'Y' left in the colored output.
expect_false(grepl("Y", out, fixed = TRUE))
