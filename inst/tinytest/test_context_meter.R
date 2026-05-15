library(tinytest)

no_color <- stats::setNames(as.list(rep("", 16L)),
                            c("reset", "bold", "dim", "red", "green",
                              "yellow", "blue", "magenta", "cyan", "white",
                              "bright_red", "bright_green", "bright_yellow",
                              "bright_blue", "bright_magenta", "bright_cyan"))

# Header carries used / total / pct / compact threshold.
block <- corteza:::format_context_block(
                                        used = 24700,
                                        limit = 128000,
                                        breakdown = list(system = 22000, tools = 2700,
                                                         history = 56),
                                        compact_pct = 90,
                                        palette = no_color
)
expect_true(grepl("24.7K / 128.0K", block, fixed = TRUE))
expect_true(grepl("19%", block, fixed = TRUE))
expect_true(grepl("compact 90%", block, fixed = TRUE))
# Bar present.
expect_true(grepl("\\[.{50}\\]", block))
# Breakdown rows.
expect_true(grepl("system", block, fixed = TRUE))
expect_true(grepl("22.0K", block, fixed = TRUE))
expect_true(grepl("89%", block, fixed = TRUE))
expect_true(grepl("tools", block, fixed = TRUE))
expect_true(grepl("2.7K", block, fixed = TRUE))
expect_true(grepl("11%", block, fixed = TRUE))
expect_true(grepl("history", block, fixed = TRUE))
# < 1% rows omit the percent display.
hist_row <- grep("history", strsplit(block, "\n")[[1]], value = TRUE)
expect_false(any(grepl("0%", hist_row, fixed = TRUE)))
# No verbose "Project context comes from saber" paragraph.
expect_false(grepl("Project context", block, fixed = TRUE))

# No-files variant prints the dim short note.
expect_true(grepl("No context files loaded", block, fixed = TRUE))

# With files: list rendered, count in header.
block_files <- corteza:::format_context_block(
                                              used = 1000, limit = 100000,
                                              breakdown = list(system = 1000),
                                              files = c("a.md", "b.md"),
                                              palette = no_color
)
expect_true(grepl("Context files (2)", block_files, fixed = TRUE))
expect_true(grepl("  a.md", block_files, fixed = TRUE))
expect_true(grepl("  b.md", block_files, fixed = TRUE))

# Filled-cell counts: at 0% none; at 50% half; at 100%+ all.
empty_bar <- corteza:::.context_meter_bar(0, palette = no_color,
                                          width = 20L)
expect_equal(sum(strsplit(empty_bar, "")[[1]] == "█"), 0L)
half_bar <- corteza:::.context_meter_bar(50, palette = no_color,
                                         width = 20L)
expect_equal(sum(strsplit(half_bar, "")[[1]] == "█"), 10L)
full_bar <- corteza:::.context_meter_bar(100, palette = no_color,
                                         width = 20L)
expect_equal(sum(strsplit(full_bar, "")[[1]] == "█"), 20L)
over_bar <- corteza:::.context_meter_bar(150, palette = no_color,
                                         width = 20L)
expect_equal(sum(strsplit(over_bar, "")[[1]] == "█"), 20L)

# Compact tick appears at the right cell when the bar isn't full
# past it.
tick_bar <- corteza:::.context_meter_bar(50, compact_pct = 80,
                                         palette = no_color,
                                         width = 10L)
# 10 cells, compact at 80% = cell 8. Used at 50% = 5 cells. Cells
# 6/7/9/10 should be ".", cell 8 should be "│".
chars <- strsplit(tick_bar, "")[[1]]
brackets <- which(chars %in% c("[", "]"))
# Strip the brackets to get the inner cells.
inner <- chars[(brackets[1] + 1):(brackets[2] - 1)]
expect_equal(sum(inner == "│"), 1L)
expect_equal(inner[8], "│")

# Color thresholds: < warn = green; warn..high = yellow; high..crit =
# bright_yellow; >= crit = bright_red.
palette_named <- list(green = "G", yellow = "Y", bright_yellow = "O",
                      bright_red = "R", dim = "D", reset = "Z", bold = "B")
expect_identical(corteza:::.context_pct_color(50, palette_named), "G")
expect_identical(corteza:::.context_pct_color(80, palette_named), "Y")
expect_identical(corteza:::.context_pct_color(92, palette_named), "O")
expect_identical(corteza:::.context_pct_color(99, palette_named), "R")
