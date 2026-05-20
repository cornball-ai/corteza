# Tests for the RStudio-addin prefix selector. The addin's
# rstudioapi calls are hard to mock; this exercises the pure
# `.corteza_prefix_for(ext, in_chat)` decision function instead.

library(tinytest)

prefix_for <- corteza:::.corteza_prefix_for

# --- in_chat = FALSE: addin is a pass-through, no prefix ----------

expect_equal(prefix_for("r", FALSE), "")
expect_equal(prefix_for("R", FALSE), "")
expect_equal(prefix_for("sh", FALSE), "")
expect_equal(prefix_for("py", FALSE), "")
expect_equal(prefix_for("", FALSE), "")

# --- in_chat = TRUE: route by extension --------------------------

# R script -> /r prefix (matches the chat() / CLI slash command)
expect_equal(prefix_for("r", TRUE), "/r ")
# Case-insensitive
expect_equal(prefix_for("R", TRUE), "/r ")

# Shell script -> ! prefix
expect_equal(prefix_for("sh", TRUE), "! ")
expect_equal(prefix_for("bash", TRUE), "! ")
expect_equal(prefix_for("SH", TRUE), "! ")

# Other extensions -> no prefix (sent as plain LLM input)
expect_equal(prefix_for("py", TRUE), "")
expect_equal(prefix_for("md", TRUE), "")
expect_equal(prefix_for("txt", TRUE), "")
# Untitled buffer (no extension)
expect_equal(prefix_for("", TRUE), "")
