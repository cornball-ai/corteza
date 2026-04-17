# Test deterministic retrieval

# Setup
corteza:::ws_clear()

# ---- Scoring ----

# Recent object scores higher than old
corteza:::ws_set_turn(10L)
corteza:::ws_put("recent", data.frame(x = 1))
meta_recent <- corteza:::ws_meta("recent")
meta_recent$turn <- 9L
corteza:::ws_set_meta("recent", meta_recent)

corteza:::ws_put("old", data.frame(y = 1))
meta_old <- corteza:::ws_meta("old")
meta_old$turn <- 1L
corteza:::ws_set_meta("old", meta_old)

score_recent <- corteza:::ws_score(corteza:::ws_meta("recent"), "anything", 10L)
score_old <- corteza:::ws_score(corteza:::ws_meta("old"), "anything", 10L)
expect_true(score_recent > score_old)

# Keyword match boosts score
corteza:::ws_put("sales_data", data.frame(sales = 1:5))
score_match <- corteza:::ws_score(corteza:::ws_meta("sales_data"), "sales data", 10L)
score_nomatch <- corteza:::ws_score(corteza:::ws_meta("sales_data"), "xyz abc", 10L)
expect_true(score_match > score_nomatch)

# Stale penalty
corteza:::ws_put("fresh_obj", "fresh")
corteza:::ws_put("stale_obj", "stale")
corteza:::ws_invalidate("stale_obj")
score_fresh <- corteza:::ws_score(corteza:::ws_meta("fresh_obj"), "test", 10L)
score_stale <- corteza:::ws_score(corteza:::ws_meta("stale_obj"), "test", 10L)
expect_true(score_fresh > score_stale)

# Pinned bonus
corteza:::ws_put("pinned_obj", "pinned", pinned = TRUE)
corteza:::ws_put("normal_obj", "normal")
score_pinned <- corteza:::ws_score(corteza:::ws_meta("pinned_obj"), "test", 10L)
score_normal <- corteza:::ws_score(corteza:::ws_meta("normal_obj"), "test", 10L)
expect_true(score_pinned > score_normal)

# ---- Budget selection ----

corteza:::ws_clear()
corteza:::ws_set_turn(5L)

# Put several objects
for (i in 1:5) {
    corteza:::ws_put(paste0("item", i), paste(rep("x", 100), collapse = ""))
}

# Small budget should return fewer items
small <- corteza:::ws_retrieve("test", budget_chars = 300L)
all <- corteza:::ws_retrieve("test", budget_chars = 50000L)
expect_true(length(small) <= length(all))
expect_true(length(small) > 0)

# ---- Empty workspace ----

corteza:::ws_clear()
result <- corteza:::ws_retrieve("anything")
expect_equal(length(result), 0L)

# ---- Format output ----

corteza:::ws_clear()
corteza:::ws_set_turn(1L)
corteza:::ws_put("df", data.frame(a = 1:3, b = c("x", "y", "z")),
                origin = list(tool = "run_r"))

retrieved <- corteza:::ws_retrieve("data frame", budget_chars = 5000L)
ctx <- corteza:::ws_format_context(retrieved)
expect_true(nchar(ctx) > 0)
expect_true(grepl("Workspace State", ctx))
expect_true(grepl("df", ctx))

# Empty format (clear workspace first so digest is empty too)
corteza:::ws_clear()
expect_equal(corteza:::ws_format_context(list()), "")

# ---- Summarize ----

# data.frame summary includes dimensions and column types
corteza:::ws_put("test_df", data.frame(x = 1:5, y = letters[1:5]))
meta <- corteza:::ws_meta("test_df")
summary <- corteza:::ws_summarize("test_df", corteza:::ws_get("test_df"), meta)
expect_true(grepl("5x2", summary))
# Enhanced summary includes type and range for numeric columns
expect_true(grepl("x(integer", summary, fixed = TRUE))

# function summary shows args
corteza:::ws_put("test_fn", function(a, b, c) a + b + c)
meta_fn <- corteza:::ws_meta("test_fn")
summary_fn <- corteza:::ws_summarize("test_fn", corteza:::ws_get("test_fn"), meta_fn)
expect_true(grepl("function(a, b, c)", summary_fn, fixed = TRUE))

# ---- Digest ----

# Empty workspace gives empty digest
corteza:::ws_clear()
expect_equal(corteza:::ws_digest(), "")

# With objects, digest is non-empty
corteza:::ws_put("d1", data.frame(x = 1:100, y = rnorm(100)))
corteza:::ws_put("v1", letters)
digest <- corteza:::ws_digest()
expect_true(nchar(digest) > 0)
expect_true(grepl("2 objects", digest))
expect_true(grepl("d1", digest))
expect_true(grepl("data.frame 100x2", digest))
expect_true(grepl("v1", digest))

# Pinned objects get asterisk
corteza:::ws_put("pinned1", 42, pinned = TRUE)
digest2 <- corteza:::ws_digest()
expect_true(grepl("pinned1.*\\*", digest2))

# ws_format_context includes digest
retrieved <- corteza:::ws_retrieve("anything", budget_chars = 50000L)
ctx <- corteza:::ws_format_context(retrieved)
expect_true(grepl("Workspace State", ctx))
expect_true(grepl("objects", ctx))  # digest line present

# ---- Enhanced summaries ----

# Data frame with NAs shows NA count
corteza:::ws_clear()
df_na <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
corteza:::ws_put("df_na", df_na)
meta_na <- corteza:::ws_meta("df_na")
summary_na <- corteza:::ws_summarize("df_na", df_na, meta_na)
expect_true(grepl("1NA", summary_na))

# Data frame with numeric shows range
df_range <- data.frame(price = c(10.5, 20.0, 99.9))
corteza:::ws_put("df_range", df_range)
meta_range <- corteza:::ws_meta("df_range")
summary_range <- corteza:::ws_summarize("df_range", df_range, meta_range)
expect_true(grepl("10.5", summary_range))
expect_true(grepl("99.9", summary_range))

# Data frame with factor shows levels
df_fac <- data.frame(color = factor(c("red", "blue", "green")))
corteza:::ws_put("df_fac", df_fac)
meta_fac <- corteza:::ws_meta("df_fac")
summary_fac <- corteza:::ws_summarize("df_fac", df_fac, meta_fac)
expect_true(grepl("blue", summary_fac))

# Matrix summary
mat <- matrix(1:12, nrow = 3)
corteza:::ws_put("mat1", mat)
meta_mat <- corteza:::ws_meta("mat1")
summary_mat <- corteza:::ws_summarize("mat1", mat, meta_mat)
expect_true(grepl("matrix", summary_mat))
expect_true(grepl("3x4", summary_mat))

# Character vector summary shows length and samples
corteza:::ws_put("words", c("hello", "world", "foo", "bar"))
meta_words <- corteza:::ws_meta("words")
summary_words <- corteza:::ws_summarize("words",
    corteza:::ws_get("words"), meta_words)
expect_true(grepl("character\\[4\\]", summary_words))
expect_true(grepl("hello", summary_words))

# max_chars truncation
big_df <- data.frame(matrix(rnorm(100), ncol = 20))
corteza:::ws_put("big_df", big_df)
meta_big <- corteza:::ws_meta("big_df")
short_summary <- corteza:::ws_summarize("big_df", big_df, meta_big,
    max_chars = 100L)
expect_true(nchar(short_summary) <= 100)

# Clean up
corteza:::ws_clear()
