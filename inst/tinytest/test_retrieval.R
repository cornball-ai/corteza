# Test deterministic retrieval

# Setup
llamaR:::ws_clear()

# ---- Scoring ----

# Recent object scores higher than old
llamaR:::ws_set_turn(10L)
llamaR:::ws_put("recent", data.frame(x = 1))
meta_recent <- llamaR:::ws_meta("recent")
meta_recent$turn <- 9L
llamaR:::ws_set_meta("recent", meta_recent)

llamaR:::ws_put("old", data.frame(y = 1))
meta_old <- llamaR:::ws_meta("old")
meta_old$turn <- 1L
llamaR:::ws_set_meta("old", meta_old)

score_recent <- llamaR:::ws_score(llamaR:::ws_meta("recent"), "anything", 10L)
score_old <- llamaR:::ws_score(llamaR:::ws_meta("old"), "anything", 10L)
expect_true(score_recent > score_old)

# Keyword match boosts score
llamaR:::ws_put("sales_data", data.frame(sales = 1:5))
score_match <- llamaR:::ws_score(llamaR:::ws_meta("sales_data"), "sales data", 10L)
score_nomatch <- llamaR:::ws_score(llamaR:::ws_meta("sales_data"), "xyz abc", 10L)
expect_true(score_match > score_nomatch)

# Stale penalty
llamaR:::ws_put("fresh_obj", "fresh")
llamaR:::ws_put("stale_obj", "stale")
llamaR:::ws_invalidate("stale_obj")
score_fresh <- llamaR:::ws_score(llamaR:::ws_meta("fresh_obj"), "test", 10L)
score_stale <- llamaR:::ws_score(llamaR:::ws_meta("stale_obj"), "test", 10L)
expect_true(score_fresh > score_stale)

# Pinned bonus
llamaR:::ws_put("pinned_obj", "pinned", pinned = TRUE)
llamaR:::ws_put("normal_obj", "normal")
score_pinned <- llamaR:::ws_score(llamaR:::ws_meta("pinned_obj"), "test", 10L)
score_normal <- llamaR:::ws_score(llamaR:::ws_meta("normal_obj"), "test", 10L)
expect_true(score_pinned > score_normal)

# ---- Budget selection ----

llamaR:::ws_clear()
llamaR:::ws_set_turn(5L)

# Put several objects
for (i in 1:5) {
    llamaR:::ws_put(paste0("item", i), paste(rep("x", 100), collapse = ""))
}

# Small budget should return fewer items
small <- llamaR:::ws_retrieve("test", budget_chars = 300L)
all <- llamaR:::ws_retrieve("test", budget_chars = 50000L)
expect_true(length(small) <= length(all))
expect_true(length(small) > 0)

# ---- Empty workspace ----

llamaR:::ws_clear()
result <- llamaR:::ws_retrieve("anything")
expect_equal(length(result), 0L)

# ---- Format output ----

llamaR:::ws_clear()
llamaR:::ws_set_turn(1L)
llamaR:::ws_put("df", data.frame(a = 1:3, b = c("x", "y", "z")),
                origin = list(tool = "run_r"))

retrieved <- llamaR:::ws_retrieve("data frame", budget_chars = 5000L)
ctx <- llamaR:::ws_format_context(retrieved)
expect_true(nchar(ctx) > 0)
expect_true(grepl("Workspace State", ctx))
expect_true(grepl("df", ctx))

# Empty format
expect_equal(llamaR:::ws_format_context(list()), "")

# ---- Summarize ----

# data.frame summary includes dimensions and column types
llamaR:::ws_put("test_df", data.frame(x = 1:5, y = letters[1:5]))
meta <- llamaR:::ws_meta("test_df")
summary <- llamaR:::ws_summarize("test_df", llamaR:::ws_get("test_df"), meta)
expect_true(grepl("5x2", summary))
expect_true(grepl("x(integer)", summary, fixed = TRUE))

# function summary shows args
llamaR:::ws_put("test_fn", function(a, b, c) a + b + c)
meta_fn <- llamaR:::ws_meta("test_fn")
summary_fn <- llamaR:::ws_summarize("test_fn", llamaR:::ws_get("test_fn"), meta_fn)
expect_true(grepl("function(a, b, c)", summary_fn, fixed = TRUE))

# Clean up
llamaR:::ws_clear()
