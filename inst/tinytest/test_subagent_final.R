# Tests for subagent return-by-handle (the return_name path). These
# cover the resolution + formatting logic directly; the end-to-end
# spawn/query roundtrip needs a live LLM and is exercised manually.

# .valid_return_name: accept simple names and .h_NNN handles
expect_true(corteza:::.valid_return_name("out"))
expect_true(corteza:::.valid_return_name(".h_001"))
expect_true(corteza:::.valid_return_name("my.result_2"))
# reject anything that isn't a single syntactic name
expect_false(corteza:::.valid_return_name("x$y"))
expect_false(corteza:::.valid_return_name("a b"))
expect_false(corteza:::.valid_return_name(""))
expect_false(corteza:::.valid_return_name(c("a", "b")))
expect_false(corteza:::.valid_return_name(NULL))
expect_false(corteza:::.valid_return_name(42))

# .resolve_return_value: handle store wins
corteza:::clear_handles()
stashed <- corteza:::with_handle(data.frame(x = 1:3))
res_h <- corteza:::.resolve_return_value(stashed$handle)
expect_true(res_h$found)
expect_equal(nrow(res_h$value), 3)

# .resolve_return_value: falls back to a globalenv binding
assign("ztest_final_obj", list(a = 1), envir = globalenv())
res_g <- corteza:::.resolve_return_value("ztest_final_obj")
expect_true(res_g$found)
expect_equal(res_g$value$a, 1)

# .resolve_return_value: missing name
res_m <- corteza:::.resolve_return_value("definitely_not_bound_xyz")
expect_false(res_m$found)
expect_null(res_m$value)

rm("ztest_final_obj", envir = globalenv())
corteza:::clear_handles()

# .format_subagent_reply: a returned value is stashed and announced
corteza:::clear_handles()
out1 <- corteza:::.format_subagent_reply(
    list(reply = "done", final = data.frame(y = 1:5), final_found = TRUE,
         final_note = NULL))
expect_true(grepl("[stored as .h_", out1, fixed = TRUE))
expect_true(grepl("done", out1, fixed = TRUE))
expect_equal(length(corteza:::list_handles()), 1L)  # minted in parent store
corteza:::clear_handles()

# .format_subagent_reply: a NULL result is still a result (final_found
# gates this, not is.null(final)), so it mints a handle too
corteza:::clear_handles()
out_null <- corteza:::.format_subagent_reply(
    list(reply = "empty", final = NULL, final_found = TRUE, final_note = NULL))
expect_true(grepl("[stored as .h_", out_null, fixed = TRUE))
expect_equal(length(corteza:::list_handles()), 1L)
corteza:::clear_handles()

# .format_subagent_reply: a final_note is appended as plain text
out2 <- corteza:::.format_subagent_reply(
    list(reply = "tried", final = NULL, final_found = FALSE,
         final_note = "return_name 'out' not found in the subagent session; no value returned."))
expect_true(grepl("not found", out2, fixed = TRUE))
expect_true(grepl("tried", out2, fixed = TRUE))

# .format_subagent_reply: plain reply passes through untouched
out3 <- corteza:::.format_subagent_reply(
    list(reply = "just text", final = NULL, final_found = FALSE,
         final_note = NULL))
expect_equal(out3, "just text")
