# Tests for session spend tracking and the /spent report.
# Scope: current-run main-agent turns (subagent spend is out of scope
# until the CLI worker backend is unified).

# session_accumulate_spend on a session ENV (chat() path): mutates in place
e <- new.env()
corteza:::session_accumulate_spend(
    e, list(input_tokens = 100L, output_tokens = 50L,
            total_tokens = 150L, cost = 0.01))
expect_equal(e$spend$turns, 1L)
expect_equal(e$spend$total_tokens, 150L)
expect_equal(e$spend$cost, 0.01)
expect_false(e$spend$cost_missing)

# Second turn with an NA cost: tokens add, cost doesn't, floor flag flips
corteza:::session_accumulate_spend(
    e, list(input_tokens = 10L, output_tokens = 5L,
            total_tokens = 15L, cost = NA_real_))
expect_equal(e$spend$turns, 2L)
expect_equal(e$spend$total_tokens, 165L)
expect_equal(e$spend$cost, 0.01)        # NA not added
expect_true(e$spend$cost_missing)

# session_accumulate_spend on a session LIST (CLI path): returns updated copy
s <- list()
s <- corteza:::session_accumulate_spend(
    s, list(input_tokens = 20L, output_tokens = 10L,
            total_tokens = 30L, cost = 0.02))
expect_equal(s$spend$cost, 0.02)
expect_equal(s$spend$turns, 1L)

# NULL usage is a no-op
e2 <- new.env()
corteza:::session_accumulate_spend(e2, NULL)
expect_null(e2$spend)

# format_spend renders the headline, dollar figure, and turn count
e3 <- new.env()
corteza:::session_accumulate_spend(
    e3, list(input_tokens = 1000L, output_tokens = 500L,
             total_tokens = 1500L, cost = 0.03))
out <- corteza:::format_spend(e3)
expect_true(grepl("Session spend", out, fixed = TRUE))
expect_true(grepl("$0.03", out, fixed = TRUE))
expect_true(grepl("1 turns", out, fixed = TRUE))

# floor note appears once a cost goes missing
corteza:::session_accumulate_spend(
    e3, list(input_tokens = 1L, output_tokens = 1L,
             total_tokens = 2L, cost = NA_real_))
expect_true(grepl("floor", corteza:::format_spend(e3), fixed = TRUE))

# reset_session_spend clears the tally for a fresh conversation
corteza:::reset_session_spend(e3)
expect_null(e3$spend)
