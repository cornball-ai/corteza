# /agents visibility — pure-function tests for the new fields.
# The live-tokens callr round-trip is exercised in
# test_subagent_callr.R (at_home).

# format_age ----

expect_equal(corteza::format_age(0),    "0s")
expect_equal(corteza::format_age(5),    "5s")
expect_equal(corteza::format_age(59),   "59s")
expect_equal(corteza::format_age(60),   "1m")
expect_equal(corteza::format_age(90),   "2m")
expect_equal(corteza::format_age(600),  "10m")
expect_equal(corteza::format_age(3600), "1.0h")
expect_equal(corteza::format_age(7200), "2.0h")
expect_equal(corteza::format_age(NA),   "?")
expect_equal(corteza::format_age(-1),   "?")

# format_live_ctx ----

expect_equal(corteza::format_live_ctx(4200, 200000), "ctx 4.2K/200.0K")
expect_equal(corteza::format_live_ctx(0,    128000), "ctx 0/128.0K")
expect_equal(corteza::format_live_ctx(NA,   128000), "ctx ?")
expect_equal(corteza::format_live_ctx(4200, NA),     "ctx ?")
expect_equal(corteza::format_live_ctx(NULL, 128000), "ctx ?")

# subagent_accumulate_usage ----

base <- list(cumulative_input_tokens = 0L,
             cumulative_output_tokens = 0L,
             cumulative_total_tokens = 0L,
             cumulative_cost = NA_real_,
             query_count = 0L)

# NULL usage is a no-op.
expect_identical(corteza:::subagent_accumulate_usage(base, NULL), base)

# First call accumulates non-NULL fields.
after1 <- corteza:::subagent_accumulate_usage(
    base,
    list(input_tokens = 100L, output_tokens = 20L, total_tokens = 120L))
expect_equal(after1$cumulative_input_tokens, 100L)
expect_equal(after1$cumulative_output_tokens, 20L)
expect_equal(after1$cumulative_total_tokens, 120L)
expect_true(is.na(after1$cumulative_cost),
            info = "no cost in usage -> cumulative stays NA")
expect_equal(after1$query_count, 1L)

# Second call adds, query_count increments.
after2 <- corteza:::subagent_accumulate_usage(
    after1,
    list(input_tokens = 50L, output_tokens = 5L, total_tokens = 55L,
         cost = 0.001))
expect_equal(after2$cumulative_input_tokens, 150L)
expect_equal(after2$cumulative_output_tokens, 25L)
expect_equal(after2$cumulative_total_tokens, 175L)
expect_equal(after2$cumulative_cost, 0.001)
expect_equal(after2$query_count, 2L)

# Third call with cost adds to running cost.
after3 <- corteza:::subagent_accumulate_usage(
    after2,
    list(input_tokens = 10L, output_tokens = 1L, total_tokens = 11L,
         cost = 0.0001))
expect_equal(after3$cumulative_cost, 0.0011)

# Missing fields don't crash and don't change those counters.
partial <- corteza:::subagent_accumulate_usage(
    base, list(input_tokens = 7L))
expect_equal(partial$cumulative_input_tokens, 7L)
expect_equal(partial$cumulative_output_tokens, 0L)

# format_subagent_list with full fields ----
# Stub a registry-style entry and confirm the formatter renders all
# the new fields without crashing on missing pieces.

mock_agent <- list(
    id = "stub-12345678", seq = 1L,
    task = "demo task",
    model = "moonshot-v1-8k",
    started_at = Sys.time() - 30,
    age_seconds = 30,
    time_remaining = 29.5,
    live_tokens = 1500L,
    context_limit = 128000L,
    cumulative_input_tokens = 300L,
    cumulative_output_tokens = 50L,
    cumulative_total_tokens = 350L,
    cumulative_cost = NA_real_,
    query_count = 2L,
    pending = NULL,
    pending_started_at = NULL)
out <- corteza:::format_subagent_list(list(mock_agent))
expect_true(grepl("moonshot-v1-8k", out, fixed = TRUE))
expect_true(grepl("30s", out, fixed = TRUE))
expect_true(grepl("ctx 1.5K/128.0K", out, fixed = TRUE))
expect_true(grepl("300 in / 50 out", out, fixed = TRUE))
# Cost is "?" when NA.
expect_true(grepl("· ?)", out, fixed = TRUE))
expect_true(grepl("idle", out, fixed = TRUE))

# Busy agent: live tokens NA, state shows the pending prompt.
busy_agent <- mock_agent
busy_agent$pending <- "investigating the deploy log"
busy_agent$live_tokens <- NA_integer_
busy_agent$context_limit <- NA_integer_
out_busy <- corteza:::format_subagent_list(list(busy_agent))
expect_true(grepl("ctx ?", out_busy, fixed = TRUE),
            info = "busy agent shows ctx ? (callr can't ask a busy child)")
expect_true(grepl("busy:", out_busy, fixed = TRUE))
expect_true(grepl("investigating the deploy log", out_busy, fixed = TRUE))

# Cost rendering when provider does supply it.
costing_agent <- mock_agent
costing_agent$cumulative_cost <- 0.0153
out_cost <- corteza:::format_subagent_list(list(costing_agent))
expect_true(grepl("$0.0153", out_cost, fixed = TRUE))

# default_provider_model ----
# Regression for the case where a subagent spawned with the provider
# default model (no explicit model_map$cloud) used to display the
# provider name as the model and "ctx ?" because the limit lookup
# had no key. The helper now delegates to llm.api's canonical table,
# so /agents shows a real model name and a real context window.
# Assert non-NULL + a context-limit entry rather than specific model
# strings, so the test tracks llm.api's picks without churn.
for (p in c("anthropic", "openai", "moonshot", "ollama")) {
    m <- corteza::default_provider_model(p)
    expect_true(is.character(m) && nzchar(m))
    expect_true(corteza::context_limit_for_model(m) > 0L)
}
# Unknown / empty / NULL providers resolve to NULL (llm.api errors
# there; default_provider_model maps that to NULL).
expect_null(corteza::default_provider_model("unknown-provider"))
expect_null(corteza::default_provider_model(NULL))
