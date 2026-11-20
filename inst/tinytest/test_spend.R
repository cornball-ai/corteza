# Tests for session spend tracking and the /spent report.
# Spend is process-lifetime: /clear closes a conversation segment
# rather than zeroing the tally, and subagent spend rolls up as a
# separate process-level line that survives kills.

# Reset the package-level subagent spend state so these tests are
# independent of any other test that spawned/killed an agent.
reset_sub_spend <- corteza:::subagent_spend_reset
reset_sub_spend()

# --- main-agent accumulation -------------------------------------------

# session_accumulate_spend on a session ENV (chat() path): mutates in place
e <- new.env()
corteza:::session_accumulate_spend(
    e, list(input_tokens = 100L, output_tokens = 50L,
            total_tokens = 150L, cost = 0.01))
seg <- e$spend$segments[[1]]
expect_equal(length(e$spend$segments), 1L)
expect_equal(seg$turns, 1L)
expect_equal(seg$total_tokens, 150L)
expect_equal(seg$cost, 0.01)
expect_false(seg$cost_missing)

# Second turn with an NA cost: tokens add, cost doesn't, floor flag flips
corteza:::session_accumulate_spend(
    e, list(input_tokens = 10L, output_tokens = 5L,
            total_tokens = 15L, cost = NA_real_))
seg <- e$spend$segments[[1]]
expect_equal(seg$turns, 2L)
expect_equal(seg$total_tokens, 165L)
expect_equal(seg$cost, 0.01)        # NA not added
expect_true(seg$cost_missing)

# session_accumulate_spend on a session LIST (CLI path): returns updated copy
s <- list()
s <- corteza:::session_accumulate_spend(
    s, list(input_tokens = 20L, output_tokens = 10L,
            total_tokens = 30L, cost = 0.02))
expect_equal(s$spend$segments[[1]]$cost, 0.02)
expect_equal(s$spend$segments[[1]]$turns, 1L)

# NULL usage is a no-op
e2 <- new.env()
corteza:::session_accumulate_spend(e2, NULL)
expect_null(e2$spend)

# --- /clear segmenting --------------------------------------------------

# spend_open_segment closes the current conversation and opens a new one;
# prior spend is kept (process-lifetime), and later turns land in the new
# segment.
e4 <- new.env()
e4$sessionId <- "aaaa1111"
corteza:::session_accumulate_spend(
    e4, list(input_tokens = 100L, output_tokens = 0L,
             total_tokens = 100L, cost = 0.05))
e4$sessionId <- "bbbb2222"
corteza:::spend_open_segment(e4)
# Deferred: the new segment is only marked pending, not created yet, so
# a /clear with no following turn adds no empty conversation.
expect_equal(length(e4$spend$segments), 1L)
expect_true(isTRUE(e4$spend$pending_new))
corteza:::session_accumulate_spend(
    e4, list(input_tokens = 50L, output_tokens = 0L,
             total_tokens = 50L, cost = 0.03))
expect_equal(length(e4$spend$segments), 2L)       # opened by the turn
expect_false(isTRUE(e4$spend$pending_new))
expect_equal(e4$spend$segments[[1]]$cost, 0.05)   # segment 1 untouched
expect_equal(e4$spend$segments[[2]]$cost, 0.03)
expect_equal(e4$spend$segments[[1]]$id, "aaaa1111")
expect_equal(e4$spend$segments[[2]]$id, "bbbb2222")

# Repeated /clear with no turns in between does not stack empty
# segments: still just the one real conversation, with a pending flag.
e6 <- new.env()
e6$sessionId <- "c1"
corteza:::session_accumulate_spend(
    e6, list(input_tokens = 10L, output_tokens = 0L,
             total_tokens = 10L, cost = 0.01))
corteza:::spend_open_segment(e6)
corteza:::spend_open_segment(e6)
expect_equal(length(e6$spend$segments), 1L)
expect_true(isTRUE(e6$spend$pending_new))
# A pending session marks no segment as current.
expect_false(grepl("current", corteza:::format_spend(e6), fixed = TRUE))
# The next turn opens exactly one fresh segment.
corteza:::session_accumulate_spend(
    e6, list(input_tokens = 10L, output_tokens = 0L,
             total_tokens = 10L, cost = 0.01))
expect_equal(length(e6$spend$segments), 2L)

# --- format_spend rendering --------------------------------------------

# Compact form: one segment, no subagents
e3 <- new.env()
corteza:::session_accumulate_spend(
    e3, list(input_tokens = 1000L, output_tokens = 500L,
             total_tokens = 1500L, cost = 0.03))
out <- corteza:::format_spend(e3)
expect_true(grepl("Session spend", out, fixed = TRUE))
expect_true(grepl("$0.03", out, fixed = TRUE))
expect_true(grepl("1 turn", out, fixed = TRUE))

# A zero-token query with no cost does NOT flip the floor flag: there
# was no spend whose price is unknown.
e3b <- new.env()
corteza:::session_accumulate_spend(
    e3b, list(input_tokens = 0L, output_tokens = 0L,
              total_tokens = 0L, cost = NA_real_))
expect_false(e3b$spend$segments[[1]]$cost_missing)
expect_false(grepl("floor", corteza:::format_spend(e3b), fixed = TRUE))

# floor note appears once a cost goes missing on a token-consuming turn
corteza:::session_accumulate_spend(
    e3, list(input_tokens = 1L, output_tokens = 1L,
             total_tokens = 2L, cost = NA_real_))
expect_true(grepl("floor", corteza:::format_spend(e3), fixed = TRUE))

# Itemized form: multiple segments render line items and a total
out4 <- corteza:::format_spend(e4)
expect_true(grepl("[1]", out4, fixed = TRUE))
expect_true(grepl("[2]", out4, fixed = TRUE))
expect_true(grepl("current", out4, fixed = TRUE))
expect_true(grepl("total", out4, fixed = TRUE))
expect_true(grepl("$0.0800", out4, fixed = TRUE))   # 0.05 + 0.03

# --- subagent rollup ----------------------------------------------------

# Empty to start
tot <- corteza:::subagent_spend_total()
expect_equal(tot$n_agents, 0L)
expect_equal(tot$cost, 0)

# Bind the registry env to a local; it is a reference, so mutating the
# local mutates the package env. (Assigning into `pkg:::env[[...]]`
# directly would trigger a reassignment back through `:::` and fail.)
reg <- corteza:::.subagent_registry

# A live registry entry contributes to the total
reg[["agent-x"]] <- list(
    seq = 1L, cumulative_input_tokens = 800L, cumulative_output_tokens = 200L,
    cumulative_total_tokens = 1000L, cumulative_cost = 0.02,
    cost_missing = FALSE, query_count = 2L, session_key = "k1", session = NULL)
tot <- corteza:::subagent_spend_total()
expect_equal(tot$n_agents, 1L)
expect_equal(tot$total_tokens, 1000L)
expect_equal(tot$cost, 0.02)
expect_equal(tot$query_count, 2L)

# Retiring an entry preserves its spend after the live entry is gone
corteza:::subagent_retire_spend(reg[["agent-x"]])
rm("agent-x", envir = reg)
tot <- corteza:::subagent_spend_total()
expect_equal(tot$n_agents, 1L)            # counted via retired
expect_equal(tot$cost, 0.02)
expect_equal(tot$total_tokens, 1000L)

# A cost-blind agent (NA cost, nonzero tokens) flips the floor flag
reg[["agent-y"]] <- list(
    seq = 2L, cumulative_input_tokens = 100L, cumulative_output_tokens = 0L,
    cumulative_total_tokens = 100L, cumulative_cost = NA_real_,
    cost_missing = TRUE, query_count = 1L, session_key = "k2", session = NULL)
tot <- corteza:::subagent_spend_total()
expect_true(tot$cost_missing)
expect_equal(tot$cost, 0.02)              # NA agent adds no cost
expect_equal(tot$n_agents, 2L)            # 1 live + 1 retired

# format_spend shows the subagent line and folds it into the total
out5 <- corteza:::format_spend(e4)
expect_true(grepl("subagents:", out5, fixed = TRUE))
# total = segments (0.08) + subagent cost (0.02) = 0.10
expect_true(grepl("$0.1000", out5, fixed = TRUE))

reset_sub_spend()

# subagent_accumulate_usage flips cost_missing on a no-cost query that
# consumed tokens
info <- list(cumulative_input_tokens = 0L, cumulative_output_tokens = 0L,
             cumulative_total_tokens = 0L, cumulative_cost = NA_real_,
             cost_missing = FALSE, query_count = 0L)
info <- corteza:::subagent_accumulate_usage(
    info, list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L))
expect_true(info$cost_missing)
expect_equal(info$cumulative_total_tokens, 15L)

# ...but a zero-token no-cost query leaves the flag alone
info2 <- list(cumulative_input_tokens = 0L, cumulative_output_tokens = 0L,
              cumulative_total_tokens = 0L, cumulative_cost = NA_real_,
              cost_missing = FALSE, query_count = 0L)
info2 <- corteza:::subagent_accumulate_usage(
    info2, list(input_tokens = 0L, output_tokens = 0L, total_tokens = 0L))
expect_false(info2$cost_missing)
expect_equal(info2$query_count, 1L)        # still counted as a query

# subagent_kill moves an agent from live to retired atomically: it is
# counted exactly once afterwards (not zero, not twice). Redirect the
# data dir so store_update's bookkeeping write lands in temp.
reset_sub_spend()
old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
tmp_data <- file.path(tempdir(), "spend_kill_data")
Sys.setenv(R_USER_DATA_DIR = tmp_data)
reg[["kill-once"]] <- list(
    id = "kill-once", seq = 9L, session_key = "agent:main:subagent:ko",
    session = NULL, cumulative_input_tokens = 200L,
    cumulative_output_tokens = 100L, cumulative_total_tokens = 300L,
    cumulative_cost = 0.05, cost_missing = FALSE, query_count = 4L)
# Live: counted via the registry.
expect_equal(corteza:::subagent_spend_total()$n_agents, 1L)
expect_true(corteza:::subagent_kill("kill-once"))
expect_equal(length(ls(reg)), 0L)                  # entry dropped
tot <- corteza:::subagent_spend_total()
expect_equal(tot$n_agents, 1L)                     # retired, counted once
expect_equal(tot$cost, 0.05)                       # not doubled to 0.10
expect_equal(tot$total_tokens, 300L)
if (is.na(old_data)) {
    Sys.unsetenv("R_USER_DATA_DIR")
} else {
    Sys.setenv(R_USER_DATA_DIR = old_data)
}
unlink(tmp_data, recursive = TRUE)
reset_sub_spend()

# --- cached input -------------------------------------------------------

# A request to an Anthropic provider with a prompt cache on. Its input
# is 3 counts: 48,000 read from the cache, 3,000 written to it, and 12
# in `input_tokens`. llm.api's total is 12 + 340.
cached_usage <- list(input_tokens = 12L, output_tokens = 340L,
                     total_tokens = 352L,
                     cache_read_input_tokens = 48000L,
                     cache_creation_input_tokens = 3000L, cost = 0.02)
no_total <- function(x) x[names(x) != "total_tokens"]
for (p in c("anthropic", "anthropic_claude")) {
    expect_equal(corteza:::.usage_cached_input(cached_usage, p), 51000)
    counted <- corteza:::.usage_count_cached(cached_usage, p)
    expect_equal(counted$total_tokens, 51352)
    # Only the total changes, and a second pass changes nothing.
    expect_identical(no_total(counted), no_total(cached_usage))
    expect_identical(corteza:::.usage_count_cached(counted, p), counted)
}
# Every other provider counts cached input inside its input count, so
# the same numbers from one of them are left as they are. So is a
# usage whose provider is not known.
for (p in list("openai", "openai_codex", "openai_compatible", "moonshot",
               "ollama", NULL, NA_character_, character(0),
               c("anthropic", "openai"))) {
    expect_equal(corteza:::.usage_cached_input(cached_usage, p), 0)
    expect_identical(corteza:::.usage_count_cached(cached_usage, p),
                     cached_usage)
}
# Nothing cached: the usage comes back as it was, with no total added.
for (u in list(list(input_tokens = 5L, output_tokens = 2L),
               list(input_tokens = 5L, output_tokens = 2L,
                    cache_read_input_tokens = 0L,
                    cache_creation_input_tokens = 0L),
               list(input_tokens = 5L, output_tokens = 2L,
                    cache_read_input_tokens = NA_integer_,
                    cache_creation_input_tokens = -4L),
               list(input_tokens = 5L, output_tokens = 2L,
                    cache_read_input_tokens = "many"),
               list())) {
    expect_equal(corteza:::.usage_cached_input(u, "anthropic"), 0)
    expect_identical(corteza:::.usage_count_cached(u, "anthropic"), u)
}
expect_null(corteza:::.usage_count_cached(NULL, "anthropic"))
# Either count alone is enough, and one request's raw usage, which has
# no total, gets one.
expect_equal(corteza:::.usage_count_cached(
    list(input_tokens = 1L, output_tokens = 1L,
         cache_read_input_tokens = 10L), "anthropic")$total_tokens, 12)
expect_equal(corteza:::.usage_count_cached(
    list(input_tokens = 1L, output_tokens = 1L,
         cache_creation_input_tokens = 10L), "anthropic")$total_tokens, 12)
# With no input or no output count there is nothing to add it to.
for (u in list(list(output_tokens = 2L, cache_read_input_tokens = 10L),
               list(input_tokens = 2L, cache_read_input_tokens = 10L),
               list(input_tokens = NA_integer_, output_tokens = 2L,
                    cache_read_input_tokens = 10L))) {
    expect_identical(corteza:::.usage_count_cached(u, "anthropic"), u)
}
# The names are matched whole. Neither `cache_creation`, the
# per-lifetime split llm.api adds, nor a longer name that starts with
# one of the 2 is read as the count.
expect_equal(corteza:::.usage_cached_input(
    list(cache_creation = list(ephemeral_5m_input_tokens = 9L)),
    "anthropic"), 0)
expect_equal(corteza:::.usage_cached_input(
    list(cache_read_input_tokens_by_tier = 9L,
         cache_creation_input_tokens_by_tier = 9L), "anthropic"), 0)

# The input of a usage, cached or not: what the total holds beyond the
# output, where that is more than `input_tokens`.
counted <- corteza:::.usage_count_cached(cached_usage, "anthropic")
expect_equal(corteza:::.spend_input_tokens(counted), 51012)
expect_identical(corteza:::.spend_input_tokens(cached_usage), 12L)
expect_identical(corteza:::.spend_input_tokens(
    list(input_tokens = 100L, output_tokens = 50L)), 100L)
# A total under input plus output, no output count, an unknown count:
# the input as reported.
expect_identical(corteza:::.spend_input_tokens(
    list(input_tokens = 100L, output_tokens = 50L, total_tokens = 120L)),
    100L)
expect_identical(corteza:::.spend_input_tokens(
    list(input_tokens = 5L, total_tokens = 100L)), 5L)
expect_identical(corteza:::.spend_input_tokens(
    list(input_tokens = NA_integer_, output_tokens = 1L,
         total_tokens = 5L)), NA_integer_)
expect_null(corteza:::.spend_input_tokens(
    list(output_tokens = 50L, total_tokens = 150L)))

# The session tally, which /spent prints and /auto's token cap reads,
# counts all of it. Uncounted, the same turn would add 352.
e_cached <- new.env()
auto_base <- corteza:::auto_spend_baseline(e_cached)
corteza:::session_accumulate_spend(e_cached, counted)
seg <- e_cached$spend$segments[[1]]
expect_equal(seg$total_tokens, 51352L)
expect_equal(seg$input_tokens, 51012L)
expect_equal(seg$output_tokens, 340L)
expect_equal(corteza:::auto_spend_since(e_cached, auto_base)$tokens, 51352)
out_cached <- corteza:::format_spend(e_cached)
expect_true(grepl("51.4K tok (51.0K in / 340 out)", out_cached, fixed = TRUE))
# A turn with nothing cached adds what it always did.
corteza:::session_accumulate_spend(
    e_cached, list(input_tokens = 100L, output_tokens = 50L,
                   total_tokens = 150L, cost = 0.01))
seg <- e_cached$spend$segments[[1]]
expect_equal(seg$total_tokens, 51502L)
expect_equal(seg$input_tokens, 51112L)

# A subagent's tally, which subagents.mcp_spend_cap_tokens reads.
info_cached <- list(cumulative_input_tokens = 0L,
                    cumulative_output_tokens = 0L,
                    cumulative_total_tokens = 0L, cumulative_cost = NA_real_,
                    cost_missing = FALSE, query_count = 0L)
info_cached <- corteza:::subagent_accumulate_usage(info_cached, counted)
expect_equal(info_cached$cumulative_total_tokens, 51352L)
expect_equal(info_cached$cumulative_input_tokens, 51012L)
expect_equal(info_cached$cumulative_output_tokens, 340L)
reset_sub_spend()
