# Output-token budget wiring (new_session -> .session_max_tokens ->
# llm.api::agent). Same resolution idiom as web_search / base_url:
# explicit session field wins, config is the fallback, NULL defers to
# llm.api's provider default.

# .session_max_tokens(): resolution order and integer coercion
s <- new.env()
expect_null(corteza:::.session_max_tokens(s))
s$config <- list(max_tokens = 8192)
expect_identical(corteza:::.session_max_tokens(s), 8192L)
s$max_tokens <- 16000L
expect_identical(corteza:::.session_max_tokens(s), 16000L)
s$max_tokens <- NULL
expect_identical(corteza:::.session_max_tokens(s), 8192L)

# new_session() stores it as integer; NULL stays NULL ("defer")
ns_set <- corteza::new_session(channel = "console",
                               provider = "anthropic",
                               max_tokens = 16000)
expect_identical(ns_set$max_tokens, 16000L)
ns_default <- corteza::new_session(channel = "console",
                                   provider = "anthropic")
expect_null(ns_default$max_tokens)
