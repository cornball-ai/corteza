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

# Validation at both boundaries: as.integer() alone would silently
# truncate 2.5 and turn Inf/overflow into NA; all of these must refuse
# instead of reaching the provider request.
for (bad in list(0, -5, 2.5, Inf, NaN, NA_real_, "8192", c(1, 2),
                 .Machine$integer.max + 1)) {
    expect_error(corteza::new_session(channel = "console",
                                      provider = "anthropic",
                                      max_tokens = bad),
                 pattern = "positive whole number")
}
s_bad <- new.env()
s_bad$config <- list(max_tokens = -1)
expect_error(corteza:::.session_max_tokens(s_bad),
             pattern = "positive whole number")
s_bad$config <- list(max_tokens = 2.5)
expect_error(corteza:::.session_max_tokens(s_bad),
             pattern = "positive whole number")
# A whole-number double is fine (config.json numbers parse as doubles)
s_dbl <- new.env()
s_dbl$config <- list(max_tokens = 8192)
expect_identical(corteza:::.session_max_tokens(s_dbl), 8192L)

# turn() forwards the resolved budget to llm.api::agent -- and omits it
# when unset, so NULL never pins the provider default. Stubs the agent
# in llm.api's namespace; tools = list() keeps the whole call offline.
local({
    ns <- asNamespace("llm.api")
    orig <- get("agent", envir = ns, inherits = FALSE)
    captured <- NULL
    stub <- function(...) {
        captured <<- list(...)
        list(content = "ok", history = list(),
             usage = list(input_tokens = 1L, output_tokens = 1L))
    }
    assignInNamespace("agent", stub, ns = "llm.api")
    on.exit(assignInNamespace("agent", orig, ns = "llm.api"), add = TRUE)

    s1 <- corteza::new_session(channel = "console", provider = "anthropic",
                               model_map = list(cloud = "claude-test",
                                                local = NULL),
                               web_search = FALSE, max_tokens = 16000)
    out1 <- corteza::turn("hi", s1, tools = list())
    expect_identical(out1$reply, "ok")
    expect_identical(captured$max_tokens, 16000L)

    captured <- NULL
    s2 <- corteza::new_session(channel = "console", provider = "anthropic",
                               model_map = list(cloud = "claude-test",
                                                local = NULL),
                               web_search = FALSE)
    out2 <- corteza::turn("hi", s2, tools = list())
    expect_identical(out2$reply, "ok")
    expect_false("max_tokens" %in% names(captured))
})
