# Provider-native web search wiring (turn() -> llm.api::agent).

# .web_search_supported(): hosted providers yes, local / unknown no.
expect_true(corteza:::.web_search_supported("anthropic"))
expect_true(corteza:::.web_search_supported("anthropic_claude"))
expect_true(corteza:::.web_search_supported("openai"))
expect_true(corteza:::.web_search_supported("openai_codex"))
expect_true(corteza:::.web_search_supported("moonshot"))
expect_false(corteza:::.web_search_supported("ollama"))
expect_false(corteza:::.web_search_supported(NULL))

# .session_web_search(): on by default, explicit field wins, config is the
# fallback, and an explicit FALSE is respected (not overridden by default).
s <- new.env()
expect_true(corteza:::.session_web_search(s))            # nothing set -> default on
s$web_search <- FALSE
expect_false(corteza:::.session_web_search(s))           # explicit off
s$web_search <- NULL
s$config <- list(web_search = FALSE)
expect_false(corteza:::.session_web_search(s))           # config off
s$config <- list(web_search = TRUE)
expect_true(corteza:::.session_web_search(s))            # config on

# new_session() carries the field through; NULL means "defer to turn()".
ns_off <- new_session(channel = "cli", provider = "anthropic",
                      web_search = FALSE)
expect_equal(ns_off$web_search, FALSE)
ns_default <- new_session(channel = "cli", provider = "anthropic")
expect_true(is.null(ns_default$web_search))
expect_true(corteza:::.session_web_search(ns_default))   # NULL -> default on

# .drop_redundant_web_search(): when native search is active, corteza's own
# web_search (Tavily) tool is removed so the request can't carry two tools
# named "web_search" (the duplicate-tool-name 400 on anthropic_claude).
tool_list <- list(list(name = "read_file"), list(name = "web_search"),
                  list(name = "bash"))
kept <- corteza:::.drop_redundant_web_search(tool_list, TRUE)
expect_equal(length(kept), 2L)
expect_false("web_search" %in% vapply(kept, function(t) t$name, ""))
# Inactive: tools pass through untouched (Tavily stays for local models).
expect_identical(corteza:::.drop_redundant_web_search(tool_list, FALSE),
                 tool_list)
