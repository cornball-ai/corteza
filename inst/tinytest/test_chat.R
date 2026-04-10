# Test llm.api provider compatibility guard

if (!requireNamespace("llm.api", quietly = TRUE)) {
    exit_file("llm.api not installed")
}

supported <- llamaR:::llm_api_supported_providers()
expect_true("moonshot" %in% supported)
expect_silent(llamaR:::ensure_llm_api_provider("moonshot"))
expect_error(
    llamaR:::ensure_llm_api_provider("not-a-provider"),
    pattern = "does not support provider"
)
