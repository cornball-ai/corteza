# Test llm.api provider compatibility guard

if (!requireNamespace("llm.api", quietly = TRUE)) {
    exit_file("llm.api not installed")
}

supported <- corteza:::llm_api_supported_providers()
expect_true("moonshot" %in% supported)
expect_silent(corteza:::ensure_llm_api_provider("moonshot"))
expect_error(
    corteza:::ensure_llm_api_provider("not-a-provider"),
    pattern = "does not support provider"
)
# An unknown provider leaves the person's search path alone: an attached
# llm.api stays attached (it was detached, in a reload that could never
# unload a namespace corteza imports).
local({
    attached <- "package:llm.api" %in% search()
    if (!attached) {
        suppressPackageStartupMessages(library(llm.api))
        on.exit(if ("package:llm.api" %in% search()) {
            detach("package:llm.api", character.only = TRUE)
        }, add = TRUE)
    }
    expect_error(corteza:::ensure_llm_api_provider("not-a-provider"))
    expect_true("package:llm.api" %in% search())
})
