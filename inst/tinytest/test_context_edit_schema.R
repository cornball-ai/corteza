# Exercise the real provider request encoders. Intercept the transport only
# after JSON serialization, so a single required field cannot become a scalar.
local({
    ns <- asNamespace("llm.api")
    original <- get(".llm_transport_retry", ns)
    on.exit(utils::assignInNamespace(".llm_transport_retry", original, "llm.api"), add = TRUE)
    payload <- NULL
    utils::assignInNamespace(".llm_transport_retry", function(run, ...) {
        payload <<- get("payload", environment(run))
        stop("captured serialized request", call. = FALSE)
    }, "llm.api")
    for (provider in c("anthropic", "anthropic_claude", "openai")) {
        payload <- NULL
        schema <- corteza:::.context_edit_schema()
        tools <- get(".convert_tools", ns)(list(schema), provider)
        config <- list(base_url = "https://example.invalid", chat_path = "/chat",
                       api_key = "fixture", provider = provider)
        if (provider == "anthropic_claude") {
            config$credentials <- function() c(Authorization = "Bearer fixture")
        }
        request <- get(if (provider == "openai") ".agent_openai" else ".agent_anthropic", ns)
        expect_error(request(list(list(role = "user", content = "Fixture")),
            tools, "Fixture system", "fixture", config), "captured serialized request")
        expect_true(is.character(payload) && nzchar(payload))
        wire <- jsonlite::fromJSON(payload, simplifyVector = FALSE)$tools[[1L]]
        input <- if (provider == "openai") wire$`function`$parameters else wire$input_schema
        expect_identical(input$required, list("op"))
        expect_identical(input$properties$op$enum, list("drop", "replace", "note"))
    }
})
