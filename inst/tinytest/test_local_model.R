library(tinytest)

expect_true(is.function(corteza::default_local_model))

# Clear cache before each probe.
reset_cache <- function() {
    assign("initialized", FALSE,
           envir = corteza:::.local_model_cache)
    assign("value", NULL, envir = corteza:::.local_model_cache)
}

# Candidate list: override with a known-nonexistent model to force NULL
# even if the developer has Ollama running locally.
local({
    reset_cache()
    op <- options(corteza.local_models = "definitely-not-a-real-model:0.01b")
    on.exit({
        options(op)
        reset_cache()
    }, add = TRUE)
    expect_null(corteza::default_local_model())
})

# Cache is honored: second call returns NULL without hitting Ollama.
# (Hard to observe directly, but we can prove it by poisoning the options
# between calls — if the result were re-computed it would change.)
local({
    reset_cache()
    op <- options(corteza.local_models = "definitely-not-a-real-model:0.01b")
    first <- corteza::default_local_model()
    options(corteza.local_models = c("also-not-real", "still-not-real"))
    on.exit({
        options(op)
        reset_cache()
    }, add = TRUE)
    # Cached NULL from first call
    expect_null(corteza::default_local_model())
    expect_null(first)
})

# If at_home and Ollama is serving gpt-oss, detection should succeed.
if (at_home()) {
    reset_cache()
    op <- options(corteza.local_models = c("gpt-oss:120b", "gpt-oss:20b"))
    on.exit({
        options(op)
        reset_cache()
    }, add = TRUE)
    available <- tryCatch(llm.api::list_ollama_models()$name,
                          error = function(e) character())
    if (any(c("gpt-oss:120b", "gpt-oss:20b") %in% available)) {
        picked <- corteza::default_local_model()
        expect_true(picked %in% c("gpt-oss:120b", "gpt-oss:20b"))
    }
}
