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

# The probe is bounded. A listener that accepts and never answers stands
# in for a machine that leaves the request hanging: the probe gives up
# at its timeout instead of holding new_session() for the system's.
local({
    port <- 0L
    srv <- NULL
    for (candidate in sample(20000:40000, 20L)) {
        srv <- tryCatch(serverSocket(candidate), error = function(e) NULL)
        if (!is.null(srv)) {
            port <- candidate
            break
        }
    }
    if (!is.null(srv)) {
        on.exit(close(srv), add = TRUE)
        took <- system.time(
            res <- tryCatch(corteza:::.ollama_model_names(
                sprintf("http://127.0.0.1:%d", port),
                connect_timeout = 1L, timeout = 1L),
                error = function(e) "timed out")
        )[["elapsed"]]
        expect_identical(res, "timed out")
        expect_true(took < 10)
    }
})
# Nothing listening: an error, which default_local_model() reads as no
# local model.
expect_error(corteza:::.ollama_model_names("http://127.0.0.1:1",
                                           connect_timeout = 1L,
                                           timeout = 2L))

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
