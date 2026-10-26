# Config belongs to its provider; a saved conversation keeps its own pair.
local({
    route <- corteza:::.session_model_pair
    cfg <- list(provider = "anthropic", model = "claude-config")
    saved <- list(provider = "openai_codex", model = "gpt-saved")
    expect_identical(route(config = cfg), cfg)
    expect_identical(route(config = cfg, saved = saved), saved)
    expect_identical(route(config = saved, saved = saved), saved)
    expect_identical(route(config = modifyList(saved, list(model = "gpt-new")),
                           saved = saved), saved)
    expect_identical(route("openai_codex", config = cfg, saved = saved), saved)
    expect_identical(route(model = "gpt-override", config = cfg, saved = saved),
                     list(provider = "openai_codex", model = "gpt-override"))
    expect_identical(route("anthropic", config = cfg, saved = saved), cfg)
    expect_identical(route("anthropic", "claude-override", cfg, saved),
                     list(provider = "anthropic", model = "claude-override"))
    for (prior in list(NULL, saved)) {
        expect_identical(route("openai", config = cfg, saved = prior),
                         list(provider = "openai",
                              model = corteza::default_provider_model("openai")))
        expect_identical(route("openai_compatible", config = cfg, saved = prior),
                         list(provider = "openai_compatible", model = NULL))
    }
    expect_identical(route(config = cfg, saved = list(provider = "openai_codex")),
                     list(provider = "openai_codex",
                          model = corteza::default_provider_model("openai_codex")))
    expect_identical(route(config = cfg, saved = list(model = "orphan")), cfg)
})
