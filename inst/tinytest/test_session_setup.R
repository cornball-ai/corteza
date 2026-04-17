library(tinytest)

expect_true(is.function(llamaR::session_setup))

# Require an API key for the selected provider.
local({
    orig <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.unsetenv("ANTHROPIC_API_KEY")
    on.exit({
        if (!is.na(orig)) Sys.setenv(ANTHROPIC_API_KEY = orig)
    }, add = TRUE)

    expect_error(
        llamaR::session_setup(
            channel = "console",
            cwd = tempdir(),
            provider = "anthropic",
            load_project_context = FALSE,
            validate_api_key = TRUE
        ),
        "ANTHROPIC_API_KEY"
    )
})

# Skip key validation on request.
local({
    orig <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.unsetenv("ANTHROPIC_API_KEY")
    on.exit({
        if (!is.na(orig)) Sys.setenv(ANTHROPIC_API_KEY = orig)
    }, add = TRUE)

    s <- llamaR::session_setup(
        channel = "console",
        cwd = tempdir(),
        provider = "anthropic",
        model = "claude-test",
        load_project_context = FALSE,
        validate_api_key = FALSE
    )
    expect_true(is.environment(s))
    expect_equal(s$channel, "console")
    expect_equal(s$provider, "anthropic")
    expect_equal(s$model_map$cloud, "claude-test")
})

# Skills are registered after setup.
local({
    s <- llamaR::session_setup(
        channel = "matrix",
        cwd = tempdir(),
        provider = "anthropic",
        model = "claude-sonnet-4-6",
        system = "tiny",
        load_project_context = FALSE,
        validate_api_key = FALSE
    )
    tools <- llamaR:::skills_as_api_tools(s$tools_filter)
    expect_true(length(tools) > 0L)
})
