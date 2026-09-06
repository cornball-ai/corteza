library(tinytest)

expect_true(is.function(corteza::session_setup))

# Require an API key for the selected provider.
local({
    orig <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
    Sys.unsetenv("ANTHROPIC_API_KEY")
    on.exit({
        if (!is.na(orig)) Sys.setenv(ANTHROPIC_API_KEY = orig)
    }, add = TRUE)

    expect_error(
        corteza::session_setup(
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

    s <- corteza::session_setup(
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
    s <- corteza::session_setup(
        channel = "matrix",
        cwd = tempdir(),
        provider = "anthropic",
        model = "claude-sonnet-4-6",
        system = "tiny",
        load_project_context = FALSE,
        validate_api_key = FALSE
    )
    tools <- corteza:::skills_as_api_tools(s$tools_filter)
    expect_true(length(tools) > 0L)
})

# openai_compatible: a missing endpoint errors at setup, not first request.
local({
    orig <- Sys.getenv("OPENAI_COMPATIBLE_BASE_URL", unset = NA_character_)
    Sys.unsetenv("OPENAI_COMPATIBLE_BASE_URL")
    on.exit({
        if (!is.na(orig)) Sys.setenv(OPENAI_COMPATIBLE_BASE_URL = orig)
    }, add = TRUE)

    expect_error(
        corteza::session_setup(
            channel = "console",
            cwd = tempdir(),
            provider = "openai_compatible",
            model = "some/model",
            load_project_context = FALSE,
            validate_api_key = TRUE
        ),
        "needs an endpoint"
    )
})

# openai_compatible: endpoint present (via env) but a blank model errors too.
# NULL falls back to config$model, which may be set in the user's global config.
local({
    orig <- Sys.getenv("OPENAI_COMPATIBLE_BASE_URL", unset = NA_character_)
    Sys.setenv(OPENAI_COMPATIBLE_BASE_URL = "https://gateway.example.com/v1")
    on.exit({
        if (is.na(orig)) {
            Sys.unsetenv("OPENAI_COMPATIBLE_BASE_URL")
        } else {
            Sys.setenv(OPENAI_COMPATIBLE_BASE_URL = orig)
        }
    }, add = TRUE)

    expect_error(
        corteza::session_setup(
            channel = "console",
            cwd = tempdir(),
            provider = "openai_compatible",
            model = "",
            load_project_context = FALSE,
            validate_api_key = TRUE
        ),
        "needs a model"
    )
})

# openai_compatible: base_url from config + model builds a session whose
# base_url is carried through (validate_api_key = FALSE to skip checks).
local({
    proj <- file.path(tempdir(), "octest")
    dir.create(file.path(proj, ".corteza"), recursive = TRUE,
               showWarnings = FALSE)
    on.exit(unlink(proj, recursive = TRUE), add = TRUE)
    writeLines(
        jsonlite::toJSON(list(
            provider = "openai_compatible",
            base_url = "https://openrouter.ai/api/v1",
            model = "meta-llama/llama-3-70b-instruct"
        ), auto_unbox = TRUE),
        file.path(proj, ".corteza", "config.json")
    )

    s <- corteza::session_setup(
        channel = "console",
        cwd = proj,
        load_project_context = FALSE,
        validate_api_key = FALSE
    )
    expect_equal(s$provider, "openai_compatible")
    expect_equal(s$base_url, "https://openrouter.ai/api/v1")
    expect_equal(s$model_map$cloud, "meta-llama/llama-3-70b-instruct")
    expect_equal(corteza:::.session_base_url(s), "https://openrouter.ai/api/v1")
})
# Auto-built systems retain saber provenance on the session.
local({
    proj <- tempfile("session-context-")
    cache <- tempfile("session-context-cache-")
    dir.create(file.path(proj, ".corteza"), recursive = TRUE)
    writeLines("# Session Agent Context", file.path(proj, "AGENTS.md"))
    writeLines(
        '{"context_include_user": false, "context_include_soul": false}',
        file.path(proj, ".corteza", "config.json")
    )

    previous_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CACHE_DIR = cache)
    on.exit({
        unlink(proj, recursive = TRUE)
        unlink(cache, recursive = TRUE)
        if (is.na(previous_cache)) {
            Sys.unsetenv("R_USER_CACHE_DIR")
        } else {
            Sys.setenv(R_USER_CACHE_DIR = previous_cache)
        }
    }, add = TRUE)

    s <- corteza::session_setup(
        channel = "console",
        cwd = proj,
        provider = "anthropic",
        model = "claude-test",
        load_project_context = TRUE,
        validate_api_key = FALSE
    )
    expect_true(inherits(s$context_manifest, "saber_context_manifest"))
    expect_identical(s$system, saber::context_render(s$context_manifest))
    expect_true("project_agents" %in% s$context_manifest$sources$id)
})
