# Drive both real startup paths with an on-disk conversation whose saved
# provider differs from config. Only discovery, terminal input and the API are
# replaced; persistence, argument parsing, session construction and turn run.
local({
    root <- tempfile("resume-routes-")
    dir.create(root)
    env_names <- c("R_USER_DATA_DIR", "R_USER_CONFIG_DIR", "R_USER_CACHE_DIR",
                   "CORTEZA_STATE_DIR", "CORTEZA_MATRIX_CONFIG")
    old_env <- Sys.getenv(env_names, unset = NA_character_)
    do.call(Sys.setenv, as.list(setNames(file.path(root, env_names), env_names)))
    old_wd <- setwd(root)
    old_opts <- options(corteza.experimental_ce = FALSE,
                        corteza.experimental_heartbeat = FALSE,
                        corteza.trace = FALSE)
    old_tools <- getOption("corteza.tools")
    on.exit({
        setwd(old_wd)
        for (name in env_names) {
            if (is.na(old_env[[name]])) Sys.unsetenv(name) else
                do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
        }
        options(old_opts)
        options(corteza.tools = old_tools)
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    dir.create(".corteza")
    writeLines('{"provider":"anthropic","model":"claude-config", "web_search":false,
        "workspace":{"enabled":false}, "context_edit":{"enabled":false}}',
        ".corteza/config.json")

    ns <- asNamespace("corteza")
    originals <- list()
    stub <- function(name, value) {
        originals[[name]] <<- get(name, ns)
        assignInNamespace(name, value, "corteza")
    }
    old_agent <- llm.api::agent
    on.exit({
        for (name in names(originals)) assignInNamespace(name, originals[[name]], "corteza")
        assignInNamespace("agent", old_agent, "llm.api")
    }, add = TRUE)
    catalog <- list(entries = list(), diagnostics = list(), hash = "fixture")
    bundle <- list(system = "fixture", manifest = NULL,
                   prefix_sources = list(), instruction_catalog = catalog)
    for (name in c("load_skills", "load_skill_docs", "load_skill_packages")) {
        stub(name, function(...) invisible(NULL))
    }
    stub("build_instruction_catalog", function(...) catalog)
    stub("load_context_bundle", function(...) bundle)
    stub("list_context_files", function(...) character())
    stub("schema_from_registry", function(...) list())
    stub("skills_as_api_tools", function(...) list())
    # The API fake records the actual request and does not look up any key.
    requests <- list()
    assignInNamespace("agent", function(provider = c("anthropic", "openai",
                                                     "openai_codex", "openai_compatible"), ...) {
        args <- c(list(provider = provider), list(...))
        requests[[length(requests) + 1L]] <<- args
        list(content = "fixture reply", history = args$history,
             usage = list(input_tokens = 1L, output_tokens = 1L))
    }, "llm.api")
    seen <- NULL
    loops <- 0L
    stub("run_repl_loop", function(ctx) {
        loops <<- loops + 1L
        seen <<- ctx
        ctx$turn_fn("continue fixture", ctx$session)
        ns$session_save(ctx$disk_session$session)
    })

    cli <- new.env(parent = ns)
    definitions <- Filter(function(e) is.call(e) &&
        identical(e[[1L]], as.name("<-")) && is.call(e[[3L]]) &&
        identical(e[[3L]][[1L]], as.name("function")),
        parse(system.file("bin", "corteza", package = "corteza")))
    for (definition in definitions) eval(definition, cli)
    cli$color <- ns$ansi_colors()
    cli$print_banner <- function(...) invisible(NULL)

    # chat() is interactive-only; bypass that guard and API-key validation,
    # retaining its real setup and the shared turn implementation.
    console <- new.env(parent = ns)
    console$interactive <- function() TRUE
    console$session_setup <- function(...) {
        args <- list(...)
        args$validate_api_key <- FALSE
        do.call(corteza::session_setup, args)
    }
    console$chat_workspace_init <- function(...) invisible(NULL)
    chat <- corteza::chat
    environment(chat) <- console

    cases <- list(
        list(provider = NULL, model = NULL, expected = c("openai_codex", "gpt-saved")),
        list(provider = "anthropic", model = NULL, expected = c("anthropic", "claude-config")),
        list(provider = NULL, model = "gpt-override", expected = c("openai_codex", "gpt-override")),
        list(provider = "openai", model = NULL,
             expected = c("openai", corteza::default_provider_model("openai"))),
        list(provider = "anthropic", model = "claude-override",
             expected = c("anthropic", "claude-override"))
    )
    for (surface in c("cli", "chat")) for (case in cases) {
        saved <- ns$session_new("openai_codex", "gpt-saved", root,
                                session_key = "corteza:resume-fixture")
        ns$transcript_append(saved, "user", "previous question")
        ns$transcript_append(saved, "assistant", "previous reply")
        # Exercise --resume/latest as well as explicit session keys.
        latest <- is.null(case$provider) && is.null(case$model)
        before <- length(requests)
        if (surface == "cli") {
            flags <- if (latest) "--resume" else c("--session", saved$sessionKey)
            if (!is.null(case$provider)) flags <- c(flags, "--provider", case$provider)
            if (!is.null(case$model)) flags <- c(flags, "--model", case$model)
            cli$commandArgs <- function(...) flags
            capture.output(cli$run_agent(cli$parse_args()))
        } else {
            capture.output(chat(provider = case$provider, model = case$model,
                                session = if (latest) TRUE else saved$sessionKey,
                                tools = character(), web_search = FALSE))
        }
        expect_equal(length(requests), before + 1L)
        request <- requests[[length(requests)]]
        expect_equal(c(request$provider, request$model), case$expected)
        expect_equal(c(seen$session$provider, seen$session$model_map$cloud), case$expected)
        expect_equal(vapply(request$history, function(m) m$content, ""),
                     c("previous question", "previous reply"))
        reloaded <- ns$session_load(saved$sessionKey)
        expect_equal(c(reloaded$provider, reloaded$model), case$expected)
        expect_equal(length(reloaded$messages), 2L)
    }
    expect_equal(loops, 10L)

    # A new CLI conversation still takes its configured pair.
    cli$commandArgs <- function(...) c("--session", "corteza:fresh-fixture")
    capture.output(cli$run_agent(cli$parse_args()))
    expect_equal(c(seen$session$provider, seen$session$model_map$cloud),
                 c("anthropic", "claude-config"))
    fresh <- ns$session_load("corteza:fresh-fixture")
    expect_equal(c(fresh$provider, fresh$model), c("anthropic", "claude-config"))

    # Shared setup must not reintroduce the unrelated config model, including
    # a gateway that has no built-in model default.
    for (provider in c("openai", "openai_compatible")) {
        s <- corteza::session_setup(provider = provider, cwd = root,
            system = "fixture", load_project_context = FALSE, validate_api_key = FALSE)
        expect_null(s$model_map$cloud)
    }
})
