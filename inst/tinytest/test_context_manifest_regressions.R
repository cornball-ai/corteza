# Context-manifest regressions across configuration, CLI construction, and
# archival refresh. All cases are offline and isolate filesystem state.

# A project config that does not define context_files must not claim ownership
# of a value inherited from the global config.
local({
    root <- tempfile("context-config-origin-")
    project <- tempfile("context-config-project-")
    dir.create(root, recursive = TRUE)
    dir.create(file.path(project, ".corteza"), recursive = TRUE)

    previous <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CONFIG_DIR = root)
    on.exit({
        if (is.na(previous)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = previous)
        }
        unlink(root, recursive = TRUE)
        unlink(project, recursive = TRUE)
    }, add = TRUE)

    global_path <- corteza:::corteza_config_path("config.json")
    dir.create(dirname(global_path), recursive = TRUE)
    writeLines('{"context_files": ["global.md"]}', global_path)
    project_path <- file.path(project, ".corteza", "config.json")
    writeLines('{"provider": "anthropic"}', project_path)

    inherited <- corteza:::context_custom_sources(
        project, corteza:::load_config(project)
    )
    expect_identical(inherited[[1L]]$config_path, global_path)

    writeLines('{"context_files": ["project.md"]}', project_path)
    overridden <- corteza:::context_custom_sources(
        project, corteza:::load_config(project)
    )
    expect_identical(overridden[[1L]]$config_path, project_path)
})

# Exercise the constructor helper defined and used by the installed CLI rather
# than session_setup(), which the CLI entrypoint does not call.
local({
    cli_path <- system.file("bin", "corteza", package = "corteza")
    expressions <- parse(cli_path)
    is_definition <- function(expr, name) {
        is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
            identical(expr[[2L]], as.name(name))
    }
    constructor <- Filter(
        function(expr) is_definition(expr, "cli_new_turn_session"),
        expressions
    )
    entrypoint <- Filter(
        function(expr) is_definition(expr, "run_agent"),
        expressions
    )
    expect_equal(length(constructor), 1L)
    expect_equal(length(entrypoint), 1L)

    cli_env <- new.env(parent = globalenv())
    eval(constructor[[1L]], envir = cli_env)
    manifest <- structure(list(sources = data.frame(id = "sentinel")),
                          class = "saber_context_manifest")
    catalog <- structure(
        list(entries = list(), diagnostics = list(), hash = "catalog-sentinel"),
        class = "corteza_instruction_catalog"
    )
    bundle <- list(system = "cli-system-sentinel", manifest = manifest,
                   prefix_sources = list(), instruction_catalog = catalog)
    session <- cli_env$cli_new_turn_session(
        provider = "anthropic",
        model = "claude-test",
        context_bundle = bundle,
        history = list(),
        tools = NULL,
        approval_cb = function(...) TRUE
    )
    expect_identical(session$system, bundle$system)
    expect_identical(session$context_manifest, manifest)
    expect_identical(session$context_prefix_sources, list())
    expect_identical(session$instruction_catalog, catalog)

    entry_text <- paste(deparse(entrypoint[[1L]], width.cutoff = 500L),
                        collapse = " ")
    expect_true(grepl("load_context_bundle", entry_text, fixed = TRUE))
    expect_true(grepl("cli_new_turn_session", entry_text, fixed = TRUE))
})

# Archival must refresh rendered context and provenance atomically while
# retaining a consumer-owned prefix such as Matrix room guidance.
local({
    project <- tempfile("archival-context-project-")
    cache <- tempfile("archival-context-cache-")
    dir.create(file.path(project, ".corteza"), recursive = TRUE)
    writeLines(
        paste0('{"context_include_user": false, ',
               '"context_include_soul": false, ',
               '"context_files": ["NOTE.md"]}'),
        file.path(project, ".corteza", "config.json")
    )
    note_path <- file.path(project, "NOTE.md")
    writeLines("old-context-sentinel", note_path)
    skill_dir <- file.path(project, ".corteza", "skills", "refresh")
    dir.create(skill_dir, recursive = TRUE)
    skill_path <- file.path(skill_dir, "SKILL.md")
    writeLines(c("---", "name: Refresh", "description: Refresh test.", "---",
                 "old-skill-body"), skill_path)

    previous_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CACHE_DIR = cache)
    original_archive <- getFromNamespace("archival_archive_turn", "corteza")
    assignInNamespace(
        "archival_archive_turn",
        function(...) list(summary = "archived", subagent_id = "fake-holder"),
        ns = "corteza"
    )
    on.exit({
        assignInNamespace("archival_archive_turn", original_archive,
                          ns = "corteza")
        if (is.na(previous_cache)) {
            Sys.unsetenv("R_USER_CACHE_DIR")
        } else {
            Sys.setenv(R_USER_CACHE_DIR = previous_cache)
        }
        unlink(project, recursive = TRUE)
        unlink(cache, recursive = TRUE)
    }, add = TRUE)

    prefix <- list(corteza:::context_text_source(
        "matrix_system", "runtime", "matrix-prefix-sentinel", -400,
        "test matrix prefix", "matrix"
    ))
    initial <- corteza:::load_context_bundle(project,
                                             prefix_sources = prefix)
    session <- corteza::new_session(
        channel = "matrix", provider = "anthropic",
        model_map = list(cloud = "claude-test", local = NULL),
        system = initial$system
    )
    session$cwd <- project
    session$context_manifest <- initial$manifest
    session$context_prefix_sources <- initial$prefix_sources
    session$instruction_catalog <- initial$instruction_catalog
    old_catalog_hash <- initial$instruction_catalog$hash
    session$history <- list(
        list(role = "user", content = "archive this turn"),
        list(role = "assistant", content = "finished")
    )

    writeLines("new-context-sentinel", note_path)
    writeLines(c("---", "name: Refresh", "description: Refresh test.", "---",
                 "new-skill-body"), skill_path)
    config <- corteza:::load_config(project)
    config$archival <- list(
        enabled = TRUE,
        trigger = list(token_threshold = 1L,
                       tool_call_threshold = 100L, depth_cap = 3L)
    )
    corteza:::maybe_archive_turn(
        turn_session = session,
        prompt = "archive this turn",
        pre_turn_len = 0L,
        result = list(reply = "finished"),
        config = config,
        parent_session_id = "test-parent"
    )

    expect_true(grepl("new-context-sentinel", session$system, fixed = TRUE))
    expect_false(grepl("old-context-sentinel", session$system, fixed = TRUE))
    expect_true(grepl("matrix-prefix-sentinel", session$system, fixed = TRUE))
    expect_identical(session$system,
                     saber::context_render(session$context_manifest))
    expect_identical(session$context_prefix_sources, prefix)
    expect_true(inherits(session$instruction_catalog,
                         "corteza_instruction_catalog"))
    expect_false(identical(session$instruction_catalog$hash, old_catalog_hash))
    refreshed_skill <- corteza:::instruction_catalog_read(
        session$instruction_catalog, "project:refresh"
    )
    expect_true(grepl("new-skill-body", refreshed_skill, fixed = TRUE))
})
