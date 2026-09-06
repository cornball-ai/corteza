# Session-scoped instruction catalogs: deterministic discovery, compact
# metadata, scoped lazy reads, drift refusal, and legacy-registry separation.

.write_instruction <- function(dir, name, description, body = "BODY",
                               folded = FALSE) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    front <- if (folded) {
        c("---", paste0("name: ", name), "description: >-",
            paste0("  ", description), "---")
    } else {
        c("---", paste0("name: ", name),
            paste0("description: ", description), "---")
    }
    writeLines(c(front, body), file.path(dir, "SKILL.md"))
}

local({
    root <- tempfile("instruction-catalog-")
    cfg_home <- file.path(root, "config-home")
    data_home <- file.path(root, "data-home")
    project <- file.path(root, "project")
    dir.create(file.path(project, ".corteza"), recursive = TRUE)

    old_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    old_data <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
    old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CONFIG_DIR = cfg_home, R_USER_DATA_DIR = data_home)
    on.exit({
        if (is.na(old_config)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = old_config)
        }
        if (is.na(old_data)) {
            Sys.unsetenv("R_USER_DATA_DIR")
        } else {
            Sys.setenv(R_USER_DATA_DIR = old_data)
        }
        if (is.na(old_cache)) {
            Sys.unsetenv("R_USER_CACHE_DIR")
        } else {
            Sys.setenv(R_USER_CACHE_DIR = old_cache)
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)

    global_config <- corteza:::corteza_config_path("config.json")
    global_root <- file.path(dirname(global_config), "global-skills")
    replacement_global <- file.path(dirname(global_config), "old-replacement")
    replacement_project <- file.path(project, "new-replacement")
    .write_instruction(file.path(global_root, "shared", "audit"),
                       "Audit traces", "Find repeated trace patterns.",
                       "GLOBAL-BODY-SENTINEL")
    .write_instruction(global_root, "Shared root", "Root-level instruction.",
                       "ROOT-BODY")
    .write_instruction(file.path(replacement_global, "old"),
                       "Old root", "Must be replaced.", "OLD-SENTINEL")
    .write_instruction(file.path(replacement_project, "new"),
                       "New root", "Project replacement.", "NEW-SENTINEL")
    dir.create(dirname(global_config), recursive = TRUE, showWarnings = FALSE)
    writeLines(
               paste0(
                      '{"instruction_roots": {',
                      '"shared": "global-skills", ',
                      '"replace": "old-replacement"',
                      '}, "instruction_disabled": ["shared:disabled"]}'
        ),
               global_config
    )

    project_skills <- file.path(project, ".corteza", "skills")
    .write_instruction(
                       file.path(project_skills, "analysis", "logs"),
                       "Log analyst", "Analyze recurring programming themes.",
                       "PROJECT-BODY-SENTINEL", folded = TRUE
    )
    writeLines("SUPPORT-SENTINEL",
               file.path(project_skills, "analysis", "logs", "guide.txt"))
    .write_instruction(file.path(project_skills, "disabled"),
                       "Disabled", "Should not load.", "DISABLED-SENTINEL")
    writeLines("README-BODY-SENTINEL", file.path(project_skills, "README.md"))

    # The narrower root owns this overlapping path and therefore its id.
    overlap <- file.path(project_skills, "team")
    .write_instruction(file.path(overlap, "special"),
                       "Special", "Most-specific root wins.", "SPECIAL")
    .write_instruction(file.path(project_skills, "duplicate-name"),
                       "Log analyst", "Same display name, different id.", "DUP")

    project_config <- file.path(project, ".corteza", "config.json")
    writeLines(
               paste0(
                      '{"instruction_roots": {',
                      '"replace": "new-replacement", ',
                      '"narrow": ".corteza/skills/team"',
                      '}, "instruction_disabled": ["project:disabled"]}'
        ),
               project_config
    )

    catalog <- corteza:::build_instruction_catalog(project)
    ids <- names(catalog$entries)
    expect_true(inherits(catalog, "corteza_instruction_catalog"))
    expect_identical(ids, sort(ids))
    expect_true("shared:shared/audit" %in% ids)
    expect_true("shared" %in% ids)
    expect_true("replace:new" %in% ids)
    expect_false("replace:old" %in% ids)
    expect_true("project:analysis/logs" %in% ids)
    expect_true("narrow:special" %in% ids)
    expect_false("project:team/special" %in% ids)
    expect_false("project:disabled" %in% ids)
    expect_false(any(grepl("README", ids, fixed = TRUE)))
    expect_true("shared:disabled" %in% catalog$disabled)
    expect_true("project:disabled" %in% catalog$disabled)
    expect_true(any(vapply(catalog$diagnostics, function(x) {
        identical(x$code, "duplicate_name")
    }, logical(1L))))

    header <- corteza:::format_instruction_catalog(catalog)
    expect_true(grepl("Analyze recurring programming themes.", header,
                      fixed = TRUE))
    expect_true(grepl("project:analysis/logs", header, fixed = TRUE))
    expect_false(grepl("PROJECT-BODY-SENTINEL", header, fixed = TRUE))
    expect_false(grepl("README-BODY-SENTINEL", header, fixed = TRUE))
    expect_false(grepl(normalizePath(project), header, fixed = TRUE))
    expect_error(corteza:::instruction_catalog_read(
            catalog, "shared", "shared/audit/SKILL.md"
        ), pattern = "not part")

    main <- corteza:::instruction_catalog_read(catalog, "project:analysis/logs")
    expect_true(grepl("PROJECT-BODY-SENTINEL", main, fixed = TRUE))
    support <- corteza:::instruction_catalog_read(
        catalog, "project:analysis/logs", "guide.txt"
    )
    expect_true(grepl("SUPPORT-SENTINEL", support, fixed = TRUE))
    expect_error(corteza:::instruction_catalog_read(
            catalog, "project:analysis/logs", c("guide.txt", "SKILL.md")
        ), pattern = "one relative path")
    expect_error(corteza:::instruction_catalog_read(
            catalog, "project:analysis/logs", "../SKILL.md"
        ), pattern = "stay within")
    expect_error(corteza:::instruction_catalog_read(
            catalog, "project:analysis/logs", normalizePath(project_config)
        ), pattern = "stay within")
    expect_error(corteza:::instruction_catalog_read(
            catalog, "foreign:skill"
        ), pattern = "not in this session catalog")

    # Resources are hashed at session start, so even a mutation before their
    # first read is refused.
    fresh_dir <- file.path(project_skills, "fresh")
    .write_instruction(fresh_dir, "Fresh", "Drift test.", "FRESH")
    fresh_resource <- file.path(fresh_dir, "reference.md")
    writeLines("before", fresh_resource)
    fresh_catalog <- corteza:::build_instruction_catalog(project)
    original_hash <- fresh_catalog$hash
    writeLines("after", fresh_resource)
    changed_catalog <- corteza:::build_instruction_catalog(project)
    expect_false(identical(original_hash, changed_catalog$hash))
    expect_error(corteza:::instruction_catalog_read(
            fresh_catalog, "project:fresh", "reference.md"
        ), pattern = "drifted")

    main_path <- file.path(fresh_dir, "SKILL.md")
    writeLines(c("---", "name: Fresh", "description: Drift test.", "---",
                 "changed"), main_path)
    expect_error(corteza:::instruction_catalog_read(
            fresh_catalog, "project:fresh"
        ), pattern = "drifted")

    # A catalog remains bound to its project even after another one is built.
    other <- file.path(root, "other")
    .write_instruction(file.path(other, ".corteza", "skills", "only-b"),
                       "Only B", "Belongs to B.", "B-BODY")
    other_catalog <- corteza:::build_instruction_catalog(other)
    expect_true("project:only-b" %in% names(other_catalog$entries))
    expect_false("project:only-b" %in% names(catalog$entries))
    expect_error(corteza:::instruction_catalog_read(
            catalog, "project:only-b"
        ), pattern = "not in this session catalog")

    # Building the new catalog must not mutate the old process-global docs
    # registry or the executable tool registry.
    corteza:::clear_skill_docs()
    legacy <- file.path(root, "legacy")
    dir.create(legacy)
    writeLines(c("---", "name: legacy", "description: old", "---",
                 "LEGACY-BODY-SENTINEL"), file.path(legacy, "legacy.md"))
    corteza:::load_skill_docs(legacy)
    before_docs <- corteza:::format_skill_docs()
    before_tools <- corteza:::list_skills()
    invisible(corteza:::build_instruction_catalog(project))
    expect_identical(corteza:::format_skill_docs(), before_docs)
    expect_true(grepl("LEGACY-BODY-SENTINEL", before_docs, fixed = TRUE))
    expect_identical(corteza:::list_skills(), before_tools)

    cache_home <- file.path(root, "cache-home")
    Sys.setenv(R_USER_CACHE_DIR = cache_home)
    session <- corteza::session_setup(
                                      channel = "console", cwd = project, provider = "anthropic",
                                      model = "test-model", tools = "file",
                                      validate_api_key = FALSE
    )
    expect_true(inherits(session$instruction_catalog,
                         "corteza_instruction_catalog"))
    expect_true(grepl("project:analysis/logs", session$system, fixed = TRUE))
    tool_result <- corteza:::call_skill(
                                        "skill_instructions",
                                        list(id = "project:analysis/logs"),
                                        ctx = list(session = session, cwd = project)
    )
    expect_true(grepl("PROJECT-BODY-SENTINEL",
                      tool_result$content[[1L]]$text, fixed = TRUE))

    filtered <- corteza::session_setup(
                                       channel = "console", cwd = project, provider = "anthropic",
                                       model = "test-model", tools = c("run_r"),
                                       validate_api_key = FALSE
    )
    expect_true(inherits(filtered$instruction_catalog,
                         "corteza_instruction_catalog"))
    expect_false(grepl("## Instruction catalog", filtered$system, fixed = TRUE))
})

local({
    root <- tempfile("instruction-invalid-config-")
    cfg_home <- file.path(root, "config")
    old_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA_character_)
    Sys.setenv(R_USER_CONFIG_DIR = cfg_home)
    on.exit({
        if (is.na(old_config)) {
            Sys.unsetenv("R_USER_CONFIG_DIR")
        } else {
            Sys.setenv(R_USER_CONFIG_DIR = old_config)
        }
        unlink(root, recursive = TRUE)
    }, add = TRUE)
    cfg <- corteza:::corteza_config_path("config.json")
    dir.create(dirname(cfg), recursive = TRUE)
    writeLines('{"instruction_disabled": 7}', cfg)
    catalog <- corteza:::build_instruction_catalog(file.path(root, "project"))
    expect_true(any(vapply(catalog$diagnostics, function(x) {
        identical(x$code, "invalid_disabled")
    }, logical(1L))))
})

local({
    root <- tempfile("instruction-symlink-")
    inside <- file.path(root, "inside")
    outside <- tempfile("instruction-outside-")
    dir.create(inside, recursive = TRUE)
    dir.create(outside, recursive = TRUE)
    .write_instruction(file.path(inside, "ok"), "OK", "Inside.", "OK")
    .write_instruction(file.path(outside, "escape"), "Escape", "Outside.",
                       "ESCAPE")
    made_escape <- file.symlink(file.path(outside, "escape"),
                                file.path(inside, "escape-link"))
    made_cycle <- file.symlink(inside, file.path(inside, "cycle"))
    walk <- corteza:::instruction_walk_root(inside)
    expect_true(any(basename(walk$skills) == "SKILL.md"))
    if (isTRUE(made_escape)) {
        expect_true(any(vapply(walk$diagnostics, function(x) {
            identical(x$code, "symlink_escape")
        }, logical(1L))))
    }
    if (isTRUE(made_cycle)) {
        expect_true(any(vapply(walk$diagnostics, function(x) {
            identical(x$code, "directory_cycle")
        }, logical(1L))))
    }
    unlink(root, recursive = TRUE)
    unlink(outside, recursive = TRUE)
})

# Filter semantics and model-facing tool classification.
expect_true(corteza:::instruction_reader_selected(NULL))
expect_true(corteza:::instruction_reader_selected("core"))
expect_true(corteza:::instruction_reader_selected("file"))
expect_false(corteza:::instruction_reader_selected(c("run_r", "bash")))
expect_identical(corteza:::classify_op("skill_instructions"), "read")
file_tool_names <- vapply(corteza::schema_from_registry("file"),
                          function(x) x$name, character(1L))
expect_true("skill_instructions" %in% file_tool_names)
