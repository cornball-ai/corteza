# Archival config block defaults + startup validation. Pure-function,
# offline.

cfg <- corteza:::load_config(tempdir())

# Default block is present, off by default.
expect_false(isTRUE(cfg$archival$enabled))
expect_true(isTRUE(cfg$archival$trigger$on_max_turns))
expect_equal(cfg$archival$trigger$token_threshold, 8000L)
expect_equal(cfg$archival$trigger$tool_call_threshold, 5L)
expect_equal(cfg$archival$trigger$depth_cap, 3L)
expect_equal(cfg$archival$summary$style, "structured")
# summary$model should be NULL (= match parent), not a literal string.
expect_null(cfg$archival$summary$model)
# summary$timeout_seconds bounds the LLM summary call so a hung
# provider can't wedge the parent.
expect_equal(cfg$archival$summary$timeout_seconds, 60L)
# Async is on by default so the parent CLI never blocks on a slow
# summarization call.
expect_true(isTRUE(cfg$archival$async))

# Subagents block defaults TRUE so a normal cfg passes the
# archival/subagents validation when archival is later enabled.
expect_true(isTRUE(cfg$subagents$enabled))

# Validation: archival.enabled with subagents.enabled = FALSE fails.
project_cfg_dir <- tempfile("archival_validation_test_")
dir.create(project_cfg_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(project_cfg_dir, recursive = TRUE), add = TRUE)
dir.create(file.path(project_cfg_dir, ".corteza"), recursive = TRUE,
           showWarnings = FALSE)
writeLines(jsonlite::toJSON(list(
    subagents = list(enabled = FALSE),
    archival = list(enabled = TRUE)
), auto_unbox = TRUE),
con = file.path(project_cfg_dir, ".corteza", "config.json"))

err <- tryCatch(corteza:::load_config(project_cfg_dir),
                error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("archival.enabled requires subagents.enabled",
                  conditionMessage(err)))
