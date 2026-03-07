# Test workspace environment and registry

# Setup: clean workspace before each test group
llamaR:::ws_clear()

# ---- CRUD ----

# ws_put and ws_get
llamaR:::ws_put("x", 42)
expect_equal(llamaR:::ws_get("x"), 42)

# ws_exists
expect_true(llamaR:::ws_exists("x"))
expect_false(llamaR:::ws_exists("nope"))

# ws_get returns NULL for missing
expect_null(llamaR:::ws_get("nope"))

# ws_meta returns provenance
llamaR:::ws_put("df1", data.frame(a = 1:3),
                origin = list(tool = "run_r", args = list(code = "data.frame(a=1:3)")))
meta <- llamaR:::ws_meta("df1")
expect_equal(meta$name, "df1")
expect_equal(meta$class, "data.frame")
expect_equal(meta$origin$tool, "run_r")
expect_true(meta$byte_size > 0)
expect_false(meta$stale)
expect_false(meta$pinned)

# ws_remove
llamaR:::ws_put("temp", "hello")
expect_true(llamaR:::ws_remove("temp"))
expect_false(llamaR:::ws_exists("temp"))
expect_false(llamaR:::ws_remove("nonexistent"))

# ws_meta returns NULL for missing
expect_null(llamaR:::ws_meta("nonexistent"))

# ---- Listing ----

llamaR:::ws_clear()
llamaR:::ws_set_turn(5L)
llamaR:::ws_put("a", 1L, pinned = TRUE)
llamaR:::ws_put("b", "hello")
llamaR:::ws_put("c", data.frame(x = 1))

# ws_names
nms <- llamaR:::ws_names()
expect_true(all(c("a", "b", "c") %in% nms))

# ws_list returns data.frame with expected columns
listing <- llamaR:::ws_list()
expect_true(is.data.frame(listing))
expect_true(all(c("name", "class", "turn", "byte_size", "stale", "pinned") %in%
                names(listing)))
expect_equal(nrow(listing), 3L)

# Pinned flag
a_row <- listing[listing$name == "a", ]
expect_true(a_row$pinned)

# ws_size > 0
expect_true(llamaR:::ws_size() > 0)

# Empty workspace
llamaR:::ws_clear()
expect_equal(nrow(llamaR:::ws_list()), 0L)
expect_equal(llamaR:::ws_size(), 0L)

# ---- Turn counter ----

llamaR:::ws_set_turn(10L)
expect_equal(llamaR:::ws_current_turn(), 10L)

# ws_put records current turn
llamaR:::ws_put("t", TRUE)
expect_equal(llamaR:::ws_meta("t")$turn, 10L)

# ---- Overwrite ----

llamaR:::ws_clear()
llamaR:::ws_put("x", 1)
llamaR:::ws_put("x", 2)
expect_equal(llamaR:::ws_get("x"), 2)
expect_equal(llamaR:::ws_meta("x")$class, "numeric")

# ---- Invalidation ----

llamaR:::ws_clear()
llamaR:::ws_put("base", 100)
llamaR:::ws_put("derived", 200, deps = "base")
llamaR:::ws_put("derived2", 300, deps = "derived")

# Invalidate base: should cascade
result <- llamaR:::ws_invalidate("base")
expect_true("base" %in% result)
expect_true("derived" %in% result)
expect_true("derived2" %in% result)
expect_true(llamaR:::ws_meta("base")$stale)
expect_true(llamaR:::ws_meta("derived")$stale)
expect_true(llamaR:::ws_meta("derived2")$stale)

# mark_fresh
llamaR:::ws_mark_fresh("base")
expect_false(llamaR:::ws_meta("base")$stale)
# derived stays stale
expect_true(llamaR:::ws_meta("derived")$stale)

# Cycle protection: A -> B -> A shouldn't infinite loop
llamaR:::ws_clear()
llamaR:::ws_put("cycA", 1, deps = "cycB")
llamaR:::ws_put("cycB", 2, deps = "cycA")
result <- llamaR:::ws_invalidate("cycA")
expect_true("cycA" %in% result)
expect_true("cycB" %in% result)

# ---- Auto-capture ----

llamaR:::ws_clear()
llamaR:::ws_set_turn(3L)

# Capture a tool result
key <- llamaR:::ws_capture_tool_result("grep_files",
                                       list(pattern = "foo"),
                                       "file.R:10: foo bar", 3L)
expect_equal(key, "grep:foo")
expect_equal(llamaR:::ws_get("grep:foo"), "file.R:10: foo bar")

# Skip no-capture tools
key <- llamaR:::ws_capture_tool_result("memory_store",
                                       list(fact = "test"),
                                       "Stored: test", 3L)
expect_null(key)

# Skip errors
key <- llamaR:::ws_capture_tool_result("bash",
                                       list(command = "false"),
                                       "Error: command failed", 3L)
expect_null(key)

# Skip oversized results
big <- paste(rep("x", 60000), collapse = "")
key <- llamaR:::ws_capture_tool_result("bash",
                                       list(command = "cat big"),
                                       big, 3L)
expect_null(key)

# File key naming
key <- llamaR:::ws_capture_tool_result("base::readLines",
                                       list(con = "test.R"),
                                       "line1\nline2", 3L)
expect_equal(key, "file:test.R")

# Bash key naming
key <- llamaR:::ws_capture_tool_result("bash",
                                       list(command = "ls -la"),
                                       "total 42", 3L)
expect_equal(key, "bash:ls -la")

# Fallback key naming
key <- llamaR:::ws_capture_tool_result("r_help",
                                       list(topic = "mean"),
                                       "help text", 3L)
expect_equal(key, "r_help:turn3")

# File invalidation
llamaR:::ws_capture_tool_result("base::readLines",
                                list(con = "foo.R"),
                                "contents", 4L)
expect_true(llamaR:::ws_exists("file:foo.R"))
llamaR:::ws_invalidate_file("foo.R")
expect_true(llamaR:::ws_meta("file:foo.R")$stale)

# ---- Persistence ----

if (at_home()) {
    llamaR:::ws_clear()
    llamaR:::ws_set_turn(7L)
    llamaR:::ws_put("persist_df", data.frame(a = 1:3, b = letters[1:3]))
    llamaR:::ws_put("persist_fn", mean)
    llamaR:::ws_put("persist_pinned", "keep me", pinned = TRUE)

    test_id <- paste0("test_ws_", format(Sys.time(), "%Y%m%d%H%M%S"))

    # Save
    llamaR:::ws_save(test_id)

    # Clear and verify empty
    llamaR:::ws_clear()
    expect_equal(length(llamaR:::ws_names()), 0L)

    # Load
    loaded <- llamaR:::ws_load(test_id)
    expect_true(loaded)
    expect_equal(llamaR:::ws_current_turn(), 7L)
    expect_true(llamaR:::ws_exists("persist_df"))
    expect_true(llamaR:::ws_exists("persist_pinned"))
    expect_true(is.data.frame(llamaR:::ws_get("persist_df")))
    expect_true(llamaR:::ws_meta("persist_pinned")$pinned)

    # Load missing session returns FALSE
    expect_false(llamaR:::ws_load("nonexistent_session_id_12345"))

    # Cleanup
    dir <- llamaR:::sessions_dir("main")
    unlink(file.path(dir, paste0(test_id, "_workspace.rds")))
    unlink(file.path(dir, paste0(test_id, "_workspace.json")))
}

# ---- Pruning ----

llamaR:::ws_clear()
llamaR:::ws_set_turn(100L)

# Old object (turn 10, age = 90)
llamaR:::ws_put("old_obj", "old data")
meta <- llamaR:::ws_meta("old_obj")
meta$turn <- 10L
llamaR:::ws_set_meta("old_obj", meta)

# Recent object (turn 99, age = 1)
llamaR:::ws_set_turn(100L)
llamaR:::ws_put("new_obj", "new data")
meta2 <- llamaR:::ws_meta("new_obj")
meta2$turn <- 99L
llamaR:::ws_set_meta("new_obj", meta2)

# Pinned old object (should survive)
llamaR:::ws_put("pinned_old", "pinned")
meta3 <- llamaR:::ws_meta("pinned_old")
meta3$turn <- 5L
meta3$pinned <- TRUE
llamaR:::ws_set_meta("pinned_old", meta3)

pruned <- llamaR:::ws_prune(max_age_turns = 50L)
expect_true("old_obj" %in% pruned)
expect_false("new_obj" %in% pruned)
expect_false("pinned_old" %in% pruned)
expect_false(llamaR:::ws_exists("old_obj"))
expect_true(llamaR:::ws_exists("new_obj"))
expect_true(llamaR:::ws_exists("pinned_old"))

# Clean up
llamaR:::ws_clear()

# ---- globalenv scan ----

llamaR:::ws_clear()

# Create test objects in globalenv
assign("test_scan_df", data.frame(x = 1:10), envir = globalenv())
assign("test_scan_vec", letters, envir = globalenv())

registered <- llamaR:::ws_scan_globalenv()
expect_true("test_scan_df" %in% registered)
expect_true("test_scan_vec" %in% registered)

# Verify they're in workspace
expect_true(llamaR:::ws_exists("test_scan_df"))
expect_true(llamaR:::ws_exists("test_scan_vec"))

# Origin should be session_init
meta <- llamaR:::ws_meta("test_scan_df")
expect_equal(meta$origin$tool, "session_init")

# Second scan should not re-register
registered2 <- llamaR:::ws_scan_globalenv()
expect_false("test_scan_df" %in% registered2)
expect_false("test_scan_vec" %in% registered2)

# Clean up globalenv
rm("test_scan_df", "test_scan_vec", envir = globalenv())
llamaR:::ws_clear()
