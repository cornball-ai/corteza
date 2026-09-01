# Continual harness store: round-trip, dedupe, versioning, snapshots,
# rollback, validation, rendering, and the harness_note tool. All
# offline; all writes confined to a tempdir project scope (the global
# scope is only ever path-checked, never written).

td <- file.path(tempfile("harness-test-"), "proj")
dir.create(td, recursive = TRUE)

# path shapes
expect_true(grepl("\\.corteza/harness\\.json$",
                  corteza:::harness_path("project", td)))
expect_true(grepl("harness\\.json$", corteza:::harness_path("global")))

# empty load
p <- corteza:::harness_path("project", td)
st0 <- corteza:::harness_load(p)
expect_identical(length(st0$entries), 0L)

# create -> round trip, version 1, ledger with before=NULL snapshot
ref1 <- corteza:::harness_apply(
    list(list(action = "create", title = "Game ids",
              content = "Game-instance ids are session-scoped.",
              kind = "memory",
              evidence = "arc driver debugging",
              provenance = list(model = "claude-opus-5"))),
    scope = "project", cwd = td, trigger = "test")
expect_true(startsWith(ref1, "refine_"))
st1 <- corteza:::harness_load(p)
expect_identical(length(st1$entries), 1L)
e1 <- st1$entries[["game-ids"]]
expect_identical(e1$version, 1L)
expect_identical(e1$content, "Game-instance ids are session-scoped.")
expect_identical(st1$refinements[[1L]]$edits[[1L]]$action, "create")
expect_null(st1$refinements[[1L]]$edits[[1L]]$before)

# exact-content dedupe under a different title -> no-op
ref2 <- corteza:::harness_apply(
    list(list(action = "create", title = "Different title",
              content = "Game-instance ids are session-scoped.")),
    scope = "project", cwd = td, trigger = "test")
expect_null(ref2)
expect_identical(length(corteza:::harness_load(p)$entries), 1L)

# update bumps version and snapshots before/after
ref3 <- corteza:::harness_apply(
    list(list(action = "update", id = "game-ids",
              content = "Game ids are per cookie session; resolve in-session.")),
    scope = "project", cwd = td, trigger = "test")
st3 <- corteza:::harness_load(p)
expect_identical(st3$entries[["game-ids"]]$version, 2L)
last_edit <- st3$refinements[[length(st3$refinements)]]$edits[[1L]]
expect_identical(last_edit$before$version, 1L)
expect_identical(last_edit$after$version, 2L)

# rollback of the update restores v-old content (as a new refinement)
ref4 <- corteza:::harness_rollback(ref3, scope = "project", cwd = td)
st4 <- corteza:::harness_load(p)
expect_identical(st4$entries[["game-ids"]]$content,
                 "Game-instance ids are session-scoped.")
expect_true(length(st4$refinements) > length(st3$refinements))

# delete + rollback of the delete restores the entry
ref5 <- corteza:::harness_apply(
    list(list(action = "delete", id = "game-ids")),
    scope = "project", cwd = td, trigger = "test")
expect_identical(length(corteza:::harness_load(p)$entries), 0L)
corteza:::harness_rollback(ref5, scope = "project", cwd = td)
expect_identical(
    corteza:::harness_load(p)$entries[["game-ids"]]$content,
    "Game-instance ids are session-scoped.")

# validation refusals
expect_error(corteza:::harness_apply(
    list(list(action = "create", title = "x", content = "two\nlines")),
    scope = "project", cwd = td), pattern = "one line")
expect_error(corteza:::harness_apply(
    list(list(action = "create", title = "x",
              content = paste(rep("a", 301), collapse = ""))),
    scope = "project", cwd = td), pattern = "300")
expect_error(corteza:::harness_apply(
    list(list(action = "create", id = "base_system_prompt",
              content = "nope")),
    scope = "project", cwd = td), pattern = "not editable")
expect_error(corteza:::harness_apply(
    list(list(action = "create", title = "x", content = "ok",
              kind = "spell")),
    scope = "project", cwd = td), pattern = "kind")

# corrupt file degrades to empty with a warning
writeLines("{not json", p)
expect_warning(stc <- corteza:::harness_load(p), pattern = "unreadable")
expect_identical(length(stc$entries), 0L)

# rendering: rebuild a small store, block carries header + provenance
corteza:::harness_apply(
    list(list(action = "create", title = "Fact one", content = "Fact one.",
              provenance = list(model = "claude-opus-5")),
         list(action = "create", title = "Fact two", content = "Fact two.")),
    scope = "project", cwd = td, trigger = "test")
blk <- corteza:::harness_context_block(td, list())
expect_true(grepl("# Lessons", blk, fixed = TRUE))
expect_true(grepl("- [project] Fact one. (via claude-opus-5)", blk,
                  fixed = TRUE))
expect_true(grepl("re-verify", blk))
# cap: tiny budget omits and says so
blk_cap <- corteza:::harness_context_block(td,
    list(harness_max_chars = 30L))
expect_true(grepl("omitted", blk_cap))
# empty dir renders no block
td2 <- file.path(tempfile("harness-empty-"), "proj")
dir.create(td2, recursive = TRUE)
expect_identical(corteza:::harness_context_block(td2, list()), "")

# slug
expect_identical(corteza:::.harness_slug("A  Fact, About Things!"),
                 "a-fact-about-things")

# history flattening handles both content shapes
h <- list(list(role = "user", content = "hello"),
          list(role = "assistant", content = list(
              list(type = "text", text = "hi"),
              list(type = "tool_use", name = "run_r", input = list()))))
txt <- corteza:::.harness_history_text(h)
expect_true(grepl("user: hello", txt, fixed = TRUE))
expect_true(grepl("[tool: run_r]", txt, fixed = TRUE))

# the tool: applies via ctx$cwd, reports id; exact duplicate reports so
sess <- new.env()
res_tool <- corteza:::tool_harness_note(
    title = "Catalog size", fact = "The public catalog has 25 games.",
    evidence = "arc_games listing", scope = "project",
    ctx = list(cwd = td, session = sess))
expect_true(grepl("Recorded [project] catalog-size", res_tool$content[[1L]]$text,
                  fixed = TRUE))
res_dup <- corteza:::tool_harness_note(
    title = "Other name", fact = "The public catalog has 25 games.",
    scope = "project", ctx = list(cwd = td, session = sess))
expect_true(grepl("Already recorded", res_dup$content[[1L]]$text, fixed = TRUE))

# default config forces approval on the tool
cfg <- corteza:::load_config(td2)
expect_identical(cfg$permissions[["harness_note"]], "ask")
