# Tests for R/handles.R — large-result stashing and read_handle.

corteza::ensure_skills()
corteza:::clear_handles()
on.exit(corteza:::clear_handles(), add = TRUE)

# --- is_large_result heuristics ----------------------------------------

expect_false(corteza:::.is_large_result(NULL))
expect_false(corteza:::.is_large_result(1L))
expect_false(corteza:::.is_large_result("x"))
expect_false(corteza:::.is_large_result(TRUE))
# Medium vectors pass through.
expect_false(corteza:::.is_large_result(1:10))
# Long vectors get stashed.
expect_true(corteza:::.is_large_result(1:100))
# Data frames and matrices always handle.
expect_true(corteza:::.is_large_result(data.frame(x = 1, y = 2)))
expect_true(corteza:::.is_large_result(matrix(1:4, 2, 2)))
# Long lists handle.
expect_true(corteza:::.is_large_result(as.list(1:20)))

# --- with_handle / get_handle round trip -------------------------------

df <- data.frame(x = 1:3, y = letters[1:3])
stashed <- corteza:::with_handle(df)
expect_true(is.list(stashed))
expect_true(is.character(stashed$handle))
expect_true(grepl("^\\.h_\\d+$", stashed$handle))
expect_true(is.character(stashed$summary))
expect_true(nchar(stashed$summary) > 0L)

retrieved <- corteza:::get_handle(stashed$handle)
expect_equal(retrieved, df)
expect_true(stashed$handle %in% corteza:::list_handles())

# Unknown handle returns NULL.
expect_null(corteza:::get_handle(".h_does_not_exist"))

# Multiple handles get distinct ids.
h2 <- corteza:::with_handle(matrix(1:4, 2, 2))
expect_false(identical(stashed$handle, h2$handle))

# --- run_r: scalars pass through, large values stash --------------------

corteza:::clear_handles()

# Scalar result prints normally, no handle.
res <- corteza:::call_tool("run_r", list(code = "2 + 2"))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 4", res$content[[1]]$text))
expect_equal(length(corteza:::list_handles()), 0L)

# Data frame stashed as handle; output is summary + marker.
res <- corteza:::call_tool("run_r",
                           list(code = "data.frame(a = 1:5, b = letters[1:5])"))
expect_false(isTRUE(res$isError))
text <- res$content[[1]]$text
expect_true(grepl("stored as \\.h_\\d+", text))
# str() output mentions columns.
expect_true(grepl("'data.frame'", text, fixed = TRUE))
expect_equal(length(corteza:::list_handles()), 1L)

# Invisible assignments persist in globalenv. run_r evaluates in
# globalenv (PR <fix> 2026-05-20) so `<-` matches the tool's
# docstring; the earlier child-env behavior silently dropped
# assignments. The handle stash should still be empty since `NULL`
# produced no visible large result. Cleanup is inline (on.exit at
# tinytest top-level fires immediately after the expression that
# registered it, so it would nuke the variable before the
# assertion below could see it).
corteza:::clear_handles()
suppressWarnings(rm("x_internal_assign", envir = globalenv()))
res <- corteza:::call_tool("run_r",
                           list(code = "x_internal_assign <- 1:1000; NULL"))
expect_false(isTRUE(res$isError))
expect_true("x_internal_assign" %in% ls(globalenv()))
expect_equal(length(corteza:::list_handles()), 0L)
suppressWarnings(rm("x_internal_assign", envir = globalenv()))

# --- Handle visible in subsequent run_r --------------------------------

corteza:::clear_handles()
h <- corteza:::with_handle(data.frame(x = 1:10, y = 11:20))
res <- corteza:::call_tool("run_r",
                           list(code = sprintf("nrow(%s)", h$handle)))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 10", res$content[[1]]$text))

# Regression (codex 2026-05-20): handle_eval_env() used to skip
# reassignment when the .h_NNN symbol already existed in globalenv,
# so re-binding a handle in the store left the old globalenv copy
# stale. Force a rebind and verify the globalenv binding reflects
# the new value.
corteza:::clear_handles()
h1 <- corteza:::with_handle(data.frame(x = 1:10, y = 11:20))
# Replace the same handle id with a smaller frame.
assign(h1$handle, data.frame(x = 1:5), envir = corteza:::.handle_store)
res <- corteza:::call_tool("run_r",
                           list(code = sprintf("nrow(%s)", h1$handle)))
expect_false(isTRUE(res$isError))
expect_true(grepl("^\\[1\\] 5", res$content[[1]]$text))

# Stale handles are removed from globalenv when they drop out of
# the store. After clear_handles(), the previously copied .h_NNN
# symbol must not linger -- clear_handles() itself sweeps the
# managed bindings out of globalenv.
corteza:::clear_handles()
h2 <- corteza:::with_handle(data.frame(x = 1:3))
corteza:::call_tool("run_r", list(code = sprintf("nrow(%s)", h2$handle)))
expect_true(exists(h2$handle, envir = globalenv(), inherits = FALSE))
corteza:::clear_handles()
expect_false(exists(h2$handle, envir = globalenv(), inherits = FALSE))

# Scoped evaluators retain their own handles. A handle created in one scope
# must not become visible to another scope or to global run_r.
scope_a <- new.env(parent = baseenv())
scope_b <- new.env(parent = baseenv())
scoped <- corteza::tool_run_r("matrix(1:4, 2, 2)", envir = scope_a)
expect_false(isTRUE(scoped$isError))
expect_true(grepl("stored as .h_001", scoped$content[[1]]$text, fixed = TRUE))
expect_true(exists(".h_001", envir = scope_a, inherits = FALSE) == FALSE)

scoped <- corteza::tool_run_r("dim(.h_001)", envir = scope_a)
expect_true(grepl("2 2", scoped$content[[1]]$text, fixed = TRUE))
other <- corteza::tool_run_r("dim(.h_001)", envir = scope_b)
expect_true(grepl("object '.h_001' not found", other$content[[1]]$text,
                  fixed = TRUE))
global <- corteza::tool_run_r("exists('.h_001', inherits = FALSE)")
expect_true(grepl("FALSE", global$content[[1]]$text, fixed = TRUE))

# Output-cap handles are scoped too, including large scalar strings that do
# not meet the ordinary large-object heuristic.
capped <- corteza::tool_run_r("paste(rep('x', 6000), collapse = '')",
                              envir = scope_b)
expect_true(grepl("tool output truncated", capped$content[[1]]$text,
                  fixed = TRUE))
expect_true(grepl("full output stored as: .h_001", capped$content[[1]]$text,
                  fixed = TRUE))
expect_true(grepl("TRUE", corteza::tool_run_r(
    "nchar(paste(.h_001, collapse = '\\n')) > 6000", envir = scope_b
)$content[[1]]$text, fixed = TRUE))

# --- read_handle ops ---------------------------------------------------

corteza:::clear_handles()
h <- corteza:::with_handle(data.frame(x = 1:6, y = letters[1:6]))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "head"))
expect_false(isTRUE(res$isError))
expect_true(grepl("x y", res$content[[1]]$text))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "str"))
expect_false(isTRUE(res$isError))
expect_true(grepl("'data.frame'", res$content[[1]]$text, fixed = TRUE))

res <- corteza:::call_tool("read_handle", list(handle = h$handle, op = "summary"))
expect_false(isTRUE(res$isError))

# Default op is "str".
res <- corteza:::call_tool("read_handle", list(handle = h$handle))
expect_false(isTRUE(res$isError))

# Unknown handle produces a clean error, not a crash.
res <- corteza:::call_tool("read_handle",
                           list(handle = ".h_does_not_exist", op = "str"))
expect_true(isTRUE(res$isError))
expect_true(grepl("Unknown handle", res$content[[1]]$text))

# Unknown op errors cleanly.
res <- corteza:::call_tool("read_handle",
                           list(handle = h$handle, op = "bogus"))
expect_true(isTRUE(res$isError))

# No handle, or several, is a clean error too: a model that passes what
# a regular expression found in a marker can pass nothing.
for (bad in list(character(), c(".h_001", ".h_002"), NA_character_)) {
    res <- corteza:::tool_read_handle(bad, op = "grep", pattern = "x")
    expect_true(isTRUE(res$isError))
    expect_true(grepl("one handle id", res$content[[1]]$text, fixed = TRUE))
}

# --- read_handle: searching and paging a stored tool result ------------

corteza:::clear_handles()
rooms <- sprintf("- room %02d (!r%d:ex), working in /home/u/project%02d",
                 1:60, 1:60, 1:60)
rooms[47] <- "- Hacer (!r47:ex), working in /home/u/hacer"
lst <- corteza:::with_handle(rooms)
read <- function(...) {
    res <- corteza:::call_tool("read_handle", list(handle = lst$handle, ...))
    list(error = isTRUE(res$isError), text = res$content[[1]]$text,
         lines = strsplit(res$content[[1]]$text, "\n", fixed = TRUE)[[1]])
}

# grep finds a line past where a cut result's preview ends, with its
# line number, and says how many matched of how many.
g <- read(op = "grep", pattern = "hacer")
expect_false(g$error)
expect_identical(g$lines[[1]], "1 of 60 lines match 'hacer':")
expect_identical(g$lines[[2]], paste0("47: ", rooms[47]))
# No match is an answer about every line, not a gap.
none <- read(op = "grep", pattern = "nowhere")
expect_false(none$error)
expect_identical(none$text, "No line matches 'nowhere'. All 60 lines were searched.")
# A regular expression, and text that is not one.
expect_identical(read(op = "grep", pattern = "project0[12]$")$lines[[1]],
                 "2 of 60 lines match 'project0[12]$':")
expect_identical(read(op = "grep", pattern = "(!r47")$lines[[1]],
                 "1 of 60 lines match '(!r47':")
# More matches than fit: the first 40, a count, and what to do next.
many <- read(op = "grep", pattern = "working in")
expect_identical(length(many$lines), 41L)
expect_true(grepl("^60 of 60 lines match 'working in'; the first 40 are shown",
                  many$lines[[1]]))
# A pattern is required.
expect_true(read(op = "grep")$error)
expect_true(read(op = "grep", pattern = "")$error)

# lines pages through it, and says where to continue.
p <- read(op = "lines", start = 41)
expect_identical(p$lines[[1]], "lines 41 to 60 of 60")
expect_identical(p$lines[[2]], rooms[41])
expect_identical(length(p$lines), 21L)
first <- read(op = "lines")
expect_identical(first$lines[[1]], "lines 1 to 40 of 60 (more with start = 41)")
expect_identical(length(first$lines), 41L)
expect_identical(read(op = "lines", start = 45, end = 47)$lines,
                 c("lines 45 to 47 of 60 (more with start = 48)", rooms[45:47]))
# An end past the last line is the last line.
expect_identical(read(op = "lines", start = 58, end = 500)$lines[[1]],
                 "lines 58 to 60 of 60")
expect_identical(read(op = "lines", start = 61)$text,
                 "There is no line 61: the value has 60 lines.")
expect_true(read(op = "lines", start = 0)$error)
expect_true(read(op = "lines", start = 5, end = 2)$error)
expect_true(read(op = "lines", start = "x")$error)

# Neither read comes back long enough to be cut again.
wide <- corteza:::with_handle(rep(strrep("x", 900L), 100L))
for (res in list(
    corteza:::call_tool("read_handle", list(handle = wide$handle, op = "lines")),
    corteza:::call_tool("read_handle", list(handle = wide$handle, op = "grep",
                                            pattern = "x")),
    corteza:::call_tool("read_handle", list(handle = lst$handle, op = "grep",
                                            pattern = "working in")))) {
    txt <- res$content[[1]]$text
    expect_identical(corteza:::admit_tool_result(txt, tool = "read_handle"), txt)
}
# One line longer than a whole read is still shown, cut.
long <- corteza:::with_handle(strrep("y", 20000L))
txt <- corteza:::call_tool("read_handle",
                           list(handle = long$handle, op = "lines"))$content[[1]]$text
expect_true(nchar(txt) < 4200L)
expect_true(grepl("^lines 1 to 1 of 1\ny", txt))

# A value that is not text is searched as it prints.
df <- corteza:::with_handle(data.frame(n = 1:30, name = sprintf("row%02d", 1:30)))
res <- corteza:::call_tool("read_handle", list(handle = df$handle, op = "grep",
                                               pattern = "row17"))
expect_true(grepl("row17", res$content[[1]]$text))
expect_true(grepl("^1 of 31 lines match", res$content[[1]]$text))
corteza:::clear_handles()
