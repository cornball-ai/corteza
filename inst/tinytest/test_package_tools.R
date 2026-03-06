# Tests for package-as-skills (R/package-tools.R)

library(tinytest)

# --- infer_param_type ---

expect_equal(llamaR:::infer_param_type(TRUE), "boolean")
expect_equal(llamaR:::infer_param_type(FALSE), "boolean")
expect_equal(llamaR:::infer_param_type(42), "number")
expect_equal(llamaR:::infer_param_type(3.14), "number")
expect_equal(llamaR:::infer_param_type("foo"), "string")
expect_equal(llamaR:::infer_param_type(NULL), "string")
# Multi-element numeric defaults -> string (not a single scalar)
expect_equal(llamaR:::infer_param_type(c(1, 2, 3)), "string")

# --- is_missing_formal ---

f1 <- function(x, y = 1) NULL
fmls <- formals(f1)
expect_true(llamaR:::is_missing_formal(fmls, 1))   # x has no default
expect_false(llamaR:::is_missing_formal(fmls, 2))   # y = 1

# --- parse_basalt_params ---

md <- paste(
    "#### Arguments",
    "",
    "- **`short`**: Show short format output",
    "- **`branch`**: Show branch and tracking info",
    "",
    "#### Value",
    sep = "\n"
)
params <- llamaR:::parse_basalt_params(md)
expect_equal(params$short, "Show short format output")
expect_equal(params$branch, "Show branch and tracking info")
expect_equal(length(params), 2)

# NULL input returns empty
expect_equal(length(llamaR:::parse_basalt_params(NULL)), 0)

# --- extract_rd_title ---

md2 <- paste(
    "### Get git repository status",
    "",
    "#### Description",
    sep = "\n"
)
expect_equal(llamaR:::extract_rd_title(md2), "Get git repository status")
expect_true(is.null(llamaR:::extract_rd_title(NULL)))
expect_true(is.null(llamaR:::extract_rd_title("no heading here")))

# --- build_params_from_formals ---

f2 <- function(x, y = TRUE, z = 10, w = "hi", ...) NULL
params2 <- llamaR:::build_params_from_formals(f2, NULL)

expect_equal(length(params2), 4)   # ... is skipped
expect_equal(params2$x$type, "string")
expect_true(params2$x$required)
expect_equal(params2$y$type, "boolean")
expect_false(params2$y$required)
expect_equal(params2$z$type, "number")
expect_equal(params2$w$type, "string")

# With basalt markdown for descriptions
md3 <- paste(
    "#### Arguments",
    "",
    "- **`x`**: The input value",
    "- **`y`**: Enable verbose mode",
    sep = "\n"
)
params3 <- llamaR:::build_params_from_formals(f2, md3)
expect_equal(params3$x$description, "The input value")
expect_equal(params3$y$description, "Enable verbose mode")
expect_equal(params3$z$description, "")  # not in help

# --- make_pkg_handler: silent return value ---

# readLines returns silently - handler should capture via print(val)
tmp <- tempfile()
writeLines(c("hello", "world"), tmp)
handler <- llamaR:::make_pkg_handler("base", "readLines")
result <- handler(list(con = tmp), list())
expect_true(!is.null(result$content))
expect_true(grepl("hello", result$content[[1]]$text))
expect_true(grepl("world", result$content[[1]]$text))

# --- make_pkg_handler: NULL return (side-effect function) ---

handler2 <- llamaR:::make_pkg_handler("base", "writeLines")
tmp2 <- tempfile()
result2 <- handler2(list(text = "test output", con = tmp2), list())
expect_true(grepl("OK: base::writeLines completed", result2$content[[1]]$text))
expect_equal(readLines(tmp2), "test output")
unlink(c(tmp, tmp2))

# --- make_pkg_handler: printed output ---

handler3 <- llamaR:::make_pkg_handler("jsonlite", "toJSON")
result3 <- handler3(list(x = list(a = 1)), list())
expect_true(!is.null(result3$content))
expect_true(grepl("\\{", result3$content[[1]]$text))

# --- package_as_skills: selective loading (functions param) ---

llamaR:::clear_skills()
registered <- llamaR:::package_as_skills("base",
    functions = c("readLines", "writeLines"))
expect_equal(length(registered), 2)
expect_true("base::readLines" %in% registered)
expect_true("base::writeLines" %in% registered)
# Should NOT have registered other base exports
expect_true(is.null(llamaR:::get_skill("base::list.files")))

# --- package_as_skills: non-existent functions filtered ---

llamaR:::clear_skills()
registered2 <- llamaR:::package_as_skills("base",
    functions = c("readLines", "nonexistent_fn_xyz"))
expect_equal(length(registered2), 1)
expect_true("base::readLines" %in% registered2)

# --- package_as_skills: multiple functions from a package ---

llamaR:::clear_skills()
llamaR:::register_builtin_skills()
n_before <- length(llamaR:::list_skills())

registered3 <- llamaR:::package_as_skills("jsonlite",
    functions = c("toJSON", "fromJSON", "read_json", "write_json"))
n_after <- length(llamaR:::list_skills())

expect_true(n_after > n_before)
expect_true("jsonlite::toJSON" %in% registered3)
expect_true("jsonlite::fromJSON" %in% registered3)

# Verify a registered skill has correct structure
skill <- llamaR:::get_skill("jsonlite::toJSON")
expect_false(is.null(skill))
expect_true(grepl("jsonlite", skill$description))
expect_equal(skill$inputSchema$type, "object")

# Non-installed package errors
expect_error(llamaR:::package_as_skills("nonexistent_pkg_12345"))

# --- load_skill_packages ---

llamaR:::clear_skills()
config <- list(skill_packages = list(
    list(package = "base", functions = c("readLines", "writeLines")),
    list(package = "jsonlite", functions = c("toJSON", "fromJSON"))
))
llamaR:::load_skill_packages(config)
expect_true(!is.null(llamaR:::get_skill("base::readLines")))
expect_true(!is.null(llamaR:::get_skill("base::writeLines")))
expect_true(!is.null(llamaR:::get_skill("jsonlite::toJSON")))

# --- dynamic tool categories after package loading ---

cats <- llamaR:::get_tool_categories()
expect_true("base" %in% names(cats))
expect_true("base::readLines" %in% cats$base)
expect_true("jsonlite" %in% names(cats))

# Clean up
llamaR:::clear_skills()
