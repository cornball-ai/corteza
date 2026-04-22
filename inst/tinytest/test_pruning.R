# Context-aware tool pruning: schema_from_registry() drops tools whose
# available() predicate returns FALSE.

corteza::ensure_skills()

orig_wd <- getwd()
orig_tavily <- Sys.getenv("TAVILY_API_KEY", unset = NA)
on.exit({
    setwd(orig_wd)
    if (is.na(orig_tavily)) {
        Sys.unsetenv("TAVILY_API_KEY")
    } else {
        Sys.setenv(TAVILY_API_KEY = orig_tavily)
    }
}, add = TRUE)

tool_names_from_schema <- function(filter = NULL) {
    vapply(corteza::schema_from_registry(filter),
           function(s) s$name, character(1L))
}

# --- Web: web_search hidden when TAVILY_API_KEY is unset, visible
#     when set -----------------------------------------------------------

Sys.unsetenv("TAVILY_API_KEY")
names_no_key <- tool_names_from_schema()
expect_false("web_search" %in% names_no_key)

Sys.setenv(TAVILY_API_KEY = "tvly-test-fake")
names_with_key <- tool_names_from_schema()
expect_true("web_search" %in% names_with_key)

# fetch_url doesn't need a key, should always be present.
expect_true("fetch_url" %in% names_no_key)
expect_true("fetch_url" %in% names_with_key)

# --- Git: hidden outside a git repo, visible inside ---------------------

# Outside any git repo.
tmp_nongit <- tempfile("nongit_")
dir.create(tmp_nongit)
setwd(tmp_nongit)
names_nongit <- tool_names_from_schema()
expect_false("git_status" %in% names_nongit)
expect_false("git_diff" %in% names_nongit)
expect_false("git_log" %in% names_nongit)
setwd(orig_wd)
unlink(tmp_nongit, recursive = TRUE)

# Inside a repo.
tmp_git <- tempfile("git_")
dir.create(tmp_git)
setwd(tmp_git)
suppressWarnings(system2("git", c("init", "-q", "."),
                         stdout = FALSE, stderr = FALSE))
names_git <- tool_names_from_schema()
expect_true("git_status" %in% names_git)
expect_true("git_diff" %in% names_git)
expect_true("git_log" %in% names_git)
setwd(orig_wd)
unlink(tmp_git, recursive = TRUE)

# --- Tools without an available() predicate stay visible either way -----

expect_true("run_r" %in% names_no_key)
expect_true("bash" %in% names_no_key || "cmd" %in% names_no_key)
expect_true("read_file" %in% names_no_key)

# --- Tools with unavailable predicates stay registered / callable -------

Sys.unsetenv("TAVILY_API_KEY")
# web_search is absent from the payload but still registered.
expect_false(is.null(corteza:::get_skill("web_search")))

# --- Predicate error defaults to TRUE (tool stays visible) --------------

reg <- corteza:::.skill_registry
orig_skill <- reg[["run_r"]]
boom_skill <- orig_skill
boom_skill$available <- function() stop("boom")
assign("run_r", boom_skill, envir = reg)
on.exit(assign("run_r", orig_skill, envir = reg), add = TRUE)
names_boom <- tool_names_from_schema()
expect_true("run_r" %in% names_boom)
