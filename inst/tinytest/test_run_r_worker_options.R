library(tinytest)

# config$run_r_worker_options reaches callr::r_session_options(), so a host
# can shape the supervised worker process: its environment, library path,
# command-line arguments, or the `arch` binary layout used for a sandbox
# wrapper. Nothing about the host process changes.

# ---- .run_r_worker_session_options ----

default <- corteza:::.run_r_worker_session_options(NULL)
expect_true(is.list(default))
expect_equal(names(default), names(callr::r_session_options()))
opts <- corteza:::.run_r_worker_session_options(list(
    run_r_worker_options = list(
        env = c(TERM = "dumb", CORTEZA_WORKER_OPTION_TEST = "set"))))
expect_equal(opts$env[["CORTEZA_WORKER_OPTION_TEST"]], "set")
expect_equal(opts$arch, default$arch)
expect_error(corteza:::.run_r_worker_session_options(
    list(run_r_worker_options = list("a"))), "named list")
expect_error(corteza:::.run_r_worker_session_options(
    list(run_r_worker_options = c(env = "x"))), "named list")
expect_error(corteza:::.run_r_worker_session_options(
    list(run_r_worker_options = list(no_such_option = 1))))

# The worker starts with R's normal default packages so worker run_r matches
# in-process run_r; a bare callr worker would carry only base plus callr.
expect_true(any(names(default$env) == "R_DEFAULT_PACKAGES"))
expect_true(grepl("utils", default$env[["R_DEFAULT_PACKAGES"]], fixed = TRUE))
expect_true(grepl("stats", default$env[["R_DEFAULT_PACKAGES"]], fixed = TRUE))
# A host that set R_DEFAULT_PACKAGES itself wins.
hostpkg <- corteza:::.run_r_worker_session_options(
    list(run_r_worker_options = list(env = c(R_DEFAULT_PACKAGES = "utils"))))
expect_equal(hostpkg$env[["R_DEFAULT_PACKAGES"]], "utils")

# ---- worker integration ----

# The worker sees the configured environment; the host does not.
corteza::ensure_skills()
s <- corteza::new_session("cli")
s$cwd <- tempdir()
s$config <- list(run_r_mode = "worker", skill_timeout = 5,
                 skill_timeout_max = 10,
                 run_r_worker_options = list(
                     env = c(TERM = "dumb", CORTEZA_WORKER_OPTION_TEST = "set")))
res <- corteza:::call_skill(
    "run_r", list(code = 'Sys.getenv("CORTEZA_WORKER_OPTION_TEST")'),
    ctx = list(session = s, cwd = s$cwd))
expect_false(isTRUE(res$isError))
expect_true(grepl('"set"', res$content[[1L]]$text, fixed = TRUE))
expect_equal(Sys.getenv("CORTEZA_WORKER_OPTION_TEST"), "")
# utils/stats helpers are on the worker's search path, not just base.
res_pkgs <- corteza:::call_skill(
    "run_r",
    list(code = 'c(exists("str", mode = "function"), exists("tail", mode = "function"), exists("setNames", mode = "function"))'),
    ctx = list(session = s, cwd = s$cwd))
expect_false(isTRUE(res_pkgs$isError))
expect_false(grepl("FALSE", res_pkgs$content[[1L]]$text, fixed = TRUE))
corteza:::.run_r_worker_close(s)
expect_null(s$.run_r_worker)
