# Supervised run_r keeps a private persistent workspace and enforces a real
# wall-clock deadline without changing direct tool_run_r() behavior.

make_worker_session <- function(timeout = 1, maximum = 5) {
    s <- corteza::new_session("cli")
    s$cwd <- tempdir()
    s$config <- list(run_r_mode = "worker", skill_timeout = timeout,
                     skill_timeout_max = maximum)
    s
}

call_worker <- function(session, code, timeout = NULL, bindings = NULL,
                        cap = NULL) {
    args <- list(code = code)
    if (!is.null(timeout)) {
        args$timeout <- timeout
    }
    corteza:::call_skill(
        "run_r", args,
        ctx = list(session = session, cwd = session$cwd,
                   run_r_bindings = bindings, timeout_cap = cap)
    )
}

corteza::ensure_skills()

# Historical direct API: same signature and the caller-owned environment.
scoped <- new.env(parent = globalenv())
direct <- corteza::tool_run_r("x <- 41L; x + 1L", envir = scoped)
expect_false(isTRUE(direct$isError))
expect_equal(get("x", scoped), 41L)
expect_equal(direct$content[[1L]]$text, "[1] 42")

# Worker state survives calls but does not leak into the host globalenv.
s <- make_worker_session()
name <- paste0("worker_only_", Sys.getpid())
first <- call_worker(s, sprintf("%s <- 41L", name))
second <- call_worker(s, sprintf("%s + 1L", name))
expect_false(isTRUE(first$isError))
expect_false(isTRUE(second$isError))
expect_equal(second$content[[1L]]$text, "[1] 42")
expect_false(exists(name, envir = globalenv(), inherits = FALSE))
expect_equal(second$execution$status, "ok")
expect_true(second$execution$state_retained)

# Host bindings are synchronized before evaluation and remain worker-local.
bound <- call_worker(s, "sum(fr)", bindings = list(fr = 1:4))
expect_equal(bound$content[[1L]]$text, "[1] 10")
expect_false(exists("fr", envir = globalenv(), inherits = FALSE))

# Handles are owned and read by the same worker.
large <- call_worker(s, "matrix(1:100, 10, 10)")
expect_true(grepl("stored as .h_001", large$content[[1L]]$text, fixed = TRUE))
read <- corteza:::call_skill(
    "read_handle", list(handle = ".h_001", op = "str"),
    ctx = list(session = s, cwd = s$cwd)
)
expect_false(isTRUE(read$isError))
expect_true(grepl("int [1:10, 1:10]", read$content[[1L]]$text, fixed = TRUE))

# Existing subagent return_name can retrieve worker objects or handles, and
# never falls through to a same-named object in the host process.
worker_artifact <- corteza:::.resolve_return_value(name, s)
expect_true(worker_artifact$found)
expect_equal(worker_artifact$value, 41L)
worker_handle <- corteza:::.resolve_return_value(".h_001", s)
expect_true(worker_handle$found)
expect_equal(dim(worker_handle$value), c(10L, 10L))
expect_false(corteza:::.resolve_return_value("unknown_worker_artifact", s)$found)

# A host can atomically checkpoint the worker without copying values through
# the callr control channel. Dynamic host bindings and handle aliases can be
# excluded, while the checkpoint envelope preserves model-authored helpers.
call_worker(
    s,
    paste("helper_value <- 99L; helper_fun <- function() fr$state;",
          "helpers <- list(state = function() fr$state);",
          "child <- new.env(); child_alias <- child; child$read <- function() fr$state"),
    bindings = list(fr = list(state = "OLD")))
checkpoint <- tempfile(fileext = ".RData")
saved <- corteza:::.run_r_worker_save(s, checkpoint, exclude = "fr")
expect_true(file.exists(checkpoint))
expect_true("helper_value" %in% saved)
expect_false("fr" %in% saved)
expect_false(any(grepl("^\\.h_[0-9]+$", saved)))
restored <- new.env(parent = emptyenv())
loaded <- load(checkpoint, envir = restored)
payload <- restored[[corteza:::.run_r_worker_checkpoint_key]]
expect_true("helper_value" %in% payload$names)
expect_true("helper_fun" %in% payload$names)
decoded <- unserialize(payload$data, refhook = function(ref) emptyenv())
expect_equal(decoded$helper_value, 99L)
expect_identical(environment(decoded$helper_fun), emptyenv())

# Seeding a new worker rehomes helpers, including nested helpers, against the
# new authoritative binding and preserves shared environment references.
s2 <- make_worker_session()
rehomed <- call_worker(
    s2, "helper_fun()",
    bindings = c(as.list(restored, all.names = TRUE),
                 list(fr = list(state = "CURRENT"))))
expect_equal(rehomed$content[[1L]]$text, '[1] "CURRENT"')
expect_equal(call_worker(s2, "helpers$state()")$content[[1L]]$text, '[1] "CURRENT"')
expect_equal(call_worker(s2, "child$read()")$content[[1L]]$text, '[1] "CURRENT"')
expect_equal(call_worker(s2, "identical(child, child_alias)")$content[[1L]]$text,
             "[1] TRUE")
corteza:::.run_r_worker_close(s2)
unlink(checkpoint)

empty_session <- corteza::new_session("cli")
expect_null(corteza:::.run_r_worker_save(empty_session, tempfile()))

# A model request cannot raise the host maximum and invalid values are refused.
too_long <- call_worker(s, "1", timeout = 6)
expect_true(too_long$isError)
expect_true(grepl("host maximum of 5", too_long$content[[1L]]$text,
                  fixed = TRUE))
bad <- call_worker(s, "1", timeout = -1)
expect_true(bad$isError)
expect_true(grepl("positive finite", bad$content[[1L]]$text, fixed = TRUE))
expect_error(corteza:::.run_r_timeout_value(1e9, "timeout"),
             pattern = "supported wall-clock limit")

# A tighter external lease can only narrow the configured/requested timeout.
capped <- call_worker(s, "1", timeout = 4, cap = 0.5)
expect_false(isTRUE(capped$isError))
expect_equal(capped$execution$timeout_seconds, 0.5)

# A host's dynamic deadline works through the standard session dispatcher,
# without a custom tool executor. It may narrow, never raise, another cap.
remaining <- 0.4
s$run_r_timeout_cap <- function() remaining
expect_equal(call_worker(s, "1", timeout = 4)$execution$timeout_seconds, 0.4)
expect_equal(call_worker(s, "1", cap = 0.2)$execution$timeout_seconds, 0.2)
remaining <- 0
expired <- call_worker(s, "must_not_run <- TRUE")
expect_true(expired$isError)
expect_true(grepl("No run_r execution time remains", expired$content[[1]]$text))
s$run_r_timeout_cap <- NULL
expect_equal(call_worker(s, "exists('must_not_run', inherits = FALSE)")$content[[1]]$text,
             "[1] FALSE")

# Interrupt is caught inside the worker. Partial assignments are explicit and
# inspectable afterward; the process and generation remain the same.
generation <- s$.run_r_worker_generation
timed <- call_worker(s, "partial_value <- 7L; repeat {}", timeout = 0.2)
expect_true(timed$isError)
expect_equal(timed$execution$status, "timeout")
expect_true(timed$execution$state_retained)
expect_equal(s$.run_r_worker_generation, generation)
after <- call_worker(s, "partial_value")
expect_equal(after$content[[1L]]$text, "[1] 7")
expect_equal(s$.run_r_worker_generation, generation)

# In-process remains the default and an explicit timeout is not falsely
# represented as enforceable there.
plain <- corteza::new_session("console")
plain$config <- list(run_r_mode = "in_process", skill_timeout = 0.01,
                     skill_timeout_max = 1)
local_name <- paste0("plain_", Sys.getpid())
in_process <- corteza:::call_skill(
    "run_r", list(code = sprintf("%s <- 9L", local_name)),
    ctx = list(session = plain)
)
expect_false(isTRUE(in_process$isError))
expect_equal(get(local_name, envir = globalenv()), 9L)
rm(list = local_name, envir = globalenv())

# The new model-facing timeout is never represented as enforceable in the
# historical in-process mode. Direct tool_run_r(code, envir) is unchanged.
unsupported <- corteza:::call_skill(
    "run_r", list(code = "1", timeout = 0.1),
    ctx = list(session = plain)
)
expect_true(unsupported$isError)
expect_true(grepl("requires run_r_mode", unsupported$content[[1L]]$text))

corteza:::.run_r_worker_close(s)
expect_null(s$.run_r_worker)

# config$skill_timeout now reaches the shared wrapper for ordinary skills.
slow_skill <- corteza:::skill_spec(
    "slow_test_skill", "test", list(),
    function(args, ctx) {
        started <- Sys.time()
        repeat {
            for (i in 1:5000) value <- i * i
            if (as.numeric(difftime(Sys.time(), started,
                                    units = "secs")) >= 0.2) break
        }
        corteza:::ok("finished")
    }
)
corteza:::register_skill(slow_skill)
cfg_session <- corteza::new_session("cli")
cfg_session$config <- list(skill_timeout = 0.02)
limited <- corteza:::call_skill(
    "slow_test_skill", list(), ctx = list(session = cfg_session)
)
expect_true(limited$isError)
expect_true(grepl("timed out", limited$content[[1L]]$text, fixed = TRUE))
