# Tests for R/dispatch.R (worker_init). worker_init runs inside a
# subagent's callr session to set cwd and register skills.

# worker_init smoke test: returns TRUE invisibly and registers skills.
res <- corteza:::worker_init(tempdir())
expect_true(res)
expect_true(!is.null(corteza:::get_skill("run_r")))
