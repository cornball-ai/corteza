library(tinytest)

# #142 / #139: self-bounding tools (bash/cmd/run_r/run_r_script) must not
# be wrapped in an R-level setTimeLimit -- they own their own timeout via
# processx/callr, or are in-process evals setTimeLimit cannot safely
# abort. Wrapping them caused the transient interrupt to leak onto the
# next call (#142) or corrupt processx's poll loop (#139).

# --- the exemption list ---
expect_true(all(c("bash", "cmd", "run_r", "run_r_script") %in%
                corteza:::.self_bounded_tools))
expect_false("read_file" %in% corteza:::.self_bounded_tools)

# --- behavioral: skill_run skips the R limit for an exempt tool, keeps
# it for everyone else. A real timing test, so local-only. ---
if (at_home()) {
    # CPU-bound loop: reliably interruptible by setTimeLimit (Sys.sleep
    # is not). Base-only so it runs under littler too.
    busy_until <- function(secs) {
        t0 <- Sys.time()
        repeat {
            for (i in 1:5000) tmp <- i * i
            if (as.numeric(Sys.time() - t0, units = "secs") >= secs) break
        }
    }
    mk_skill <- function(name) {
        list(name = name,
             inputSchema = list(type = "object", properties = list(),
                                required = list()),
             handler = function(args, ctx) {
                 busy_until(0.6)
                 corteza:::ok("finished")
             })
    }

    # Exempt tool: no R timeout, so it finishes past the 0.3s limit.
    r_exempt <- corteza:::skill_run(mk_skill("bash"), list(), timeout = 0.3)
    expect_false(isTRUE(r_exempt$isError))
    expect_equal(r_exempt$content[[1]]$text, "finished")

    # Non-exempt tool: the R limit still fires.
    r_limited <- corteza:::skill_run(mk_skill("read_file"), list(),
                                     timeout = 0.3)
    expect_true(isTRUE(r_limited$isError))
    expect_true(grepl("timed out", r_limited$content[[1]]$text,
                      ignore.case = TRUE))
}
