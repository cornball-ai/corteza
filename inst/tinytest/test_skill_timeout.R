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

# --- Option B: fetch_url / web_search self-bound via curl's own connect/
# total timeout, so they are exempt from the R-level setTimeLimit too. ---
expect_true(all(c("fetch_url", "web_search") %in%
                corteza:::.self_bounded_tools))

# --- #142 item 2: the pending-interrupt flush must run before the validation
# and dry-run early returns, and only an armed path leaves the flag set.
# These exercise skill_run's side effects + timing, so local-only like above.
if (at_home()) {
    busy_until <- function(secs) {
        t0 <- Sys.time()
        repeat {
            for (i in 1:5000) tmp <- i * i
            if (as.numeric(Sys.time() - t0, units = "secs") >= secs) break
        }
    }
    mk <- function(name, handler, required = list(), props = list()) {
        list(name = name, description = "test",
             inputSchema = list(type = "object", properties = props,
                                required = required),
             handler = handler)
    }
    ok_h <- function(args, ctx) corteza:::ok("ran")
    needs_arg <- mk("needs_arg", ok_h, required = list("x"),
                    props = list(x = list(type = "string")))
    noop <- mk("noop_tool", ok_h)
    # read_file is non-exempt, so it executes under the R limit (arms).
    limited <- mk("read_file",
                  function(args, ctx) { busy_until(0.6); corteza:::ok("done") })

    # Same env object as the package's internal flag, so writes here are seen
    # by skill_run (can't assign through `:::`, hence the local binding).
    tlimit <- corteza:::.timelimit_armed

    # Validation failure returns before arming -> flag untouched, so an
    # enclosing caller limit would survive.
    tlimit$pending <- FALSE
    vr <- corteza:::skill_run(needs_arg, list(), timeout = 0.3)
    expect_true(isTRUE(vr$isError))
    expect_true(grepl("Missing required", vr$content[[1]]$text))
    expect_false(tlimit$pending)

    # Dry-run returns before arming -> same invariant.
    tlimit$pending <- FALSE
    dr <- corteza:::skill_run(noop, list(), timeout = 0.3, dry_run = TRUE)
    expect_false(isTRUE(dr$isError))
    expect_false(tlimit$pending)

    # A timed-out armed call leaves the flag set; the validation failure that
    # follows flushes the queued interrupt at entry (its early return is above
    # the old flush site) and clears the flag, so subsequent code is not
    # interrupted.
    tlimit$pending <- FALSE
    r1 <- corteza:::skill_run(limited, list(), timeout = 0.3)
    expect_true(isTRUE(r1$isError))
    expect_true(grepl("timed out", r1$content[[1]]$text, ignore.case = TRUE))
    expect_true(tlimit$pending)

    r2 <- corteza:::skill_run(needs_arg, list(), timeout = 0.3)
    expect_true(isTRUE(r2$isError))
    expect_false(tlimit$pending)

    leaked <- FALSE
    tryCatch(busy_until(0.6), error = function(e) leaked <<- TRUE)
    expect_false(leaked)
}
