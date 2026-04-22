#!/usr/bin/env r
#
# Per-tool-call round trip on a warm worker. Excludes warm-start
# cost. Measures pure dispatch latency for representative tools.
#
# Reports median of N runs for:
#   - bash 'echo hi'           (~10-byte out)
#   - read_file small           (self: 200 lines)
#   - run_r '2+2'               (scalar, no handle)
#   - run_r data.frame          (large, stashes handle)

suppressMessages(library(corteza))

N <- 20L
worker <- corteza::cli_worker_spawn()
# Top-level on.exit fires at script-source frame boundaries under
# some runners (closing the session before the first timed call), so
# we explicitly close at the end instead.

# Warm up (first call amortizes any one-shot loading).
worker$session$run(
    function(n, a) corteza::worker_dispatch(n, a),
    list(n = "bash", a = list(command = "echo warmup"))
)

time_call <- function(name, args, n = N) {
    times <- numeric(n)
    for (i in seq_len(n)) {
        t0 <- Sys.time()
        worker$session$run(
            function(n, a) corteza::worker_dispatch(n, a),
            list(n = name, a = args)
        )
        times[i] <- as.numeric(Sys.time() - t0, units = "secs") * 1000  # ms
    }
    times
}

cat(sprintf("per-call dispatch (n=%d, median ms):\n", N))

t_bash <- time_call("bash", list(command = "echo hi"))
cat(sprintf("  bash 'echo hi':         %.1f ms (min %.1f, max %.1f)\n",
            median(t_bash), min(t_bash), max(t_bash)))

self_path <- system.file("bin", "corteza", package = "corteza")
if (nzchar(self_path) && file.exists(self_path)) {
    t_read <- time_call("read_file",
                        list(path = self_path, from = 1L, lines = 50L))
    cat(sprintf("  read_file (50 lines):   %.1f ms (min %.1f, max %.1f)\n",
                median(t_read), min(t_read), max(t_read)))
}

t_scalar <- time_call("run_r", list(code = "2 + 2"))
cat(sprintf("  run_r '2+2':            %.1f ms (min %.1f, max %.1f)\n",
            median(t_scalar), min(t_scalar), max(t_scalar)))

t_large <- time_call("run_r",
                     list(code = "data.frame(a = 1:100, b = runif(100))"))
cat(sprintf("  run_r data.frame:       %.1f ms (min %.1f, max %.1f)\n",
            median(t_large), min(t_large), max(t_large)))

worker$session$close()
