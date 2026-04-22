#!/usr/bin/env r
#
# Measure the cost of spinning up a callr-backed CLI worker from a
# cold process.
#
# Reports the wall-clock time for:
#   - callr::r_session$new(wait = TRUE)
#   - session$run(library(corteza); worker_init())
#
# Repeated N times, median + range reported.

suppressMessages(library(corteza))

N <- 5L
results <- numeric(N)

for (i in seq_len(N)) {
    t0 <- Sys.time()
    worker <- corteza::cli_worker_spawn()
    t1 <- Sys.time()
    worker$session$close()
    results[i] <- as.numeric(t1 - t0, units = "secs")
}

cat(sprintf("warm-start (n=%d):\n", N))
cat(sprintf("  median:  %.3fs\n", median(results)))
cat(sprintf("  min/max: %.3fs / %.3fs\n", min(results), max(results)))
cat(sprintf("  each:    %s\n",
            paste(sprintf("%.3f", results), collapse = ", ")))
