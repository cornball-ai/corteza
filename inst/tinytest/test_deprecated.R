library(tinytest)

# The matrix_* exports are wrappers now. corteza reaches every transport
# through chat.api and makes no Matrix call of its own, so the prefix
# named a coupling that no longer exists -- but cerebro drives these by
# name across two dozen call sites, and a CRAN release carries them.

deprecated <- c("matrix_configure", "matrix_send", "matrix_poll",
                "matrix_run", "matrix_run_init", "matrix_run_step",
                "matrix_archive_all", "matrix_request_flush")

# Still exported. Removing one is a breaking change for anything holding
# a corteza 0.7.1 API in its head.
ns <- asNamespace("corteza")
exports <- getNamespaceExports("corteza")
for (old in deprecated) {
    expect_true(old %in% exports, info = old)
    new <- sub("^matrix_", "bot_", old)
    expect_true(new %in% exports, info = new)
}

# Every wrapper's signature matches the function it forwards to,
# argument for argument and default for default. A wrapper that quietly
# dropped an argument would look like the caller's option was ignored,
# which is the failure mode a rename is supposed to have none of.
for (old in deprecated) {
    new <- sub("^matrix_", "bot_", old)
    expect_identical(formals(get(old, envir = ns)),
                     formals(get(new, envir = ns)), info = old)
}

# And each one warns rather than moving silently, so a caller finds out
# before 1.0.0 removes it.
local({
    for (old in deprecated) {
        body_src <- paste(deparse(body(get(old, envir = ns))), collapse = " ")
        expect_true(grepl(".Deprecated", body_src, fixed = TRUE), info = old)
        # Naming the replacement, not just "deprecated". A warning that
        # does not say what to call instead costs the reader a grep.
        expect_true(grepl(sub("^matrix_", "bot_", old), body_src, fixed = TRUE),
                    info = old)
    }
})

# The forwarding actually happens. Everything else here is structural;
# this is the one check that the wrapper runs the new function.
local({
    called <- NULL
    orig <- corteza:::bot_request_flush
    assignInNamespace("bot_request_flush", function() {
        called <<- TRUE
        invisible("flushed")
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_request_flush", orig, ns = "corteza"),
            add = TRUE)
    expect_warning(got <- corteza::matrix_request_flush(), "deprecated")
    expect_true(called)
    expect_identical(got, "flushed")
})

# Arguments reach the new function unchanged, including the ones a
# careless wrapper would drop by forwarding only the first few.
local({
    seen <- NULL
    orig <- corteza:::bot_send
    assignInNamespace("bot_send", function(text, room_id = NULL,
                                           msgtype = "m.text",
                                           markdown = FALSE) {
        seen <<- list(text = text, room_id = room_id, msgtype = msgtype,
                      markdown = markdown)
        "$id"
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_send", orig, ns = "corteza"), add = TRUE)
    suppressWarnings(corteza::matrix_send("hi", room_id = "!r:ex",
                                          msgtype = "m.notice",
                                          markdown = TRUE))
    expect_identical(seen$text, "hi")
    expect_identical(seen$room_id, "!r:ex")
    expect_identical(seen$msgtype, "m.notice")
    expect_true(seen$markdown)
})

# No internal matrix_* survives. The wrappers are the whole of the old
# vocabulary, and anything else answering to that prefix would be a
# rename that did not finish.
local({
    internal <- setdiff(ls(ns, all.names = TRUE), exports)
    expect_identical(grep("^matrix_", internal, value = TRUE), character())
})
