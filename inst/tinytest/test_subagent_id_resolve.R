# Tests for resolve_subagent_id: numeric seq, exact id, prefix,
# ambiguity, unknown. All offline — manipulates the registry directly.

resolve <- corteza:::resolve_subagent_id
reg <- corteza:::.subagent_registry

# Clean up first.
for (id in ls(reg)) {
    rm(list = id, envir = reg)
}

# Empty registry: anything resolves to NULL.
expect_null(resolve("1"))
expect_null(resolve("abcd"))
expect_null(resolve(""))
expect_null(resolve(character(0)))

# Stub two registry entries.
id_a <- "abcd1234-aaaa-0000-0000-000000000001"
id_b <- "abef5678-bbbb-0000-0000-000000000002"
assign(id_a, list(id = id_a, seq = 1L, task = "first",
                  timeout = Sys.time() + 60), envir = reg)
assign(id_b, list(id = id_b, seq = 2L, task = "second",
                  timeout = Sys.time() + 60), envir = reg)

# Cleanup at end (no on.exit; tinytest runs at top level).

# Sequence number lookup.
expect_equal(resolve("1"), id_a)
expect_equal(resolve(1L), id_a)
expect_equal(resolve("2"), id_b)

# Exact UUID.
expect_equal(resolve(id_a), id_a)
expect_equal(resolve(id_b), id_b)

# Unique prefix.
expect_equal(resolve("abcd"), id_a)
expect_equal(resolve("abef"), id_b)
expect_equal(resolve("abcd1234"), id_a)

# Ambiguous prefix raises.
err <- tryCatch(resolve("ab"), error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("Ambiguous", conditionMessage(err)))

# Unknown id returns NULL.
expect_null(resolve("zzzz"))
expect_null(resolve("999"))

# Cleanup.
rm(list = c(id_a, id_b), envir = reg)
