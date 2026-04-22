# Tests for R/schema.R — CLI-side schema generation from the registry.

# Make sure skills are registered before we start.
corteza::ensure_skills()

# schema_from_registry returns a list shaped for the Anthropic chat API.
schemas <- corteza::schema_from_registry()
expect_true(is.list(schemas))
expect_true(length(schemas) > 0)

# Each entry has the three fields the API expects.
for (s in schemas) {
    expect_true(all(c("name", "description", "input_schema") %in% names(s)))
    expect_true(is.character(s$name) && length(s$name) == 1L)
    expect_true(is.character(s$description) && length(s$description) == 1L)
    expect_true(is.list(s$input_schema))
    # input_schema is JSON-Schema-shaped.
    expect_equal(s$input_schema$type, "object")
    expect_true("properties" %in% names(s$input_schema))
}

# bash (or cmd on Windows without a real bash) is always there.
names_out <- vapply(schemas, function(s) s$name, character(1L))
expect_true(any(c("bash", "cmd") %in% names_out))

# run_r is always there.
expect_true("run_r" %in% names_out)

# Filter produces a subset.
core_schemas <- corteza::schema_from_registry(filter = "core")
expect_true(length(core_schemas) > 0)
expect_true(length(core_schemas) < length(schemas))
