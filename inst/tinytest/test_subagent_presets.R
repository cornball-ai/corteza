# Pure-function tests for the subagent preset resolver. Cheap to run
# (no callr child), so no at_home() gate.

resolve <- corteza:::resolve_subagent_tools
defaults <- corteza:::SUBAGENT_DEFAULTS$default_tools
presets <- corteza:::SUBAGENT_PRESETS

# preset = NULL, tools = NULL: falls back to default_tools arg.
expect_equal(resolve(), defaults)

# Explicit default_tools wins over the hard-coded fallback. This is the
# regression that motivated the change: subagent_spawn() must pass
# config$subagents$default_tools through.
custom <- c("read_file", "fetch_url")
expect_equal(resolve(default_tools = custom), custom)

# Each named preset returns its tool vector.
expect_equal(resolve(preset = "investigate"), presets$investigate)
expect_equal(resolve(preset = "work"), presets$work)
expect_equal(resolve(preset = "minimal"), presets$minimal)

# investigate is read-only — no bash, no write/edit.
expect_false("bash" %in% presets$investigate)
expect_false("write_file" %in% presets$investigate)
expect_false("replace_in_file" %in% presets$investigate)

# Explicit tools vector overrides preset.
expect_equal(resolve(preset = "minimal", tools = c("read_file")),
             c("read_file"))

# Unknown preset errors.
err <- tryCatch(resolve(preset = "nope"), error = function(e) e)
expect_inherits(err, "error")
expect_true(grepl("Unknown subagent preset", conditionMessage(err)))
