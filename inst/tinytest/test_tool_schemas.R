# Drift tests between tool function signatures and their derived
# schemas. Runs schema_from_fn() against every registered tool and
# verifies the roxygen @param docs match the function's formals.
#
# Catches:
#   - @param entries that don't correspond to any formal
#     (stale doc left behind after a signature change).
#   - Formals with no @param entry (undocumented param -> empty
#     description in the derived schema).
#   - Descriptions that exceed the cap or are empty (missing @title
#     / @description).
#   - Type hints that don't map to a JSON-Schema-recognized type.

corteza::ensure_skills()

# --- helpers ------------------------------------------------------------

.tool_fn_name <- function(tool_name) {
    # Convention: registered tool "foo" corresponds to R function
    # "tool_foo". Bash/cmd / run_r / read_file etc. all follow this.
    paste0("tool_", tool_name)
}

.valid_json_types <- c("string", "integer", "number", "boolean",
                       "array", "object", "null")

# --- tests --------------------------------------------------------------

skill_names <- corteza:::list_skills()
expect_true(length(skill_names) > 0L)

for (tool_name in skill_names) {
    fn_name <- .tool_fn_name(tool_name)
    # Only validate tools whose R function follows the convention and
    # lives in corteza's namespace; skill-based tools loaded from user
    # .R files are out of scope for this test.
    if (!exists(fn_name, envir = asNamespace("corteza"), inherits = FALSE)) {
        next
    }
    fn <- get(fn_name, envir = asNamespace("corteza"))
    schema <- tryCatch(
        corteza::schema_from_fn(fn_name),
        error = function(e) {
            structure(list(.error = conditionMessage(e)), class = "schema_error")
        }
    )
    expect_false(inherits(schema, "schema_error"),
                 info = sprintf("%s: schema_from_fn errored: %s",
                                fn_name,
                                if (inherits(schema, "schema_error")) schema$.error else ""))

    # Description: non-empty, within the cap.
    expect_true(is.character(schema$description) &&
                length(schema$description) == 1L,
                info = sprintf("%s: description must be a single string",
                               fn_name))
    expect_true(nchar(schema$description) > 0L,
                info = sprintf("%s: empty description (missing @title / @description)",
                               fn_name))
    expect_true(nchar(schema$description) <= 200L,
                info = sprintf("%s: description exceeds 200 chars (%d)",
                               fn_name, nchar(schema$description)))

    # Formals vs properties: every formal (except `...`) should be in
    # properties; every property must correspond to a formal.
    fml_names <- setdiff(names(formals(fn)), "...")
    prop_names <- names(schema$input_schema$properties)
    missing_from_schema <- setdiff(fml_names, prop_names)
    extra_in_schema <- setdiff(prop_names, fml_names)
    expect_true(length(missing_from_schema) == 0L,
                info = sprintf("%s: formals without @param entries: %s",
                               fn_name,
                               paste(missing_from_schema, collapse = ", ")))
    expect_true(length(extra_in_schema) == 0L,
                info = sprintf("%s: @param entries without formals: %s",
                               fn_name,
                               paste(extra_in_schema, collapse = ", ")))

    # Each property must have a JSON-Schema-valid type and a non-empty
    # description. Array types must carry `items`.
    for (pname in prop_names) {
        prop <- schema$input_schema$properties[[pname]]
        expect_true(!is.null(prop$type),
                    info = sprintf("%s.%s: missing type", fn_name, pname))
        expect_true(prop$type %in% .valid_json_types,
                    info = sprintf("%s.%s: unknown JSON type '%s'",
                                   fn_name, pname, prop$type %||% "NULL"))
        if (identical(prop$type, "array")) {
            expect_true(!is.null(prop$items),
                        info = sprintf("%s.%s: array without items", fn_name, pname))
        }
        expect_true(is.character(prop$description) && nchar(prop$description) > 0L,
                    info = sprintf("%s.%s: empty description", fn_name, pname))
    }

    # `required` must be a subset of the property names.
    required <- unlist(schema$input_schema$required)
    expect_true(all(required %in% prop_names),
                info = sprintf("%s: required contains unknown props: %s",
                               fn_name,
                               paste(setdiff(required, prop_names), collapse = ", ")))
}
