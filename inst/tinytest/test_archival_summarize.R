# archival_summarize calls llm.api::agent. Gated at_home() AND requires
# ANTHROPIC_API_KEY because the call hits the network.

if (!tinytest::at_home()) exit_file("summarize test hits the network; at_home only")
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    exit_file("summarize test needs ANTHROPIC_API_KEY")
}

slice <- list(
    list(role = "user", content = "What's the capital of France?"),
    list(role = "assistant", content = "Paris.")
)

# Structured mode: expect either a JSON object or [unparsed] fallback.
out_struct <- corteza:::archival_summarize(
    slice, style = "structured", provider = "anthropic"
)
expect_true(is.character(out_struct) && length(out_struct) == 1L)
expect_true(nzchar(out_struct))
# It should mention Paris or France somewhere; if not, the model is
# being weird, but the test mostly proves the call wires up.
expect_true(grepl("Paris|France|capital", out_struct, ignore.case = TRUE))

# Paragraph mode: any non-empty string.
out_para <- corteza:::archival_summarize(
    slice, style = "paragraph", provider = "anthropic"
)
expect_true(is.character(out_para) && length(out_para) == 1L)
expect_true(nzchar(out_para))
