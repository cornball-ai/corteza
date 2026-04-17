# Test text chunking

# Test chunk_text with short text
short <- "Hello world"
chunks <- corteza:::chunk_text(short, 100)
expect_equal(length(chunks), 1)
expect_equal(chunks[1], short)

# Test chunk_text with text at limit
at_limit <- paste(rep("a", 100), collapse = "")
chunks <- corteza:::chunk_text(at_limit, 100)
expect_equal(length(chunks), 1)

# Test chunk_text with text over limit
over_limit <- paste(rep("word ", 50), collapse = "")  # 250 chars
chunks <- corteza:::chunk_text(over_limit, 100)
expect_true(length(chunks) > 1)
expect_true(all(nchar(chunks) <= 100))

# Test chunk_text prefers newline breaks
with_newlines <- "First line\nSecond line\nThird line\nFourth line"
chunks <- corteza:::chunk_text(with_newlines, 25)
expect_true(length(chunks) >= 2)

# Test chunk_text prefers whitespace breaks
with_spaces <- "word1 word2 word3 word4 word5 word6"
chunks <- corteza:::chunk_text(with_spaces, 15)
expect_true(length(chunks) >= 2)
# No chunk should have a space at start or end
for (chunk in chunks) {
    expect_false(startsWith(chunk, " "))
    expect_false(endsWith(chunk, " "))
}

# Test chunk_by_paragraph with paragraphs
paragraphs <- "First paragraph.\n\nSecond paragraph.\n\nThird paragraph."
chunks <- corteza:::chunk_by_paragraph(paragraphs, 50)
expect_true(length(chunks) >= 1)

# Test chunk_by_paragraph packs small paragraphs
small_paras <- "One.\n\nTwo.\n\nThree."
chunks <- corteza:::chunk_by_paragraph(small_paras, 100)
expect_equal(length(chunks), 1)  # Should fit in one chunk

# Test chunk_by_paragraph splits large paragraphs
large_para <- paste(rep("word ", 100), collapse = "")  # ~500 chars
chunks <- corteza:::chunk_by_paragraph(large_para, 100)
expect_true(length(chunks) > 1)

# Test chunk_text_with_mode dispatches correctly
text <- "Para one.\n\nPara two."
length_chunks <- corteza:::chunk_text_with_mode(text, 100, "length")
newline_chunks <- corteza:::chunk_text_with_mode(text, 100, "newline")
expect_equal(length(length_chunks), 1)  # Fits in one
expect_equal(length(newline_chunks), 1)  # Also fits

# Test empty/null input
expect_equal(length(corteza:::chunk_text(NULL, 100)), 0)
expect_equal(length(corteza:::chunk_text("", 100)), 0)
expect_equal(length(corteza:::chunk_by_paragraph(NULL, 100)), 0)
expect_equal(length(corteza:::chunk_by_paragraph("", 100)), 0)

# Test find_break_point
expect_equal(corteza:::find_break_point("hello\nworld"), 6)  # Position of \n
expect_equal(corteza:::find_break_point("hello world"), 6)   # Position of space
expect_equal(corteza:::find_break_point("helloworld"), -1)   # No break point

