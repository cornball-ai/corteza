library(tinytest)

expect_equal(corteza:::matrix_command_text("@tiny:cornball.ai clear"), "clear")
expect_equal(corteza:::matrix_command_text("@tiny clear"), "clear")

expect_true(corteza:::matrix_is_clear_command("//clear"))
expect_true(corteza:::matrix_is_clear_command("clear"))
expect_true(corteza:::matrix_is_clear_command("new chat"))
expect_true(corteza:::matrix_is_clear_command("@cornelius reset"))
expect_false(corteza:::matrix_is_clear_command("please clear the list"))

expect_true(corteza:::matrix_is_status_command("status"))
expect_true(corteza:::matrix_is_status_command("@tiny status"))
expect_false(corteza:::matrix_is_status_command("status report"))

cmd1 <- corteza:::matrix_parse_model_command("model")
expect_true(cmd1$query_only)

cmd2 <- corteza:::matrix_parse_model_command("@tiny model gpt-5.5 openai_codex")
expect_equal(cmd2$model, "gpt-5.5")
expect_equal(cmd2$provider, "openai_codex")
expect_false(cmd2$query_only)

cmd3 <- corteza:::matrix_parse_model_command("//model kimi-k2.5 moonshot")
expect_equal(cmd3$model, "kimi-k2.5")
expect_equal(cmd3$provider, "moonshot")
