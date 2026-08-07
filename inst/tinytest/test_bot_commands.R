library(tinytest)

expect_equal(corteza:::bot_command_text("@tiny:cornball.ai clear"), "clear")
expect_equal(corteza:::bot_command_text("@tiny clear"), "clear")

expect_true(corteza:::bot_is_clear_command("//clear"))
expect_true(corteza:::bot_is_clear_command("clear"))
expect_true(corteza:::bot_is_clear_command("new chat"))
expect_true(corteza:::bot_is_clear_command("@cornelius reset"))
expect_false(corteza:::bot_is_clear_command("please clear the list"))

expect_true(corteza:::bot_is_status_command("status"))
expect_true(corteza:::bot_is_status_command("@tiny status"))
expect_false(corteza:::bot_is_status_command("status report"))

cmd1 <- corteza:::bot_parse_model_command("model")
expect_true(cmd1$query_only)

cmd2 <- corteza:::bot_parse_model_command("@tiny model gpt-5.5 openai_codex")
expect_equal(cmd2$model, "gpt-5.5")
expect_equal(cmd2$provider, "openai_codex")
expect_false(cmd2$query_only)

cmd3 <- corteza:::bot_parse_model_command("//model kimi-k2.5 moonshot")
expect_equal(cmd3$model, "kimi-k2.5")
expect_equal(cmd3$provider, "moonshot")
