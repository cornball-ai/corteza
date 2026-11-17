if (requireNamespace("tinytest", quietly = TRUE)) {
  # Redirect all three tools::R_user_dir roots into tempdir for the
  # check run. Cache was already covered (#96 / saber briefs); session
  # tests write transcripts under tools::R_user_dir("corteza", "data")
  # and the matrix config lands in tools::R_user_dir("corteza", "config"),
  # both of which would otherwise trip CRAN's "checking for new files
  # in some other directories" NOTE.
  Sys.setenv(R_USER_CACHE_DIR  = tempfile("corteza_cache_"),
             R_USER_DATA_DIR   = tempfile("corteza_data_"),
             R_USER_CONFIG_DIR = tempfile("corteza_config_"))
  # corteza's own overrides come next. Two of them replace a default
  # that the lines above have just sent to the temp directory, and the
  # third names a Matrix config to read. A developer who has one set,
  # say in a bot's service environment, would otherwise run these tests
  # against live state. Unset rather than pointed elsewhere, so the
  # tests take the default path; a test that needs one sets it itself.
  Sys.unsetenv(c("CORTEZA_STATE_DIR", "CORTEZA_REMOTE_DIR",
                 "CORTEZA_MATRIX_CONFIG"))
  tinytest::test_package("corteza")
}
