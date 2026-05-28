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
  tinytest::test_package("corteza")
}
