if (requireNamespace("tinytest", quietly = TRUE)) {
  Sys.setenv(R_USER_CACHE_DIR = tempfile("corteza_cache_"))
  tinytest::test_package("corteza")
}
