# Sourced first by each test file that writes under the user's folders.
#
# R CMD check runs tests/tinytest.R, which points tools::R_user_dir() at
# temporary folders before any test. tinytest::run_test_file(),
# run_test_dir() and test_package() do not, and a file run by one of
# them would write its audit records, session stores and caches into
# the user's own data and cache folders, beside a live session's. This
# gives the file temporary ones, and moves corteza's 2 state overrides
# there too where one is set.
#
# tinytest restores each variable a file set or changed when the file
# ends. It does not restore one the file unset, so nothing is unset
# here. The config folder stays: a block run at home may read the
# user's own config, and no test writes there.
local({
    Sys.setenv(R_USER_DATA_DIR = tempfile("corteza_data_"),
               R_USER_CACHE_DIR = tempfile("corteza_cache_"))
    for (v in c("CORTEZA_STATE_DIR", "CORTEZA_REMOTE_DIR")) {
        if (!is.na(Sys.getenv(v, unset = NA))) {
            do.call(Sys.setenv, stats::setNames(list(tempfile(tolower(v))), v))
        }
    }
})
