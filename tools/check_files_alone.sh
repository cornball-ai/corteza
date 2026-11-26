#!/usr/bin/env bash
# Run each test file by itself and list each one that writes under the
# user's folders, stops, or fails.
#
# WHY THIS EXISTS
#
# R CMD check runs the tests through tests/tinytest.R, which points
# tools::R_user_dir() at temporary folders and attaches the package. A
# file run by itself, through tinytest::run_test_file(), gets neither:
# one that writes under the user's folders writes beside a live
# session's records, and one that calls an export without corteza::
# stops. tools/check_clean_home.sh cannot see either.
#
# Each file runs alone with a fresh empty home directory and a bare
# environment, so tools::R_user_dir() falls under that home, and no API
# key reaches a file: a block that needs one does not run. A file that
# leaves anything in the home needs, at its top:
#   source("isolate.R", local = TRUE)
#
# Usage, with the build under test installed where R finds it:
#   tools/check_files_alone.sh [package-dir] [at_home]
# at_home is TRUE or FALSE, as tinytest::run_test_file() takes it.
# Default TRUE, as a person running a file gets.
#
# Environment:
#   CLEAN_HOME_PARENT  where the throwaway homes are made, as for
#                      tools/check_clean_home.sh. Default:
#                      $XDG_RUNTIME_DIR. It must lie outside /tmp and
#                      /var/tmp: the supervisor's rules treat those as
#                      scratch space, and the tests expect "~" to lie
#                      outside them.
#   NOT_CRAN           passed through when set.
#
# Exit status 0 when no file wrote, stopped or failed.

set -uo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
pkg_dir=${1:-$(cd "$here/.." && pwd)}
pkg_dir=$(cd "$pkg_dir" && pwd)
at_home=${2:-TRUE}
case "$at_home" in
  TRUE|FALSE) ;;
  *) echo "::error::at_home must be TRUE or FALSE, not '$at_home'"; exit 1 ;;
esac

parent=${CLEAN_HOME_PARENT:-${XDG_RUNTIME_DIR:-}}
if [ -z "$parent" ] || [ ! -d "$parent" ]; then
  echo "::error::set CLEAN_HOME_PARENT to an existing directory outside /tmp"
  exit 1
fi
parent=$(cd "$parent" && pwd -P)
case "$parent/" in
  /tmp/*|/var/tmp/*|/private/tmp/*|/private/var/tmp/*)
    echo "::error::CLEAN_HOME_PARENT is under the temp root: $parent"
    exit 1
    ;;
esac

# This R's library paths, read before HOME moves: the user library is
# found through "~".
libs=$(Rscript -e 'cat(.libPaths(), sep = ":")') || exit 1

work=$(mktemp -d "$parent/corteza-files-alone.XXXXXX") || exit 1
trap 'rm -rf "$work"' EXIT
mkdir "$work/tmp"

files=0
bad=0
for f in "$pkg_dir"/inst/tinytest/test_*.R; do
  name=$(basename "$f")
  files=$((files + 1))
  home="$work/home"
  rm -rf "$home"
  mkdir "$home"
  log="$work/log"
  env -i HOME="$home" PATH="$PATH" LANG="${LANG:-C.UTF-8}" R_LIBS="$libs" \
    TMPDIR="$work/tmp" \
    ${NOT_CRAN:+NOT_CRAN="$NOT_CRAN"} RUN_FILE="$f" AT_HOME="$at_home" \
    Rscript --vanilla -e '
      r <- tinytest::run_test_file(Sys.getenv("RUN_FILE"), verbose = 0,
                                   at_home = as.logical(Sys.getenv("AT_HOME")))
      cat(sprintf("\nRESULTS %d FAILED %d\n", length(r),
                  sum(!as.logical(r))))' > "$log" 2>&1
  status=$?
  line=$(grep -a "^RESULTS " "$log" | tail -1)
  # A test can delete the records it wrote and still leave their directories.
  wrote=$(cd "$home" && find . -mindepth 1 | sed 's|^\./||')
  what=""
  if [ "$status" -ne 0 ] || [ -z "$line" ]; then
    what="stopped"
  elif [ "$(echo "$line" | awk '{print $4}')" -gt 0 ]; then
    what="failed $(echo "$line" | awk '{print $4}') of $(echo "$line" | awk '{print $2}')"
  fi
  if [ -n "$wrote" ]; then
    n=$(echo "$wrote" | wc -l)
    what="${what:+$what; }left $n entries under the home: $(echo "$wrote" | head -3 | tr '\n' ' ')"
  fi
  if [ -n "$what" ]; then
    bad=$((bad + 1))
    echo "::error::$name: $what"
    grep -a -v "^RESULTS \|^$" "$log" | tail -5 | sed 's/^/    /'
  fi
done

echo "$files files run alone (at_home = $at_home): $bad wrote, stopped or failed"
[ "$bad" -eq 0 ]
