#!/usr/bin/env bash
# Check the package with an empty home directory and fail if the run
# leaves anything in it.
#
# WHY THIS EXISTS
#
# 0.8.0 was archived because a test made ~/bot on CRAN's check machine.
# A developer's own check cannot see that: the directory is already
# there from the last run, and R CMD check reports only what is new.
# This run starts from a home directory with nothing in it.
#
# The environment is bare as well. A developer's API keys, XDG paths and
# CORTEZA_* overrides do not reach the run, so it neither spends money
# nor writes to live state.
#
# Usage:
#   tools/check_clean_home.sh [package-dir]     # default: repo root
#
# Environment:
#   CLEAN_HOME_PARENT  where the throwaway home is made. Default:
#                      $XDG_RUNTIME_DIR. It must lie outside /tmp and
#                      /var/tmp. The supervisor's rules treat those as
#                      scratch space, and the tests expect "~" to lie
#                      outside them.
#   CLEAN_HOME_WORK    an existing directory to build and check in. It is
#                      kept afterwards, for reading the logs. Default: a
#                      temporary directory, removed on exit.
#   NOT_CRAN           passed through when set, so the run includes the
#                      test files CRAN skips. Unset, the run is the one
#                      CRAN makes. The two differ: on 0.8.0 the first
#                      made 4 directories in the home and the second 1.
#
# Exit status 0 when the check ran without an ERROR, R CMD check found
# nothing left behind, and the home directory is still empty.

set -uo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
pkg_dir=${1:-$(cd "$here/.." && pwd)}
pkg_dir=$(cd "$pkg_dir" && pwd)

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

home=$(mktemp -d "$parent/corteza-clean-home.XXXXXX") || exit 1
if [ -n "${CLEAN_HOME_WORK:-}" ]; then
  work=$(cd "$CLEAN_HOME_WORK" && pwd) || exit 1
  trap 'rm -rf "$home"' EXIT
else
  work=$(mktemp -d) || exit 1
  trap 'rm -rf "$home" "$work"' EXIT
fi

# The run gets a temp root of its own. R CMD check compares the top level
# of the temp root before and after, for every file the user owns. On a
# machine where the same user runs other R processes, theirs show up in
# the shared one: littler's tempdir() is the temp root itself, so a
# service started with `r` leaves its callr files at the top of /tmp.
# The cost is that a write to a literal "/tmp/..." path goes unseen
# here. CI checks against the shared temp root on a machine with nothing
# else running.
mkdir "$work/tmp" 2>/dev/null || true

# --no-manual, here and below: the PDF manual runs TeX, which keeps its
# own caches in the home directory.
run() {
  env -i HOME="$home" PATH="$PATH" LANG="${LANG:-C.UTF-8}" R_LIBS="$libs" \
    TMPDIR="$work/tmp" \
    ${NOT_CRAN:+NOT_CRAN="$NOT_CRAN"} \
    _R_CHECK_THINGS_IN_OTHER_DIRS_=true \
    _R_CHECK_THINGS_IN_TEMP_DIR_=true \
    _R_CHECK_THINGS_IN_CHECK_DIR_=true \
    _R_CHECK_FORCE_SUGGESTS_=false \
    _R_CHECK_CRAN_INCOMING_=false \
    "$@"
}

echo "building from: $pkg_dir"
if ! (cd "$work" && run R CMD build --no-manual "$pkg_dir" >build.log 2>&1); then
  echo "::error::R CMD build failed"
  tail -20 "$work/build.log"
  exit 1
fi
tarball=$(ls -1t "$work"/*.tar.gz 2>/dev/null | head -1)
if [ -z "$tarball" ]; then
  echo "::error::no tarball produced"
  exit 1
fi

if [ -n "${NOT_CRAN:-}" ]; then
  tests="every test file (NOT_CRAN=$NOT_CRAN)"
else
  tests="the tests CRAN runs"
fi
echo "checking: $(basename "$tarball"), $tests, home: $home"
(cd "$work" && run R CMD check --as-cran --run-donttest --no-manual \
  "$tarball" >check.log 2>&1)

name=$(basename "$tarball")
log="$work/${name%%_*}.Rcheck/00check.log"
if [ ! -f "$log" ]; then
  echo "::error::R CMD check wrote no log"
  tail -20 "$work/check.log"
  exit 1
fi

fail=0
grep '^Status:' "$log" || { echo "::error::the check did not finish"; fail=1; }
# A run that stopped on an error proves nothing about what a full run
# would have written.
if grep -q '^Status:.*ERROR' "$log"; then
  grep -B2 -A12 ' ERROR$' "$log" | head -60
  fail=1
fi

"$here/check_left_behind.sh" "$log" || fail=1

left=$(find "$home" -mindepth 1 | sed "s|^$home|~|" | sort)
if [ -n "$left" ]; then
  echo "::error::the run left these in an empty home directory:"
  printf '%s\n' "$left" | head -40
  fail=1
else
  echo "home directory: still empty"
fi
exit "$fail"
