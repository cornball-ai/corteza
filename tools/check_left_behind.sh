#!/usr/bin/env bash
# Fail unless R CMD check found nothing left behind.
#
# 0.8.0 was archived because a test made ~/bot on CRAN's check machine.
# R CMD check reports that kind of thing as a NOTE, and CI passes NOTEs.
# This reads the result of the checks that look for it and fails on
# anything but OK.
#
# A clean run prints each check's name followed by OK, so matching the
# name alone would fail every run. A check that is switched off prints
# nothing at all, and that fails here too: a gate that cannot see the
# check has not passed it.
#
# The first check needs _R_CHECK_THINGS_IN_OTHER_DIRS_=true in the
# environment of R CMD check. --as-cran switches on the other two.
#
# Usage:
#   tools/check_left_behind.sh path/to/00check.log

set -uo pipefail

log=${1:-}
if [ -z "$log" ] || [ ! -f "$log" ]; then
  echo "::error::no check log at '${log}'"
  exit 1
fi

fail=0
while IFS= read -r what; do
  lines=$(grep -F -- "* checking ${what} ..." "$log" || true)
  if [ -z "$lines" ]; then
    echo "::error::the log has no result for 'checking ${what}'"
    fail=1
  elif printf '%s\n' "$lines" | grep -qv ' OK$'; then
    echo "::error::checking ${what}: not OK"
    grep -F -A6 -- "* checking ${what} ..." "$log"
    fail=1
  else
    printf '%s\n' "$lines"
  fi
done <<'EOF'
for new files in some other directories
for detritus in the temp directory
for non-standard things in the check directory
EOF
exit "$fail"
