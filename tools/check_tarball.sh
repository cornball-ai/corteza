#!/usr/bin/env bash
# Validate what R CMD build actually puts in the tarball.
#
# WHY THIS EXISTS, AND WHY IT MUST BE RUN LOCALLY
#
# R CMD build packages the working DIRECTORY, not the git tree. Anything
# sitting in the package root that .Rbuildignore does not exclude ships --
# including untracked scratch directories. R CMD build does NOT skip
# arbitrary dot-directories; it excludes only a fixed known set (.git,
# .Rhistory, ...), so a stray `.something/` is packaged in full.
#
# Two real incidents in this codebase:
#   - a `git worktree add` left a complete second copy of the package at
#     whisper/wtest_path/, which doubled the tarball
#   - a temporary clone of an unrelated package at
#     subtitles/.godotR-boot-review/ contributed 179 of 213 tarball entries
#
# Neither was tracked by git. A CI job checks out a clean tree, so CI can
# never see either of them. **Only a local run against the real working
# tree catches this class**, which is why this is a mandatory
# pre-submission step and not merely a CI job.
#
# Portability: this is a mandatory local step, so it must run on whatever
# machine is submitting. Kept to POSIX-ish tools -- no `stat -c` (GNU-only),
# no `du -b` (GNU-only), and no parsing of `tar -tv` output, whose column
# layout differs between GNU tar and bsdtar. Sizes come from `du -sk` on
# extracted entries instead.
#
# Usage:
#   tools/check_tarball.sh [package-dir]     # default: repo root
#
# Exit status 0 if the tarball contains only expected top-level entries.

set -uo pipefail

pkg_dir=${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}
pkg_dir=$(cd "$pkg_dir" && pwd)

# Exactly what whisper ships today. Deliberately tight rather than the
# generic R-package set: if the package legitimately gains `vignettes` or
# `src`, that is a real change and belongs in a commit that adds the entry
# here. Anything unlisted is either that, or contamination.
#
# One per line, matched with `grep -Fqx`: fixed-string, whole-line. A
# substring or regex match here fails OPEN -- an entry named `R.*` would
# match `R` as a pattern and be waved through as expected content.
allowed_list() {
  cat <<'EOF'
build
DESCRIPTION
NAMESPACE
NEWS.md
R
man
inst
tests
vignettes
EOF
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

echo "building from: $pkg_dir"
if ! (cd "$work" && R CMD build "$pkg_dir" >build.log 2>&1); then
  echo "::error::R CMD build failed"
  tail -20 "$work/build.log"
  exit 1
fi

tarball=$(ls "$work"/*.tar.gz 2>/dev/null | head -1)
if [ -z "$tarball" ]; then
  echo "::error::no tarball produced"
  exit 1
fi
echo "tarball: $(basename "$tarball") ($(wc -c <"$tarball" | tr -d ' ') bytes)"

# Extract and measure on disk: avoids `tar -tv` column-layout differences
# between GNU tar and bsdtar, and gives portable sizes via `du -sk`.
mkdir -p "$work/x"
if ! tar xzf "$tarball" -C "$work/x"; then
  echo "::error::could not extract tarball"
  exit 1
fi
root=$(ls "$work/x" | head -1)
pkg_root="$work/x/$root"

# Three globs, because each misses what the others catch: `*` skips all
# dotfiles, `.[!.]*` catches `.foo` but not `..foo`, and `..?*` catches
# `..foo` without matching `.` or `..` themselves. A missed entry fails
# OPEN. `-e || -L` so a dangling symlink is inspected rather than skipped
# (`-e` follows the link and is false when the target is gone).
fail=0
count=0
for path in "$pkg_root"/* "$pkg_root"/.[!.]* "$pkg_root"/..?*; do
  [ -e "$path" ] || [ -L "$path" ] || continue
  e=$(basename "$path")
  count=$((count + 1))
  if ! allowed_list | grep -Fqx -- "$e"; then
    if [ -L "$path" ] && [ ! -e "$path" ]; then
      echo "::error::unexpected top-level entry: ${e} (dangling symlink)"
    else
      kb=$(du -sk "$path" | awk '{print $1}')
      echo "::error::unexpected top-level entry: ${e} (${kb} KB)"
    fi
    fail=1
  fi
done

if [ "$fail" -eq 0 ]; then
  echo "tarball contents: OK (${count} top-level entries)"
else
  echo
  echo "R CMD build packages the working directory, not the git tree."
  echo "Untracked scratch files in the package root ship unless"
  echo ".Rbuildignore excludes them. Remove them, or add an exclusion."
fi
exit "$fail"
