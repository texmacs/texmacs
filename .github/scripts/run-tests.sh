#!/bin/sh
# Runs the TeXmacs regression suites headless and fails unless they pass.
# Usage: run-tests.sh <texmacs binary>   (from the top of the source tree)

BIN=${1:-TeXmacs/bin/texmacs.bin}
TOP=$(pwd)
export QT_QPA_PLATFORM=offscreen
export TEXMACS_PATH="$TOP/TeXmacs"
export TEXMACS_HOME_PATH="${RUNNER_TEMP:-/tmp}/texmacs-home"
mkdir -p "$TEXMACS_HOME_PATH"

# On Windows (MSYS2), TeXmacs needs a native path in the Scheme string;
# MSYS2 converts the environment variables above, but not this string
SCRIPT="$TOP/.github/scripts/run-tests.scm"
if command -v cygpath > /dev/null 2>&1; then SCRIPT=$(cygpath -m "$SCRIPT"); fi

# A script that fails to load must not leave TeXmacs waiting
CMD="(catch #t (lambda () (load \"$SCRIPT\"))
  (lambda args (display* \"CI-TESTS-FAILED: \" args \"\\n\") (quit-TeXmacs)))"

# perl's alarm gives a portable timeout (GNU timeout is missing on macOS)
perl -e 'alarm shift; exec @ARGV' 600 "$BIN" -x "$CMD" > tests.log 2>&1
echo "texmacs exited with status $?"

grep -v "approximating font\|propagateSizeHints\|does not support" tests.log
grep -q "CI-TESTS-OK" tests.log
