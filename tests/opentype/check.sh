#!/bin/sh
# Run everything that guards the OpenType math work: the unit tests and the
# sample renders (tuned and untuned), with a pixel diff against
# tests/build/ref when it exists. Use before committing.
#
#   TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/check.sh
set -e
here=$(cd "$(dirname "$0")" && pwd); top=$(cd "$here/../.." && pwd)
cd "$top"
make -C tests
ref=""; [ -d tests/build/ref ] && ref="-c $top/tests/build/ref"
tests/opentype/render-samples.sh $ref
TM_HAND_TUNED=off tests/opentype/render-samples.sh
echo "check.sh: all passed"
