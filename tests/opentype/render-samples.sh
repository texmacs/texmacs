#!/bin/sh
# Render the OpenType math samples to PDF and PNG for visual comparison.
#
# usage: tests/opentype/render-samples.sh [-r dpi] [-o outdir] [-c refdir]
#
#   -o outdir   where to put <sample>-<rev>.pdf/.png (default tests/build/vis)
#   -c refdir   compare each PNG with refdir/<sample>.png using ImageMagick
#               and report the number of differing pixels
#   -r dpi      rasterization resolution for mutool (default 150)
#
# Environment:
#   TM_TEST_FONT_DIR   directory (searched recursively) with additional fonts,
#                      e.g. Latin Modern Math, STIX Two Math, Asana Math;
#                      exported to TeXmacs as TEXMACS_FONT_PATH
#   TM_HAND_TUNED=off  switch off the hand-tuned customizations of fonts
#                      with a MATH table (preference "hand tuned math fonts")
#                      to render the table-only result; the file names get
#                      a "-notuned" suffix
#   TEXMACS_HOME_PATH  defaults to tests/build/home so that the user's own
#                      TeXmacs settings and font database are not touched;
#                      the local font database is (re)built when missing
#                      or when FORCE_DB=1
#
# Requires mutool (mupdf) for PNG output and ImageMagick's compare for -c.

set -e
here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/vis"
ref=""
dpi=150
while getopts "o:c:r:" opt; do
  case $opt in
    o) out=$OPTARG ;;
    c) ref=$OPTARG ;;
    r) dpi=$OPTARG ;;
    *) exit 2 ;;
  esac
done

bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }

export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TEXMACS_HOME_PATH:-$top/tests/build/home}"
[ -n "$TM_TEST_FONT_DIR" ] && export TEXMACS_FONT_PATH="$TM_TEST_FONT_DIR"
mkdir -p "$out" "$TEXMACS_HOME_PATH"

rev=$(cd "$top" && git rev-parse --short HEAD 2>/dev/null || echo nogit)
if [ -n "$(cd "$top" && git status --porcelain -- src 2>/dev/null)" ]; then
  rev="$rev-dirty"
fi
tuned=""
if [ "$TM_HAND_TUNED" = "off" ]; then
  tuned="(set-hand-tuned-math-fonts #f)"
  rev="$rev-notuned"
fi

if [ ! -f "$TEXMACS_HOME_PATH/fonts/font-database.scm" ] || [ -n "$FORCE_DB" ]; then
  echo "building local font database in $TEXMACS_HOME_PATH"
  "$bin" -x "(font-database-build-local)" -q > "$out/font-database.log" 2>&1
fi

status=0
for tm in "$here"/samples/*.tm; do
  name=$(basename "$tm" .tm)
  pdf="$out/$name-$rev.pdf"
  rm -f "$pdf" "$out/$name-$rev"-*.png
  echo "rendering $name -> $pdf"
  if [ -n "$tuned" ]; then
    "$bin" -x "$tuned" -c "$tm" "$pdf" -q > "$out/$name.log" 2>&1 || true
  else
    "$bin" -c "$tm" "$pdf" -q > "$out/$name.log" 2>&1 || true
  fi
  if [ ! -f "$pdf" ]; then
    echo "  FAILED, see $out/$name.log"; status=1; continue
  fi
  command -v mutool > /dev/null || continue
  # one PNG per page: <sample>-<rev>-<page>.png
  mutool draw -q -r "$dpi" -o "$out/$name-$rev-%d.png" "$pdf" 2>/dev/null || true
  for png in "$out/$name-$rev"-*.png; do
    [ -f "$png" ] || continue
    echo "  $png"
    page=${png##*-}; page=${page%.png}
    refpng="$ref/$name-$page.png"
    if [ -n "$ref" ] && [ -f "$refpng" ] && command -v compare > /dev/null; then
      diff="${png%.png}-diff.png"
      n=$(compare -metric AE "$refpng" "$png" "$diff" 2>&1 || true)
      echo "  differing pixels w.r.t. $refpng: $n  ($diff)"
    fi
  done
done
exit $status
