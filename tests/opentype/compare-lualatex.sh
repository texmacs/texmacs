#!/bin/sh
# Typeset the formulas of tests/opentype/compare/ both with TeXmacs and with
# LuaLaTeX + unicode-math, using the same OpenType math font, and stack the
# two renders for comparison. LuaLaTeX is the reference implementation of the
# MATH table, so a disagreement is worth looking at.
#
#   tests/opentype/compare-lualatex.sh [-f <math font file>] [-m <TeXmacs family>]
#                                      [<pair> ...]
#
# Without arguments every pair of compare/ is typeset; a pair is the base name
# of a .tex and .tm file there. Defaults to Latin Modern Math. Needs lualatex,
# mutool and ImageMagick.
set -e
here=$(cd "$(dirname "$0")" && pwd); top=$(cd "$here/.." && pwd); top=$(cd "$top/.." && pwd)
font="$top/TeXmacs/fonts/truetype/lm/latinmodern-math.otf"
family="Latin Modern Math"
while getopts "f:m:" opt; do
  case $opt in
    f) font=$OPTARG ;;
    m) family=$OPTARG ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND-1))
out="$top/tests/build/compare"; mkdir -p "$out"
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TEXMACS_HOME_PATH:-$top/tests/build/home}"
[ -n "$TM_TEST_FONT_DIR" ] && export TEXMACS_FONT_PATH="$TM_TEST_FONT_DIR"
PATH="$PATH:/Library/TeX/texbin"

if [ $# -gt 0 ]; then
  pairs=""
  for name in "$@"; do pairs="$pairs $here/compare/$name.tex"; done
else
  pairs=$(echo "$here"/compare/*.tex)
fi

for tex in $pairs; do
  [ -f "$tex" ] || { echo "no $tex"; continue; }
  name=$(basename "$tex" .tex)
  tm="$here/compare/$name.tm"
  [ -f "$tm" ] || { echo "no $tm, skipping"; continue; }

  # the reference
  { printf '%s\n' '\documentclass[12pt]{article}'
    printf '%s\n' '\usepackage[papersize={24cm,8cm},margin=1cm]{geometry}'
    printf '%s\n' '\usepackage{unicode-math}'
    printf '\\setmathfont{%s}[Path=%s/]\n' "$(basename "$font")" "$(dirname "$font")"
    printf '%s\n' '\pagestyle{empty}'
    printf '%s\n' '\begin{document}'
    cat "$tex"
    printf '%s\n' '\end{document}'
  } > "$out/$name.tex"
  (cd "$out" && lualatex -interaction=batchmode "$name.tex" > /dev/null 2>&1) || true
  [ -f "$out/$name.pdf" ] || { echo "$name: lualatex failed, see $out/$name.log"; continue; }
  mutool draw -q -r 300 -o "$out/$name-ref.png" "$out/$name.pdf" 2>/dev/null

  # ours
  { printf '%s\n\n' '<TeXmacs|2.1.4>'
    printf '%s\n\n' '<style|generic>'
    printf '%s\n' '<\body>'
    printf '  <\\with|font|%s|font-base-size|12>\n' "$family"
    cat "$tm"
    printf '%s\n' '  </with>'
    printf '%s\n\n' '</body>'
    # no page number and no page breaks in the render
    printf '%s\n' '<initial|<\collection>'
    printf '%s\n' '<associate|page-medium|papyrus>'
    printf '%s\n' '<associate|page-odd-footer|>'
    printf '%s\n' '<associate|page-even-footer|>'
    printf '%s\n' '</collection>>'
  } > "$out/$name-ours.tm"
  "$top/TeXmacs/bin/texmacs.bin" -c "$out/$name-ours.tm" "$out/$name-ours.pdf" -q \
     > "$out/$name-ours.log" 2>&1 || true
  [ -f "$out/$name-ours.pdf" ] || { echo "$name: TeXmacs failed, see $out/$name-ours.log"; continue; }
  mutool draw -q -r 300 -o "$out/$name-ours.png" "$out/$name-ours.pdf" 2>/dev/null

  for v in ref ours; do
    magick "$out/$name-$v.png" -trim +repage -resize 1400x \
           -bordercolor white -border 12 "$out/$name-$v-t.png"
  done
  # LuaLaTeX on top, TeXmacs below
  magick "$out/$name-ref-t.png" "$out/$name-ours-t.png" -append "$out/$name-compare.png"
  echo "$name: $out/$name-compare.png (LuaLaTeX above, TeXmacs below)"
done
