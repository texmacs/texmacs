#!/bin/sh
# Render one specimen per math font and save it as a PNG, for the gallery of
# src/OPENTYPEMATH.md. Each specimen shows the same formulas, so the fonts and
# the code paths behind them can be compared at a glance.
#
#   tests/opentype/font-gallery.sh [-o outdir] [-r dpi] [-w width] [family ...]
#
#   -o outdir   where the PNGs go (default src/opentype-math)
#   -r dpi      rasterization resolution before scaling (default 300)
#   -w width    width of the saved PNG in pixels (default 1100)
#
# Without family arguments the installed profiled math fonts are asked from
# TeXmacs itself, through (opentype-math-font-list), and the TeX fonts and the
# hand-tuned STIX v1 are prepended as references.
#
# Environment: TM_TEST_FONT_DIR, TEXMACS_HOME_PATH as in render-samples.sh.
# Requires mutool (mupdf) and ImageMagick.

set -e
here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/src/opentype-math"
dpi=300
width=1100
while getopts "o:r:w:" opt; do
  case $opt in
    o) out=$OPTARG ;;
    r) dpi=$OPTARG ;;
    w) width=$OPTARG ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND-1))

bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TEXMACS_HOME_PATH:-$top/tests/build/home}"
[ -n "$TM_TEST_FONT_DIR" ] && export TEXMACS_FONT_PATH="$TM_TEST_FONT_DIR"
mkdir -p "$out" "$TEXMACS_HOME_PATH"
tmp="$top/tests/build/gallery"
mkdir -p "$tmp"

if [ $# -gt 0 ]; then
  families=$*
else
  # the profiled math fonts which are installed, newline separated
  query='(begin (for-each (lambda (p) (display "GALLERY ") (display (cadr p))
                            (display "\n"))
                          (opentype-math-font-list)))'
  installed=$("$bin" -x "$query" -q 2>/dev/null |
              sed -n 's/^GALLERY //p' | tr '\n' '|')
  families="roman|Stix|$installed"
fi

slug () {
  printf '%s' "$1" | tr 'A-Z ' 'a-z-' | sed 's/[^a-z0-9-]//g'
}

# The specimen: letters and digits, scripts and kerning, a fraction, radicals,
# nested delimiters, big operators, wide accents and a labelled arrow.
specimen () {
  cat <<'EOF'
    <math|a*b*c+x*y*z+A*B*C+\<alpha\>+\<beta\>+\<Gamma\>+0123456789>

    <\equation*>
      V<rsub|a><rsup|2>+f<rsup|2>+e<rsup|x<rsup|2>+y<rsup|2>>+<frac|a+b|c+d>+<sqrt|x<rsup|2>+1>+<sqrt|<frac|a|b>|n>+<around*|{|<around*|[|<around*|(|<frac|<frac|a|b>|<frac|c|d>>|)>|]>|}>
    </equation*>

    <\equation*>
      <big|int><rsub|0><rsup|1>f<around*|(|x|)>*\<mathd\>x+<big|sum><rsub|k=1><rsup|n>a<rsub|k>+<big|prod><rsub|k=1><rsup|n>b<rsub|k>+<wide|abc|^>+<wide|x+y|\<bar\>>+<wide|u+v+w|\<overbrace\>>+A<long-arrow|\<rubber-rightarrow\>|f>B
    </equation*>
EOF
}

echo "$families" | tr '|' '\n' | while read -r fam; do
  [ -n "$fam" ] || continue
  name=$(slug "$fam")
  { printf '%s\n\n' '<TeXmacs|2.1.4>'
    printf '%s\n\n' '<style|generic>'
    printf '%s\n' '<\body>'
    printf '  <\\with|font|%s|font-base-size|11>\n' "$fam"
    printf '    <strong|%s>\n\n' "$fam"
    specimen
    printf '%s\n' '  </with>'
    printf '%s\n\n' '</body>'
    printf '%s\n' '<initial|<\collection>'
    printf '%s\n' '<associate|page-medium|papyrus>'
    printf '%s\n' '<associate|page-odd-footer|>'
    printf '%s\n' '<associate|page-even-footer|>'
    printf '%s\n' '</collection>>'
  } > "$tmp/$name.tm"
  rm -f "$tmp/$name.pdf"
  "$bin" -c "$tmp/$name.tm" "$tmp/$name.pdf" -q > "$tmp/$name.log" 2>&1 || true
  if [ ! -f "$tmp/$name.pdf" ]; then
    echo "  $fam: FAILED, see $tmp/$name.log"; continue
  fi
  mutool draw -q -r "$dpi" -o "$tmp/$name-%d.png" "$tmp/$name.pdf" 2>/dev/null
  magick "$tmp/$name-1.png" -trim +repage -bordercolor white -border 24 \
         -resize "${width}x" -colorspace Gray -depth 8 -strip "$out/$name.png"
  echo "  $fam -> $out/$name.png"
done
