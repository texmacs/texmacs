#!/usr/bin/env python3
"""Which mathematical symbols does TeXmacs not know?

The reference is the symbol list of the LaTeX package `unicode-math`
(`unicode-math-table.tex`), which names every code point a mathematician is
likely to want. A symbol is reachable in TeXmacs when

  * one of the tables in TeXmacs/langs/encoding maps a TeXmacs name to its
    code point, in either direction, or
  * it is a mathematical alphanumeric that `init_unicode_substitution` in
    src/Graphics/Fonts/smart_font.cpp builds a name for: the thirteen letter
    alphabets, the five Greek ones and the five digit ones of U+1D400 and
    above, and the letterlike holes they fill (U+2102, U+210A..U+2134).

Anything else can only be typed as <#XXXX>: it has no name, so it is in no
palette, no keyboard shortcut and no LaTeX conversion table.

Two kinds of code point are counted apart rather than as missing symbols.
Ordinary text characters below U+2000, which are typed as themselves, and
the combining marks of U+0300..U+036F and U+20D0..U+20FF, which belong to
the wide accent constructs rather than to the symbol palettes.

usage: missing-symbols.py -t unicode-math-table.tex [-o report.md]
                          [--texmacs DIR] [font.otf ...]

Without font arguments the shipped math fonts of TeXmacs/fonts/truetype are
used, and each missing symbol is reported with the number of them that have
the glyph: a symbol no font draws is not worth a name.
Needs fontTools when fonts are given.
"""

import sys, os, re, glob, argparse, collections, unicodedata

# ---------------------------------------------------------------- the table

SYMBOL_RE = re.compile(
    r'\\UnicodeMathSymbol\{"([0-9A-Fa-f]+)\}\s*'
    r'\{\\([A-Za-z@]+)\s*\}\s*\{\\(\w+)\}\s*\{([^}]*)\}')
SCM_RE = re.compile(r'\("([0-9A-Fa-f]+)"\s+"([^"]*)"\s+"([^"]*)"')

def load_reference (path):
    """code point -> (latex name, class, description)"""
    syms = {}
    for line in open (path, encoding='utf-8', errors='replace'):
        m = SYMBOL_RE.search (line)
        if m:
            syms[int (m.group (1), 16)] = (m.group (2), m.group (3),
                                           m.group (4).strip ())
            continue
        m = SCM_RE.match (line.strip ())        # the .scm rendering of the same
        if m:
            syms[int (m.group (1), 16)] = (m.group (2), m.group (3), "")
    return syms

# ------------------------------------------------- what TeXmacs can name

NAME_TO_CODE = re.compile (r'\("(<[^"]+>)"\s+"#([0-9A-Fa-f]+)"\)')
CODE_TO_NAME = re.compile (r'\("#([0-9A-Fa-f]+)"\s+"(<[^"]+>)"\)')
# the tables keep the gaps as comments, in Unicode order, with the name the
# symbol would deserve: `; end of proof "#220E"`
NOTED = re.compile (r';\s*(.+?)\s+"#([0-9A-Fa-f]+)"')

def load_named (texmacs):
    """code point -> set of TeXmacs names, from langs/encoding"""
    named = collections.defaultdict (set)
    noted = {}
    for path in sorted (glob.glob (os.path.join (texmacs, 'langs', 'encoding',
                                                 '*.scm'))):
        for line in open (path, encoding='utf-8', errors='replace'):
            line = line.strip ()
            if line.startswith (';'):
                m = NOTED.match (line)
                if m and int (m.group (2), 16) not in noted:
                    noted[int (m.group (2), 16)] = m.group (1)
                continue
            m = NAME_TO_CODE.match (line)
            if m: named[int (m.group (2), 16)].add (m.group (1))
            m = CODE_TO_NAME.match (line)
            if m: named[int (m.group (1), 16)].add (m.group (2))
    return named, noted

# The alphabets smart_font.cpp generates names for. Keep this list in step
# with init_unicode_substitution; the script prints a warning when the file
# has entries this list does not know.
ALPHABET_STARTS = [
    (0x1D400, 52), (0x1D434, 52), (0x1D468, 52), (0x1D49C, 52), (0x1D4D0, 52),
    (0x1D504, 52), (0x1D56C, 52), (0x1D538, 52), (0x1D5A0, 52), (0x1D5D4, 52),
    (0x1D608, 52), (0x1D63C, 52), (0x1D670, 52),                 # letters
    (0x1D6A8, 58), (0x1D6E2, 58), (0x1D71C, 58), (0x1D756, 58), (0x1D790, 58),
                                                                 # Greek
    (0x1D7CE, 10), (0x1D7D8, 10), (0x1D7E2, 10), (0x1D7EC, 10), (0x1D7F6, 10)]
LETTERLIKE = [0x212C, 0x2130, 0x2131, 0x210B, 0x2110, 0x2112, 0x2133, 0x211B,
              0x212F, 0x210A, 0x2134, 0x212D, 0x210C, 0x2111, 0x211C, 0x2128,
              0x2102, 0x210D, 0x2115, 0x2119, 0x211A, 0x211D, 0x2124]

def alphabet_codes ():
    s = set (LETTERLIKE)
    for start, count in ALPHABET_STARTS:
        s.update (range (start, start + count))
    return s

# ------------------------------------------------------------ Unicode blocks

BLOCKS = [
    (0x0000, 0x007F, "Basic Latin"),
    (0x0080, 0x02FF, "Latin supplement and modifiers"),
    (0x0300, 0x036F, "Combining marks"),
    (0x0370, 0x03FF, "Greek"),
    (0x2000, 0x206F, "Punctuation"),
    (0x2070, 0x209F, "Super and subscripts"),
    (0x20A0, 0x20CF, "Currency"),
    (0x20D0, 0x20FF, "Combining marks for symbols"),
    (0x2100, 0x214F, "Letterlike"),
    (0x2150, 0x218F, "Number forms"),
    (0x2190, 0x21FF, "Arrows"),
    (0x2200, 0x22FF, "Mathematical operators"),
    (0x2300, 0x23FF, "Miscellaneous technical"),
    (0x2400, 0x27BF, "Miscellaneous symbols"),
    (0x27C0, 0x27EF, "Miscellaneous mathematical A"),
    (0x27F0, 0x27FF, "Supplemental arrows A"),
    (0x2800, 0x28FF, "Braille"),
    (0x2900, 0x297F, "Supplemental arrows B"),
    (0x2980, 0x29FF, "Miscellaneous mathematical B"),
    (0x2A00, 0x2AFF, "Supplemental operators"),
    (0x2B00, 0x2BFF, "Miscellaneous symbols and arrows"),
    (0x1D400, 0x1D7FF, "Mathematical alphanumerics"),
    (0x1EE00, 0x1EEFF, "Arabic mathematical")]

def block (code):
    for lo, hi, name in BLOCKS:
        if lo <= code <= hi: return name
    return "Other"

def is_text_char (code):
    "typed as itself: Latin, Greek and the rest of the text repertoire"
    return code < 0x2000 and not is_combining (code)

def is_combining (code):
    return 0x0300 <= code <= 0x036F or 0x20D0 <= code <= 0x20FF

def equivalent (code, named):
    """a named code point this one decomposes to, if any

    Unicode gives the mathematical alphanumerics a compatibility
    decomposition, so the italic dotless i of U+1D6A4 points at U+0131,
    which TeXmacs names <imath>."""
    try: d = unicodedata.decomposition (chr (code))
    except ValueError: return None
    if not d or not d.startswith ('<'): return None
    parts = d.split ()[1:]
    if len (parts) != 1: return None
    try: target = int (parts[0], 16)
    except ValueError: return None
    if target in named:
        return (target, sorted (named[target])[0])
    return None

# ------------------------------------------------------------------- fonts

def font_coverage (paths):
    """code point -> list of font names having the glyph"""
    if not paths: return {}, []
    from fontTools.ttLib import TTFont
    cov = collections.defaultdict (list)
    names = []
    for p in paths:
        try:
            f = TTFont (p, lazy=True, fontNumber=0)
            cmap = f.getBestCmap ()
        except Exception as e:
            print ("cannot read %s: %s" % (p, e), file=sys.stderr)
            continue
        nm = os.path.splitext (os.path.basename (p))[0]
        names.append (nm)
        for c in cmap: cov[c].append (nm)
    return cov, names

def shipped_math_fonts (texmacs):
    out = []
    for p in sorted (glob.glob (os.path.join (texmacs, 'fonts', 'truetype',
                                              '*', '*.otf'))):
        base = os.path.basename (p).lower ()
        if 'math' in base: out.append (p)
    return out

# ------------------------------------------------------------------- report

def main ():
    ap = argparse.ArgumentParser (add_help=True)
    ap.add_argument ('-t', '--table', required=True,
                     help='unicode-math-table.tex (or its .scm rendering)')
    ap.add_argument ('-o', '--output', help='write the report here')
    ap.add_argument ('--texmacs', default=None, help='the TeXmacs directory')
    ap.add_argument ('--all', action='store_true',
                     help='list every missing symbol, not only those a font has')
    ap.add_argument ('fonts', nargs='*', help='fonts to check for the glyphs')
    a = ap.parse_args ()

    here = os.path.dirname (os.path.abspath (__file__))
    texmacs = a.texmacs or os.path.join (here, '..', '..', 'TeXmacs')
    texmacs = os.path.normpath (texmacs)

    syms = load_reference (a.table)
    if not syms:
        sys.exit ("no symbols read from %s" % a.table)
    named, noted = load_named (texmacs)
    if not named:
        sys.exit ("no encoding tables under %s/langs/encoding" % texmacs)
    alphabet = alphabet_codes ()
    fonts = a.fonts or shipped_math_fonts (texmacs)
    cov, font_names = font_coverage (fonts)

    missing, by_name, by_alphabet = [], 0, 0
    text_chars, combining = [], []
    for code in sorted (syms):
        if code in named: by_name += 1
        elif code in alphabet: by_alphabet += 1
        elif is_combining (code): combining.append (code)
        elif is_text_char (code): text_chars.append (code)
        else: missing.append (code)

    out = []
    w = out.append
    w ("# Mathematical symbols TeXmacs cannot name")
    w ("")
    w ("Generated by `tests/opentype/missing-symbols.py` from the symbol list")
    w ("of `unicode-math`. A symbol counts as known when a table in")
    w ("`TeXmacs/langs/encoding` names its code point, or when it is a")
    w ("mathematical alphanumeric for which `smart_font.cpp` builds a name.")
    w ("The others can only be entered as `<#XXXX>`: no palette, no keyboard")
    w ("shortcut, no LaTeX conversion.")
    w ("")
    w ("The tables of `langs/encoding` keep most of these gaps as comments, in")
    w ("Unicode order, with the name the symbol deserves; where they do, the")
    w ("notes column of the lists below repeats it. Where Unicode gives the")
    w ("symbol a compatibility decomposition onto a code point TeXmacs does")
    w ("name, the column says so instead. Such a symbol is reachable in a")
    w ("sense, but the decomposition drops the style: the double-struck")
    w ("gamma of U+213D decomposes to an ordinary gamma.")
    w ("")
    w ("| | Symbols |")
    w ("|---|---|")
    w ("| In the reference list | %d |" % len (syms))
    w ("| Named in `langs/encoding` | %d |" % by_name)
    w ("| Named as a math alphanumeric | %d |" % by_alphabet)
    w ("| Text characters, typed as themselves | %d |" % len (text_chars))
    w ("| Combining marks of the accent constructs | %d |" % len (combining))
    w ("| **Symbols without a name** | **%d** |" % len (missing))
    w ("")
    if font_names:
        w ("Glyph availability is counted over %d font(s): %s."
           % (len (font_names), ", ".join (font_names)))
        w ("")

    per_block = collections.Counter (block (c) for c in missing)
    drawn = [c for c in missing if cov.get (c)]
    w ("## Where they are")
    w ("")
    half = max (1, len (font_names) // 2)
    w ("| Unicode block | Missing | Drawn by at least %d fonts |" % half)
    w ("|---|---|---|")
    for lo, hi, nm in BLOCKS + [(0, 0, "Other")]:
        n = per_block.get (nm, 0)
        if not n: continue
        d = len ([c for c in missing
                  if block (c) == nm and len (cov.get (c, [])) >= half])
        w ("| %s | %d | %d |" % (nm, n, d))
    w ("")
    w ("## The symbols")
    w ("")
    if cov and not a.all:
        w ("Only the symbols at least half of the fonts above draw are listed,")
        w ("since a symbol almost no font has is not worth a name; pass")
        w ("`--all` for the complete list.")
        w ("")
        listed = [c for c in missing if len (cov.get (c, [])) >= half]
    else:
        listed = missing

    current = None
    for code in listed:
        b = block (code)
        if b != current:
            current = b
            w ("")
            w ("### %s" % b)
            w ("")
            w ("| Code | Char | unicode-math | Class | Description | Noted in the table as | Fonts |")
            w ("|---|---|---|---|---|---|---|")
        latex, cls, desc = syms[code]
        try: ch = chr (code)
        except ValueError: ch = ""
        if ch in ('|', '\\'): ch = "`" + ch + "`"
        n = len (cov.get (code, []))
        note = noted.get (code, "")
        eq = equivalent (code, named)
        if eq: note = "decomposes to `%s`, U+%04X" % (eq[1], eq[0])
        w ("| U+%04X | %s | `\\%s` | %s | %s | %s | %d |"
           % (code, ch, latex, cls.replace ("math", ""), desc, note, n))
    w ("")

    w ("## Filling a gap")
    w ("")
    w ("Giving a symbol a name touches five places, and the class column above")
    w ("says which group it belongs to:")
    w ("")
    w ("1. `TeXmacs/langs/encoding/tmuniversaltounicode.scm`: replace the")
    w ("   comment by `(\"<name>\" \"#XXXX\")`, keeping the Unicode order.")
    w ("2. `TeXmacs/progs/language/std-symbols.scm`: declare the symbol in the")
    w ("   group that matches its class, which is what gives it its spacing.")
    w ("3. `TeXmacs/progs/math/math-menu.scm`: add `(symbol \"<name>\")` to the")
    w ("   palette where a reader would look for it.")
    w ("4. `TeXmacs/progs/math/math-kbd.scm`: a keyboard sequence, if the")
    w ("   symbol deserves one.")
    w ("5. `TeXmacs/progs/convert/latex/latex-symbol-drd.scm`: the LaTeX name,")
    w ("   so that import and export keep it.")
    w ("")
    w ("The font side needs nothing: a named symbol is looked up by code point")
    w ("in whatever font serves the formula, and the smart font finds a")
    w ("fallback when the math font lacks the glyph.")
    w ("")

    text = "\n".join (out)
    if a.output:
        open (a.output, 'w', encoding='utf-8').write (text)
        print ("%d symbols without a TeXmacs name, %d drawn by at least one font"
               % (len (missing), len (drawn)))
        print ("report written to %s" % a.output)
    else:
        print (text)

if __name__ == '__main__':
    main ()
