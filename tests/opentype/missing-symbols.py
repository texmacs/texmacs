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

usage: missing-symbols.py -t unicode-math-table.tex [-o out.md]
                          [--texmacs DIR] [font.otf ...]
       ... --emit [--block NAME] [--class CLS] [--min-fonts N]
       ... --check

The first form writes the coverage report. The second prints draft Scheme
for the selected symbols: the lines to put in place of the comments that
hold their place in the encoding table, and a group for std-symbols.scm
built from the unicode-math class, which is what gives a symbol its
spacing. The third checks the tables themselves, that no name is given two
code points and no code point two two-way names, and says which named
symbols no math font draws; it exits non-zero on a failure.

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

# The proposal tables this script writes live in the same directory but are
# not loaded by TeXmacs until a line in converter.cpp names them, so they do
# not count as coverage; pass --include-extra once they are wired in.
GENERATED = ('tmuniversaltounicode-extra.scm',
             'tmuniversaltounicode-extra-candidates.scm')

def load_named (texmacs, include_extra=False):
    """code point -> set of TeXmacs names, from langs/encoding

    Also returns the placeholder comments, with the file and line where
    they sit, and the two-way mappings with their provenance."""
    named = collections.defaultdict (set)
    noted = {}
    where = {}          # code point -> (file, line number, text)
    two_way = {}        # name -> (code point, file, line number)
    any_way = {}        # name -> code point, one-way tables included
    doubles = []        # names given two different code points
    for path in sorted (glob.glob (os.path.join (texmacs, 'langs', 'encoding',
                                                 '*.scm'))):
        if not include_extra and os.path.basename (path) in GENERATED:
            continue
        oneway = ('oneway' in path) or ('fallback' in path)
        for nr, raw in enumerate (open (path, encoding='utf-8',
                                        errors='replace'), 1):
            line = raw.strip ()
            if line.startswith (';'):
                m = NOTED.match (line)
                if m:
                    code = int (m.group (2), 16)
                    if code not in noted:
                        noted[code] = m.group (1)
                        where[code] = (path, nr, raw.rstrip ('\n'))
                continue
            m = NAME_TO_CODE.match (line)
            if m:
                code = int (m.group (2), 16)
                named[code].add (m.group (1))
                any_way.setdefault (m.group (1), code)
                if not oneway:
                    prev = two_way.get (m.group (1))
                    if prev and prev[0] != code:
                        doubles.append ((m.group (1), prev[0], code, path, nr))
                    two_way[m.group (1)] = (code, path, nr)
            m = CODE_TO_NAME.match (line)
            if m: named[int (m.group (1), 16)].add (m.group (2))
    return named, noted, where, two_way, any_way, doubles

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

# --------------------------------------------------------- draft and check

# unicode-math class -> the group of std-symbols.scm a symbol belongs to,
# with the declarations that group carries
GROUPS = {
  'ord':   ("Miscellaneous-symbol", ["(:type symbol)"]),
  'bin':   ("a binary operator group, next to Plus-symbol or Times-symbol",
            ["(:type infix)"]),
  'rel':   ("Relation-nolim-symbol",
            ["(:type infix)", "(:penalty 20)", "(:spacing wide wide)"]),
  'op':    ("N-ary-operator-symbol",
            ["(:type n-ary)", "(:penalty invalid)", "(:spacing none default)",
             "(:limits display)"]),
  'open':  ("Open-symbol", ["(:type opening-bracket)"]),
  'close': ("Close-symbol", ["(:type closing-bracket)"]),
  'fence': ("Middle-bracket-symbol",
            ["(:type middle-bracket)", "(:spacing middle middle)"]),
  'punct': ("Ponctuation-visible-symbol", ["(:type separator)"]),
  'alpha': ("Letter-symbol", ["(:type symbol)"]),
}
NOT_A_SYMBOL = ('accent', 'over', 'under', 'botaccent', 'radical')

def std_symbol_names (texmacs):
    """every name std-symbols.scm declares, mapped to Unicode or not"""
    path = os.path.join (texmacs, 'progs', 'language', 'std-symbols.scm')
    try: text = open (path, encoding='utf-8', errors='replace').read ()
    except OSError: return set ()
    return set (re.findall (r'"(<[^"]+>)"', text))

def candidate_name (latex):
    "the TeXmacs name a unicode-math name suggests"
    return "<" + latex + ">"

def emit_draft (codes, syms, noted, where, two_way, cov, texmacs):
    """draft Scheme for the selected symbols: the encoding lines to put in
    place of the comments, and a group for std-symbols.scm"""
    out, w = [], None
    out.append ("# Draft entries for %d symbol(s)" % len (codes))
    out.append ("")
    out.append ("Review the names before using them: they come from")
    out.append ("unicode-math and TeXmacs names are shape-based and")
    out.append ("compositional (`var`, `n`, `long`, `up`, `big-`).")
    out.append ("")
    declared = std_symbol_names (texmacs)
    taken, classed, ok = [], [], []
    for code in codes:
        latex, cls, desc = syms[code]
        name = candidate_name (latex)
        if name in any_way: taken.append ((code, name, any_way[name]))
        else:
            if name in declared: classed.append ((code, name))
            ok.append ((code, name, cls, desc))
    out.append ("## The Unicode mapping")
    out.append ("")
    out.append ("In `TeXmacs/langs/encoding/tmuniversaltounicode.scm`, replacing")
    out.append ("the comment that holds the place:")
    out.append ("")
    out.append ("```")
    for code, name, cls, desc in ok:
        loc = where.get (code)
        if loc:
            out.append ("%s:%d" % (os.path.relpath (loc[0], texmacs), loc[1]))
            out.append ("-%s" % loc[2])
        else:
            out.append ("(no placeholder; insert in Unicode order)")
        head = '("%s"' % name
        pad = "\t" * max (1, (31 - len (head)) // 8)
        out.append ('+%s%s"#%04X")   ; %s' % (head, pad, code, desc))
        out.append ("")
    out.append ("```")
    out.append ("")
    if classed:
        out.append ("Names that `std-symbols.scm` already declares without a")
        out.append ("Unicode mapping. Check that the shape is the same before")
        out.append ("reusing the name; where it is, the mapping also repairs")
        out.append ("the export of a symbol users already type:")
        out.append ("")
        for code, name in classed:
            out.append ("- `%s` would become U+%04X" % (name, code))
        out.append ("")
    if taken:
        out.append ("Names already in use, to be chosen by hand:")
        out.append ("")
        for code, name, other in taken:
            out.append ("- U+%04X wants `%s`, which already names U+%04X"
                        % (code, name, other))
        out.append ("")
    out.append ("## The class")
    out.append ("")
    out.append ("In `TeXmacs/progs/language/std-symbols.scm`, added to the")
    out.append ("group named below, or as a group of its own:")
    out.append ("")
    by_class = collections.defaultdict (list)
    for code, name, cls, desc in ok:
        by_class[cls.replace ("math", "")].append (name)
    for cls in sorted (by_class):
        if cls in NOT_A_SYMBOL:
            out.append ("`%s` is an accent or a radical, not a plain symbol:"
                        % cls)
            out.append ("it belongs to the wide constructs, not to a group.")
            out.append ("")
            continue
        group, decls = GROUPS.get (cls, ("Miscellaneous-symbol",
                                         ["(:type symbol)"]))
        out.append ("```scheme")
        out.append (";; %s: goes with %s" % (cls, group))
        out.append ("  (define New-%s-symbol" % cls)
        for d in decls: out.append ("    %s" % d)
        line = "   "
        for nm in by_class[cls]:
            if len (line) + len (nm) > 72:
                out.append (line); line = "   "
            line += ' "%s"' % nm
        out.append (line + ")")
        out.append ("```")
        out.append ("")
    out.append ("The font side needs nothing; see the coverage report.")
    return "\n".join (out)

READY_HEAD = """;; Conversions between TeXmacs symbols and Unicode: the ones
;; the symbol tables are missing
;;
;; Generated by tests/opentype/missing-symbols.py from the symbol list of
;; unicode-math, and meant to be reviewed and then wired in: the tables are
;; loaded by name in src/Data/String/converter.cpp, so a line
;;   hashtree_from_dictionary (dic, "tmuniversaltounicode-extra", ...)
;; next to each one for tmuniversaltounicode, in both directions, is what
;; puts these symbols into service.
;;
;; Every entry here is one that no table names, whose name is free, and
;; whose glyph at least half of the math fonts TeXmacs ships draw. The
;; names come from unicode-math and are candidates, not decisions: TeXmacs
;; names are shape-based and compositional. The comment gives the Unicode
;; description and the number of fonts.
;;
;; The class of a symbol, which is what gives it its spacing, does not live
;; here but in progs/language/std-symbols.scm; the same script drafts those
;; groups with --emit."""

CANDIDATE_HEAD = """;; Conversions between TeXmacs symbols and Unicode: the ones
;; that need a decision
;;
;; Generated by tests/opentype/missing-symbols.py, companion of
;; tmuniversaltounicode-extra.scm. Every line here is commented out and
;; says why: the name may already mean something else, the symbol may be an
;; accent rather than a symbol, or the fonts may not draw it. Nothing reads
;; this file; it is the backlog.
;;
;; The entries worth looking at first are the ones marked "already a
;; TeXmacs symbol": TeXmacs draws those today but has no Unicode for them,
;; so they degrade on export, and the name coming out of unicode-math is
;; the same one TeXmacs uses, which is a good sign that the shape agrees."""

def load_confirmed (here):
    """names checked by eye against the glyph of the code point"""
    path = os.path.join (here, 'confirmed-symbols.txt')
    out = {}
    if not os.path.exists (path): return out
    for line in open (path, encoding='utf-8'):
        line = line.split ('#')[0].strip ()
        if not line: continue
        parts = line.split ()
        if len (parts) >= 2: out[parts[0]] = int (parts[1], 16)
    return out

def write_table (path, codes, syms, noted, two_way, any_way, declared, cov,
                 font_names, floor, table_name, head, want_active, confirmed):
    """a proposal table for langs/encoding, in the shape of the others

    Symbols are written in code point order and grouped by block. With
    want_active the file holds the entries a maintainer can take as they
    are; otherwise it holds the others, commented as the existing tables
    comment the gaps they leave, with the reason attached."""
    out = []
    w = out.append
    w (head)
    w ("")
    current = None
    active = inactive = 0
    for code in codes:
        latex, cls, desc = syms[code]
        name = candidate_name (latex)
        b = block (code)
        if b != current:
            current = b
            w ("")
            w (";;; %s" % b)
            w ("")
        reason = None
        if name in any_way:
            reason = "the name already means U+%04X" % any_way[name]
        elif name in confirmed and confirmed[name] == code:
            reason = None
        elif name in declared and name not in confirmed:
            reason = "already a TeXmacs symbol, check that the shape agrees"
        elif cls.replace ("math", "") in NOT_A_SYMBOL:
            reason = "an accent or a radical, not a plain symbol"
        elif len (cov.get (code, [])) < floor:
            reason = "few fonts draw it"
        head = '("%s"' % name
        pad = "\t" * max (1, (31 - len (head)) // 8)
        line = '%s%s"#%04X")' % (head, pad, code)
        tail = "\t; %s [%d fonts]" % (desc, len (cov.get (code, [])))
        if name in confirmed and confirmed[name] == code:
            tail += ", shape checked"
        if reason:
            inactive += 1
            if not want_active: w (";%s%s, %s" % (line, tail, reason))
        else:
            active += 1
            if want_active: w ("%s%s" % (line, tail))
    # drop the block headings that ended up with nothing under them
    kept = []
    for i, l in enumerate (out):
        if l.startswith (";;; ") and not any (
                x.startswith (("(", ";(")) for x in out[i+1:i+400]
                if not x.startswith (";;; ")):
            continue
        kept.append (l)
    open (path, 'w', encoding='utf-8').write ("\n".join (kept) + "\n")
    return active, inactive

KNOWN_DOUBLE = {"<mu>"}   # U+00B5 micro and U+03BC greek mu, on purpose

def run_check (named, two_way, doubles, cov, font_names):
    """invariants of the symbol tables; returns the number of failures"""
    bad = 0
    seen = collections.defaultdict (list)
    for name, (code, path, nr) in sorted (two_way.items ()):
        seen[code].append (name)
    two = [(c, ns) for c, ns in seen.items () if len (ns) > 1]
    if two:
        bad += len (two)
        print ("FAIL: %d code point(s) with several two-way names:" % len (two))
        for c, ns in two[:10]:
            print ("   U+%04X %s" % (c, " ".join (sorted (ns))))
    for name, first, second, path, nr in doubles:
        if name in KNOWN_DOUBLE: continue
        bad += 1
        print ("FAIL: %s names both U+%04X and U+%04X (%s:%d)"
               % (name, first, second, path, nr))
    print ("checked %d two-way names over %d code points"
           % (len (two_way), len (seen)))
    if cov:
        unseen = sorted ([(n, c) for n, (c, p, l) in two_way.items ()
                          if c not in cov], key=lambda x: x[1])
        if unseen:
            print ("note: %d named symbol(s) that none of the %d math fonts "
                   "draws, so they rely on a text font or on emulation:"
                   % (len (unseen), len (font_names)))
            for n2, c in unseen[:10]:
                print ("   %-16s U+%04X" % (n2, c))
    return bad

# ------------------------------------------------------------------- report

def main ():
    ap = argparse.ArgumentParser (add_help=True)
    ap.add_argument ('-t', '--table', required=True,
                     help='unicode-math-table.tex (or its .scm rendering)')
    ap.add_argument ('-o', '--output', help='write the report here')
    ap.add_argument ('--texmacs', default=None, help='the TeXmacs directory')
    ap.add_argument ('--all', action='store_true',
                     help='list every missing symbol, not only those a font has')
    ap.add_argument ('--emit', action='store_true',
                     help='print draft Scheme entries instead of the report')
    ap.add_argument ('--block', help='with --emit: restrict to a Unicode block')
    ap.add_argument ('--class', dest='cls',
                     help='with --emit: restrict to a unicode-math class')
    ap.add_argument ('--min-fonts', type=int, default=None,
                     help='with --emit: only symbols that many fonts draw')
    ap.add_argument ('--tables', metavar='DIR',
                     help='write a proposal table for langs/encoding there')
    ap.add_argument ('--include-extra', action='store_true',
                     help='count the generated proposal tables as coverage')
    ap.add_argument ('--check', action='store_true',
                     help='check the symbol tables and exit non-zero on error')
    ap.add_argument ('fonts', nargs='*', help='fonts to check for the glyphs')
    a = ap.parse_args ()

    here = os.path.dirname (os.path.abspath (__file__))
    texmacs = a.texmacs or os.path.join (here, '..', '..', 'TeXmacs')
    texmacs = os.path.normpath (texmacs)

    syms = load_reference (a.table)
    if not syms:
        sys.exit ("no symbols read from %s" % a.table)
    named, noted, where, two_way, any_way, doubles = load_named (
        texmacs, a.include_extra)
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

    if a.check:
        sys.exit (1 if run_check (named, two_way, doubles, cov, font_names)
                  else 0)

    if a.tables:
        floor = a.min_fonts if a.min_fonts is not None else max (
            1, len (font_names) // 2)
        declared = std_symbol_names (texmacs)
        ready = os.path.join (a.tables, 'tmuniversaltounicode-extra.scm')
        cand = os.path.join (a.tables,
                             'tmuniversaltounicode-extra-candidates.scm')
        confirmed = load_confirmed (here)
        act, inact = write_table (ready, missing, syms, noted, two_way,
                                  any_way, declared, cov, font_names, floor,
                                  'tmuniversaltounicode-extra', READY_HEAD,
                                  True, confirmed)
        write_table (cand, missing, syms, noted, two_way, any_way, declared,
                     cov, font_names, floor, 'tmuniversaltounicode-extra',
                     CANDIDATE_HEAD, False, confirmed)
        print ("%s: %d entries" % (ready, act))
        print ("%s: %d commented out" % (cand, inact))
        return

    if a.emit:
        sel = missing
        if a.block:
            sel = [c for c in sel if a.block.lower () in block (c).lower ()]
        if a.cls:
            sel = [c for c in sel
                   if syms[c][1].replace ("math", "") == a.cls]
        floor = a.min_fonts if a.min_fonts is not None else 0
        sel = [c for c in sel if len (cov.get (c, [])) >= floor]
        if not sel: sys.exit ("nothing selected")
        text = emit_draft (sel, syms, noted, where, two_way, cov, texmacs)
        if a.output: open (a.output, 'w', encoding='utf-8').write (text)
        else: print (text)
        return

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
