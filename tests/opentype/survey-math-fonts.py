#!/usr/bin/env python3
"""Survey OpenType math fonts: MATH table statistics, symbol coverage,
layout features, companions. Needs fontTools.

usage: survey-math-fonts.py [-t unicode-math-table.scm] font.otf ...
Prints a Markdown summary table followed by per-font details."""
import sys, os, re, glob
from fontTools.ttLib import TTFont

def load_symbol_table(path):
    cps = set()
    if not path or not os.path.exists(path): return cps
    for line in open(path, encoding='utf-8', errors='replace'):
        m = re.match(r'\("([0-9A-Fa-f]+)"', line)
        if m: cps.add(int(m.group(1), 16))
    return cps

BLOCKS = [
    ("alnum", 0x1D400, 0x1D7FF, 996),   # math alphanumerics (assigned)
    ("greek", 0x0391, 0x03C9, 57),
    ("letterlike", 0x2100, 0x214F, 80),
    ("arrows", 0x2190, 0x21FF, 112),
    ("mathops", 0x2200, 0x22FF, 256),
    ("misctech", 0x2300, 0x23FF, 256),
    ("miscA", 0x27C0, 0x27EF, 48),
    ("arrowsB", 0x2900, 0x297F, 128),
    ("miscB", 0x2980, 0x29FF, 128),
    ("supops", 0x2A00, 0x2AFF, 256),
]
ALPHABETS = [  # (name, first code point of capital A, count of letters 52)
    ("bold", 0x1D400), ("italic", 0x1D434), ("bold-italic", 0x1D468),
    ("script", 0x1D49C), ("bold-script", 0x1D4D0), ("fraktur", 0x1D504),
    ("double-struck", 0x1D538), ("bold-fraktur", 0x1D56C), ("sans", 0x1D5A0),
    ("sans-bold", 0x1D5D4), ("sans-italic", 0x1D608), ("sans-bold-italic", 0x1D63C),
    ("mono", 0x1D670)]

def name(f, nid):
    r = f['name'].getName(nid, 3, 1, 0x409) or f['name'].getName(nid, 1, 0, 0)
    return r.toUnicode().strip() if r else ""

def features(f, table):
    if table not in f: return []
    t = f[table].table
    if not t.FeatureList: return []
    return sorted(set(r.FeatureTag for r in t.FeatureList.FeatureRecord))

def survey(path, symbols):
    f = TTFont(path)
    cmap = f.getBestCmap()
    d = {}
    d['file'] = os.path.basename(path)
    d['path'] = path
    d['family'] = name(f, 16) or name(f, 1)
    d['style'] = name(f, 17) or name(f, 2)
    d['version'] = name(f, 5)
    d['designer'] = name(f, 9)
    d['license'] = (name(f, 13)[:70] + '...') if len(name(f, 13)) > 70 else name(f, 13)
    d['license_url'] = name(f, 14)
    d['upem'] = f['head'].unitsPerEm
    d['glyphs'] = f['maxp'].numGlyphs
    d['size_kb'] = os.path.getsize(path) // 1024
    d['gsub'] = features(f, 'GSUB')
    d['gpos'] = features(f, 'GPOS')
    d['kern_table'] = 'kern' in f
    # coverage
    cov = {}
    for tag, lo, hi, total in BLOCKS:
        n = sum(1 for cp in cmap if lo <= cp <= hi)
        cov[tag] = (n, total)
    d['blocks'] = cov
    d['alphabets'] = {nm: sum(1 for i in range(52) if (start + i) in cmap) for nm, start in ALPHABETS}
    d['symbols'] = (sum(1 for cp in symbols if cp in cmap), len(symbols))
    d['ascii_letters'] = all((cp in cmap) for cp in list(range(0x41, 0x5B)) + list(range(0x61, 0x7B)))
    d['has_math'] = 'MATH' in f
    if 'MATH' in f:
        M = f['MATH'].table
        C = M.MathConstants
        d['axis'] = C.AxisHeight.Value
        d['script_pct'] = (C.ScriptPercentScaleDown, C.ScriptScriptPercentScaleDown)
        d['disp_op_min'] = C.DisplayOperatorMinHeight
        d['frac_rule'] = C.FractionRuleThickness.Value
        GI = M.MathGlyphInfo
        d['italics'] = len(GI.MathItalicsCorrectionInfo.Coverage.glyphs) if GI.MathItalicsCorrectionInfo else 0
        d['topaccent'] = len(GI.MathTopAccentAttachment.TopAccentCoverage.glyphs) if GI.MathTopAccentAttachment else 0
        d['extshape'] = len(GI.ExtendedShapeCoverage.glyphs) if GI.ExtendedShapeCoverage else 0
        d['kerninfo'] = len(GI.MathKernInfo.MathKernCoverage.glyphs) if GI.MathKernInfo else 0
        V = M.MathVariants
        d['min_overlap'] = V.MinConnectorOverlap
        d['vert'] = (V.VertGlyphCount, sum(1 for c in V.VertGlyphConstruction if c.GlyphAssembly))
        d['horiz'] = (V.HorizGlyphCount, sum(1 for c in V.HorizGlyphConstruction if c.GlyphAssembly))
        def variants(cp):
            g = cmap.get(cp)
            if g and g in V.VertGlyphCoverage.glyphs:
                c = V.VertGlyphConstruction[V.VertGlyphCoverage.glyphs.index(g)]
                first_is_base = bool(c.MathGlyphVariantRecord) and c.MathGlyphVariantRecord[0].VariantGlyph == g
                return (len(c.MathGlyphVariantRecord), bool(c.GlyphAssembly), first_is_base)
            return None
        d['paren'] = variants(0x28)
        d['integral'] = variants(0x222B)
        d['sum'] = variants(0x2211)
        d['radical'] = variants(0x221A)
    return d

def pct(t): return "%d%%" % (100 * t[0] // t[1]) if t[1] else "-"

def main():
    args = sys.argv[1:]
    symtab = None
    if args and args[0] == '-t': symtab = args[1]; args = args[2:]
    symbols = load_symbol_table(symtab)
    rows = [survey(p, symbols) for p in args]
    print("| Font | Family | Version | Glyphs | KB | symbols | alnum | ops | kern info | italics | top acc | vert (asm) | horiz (asm) | ssty | GPOS |")
    print("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    for d in rows:
        if not d['has_math']: continue
        print("| %s | %s | %s | %d | %d | %s | %s | %s | %d | %d | %d | %d (%d) | %d (%d) | %s | %s |" % (
            d['file'], d['family'], d['version'].replace('Version ', ''), d['glyphs'], d['size_kb'],
            pct(d['symbols']), pct(d['blocks']['alnum']), pct(d['blocks']['mathops']),
            d['kerninfo'], d['italics'], d['topaccent'], d['vert'][0], d['vert'][1], d['horiz'][0], d['horiz'][1],
            'yes' if 'ssty' in d['gsub'] else 'no', ','.join(d['gpos']) or '-'))
    print()
    for d in rows:
        print("### %s" % d['file'])
        print("- path: %s" % d['path'])
        print("- family/style: %s / %s; version: %s; designer: %s" % (d['family'], d['style'], d['version'], d['designer']))
        print("- license: %s %s" % (d['license'], d['license_url']))
        print("- upem %d, %d glyphs, %d KB, ASCII letters: %s" % (d['upem'], d['glyphs'], d['size_kb'], d['ascii_letters']))
        print("- GSUB: %s" % (', '.join(d['gsub']) or 'none'))
        print("- GPOS: %s; legacy kern table: %s" % (', '.join(d['gpos']) or 'none', d['kern_table']))
        print("- coverage: " + ', '.join("%s %d/%d" % (k, v[0], v[1]) for k, v in d['blocks'].items()) + "; unicode-math %d/%d" % d['symbols'])
        print("- alphabets (of 52): " + ', '.join("%s %d" % (k, v) for k, v in d['alphabets'].items()))
        if d['has_math']:
            print("- MATH: axis %d, script %d%%/%d%%, displayOperatorMinHeight %d, fraction rule %d, minConnectorOverlap %d" % (
                d['axis'], d['script_pct'][0], d['script_pct'][1], d['disp_op_min'], d['frac_rule'], d['min_overlap']))
            print("- MATH: italics %d, top accent %d, extended shapes %d, kern info %d, vertical variants %d (%d assemblies), horizontal %d (%d assemblies)" % (
                d['italics'], d['topaccent'], d['extshape'], d['kerninfo'], d['vert'][0], d['vert'][1], d['horiz'][0], d['horiz'][1]))
            print("- variants (count, assembly, first is base): paren %s, integral %s, sum %s, radical %s" % (d['paren'], d['integral'], d['sum'], d['radical']))
        else:
            print("- no MATH table")
        print()

if __name__ == '__main__':
    main()
