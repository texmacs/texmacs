#!/usr/bin/env python3
"""Report how the math fonts draw the parts of their glyph assemblies.

TeXmacs builds a delimiter which is taller than the pre-drawn variants by
stacking the parts of the MATH table, placing every part at the distance
the table prescribes from the near edge of the ink of the part below it
(see assemble() in src/Plugins/Freetype/rubber_unicode_font.cpp).  What
fonts disagree about is where the ink of a part sits with respect to its
origin: Latin Modern, TeX Gyre, DejaVu and Fira draw every part at the
origin, the others do not, and a font may also forget to declare the
advance of a part.  This report shows both, and the parts of a glyph.

usage: assembly-report.py [-g CHAR] font.otf ...
   -g CHAR   also list the parts of the assemblies of that character,
             for instance -g '(' or -g $'\\u221a'

Needs fontTools.
"""
import sys
from fontTools.ttLib import TTFont
from fontTools.pens.boundsPen import BoundsPen


def bbox(gs, name):
    bp = BoundsPen(gs)
    gs[name].draw(bp)
    return bp.bounds or (0, 0, 0, 0)


def assemblies(f):
    "the vertical and horizontal constructions of the font, by glyph name"
    mv = f['MATH'].table.MathVariants
    for vert, cov, con in (
            (True, mv.VertGlyphCoverage, mv.VertGlyphConstruction),
            (False, mv.HorizGlyphCoverage, mv.HorizGlyphConstruction)):
        if not cov:
            continue
        for g, c in zip(cov.glyphs, con or []):
            if c.GlyphAssembly is not None:
                yield vert, g, c.GlyphAssembly


def report(path, char):
    f = TTFont(path, fontNumber=0, lazy=True)
    if 'MATH' not in f:
        return
    upem = f['head'].unitsPerEm
    k = 1000.0 / upem
    gs = f.getGlyphSet()
    mv = f['MATH'].table.MathVariants
    rows, zero = [], []
    for vert, g, ga in assemblies(f):
        offs = []
        for p in ga.PartRecords:
            b = bbox(gs, p.glyph)
            offs.append(b[1] if vert else b[0])
            if p.FullAdvance == 0:
                zero.append((g, p.glyph))
        if offs:
            rows.append((vert, g, max(offs) - min(offs)))
    if not rows:
        return
    name = f['name'].getDebugName(4) or path.split('/')[-1]
    vmax = max([d for v, g, d in rows if v] or [0]) * k
    hmax = max([d for v, g, d in rows if not v] or [0]) * k
    worst = sorted(rows, key=lambda r: -r[2])[:3]
    print(f"{path.split('/')[-1]:28s} {name[:26]:26s} n={len(rows):3d} "
          f"overlap={mv.MinConnectorOverlap:4d} "
          f"spread vert={vmax:6.0f} hor={hmax:5.0f}  "
          + ", ".join(f"{g}:{d*k:.0f}" for v, g, d in worst if d > 0))
    if zero:
        print(f"{'':28s} parts without an advance: " +
              ", ".join(f"{g}/{p}" for g, p in zero))
    if char:
        cp = ord(char)
        g = f.getBestCmap().get(cp)
        for vert, gn, ga in assemblies(f):
            if gn != g:
                continue
            print(f"{'':28s} U+{cp:04X} {gn} "
                  f"({'vertical' if vert else 'horizontal'})")
            for p in ga.PartRecords:
                b = bbox(gs, p.glyph)
                ink = (b[3] - b[1]) if vert else (b[2] - b[0])
                near = b[1] if vert else b[0]
                print(f"{'':30s} {p.glyph:18s} advance={p.FullAdvance*k:6.0f} "
                      f"ink={ink*k:6.0f} at {near*k:6.0f}  "
                      f"connectors={p.StartConnectorLength*k:5.0f}"
                      f"/{p.EndConnectorLength*k:5.0f} "
                      f"{'extender' if p.PartFlags & 1 else ''}")


def main(argv):
    char = None
    files = []
    i = 0
    while i < len(argv):
        if argv[i] == '-g' and i + 1 < len(argv):
            char = argv[i+1]
            i += 2
        else:
            files.append(argv[i])
            i += 1
    if not files:
        print(__doc__)
        return 1
    for p in files:
        try:
            report(p, char)
        except Exception as e:
            print(f"{p.split('/')[-1]:28s} error {e}")
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
