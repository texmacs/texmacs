# The TeXmacs font system: a review

This document describes how fonts work in TeXmacs, from the abstract `font`
interface used by the typesetter down to the FreeType and Metafont backends
that rasterize glyphs. It is meant as an orientation for people who want to
modify the font code, and as background for the OpenType MATH work described
in `opentype-math-design.md`.

Paths are relative to the repository root. Line numbers refer to the
`wip_opentype` branch and will drift.

## 1. Where the code lives

| Area | Location | Role |
|---|---|---|
| Abstract fonts | `src/Graphics/Fonts/` | `font` interface, selection, database, smart fonts, virtual fonts, "poor" derived fonts, rubber fonts, microtypography |
| Bitmap glyph layer | `src/Graphics/Bitmap_fonts/` | `glyph`, `font_metric`, `font_glyphs`, glyph algebra (`glyph_ops.cpp`, transforms, analysis) |
| FreeType backend | `src/Plugins/Freetype/` | TrueType/OpenType loading, Unicode fonts, per-family adjustment tables, rubber fonts for Unicode fonts, font analysis, MATH table parser |
| Metafont backend | `src/Plugins/Metafont/` | TFM/PK loading, TeX text and rubber fonts |
| GUI fonts | `src/Plugins/Qt/qt_font.cpp`, X11 plugin | System fonts through the toolkit (rarely used for documents) |
| Renderers | `src/Graphics/Renderer/` | `renderer::draw (int char_code, font_glyphs, x, y)` and friends |
| Typesetter | `src/Typeset/` | Consumes fonts: `env_semantics.cpp` picks the current font, `Boxes/` and `Concat/` build boxes from glyphs |
| Data | `TeXmacs/fonts/` | `font-database.scm`, `font-features.scm`, `font-characteristics.scm`, `font-substitutions.scm`, `enc/` translators, `virtual/` virtual fonts, `truetype/`, `type1/`, `tfm/` |
| Scheme side | `TeXmacs/progs/fonts/` | Font rules for the legacy TeX font scheme, menus and widgets |

## 2. Core abstractions

### 2.1 `font` and `font_rep` (`src/Graphics/Fonts/font.hpp`)

A `font` is a reference-counted *resource*: every font has a unique string
name and `font::instances` maps names to live objects. The macro
`make (font, name, tm_new<...>)` returns the existing instance when one
exists, otherwise creates it. Names are built by concatenating the defining
parameters, so a font is fully identified by its name. This is the only
caching mechanism and it means that two different constructions that produce
the same name silently share one object.

`font_rep` is an abstract class. The key virtuals are:

- `supports (c)`: does the font have a glyph for character `c`.
- `get_extents (s, ex)`: metric of a string.
- `get_xpositions (s, xpos)`: cursor positions inside a string.
- `draw_fixed (ren, s, x, y)`: draw a string.
- `magnify (zoomx, zoomy)`: return a scaled font.
- `advance_glyph`, `get_glyph`, `index_glyph`: glyph-level access, used by
  the glyph algebra and by the PDF/PostScript exporters.
- Microtypographic hooks: `get_left_slope`, `get_right_slope`,
  `get_left_correction`, `get_right_correction`, `get_lsub_correction`,
  `get_lsup_correction`, `get_rsub_correction`, `get_rsup_correction`,
  `get_wide_correction`, `get_left_protrusion`, `get_right_protrusion`.
- Since the OpenType branch: `make_rubber_font (base)`, which lets a font
  decide which rubber font implementation serves it.

Every font also carries a set of numeric parameters, computed heuristically
by each backend from a handful of glyphs:

| Field | Meaning | Typical derivation (TrueType) |
|---|---|---|
| `y1`, `y2` | descent and ascent | extents of `f` |
| `yx` | x-height | extents of `x` |
| `yfrac` | fraction bar height (math axis) | middle of `-` |
| `ysub_*`, `ysup_*`, `yshift` | script placement limits | fractions of `yx` |
| `wpt`, `hpt` | size of one point in `SI` units | from dpi |
| `wfn` | design size in `SI` | `wpt * size` |
| `wline` | rule thickness | `wfn / 20` |
| `wquad` | quad | width of `M` |
| `spc`, `extra`, `mspc`, `sep` | spacing | width of space, `wfn/10` |
| `slope` | italic slope | ink overhang of `f` |
| `type` | `FONT_TYPE_TEX`, `FONT_TYPE_UNICODE`, `FONT_TYPE_QT`, ... | |
| `math_type` | `MATH_TYPE_NORMAL`, `MATH_TYPE_STIX`, `MATH_TYPE_TEX_GYRE`, `MATH_TYPE_OPENTYPE` | from the font name prefix, or set by the Unicode font constructor |

Plus the microtypography tables: `lsub_correct`, `lsup_correct`,
`rsub_correct`, `rsup_correct`, `above_correct`, `below_correct`
(per-character hash maps of doubles, filled by `font_scripts.cpp` and by the
per-family `adjust_*.cpp` files), protrusion maps, spacing tables, and since
the OpenType branch a block of MATH constants for fractions, radicals and
limits.

Derived fonts inherit these values via `copy_math_pars (fn)`.

### 2.2 Metrics and units

All positions are `SI` integers, with `PIXEL` units per device pixel at the
current dpi. A `metric` has a *logical* box `(x1, y1, x2, y2)` (origin at 0,
`x2` is the advance) and an *ink* box `(x3, y3, x4, y4)`. Composite fonts
combine metrics by taking the union of both boxes.

### 2.3 Strings and character names

Fonts operate on TeXmacs strings, not on code points. A string mixes bytes in
the legacy Cork-like encoding with named entities in angle brackets:
`<alpha>`, `<big-sum-2>`, `<left-(-3>`, `<#2A0C>` (explicit Unicode),
`<b-x>` (bold letter), `<it-a>` (italic letter), and, new on this branch,
`<@1F3A>` (native glyph id). Every layer of the font stack pattern-matches
these names; there is no intermediate "glyph run" representation.

### 2.4 Bitmap glyph layer (`src/Graphics/Bitmap_fonts/`)

Below `font_rep` sits a purely bitmap model:

- `glyph`: a raster with `width`, `height`, offsets `xoff`, `yoff`, logical
  width `lwidth`, a `depth` (bits per pixel, 1 for TrueType output), and an
  `index` used by exporters to find the glyph in the original font file.
- `font_metric_rep`: `exists (code)`, `get (code)` returning a `metric`, and
  `kerning (left, right)`.
- `font_glyphs_rep`: `get (code)` returning a `glyph`.

Both are resources cached by name. `glyph_ops.cpp` and `glyph_transforms.cpp`
provide an algebra on rasters (join, intersect, clip, flip, rotate,
`hor_extend`, `ver_extend`, `hor_take`, `ver_take`, `bolden`, `slanted`,
`stretched`, `make_bbb`, `unserif`, ...). This algebra is what makes virtual
fonts and the "poor" fonts possible, and it is also why the pipeline is
fundamentally bitmap based: a renderer receives a `font_glyphs` and a
character code and draws the raster. Outline information is used only at
export time (the PDF renderer re-embeds the original font using
`glyph->index`), and by the vector drawing path in `virtual_font.cpp`.

## 3. Physical font backends

### 3.1 TeX / Metafont fonts (`src/Plugins/Metafont/`)

The historical backend. `tex_font` loads TFM metrics and PK bitmaps
(generating them with `mktexpk` when needed). Character names are mapped to
positions through *translators* (`translator.hpp`), loaded from the
`TeXmacs/fonts/enc/*.enc` files. `tex_rubber_font` implements TeX's extensible
delimiters from the `cmex` pieces. A *math font* (`math_font.cpp`) is a
compound of many TeX fonts (roman, italic, symbols, AMS, stmaryrd, wasy, ...)
described by Scheme rules in `TeXmacs/progs/fonts/fonts-math.scm` and looked
up through `find_font (scheme_tree)` in `find_font.cpp`, which dispatches on
tuples such as `(tex ...)`, `(cm ...)`, `(ec ...)`, `(math ...)`,
`(tex-rubber ...)`, `(compound ...)`, `(unicode ...)`, `(unimath ...)`.

This path is still what the default "roman" (Computer Modern) font uses when
`new_fonts` is off, and it defines the reference behaviour that everything
else imitates.

### 3.2 FreeType fonts (`src/Plugins/Freetype/`)

FreeType is loaded dynamically (`free_type.cpp`) or linked; only a small set
of entry points is used: new memory face, select charmap, set char size, get
char index, load glyph, render glyph, get kerning.

- `tt_file.cpp`: locates font files. `tt_font_path ()` concatenates
  `$TEXMACS_FONT_PATH`, the "imported fonts" preference,
  `$TEXMACS_HOME_PATH/fonts/truetype`, `$TEXMACS_PATH/fonts/truetype`, and
  platform system directories, including a hard-coded list of TeX Live years
  on macOS. Results are cached in the `tt_fonts` hash map.
- `tt_face.cpp`: `tt_face` reads the whole file into memory and creates an
  `FT_Face`. `tt_font_metric_rep` and `tt_font_glyphs_rep` render each glyph
  on demand in *monochrome* mode at the requested size and dpi, and cache the
  result per code. Kerning uses `FT_Get_Kerning`, so only the legacy `kern`
  table is honored, never GPOS.
- `tt_font.cpp`: a minimal font on top of the two caches, used for
  `(truetype ...)` tuples.
- `unicode_font.cpp`: the workhorse for modern fonts. It parses `<#XXXX>`
  and named entities via `strict_cork_to_utf8`, supports a `native` table for
  glyphs without Unicode code points (`tex_gyre_operators`, `<@XXXX>`),
  implements f- and s-ligatures itself, and hosts the per-family
  microtypography: the constructor contains an `if / else if` ladder on the
  family name that installs hand-tuned correction tables for STIX, TeX Gyre
  (Termes, Pagella, Schola, Bonum), Papyrus, Libertine, Biolinum and Fira,
  with data in `adjust_*.cpp`. The OpenType MATH support was added as the
  final `else` of this ladder.
- `unicode_math_font.cpp`: the older `(unimath up it bup bit fallback)`
  compound, superseded by smart fonts.
- `rubber_unicode_font.cpp`, `rubber_assemble_font.cpp`,
  `rubber_stix_font.cpp`: stretchable characters, see section 7.
- `tt_analyze.cpp`: computes the "characteristics" stored in the font
  database (serif/sans, mono, slant, x-height, stroke widths, fill rate,
  supported Unicode ranges) by analysing rendered glyph bitmaps.

### 3.3 Toolkit fonts

`qt_font.cpp` and the X11 equivalent wrap system fonts. They exist for the
GUI and for fallback; documents essentially always go through FreeType.

## 4. Font database and selection

### 4.1 The database (`font_database.cpp`)

Four Scheme files, each existing in a global (`$TEXMACS_PATH/fonts/`) and a
local (`$TEXMACS_HOME_PATH/fonts/`) version:

- `font-database.scm`: `((Family Style) ((file.ttf index size) ...))`.
- `font-features.scm`: `(Family Master Feature ...)`, mapping a family to
  its *master* family and a list of features such as `bold`, `italic`,
  `sansserif`, `mono`, `smallcaps`.
- `font-characteristics.scm`: per style, the measured attributes from
  `tt_analyze` (`mono=no sans=no slant=0 ex=67 em=243 lvw=4 ...`) plus the
  Unicode ranges covered.
- `font-substitutions.scm`: family-level substitutions
  (`((FandolSong sansserif) (FandolHei))`).

The local database is built by scanning the font path (`font_database_build*`)
and only the delta with respect to the global one is saved. Loading is lazy,
and the global database is loaded as a fallback when a family is missing.

### 4.2 Logical fonts and features (`font_select.cpp`, `font_guess.cpp`)

TeXmacs describes a requested font as a *logical font*: an array of strings
whose first element is the family and the rest are normalized features
(`bold`, `italic`, `smallcaps`, `condensed`, `wide`, `sansserif`, `mono`,
...). `logical_font (family, variant, series, shape)` translates the four
document environment variables into such an array. `search_font` then
computes a distance between the requested features and every style of every
candidate family (with heuristics in `distance`), first strictly, then after
dropping unknown features, then using guessed distances between families
(`font_guess.cpp`). The `attempt` argument selects successively worse
matches, which the smart font uses to find fallback fonts for characters the
main font lacks.

`font_translate.cpp` maps the legacy names (`roman`, `pagella`, `stix`,
`cyrillic`, ...) to the new naming scheme and back.

## 5. Smart fonts: the entry point for the typesetter

`edit_env_rep::update_font` (`src/Typeset/Env/env_semantics.cpp`) creates the
current font from environment variables:

- text mode: `smart_font (font, font-family, font-series, font-shape, size, dpi)`
- math mode: `smart_font (math-font, math-font-family, math-font-series, math-font-shape, font, font-family, font-series, "mathitalic", size, dpi)`
- program mode: same with the `prog-*` variables.

The size passed is already the script size for the current index level
(`script (sz, level)` divides by 1.5 per level, at most twice).

`smart_font_bis` builds the name, applies family fix-ups (`tex_gyre_fix`,
`kepler_fix`, `math_fix`, `sys-*` CJK defaults), picks the closest physical
font as `fn[SUBFONT_MAIN]` and a sans-serif error font, and creates a
`smart_font_rep` (`smart_font.cpp`).

### 5.1 Family syntax

The `font` environment variable is a comma separated *font sequence*. Each
item is either a family name or `conditions=family`, where conditions are
space separated and each condition is an alternative list separated by `|`.
Conditions can be Unicode ranges (`greek`, `cyrillic`, `cjk`, `mathsymbols`,
...), pseudo ranges (`mathlarge`, `mathbigop`, `mathrubber`), single
characters, character collections, code point ranges `A:Z`, logical features,
or `math` (only in math shapes). Examples: `cjk=Songti SC,roman`,
`math=TeX Gyre Pagella Math,Linux Libertine`.

### 5.2 Per-character resolution

The smart font splits every string into runs that live in the same subfont
(`advance`). Routing is cached in a `smart_map` shared by all sizes of the
same logical font: a 256-entry vector for single bytes and a hash map for
`<...>` entities. On a miss, `resolve (c)` walks the font sequence and, for
each family, tries in order:

1. the main font, if it supports the character;
2. math families (`is_math_family`) through `REWRITE_MATH`;
3. the Greek and Cyrillic companion fonts;
4. emulated symbols from the `emu-*.vfn` virtual fonts, when the main font
   is not italic;
5. "poor" constructions such as `poor-bbb` for blackboard bold, `poor-bold`,
   `it` (slanting);
6. on later attempts, other families supporting the Unicode range of the
   character, at a dpi adjusted so that x-heights match (`adjusted_dpi`).

Math shapes add many subfonts up front (`fast-italic`, `special`,
`emu-bracket`, `regular`, `bold-math`, `cal`, `frak`, `bbb`, `tt`, `ss`, ...)
and rewrite Unicode math alphanumerics (`substitute_math_letter`) into
`<b-x>`, `<cal-A>` style names or into the corresponding Unicode plane 1
code points depending on what the font offers.

Rubber characters (`<left-...>`, `<big-...>`, `<mid-...>`, `<right-...>`,
`<large-...>`) are routed by `resolve_rubber`: the base delimiter is resolved
first, and then a `rubber` subfont wrapping the font that provided it is
created with `rubber_font (fn)`.

A loop detector fails hard when a substitution resolves to the smart font
itself.

## 6. Virtual fonts (`virtual_font.cpp`, `TeXmacs/fonts/virtual/*.vfn`)

A virtual font builds glyphs from Scheme expressions over other glyphs. A
definition looks like

```scheme
(virtual-font
  (minus (align minus* + 0.5 0.5))
  (slash (or (font minus Baskerville Cuprum) /)))
```

The evaluator (`compile_bis`) supports about ninety primitives: glyph
references, `glue`, `glue*`, `glue-above`, `glue-below`, `join`, `intersect`,
`exclude`, `align`, `magnify`, `scale`, `hor-extend`, `ver-extend`,
`hor-take`, `ver-take`, `hor-flip`, `rotate`, `crop` variants, `bar-*`,
`curly`, `unserif`, `circle`, `reslash`, `or` (first alternative that
exists), `font` (require a family), `with` (local bindings), and pen width
queries. Names of the form `<name-#>` are templates where `#` is replaced by
a variant number at lookup time (`subst_sharp`), which is how a single
definition yields a family of rubber sizes.

Virtual fonts are used for emulated symbols missing from a font
(`emu-fundamental`, `emu-greek`, `emu-operators`, `emu-arrows`, ...) and for
assembling large delimiters (`emu-large`, `emu-alt-large`). There is also a
direct vector drawing path (`draw_tree`) for high resolution output.

The dictionary of a virtual font is a `translator` (`translator.hpp`): a
`dict` from names to indices and an array `virt_def` of definitions. The
OpenType branch creates such translators at runtime instead of loading them
from `.vfn` files.

## 7. Rubber (stretchable) characters

### 7.1 Naming

The typesetter never asks for "a parenthesis of height h". It asks a font for
a named glyph with a size suffix, and searches for the smallest that fits:

- `get_delimiter (s, fn, height)` in `src/Typeset/Boxes/Basic/text_boxes.cpp`
  tries `<left-(-0>`, `<left-(-1>`, ... until the extents are tall enough or a
  credit of about twenty attempts is exhausted.
- `big_operator_box` uses `<big-sum-1>` in text style and `<big-sum-2>` in
  display style.
- `wide_box` and `wide_stix_box` do the same horizontally for accents and
  braces (`<wide-hat-N>`, `<rubber-overbrace-N>`).

### 7.2 Implementations

`rubber_font (base)` (`font.cpp`) caches one rubber font per base font, built
by `base->make_rubber_font (base)`. The default `font_rep` implementation
dispatches on the font name:

| Condition | Implementation | How it stretches |
|---|---|---|
| name mentions `stix` | `rubber_stix_font` | STIX's own size variants and assembly pieces, hard-coded glyph names |
| `mathlarge=` or `mathrubber=` in the name | the font itself | the smart font routes rubber names to a dedicated family |
| Unicode font and `has_poor_rubber` (default true) | `poor_rubber_font` | stretches the base glyph vertically with `poor_stretched_font` in eight steps, then falls back to `emu-large` virtual assembly and finally to `rubber_unicode_font` |
| other Unicode fonts | `rubber_unicode_font` | magnified base glyphs (`sqrt 2`, `2`) and `rubber_assemble_font`, which glues the Unicode bracket pieces U+239B..U+23B7 via `emu-alt-large.vfn` |
| TeX fonts | `tex_rubber_font` | TeX's `cmex` extensible recipes |

`supports_big_operators` decides whether a font's own big operators are used
or magnified small ones. The OpenType branch adds a fourth mode where the
MATH table's variants and assemblies drive this (see the design document).

## 8. Math typesetting parameters

The math boxes in `src/Typeset/Boxes/Composite/math_boxes.cpp` and
`script_boxes.cpp`, and the concatenation code in
`src/Typeset/Concat/concat_math.cpp`, position fractions, radicals, scripts,
limits and wide accents using the generic font parameters (`yfrac`, `sep`,
`wline`, `yx`, `ysub_*`, `ysup_*`) plus the microtypography hooks. On top of
that there are explicit special cases keyed on `math_type` or on substrings
of the font name (`starts (fn->res_name, "stix-")`, `occurs ("agella",
fn->res_name)`), for example the sqrt index position, the spacing after
integrals, and which wide accents to emulate. These special cases are the
practical reason for the OpenType MATH work: they encode, by hand, what an
OpenType math font already declares.

## 9. Assessment

Strengths:

- The abstract `font` interface decouples the typesetter from the backend and
  has allowed TeX bitmap fonts, TrueType, and toolkit fonts to coexist for
  twenty years.
- The bitmap glyph algebra plus virtual fonts give TeXmacs a robust fallback
  story: almost any text font can be used for mathematics, with missing
  symbols emulated and delimiters stretched.
- Font selection by features and distance, with analysed characteristics, is
  more forgiving than fontconfig-style matching and is portable.
- Smart fonts route per character and cache aggressively; performance in
  practice is good.

Weaknesses and technical debt:

- Glyphs are monochrome bitmaps rasterized per size and dpi. Anti-aliasing
  and vector output are handled downstream, and all glyph manipulation is
  raster manipulation.
- No OpenType layout. Kerning comes from the legacy `kern` table only, there
  is no GSUB or GPOS processing, so no contextual alternates, no `ssty`
  script-style variants, no real ligature support beyond the built-in f/s
  ligatures, and no complex-script shaping.
- Character addressing by string names means every layer re-parses names, and
  glyphs without Unicode code points need ad hoc escapes (`native`, `<@XXXX>`).
- Font parameters are estimated from a few glyphs (`x`, `M`, `-`, `f`) and
  per-family corrections are hard-coded C++ tables selected by family name
  prefix. Adding a font family with good math typography means adding an
  `adjust_*.cpp` file.
- Family-name string matching is scattered across `font.cpp`, `smart_font.cpp`,
  `poor_rubber.cpp`, `concat_math.cpp` and `math_boxes.cpp`, so the same font
  can be treated inconsistently depending on how its name was spelled.
- Rubber sizing is a search over integer variant numbers with heuristics for
  what each number means in each implementation; nothing tells the typesetter
  the actual set of available sizes.
- Caching by concatenated names is fragile: two constructors producing the
  same name share an object, as happens with `rubber_unicode_font` on the
  OpenType branch.
- The whole tree currently fails to compile with Apple clang 17 because of a
  pre-existing template bug in `src/Kernel/Containers/hashtree.cpp:97`
  (`*this->contains` should be `(*this)->contains`), unrelated to fonts.
