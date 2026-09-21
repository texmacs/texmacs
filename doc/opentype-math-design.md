# OpenType MATH support in TeXmacs: design and status

This document describes the OpenType MATH table support on the `wip_opentype`
branch: what was implemented, how it plugs into the font system described in
`font-system-review.md`, the known problems, and what is still missing before
it can be considered complete.

## 1. Provenance

The work originates from Ke Shi's OSPP 2024 project for Mogan, the TeXmacs
fork. It was ported to this tree in commit `68d690e0f` ("Port the OSPP24 work
of Ke Shi on OpenType support from Mogan") and adjusted in three follow-up
commits (`efae32ac9`, `7ab9eb58d`, `59a641a84`). The branch adds roughly 1300
lines, nearly all in `src/Plugins/Freetype/` and
`src/Typeset/Boxes/Composite/`.

Reference: the MATH table specification at
<https://learn.microsoft.com/en-gb/typography/opentype/spec/math>.

## 2. Goal

An OpenType math font (Latin Modern Math, Libertinus Math, XITS Math, STIX
Two Math, TeX Gyre *-math, Cambria Math, Fira Math, ...) declares in its MATH
table everything a math typesetter needs:

- **MathConstants**: 56 layout parameters (axis height, script shifts,
  fraction and radical gaps, limit gaps, ...).
- **MathGlyphInfo**: per-glyph italic correction, top accent attachment
  point, the set of "extended shapes", and cut-in kerning (MathKernInfo).
- **MathVariants**: for each stretchable glyph, a list of pre-drawn size
  variants and a recipe (GlyphAssembly) to build arbitrarily large ones from
  parts.

TeXmacs today approximates all of this with heuristics and hand-written
per-family tables (see the review, sections 7 and 8). The goal of the branch
is to read the MATH table and use it instead, so that any OpenType math font
works well without C++ changes.

## 3. Architecture

```
  .otf file
     |  tt_face_rep ctor (tt_face.cpp)
     v
  ot_mathtable  <---- parse_mathtable (tt_tools.cpp)
     |
     |  unicode_font_rep ctor, final else-branch (unicode_font.cpp)
     v
  unicode_font_rep                         font_rep fields
    math_type = MATH_TYPE_OPENTYPE  ---->  frac_*, sqrt_*, *_limit_* constants
    math_face, math_table
    get_ot_italic_correction ------------> get_right_correction, get_rsup_correction
    get_ot_kerning ----------------------> get_[lr]su[bp]_correction
    make_rubber_font --------------------> rubber_unicode_font (base, face)
                                             |
                                             |  search_font_sub_opentype
                                             v
                                   variants  -> "<@XXXX>" native glyph ids
                                   assembly  -> runtime translator + virtual_font
                                                (glue-above / glue*, ver-take / hor-take)

  Typesetter consumers: frac_box, sqrt_box, lim_box, typeset_sqrt
```

### 3.1 MATH table parser (`src/Plugins/Freetype/tt_tools.{hpp,cpp}`)

`parse_mathtable (const string& buf)` takes the raw font file, checks the
sfnt header, extracts the `MATH` table with the existing `tt_table` helper and
walks it with the big-endian readers already used by `tt_dump`. It produces
an `ot_mathtable` resource (a `concrete_struct` with `CONCRETE_NULL`
semantics) holding:

| Member | Content |
|---|---|
| `constants_table` | `MathConstantsTable`: 51 `MathValueRecord`s indexed by the `MathConstantRecordEnum` enumeration, plus the five plain integers (`scriptPercentScaleDown`, `scriptScriptPercentScaleDown`, `delimitedSubFormulaMinHeight`, `displayOperatorMinHeight`, `radicalDegreeBottomRaisePercent`). `operator[]` gives uniform access. |
| `italics_correction` | glyph id to `MathValueRecord` |
| `top_accent` | glyph id to `MathValueRecord` |
| `extended_shape_coverage` | set of glyph ids |
| `math_kern_info` | glyph id to `MathKernInfoRecord` (four optional `MathKernTable`s: top/bottom, left/right) |
| `minConnectorOverlap` | from MathVariants |
| `ver_glyph_variants`, `hor_glyph_variants` | glyph id to array of variant glyph ids, in increasing size |
| `ver_glyph_variants_adv`, `hor_glyph_variants_adv` | matching advance measurements |
| `ver_glyph_assembly`, `hor_glyph_assembly` | glyph id to `GlyphAssembly` (italics correction and `GlyphPartRecord`s with connector lengths, full advance and the extender flag) |

Helper methods: `get_init_glyphID (gid)` maps a variant glyph back to its
base glyph (lazy reverse index over both variant tables), `has_kerning` and
`get_kerning (gid, height, top, left)` implement the MathKern lookup (the
kern value for the interval containing `height`, with the usual n heights and
n+1 values).

Coverage tables of format 1 and 2 are supported. `MathValueRecord` device
tables are detected and their header read, but the delta array is not stored
and no device correction is applied anywhere. The parser accepts only MATH
version 1.0.

`dump_mathtable` prints the variants and assemblies and is hooked into
`tt_dump`, so `(tt-dump "font.otf")` from Scheme shows the MATH content.

### 3.2 Face integration (`src/Plugins/Freetype/tt_face.{hpp,cpp}`)

`tt_face_rep` gains an `ot_mathtable math_table` member. The constructor
parses the MATH table for every `.ttf` and `.otf` face right after creating
the `FT_Face`, and dumps it to `debug_fonts`. Since `tt_face` instances are
cached resources, parsing happens once per font file.

### 3.3 Font activation (`src/Plugins/Freetype/unicode_font.cpp`)

The `unicode_font_rep` constructor ends with an `if / else if` ladder on the
family name that installs hand-made correction tables for STIX, TeX Gyre,
Papyrus, Libertine, Biolinum and Fira. The branch adds a final `else`: look up
the face, and if it has a MATH table:

- store `math_face` and `math_table` on the font;
- set `math_type = MATH_TYPE_OPENTYPE` (new constant in `font.hpp`);
- compute a design-unit conversion factor;
- copy twenty constants into new `font_rep` fields:
  `upper_limit_gap_min`, `upper_limit_baseline_rise_min`,
  `lower_limit_gap_min`, `lower_limit_baseline_drop_min`,
  `frac_rule_thickness`, `frac_num_shift_up`, `frac_num_disp_shift_up`,
  `frac_num_gap_min`, `frac_num_disp_gap_min`, `frac_denom_shift_down`,
  `frac_denom_disp_shift_down`, `frac_denom_gap_min`,
  `frac_denom_disp_gap_min`, `sqrt_ver_gap`, `sqrt_ver_disp_gap`,
  `sqrt_rule_thickness`, `sqrt_extra_ascender`, `sqrt_degree_rise_percent`,
  `sqrt_kern_before_degree`, `sqrt_kern_after_degree`.

`copy_math_pars` propagates these fields to derived fonts (smart fonts, rubber
fonts, magnified fonts), so the typesetter can read them from `env->fn`.

**Unit conversion.** `init_design_unit_factor` loads the glyph `m` unscaled to
get its advance in font units, measures the same glyph through
`get_extents ("m")` in `SI`, and takes the ratio. `design_unit_to_metric` and
`metric_to_design_unit` multiply by that factor. This avoids reaching into
FreeType's size metrics but depends on rounding of one glyph advance; the
principled alternative is `units_per_EM` together with the font's size and
dpi.

**Important consequence of the ladder position.** Because STIX and the TeX
Gyre math fonts match earlier branches, the shipped math fonts
(`TeXmacs/fonts/truetype/stix/STIXMath-Regular.otf`,
`texgyre/texgyre*-math.otf`) never enter the OpenType path. Only third-party
fonts exercise it.

### 3.4 Glyph-level corrections

Native glyph addressing. `read_unicode_char` accepts `<@XXXX>` and returns
`0xc000000 + glyph id`, the same offset the FreeType layer already used for
`native` characters; `decode_index` in `tt_face.cpp` turns such codes into raw
glyph indices. `get_glyphID (s)` maps any TeXmacs character string to a glyph
id, either from the `<@XXXX>` form or through `index_glyph`.

For fonts with `math_type == MATH_TYPE_OPENTYPE`:

- `get_right_correction` returns the MATH italic correction of the last
  glyph when present (`get_ot_italic_correction`).
- `get_lsub_correction`, `get_lsup_correction` return the bottom-left and
  top-left cut-in kern of the first glyph (`get_ot_kerning`).
- `get_rsub_correction`, `get_rsup_correction` combine the bottom-right or
  top-right kern with the italic correction; for integrals (detected by
  `is_ot_integral` via a fixed list of integral names mapped to glyph ids and
  reduced to base glyphs with `get_init_glyphID`) only 60 percent
  respectively 40 percent of the italic correction is applied.

The MathKern lookup needs the height at which the script attaches. The
correction API has no access to the script box, so the code passes the font's
`y1` or `y2` (descender or ascender), an approximation.

### 3.5 Stretchable characters

`font_rep::make_rubber_font` became virtual and `rubber_font (base)` calls
it, so a font can choose its rubber implementation:

- `unicode_font_rep::make_rubber_font` returns
  `rubber_unicode_font (this, math_face)` when a MATH table exists.
- `smart_font_rep::make_rubber_font` returns itself for `mathlarge=` and
  `mathrubber=` families, forwards to the main subfont when that subfont is
  OpenType, and otherwise uses the default.

`rubber_unicode_font_rep` gains a `tt_face math_face`, a runtime translator
`virt` named `opentype_virtual[<base>]`, and two extra subfonts. The subfont
table is now:

| nr | Font | Purpose |
|---|---|---|
| 0 | base | glyphs present in the font itself, including `<@XXXX>` variants |
| 1 | base magnified by sqrt(0.5) | legacy |
| 2 | base magnified by sqrt(2) | legacy |
| 3 | base magnified by 2 | legacy |
| 4 | `rubber_assemble_font (base)` | legacy Unicode bracket pieces |
| 5 | `font_rep::make_rubber_font (base)` | fallback when the MATH table has nothing for a glyph |
| 6 | `virtual_font (base, virt->res_name, ...)` | assemblies built from the MATH table |

`search_font_cached` calls `search_font_sub_opentype` when a MATH table is
available. That function:

1. parses the rubber name with `parse_variant`: `<head-root-N>` gives
   `head` (`left`, `mid`, `right`, `large`, `big`), `root` (the delimiter or
   operator name) and the variant number `N`; `<big-...>` numbers are shifted
   down by one because there is no `<big-x-0>`;
2. converts the root to a code point and then to a glyph id with
   `ft_get_char_index`;
3. if the glyph has vertical or horizontal variants and `N` is within range,
   rewrites the string to `<@XXXX>` for the Nth variant and returns subfont 0;
4. otherwise, if it has an assembly, synthesizes on first use a virtual glyph
   definition: each part becomes `@XXXX`, extender parts become
   `(ver-take @XXXX 0.5 # 0.25)` or `(hor-take ...)`, and the parts are
   folded with `glue-above` (vertical) or `glue*` (horizontal). The
   definition is stored in `virt` under the template name `<head-root-#>`;
   `#` is replaced by `N` by the virtual font machinery, so larger `N` yields
   longer extenders. Because the virtual font caches compiled definitions,
   the existing instance is evicted from `font::instances` whenever a new
   glyph is added, and subfont 6 is re-created (marked FIXME in the code);
5. otherwise falls back to the legacy `search_font_sub`, and if that yields
   subfont 0 (meaning "not handled") uses subfont 5.

`virtual_font.cpp` gains the `hor-take` primitive (mirror of `ver-take`) in
both the bitmap compiler and the vector `draw_tree` path.

### 3.6 Typesetter changes

- `frac_box` (`math_boxes.cpp`) receives a `disp` flag (threaded from
  `typeset_frac` in `concat_math.cpp`). With an OpenType font and a
  non-zero `frac_num_gap_min`, the numerator is placed at
  `max (shift_up, bar + gap_min ...)` and the denominator symmetrically,
  using the display or text values of the four shift and gap constants. The
  rule thickness still comes from `wline`.
- `sqrt_box` uses `sqrt_rule_thickness` for the overline, `sqrt_extra_ascender`
  above it, `sqrt_degree_rise_percent` and `sqrt_kern_after_degree` for the
  index. `typeset_sqrt` uses `sqrt_ver_gap` or `sqrt_ver_disp_gap` for the
  gap between the radicand and the overline.
- `lim_box` (`script_boxes.cpp`) uses the four limit constants for the
  distance between a big operator and its limits.

Each consumer guards with `math_type == MATH_TYPE_OPENTYPE` and a non-zero
constant, so fonts with a degenerate MATH table fall back to the old code.

## 4. Status (updated 21 September 2026)

Work done on top of the port, in the order of the plan below:

- **Build.** The tree compiles again with Apple clang 17 after fixing a
  pre-existing template bug in `src/Kernel/Containers/hashtree.cpp`. The
  generated `src/makefile` also needed its macOS SDK paths refreshed and the
  removed AGL framework dropped; those are local configuration fixes, not
  source changes. Note that this configuration has no dependency tracking
  (`src/Deps` holds only a stamp): after a header change, objects must be
  removed by hand or they link with a stale vtable and crash.
- **Parser.** NULL offsets for the italic correction, top accent and kern
  info sub-tables are honored; before, fonts without MathKernInfo (TeX Gyre,
  Latin Modern) made the parser read garbage.
- **Unit conversion.** Design units are converted through `units_per_EM`,
  the font size and the horizontal or vertical dpi, with `tm_round`.
- **Constants in use.** In addition to the fraction, radical and limit
  constants of the port, the Unicode font now sets from the MATH table:
  `yfrac` from `axisHeight`; `wline` from `fractionRuleThickness` (also used
  by `frac_box` for the bar); `ysub_lo_base`, `ysub_hi_lim`, `ysup_lo_lim`,
  `ysup_lo_base` and `yshift` from `subscriptShiftDown`, `subscriptTopMax`,
  `superscriptBottomMin`, `superscriptShiftUp` and
  `superscriptShiftUpCramped`. The display denominator gap bug is fixed.
- **Display operators.** `<big-x-2>` selects the smallest vertical variant
  whose advance is at least `displayOperatorMinHeight`, or the largest one,
  instead of always the second variant.
- **Kerning at the script height.** New hooks `get_*_correction_at (s, h)`
  on `font_rep` and `*_correction_at (h)` on `box_rep`, with defaults that
  fall back to the height-less versions and delegations in the smart font,
  the rubber font, and the concat, modifier, change and wide boxes.
  `side_box_rep` evaluates the base's correction at the facing edge of the
  script and the script's correction at the facing edge of the base, which
  is what MathKernInfo expects. The height-less versions still evaluate at
  the font ascender or descender for callers that have no position.
- **Resource names.** The MATH-aware rubber font is `rubberunicode-ot[...]`,
  distinct from the legacy `rubberunicode[...]` it falls back to.

Not changed: activation is still implicit through the family-name ladder in
`unicode_font.cpp`. TeX Gyre Math and the STIX text fonts keep their
hand-tuned tables; `STIXMath-Regular` does not match the `STIX-` prefix and
already takes the OpenType path.

The uncommitted debugging edits of the original worktree (font database
prints, the `get_unicode_range` experiment) were dropped.

## 5. Tests

- `tests/Plugins/Freetype/tt_tools_test.cpp`: the parser against values
  extracted with fontTools from the shipped `texgyrepagella-math.otf`
  (constants including a negative one, glyph info, vertical and horizontal
  variants and assemblies, `get_init_glyphID`) and, when
  `TM_TEST_FONT_DIR` provides STIX Two Math, the MathKern lookup with its
  height intervals.
- `tests/Graphics/Fonts/opentype_font_test.cpp`: activation and
  `math_type` of the shipped fonts, constant conversion and its linearity in
  the size, italic correction, monotone rubber variants and assemblies for
  `<left-(-N>`, display operator sizes, and kerning at several heights
  through the Unicode font and the smart font (Latin Modern Math and STIX Two
  Math from `TM_TEST_FONT_DIR`).
- `tests/opentype/render-samples.sh` with `samples/math-overview.tm`: the
  same formulas in TeX fonts, TeX Gyre Pagella, STIX, Latin Modern Math,
  STIX Two Math, Asana Math, Fira Math, KpMath, TeX Gyre DejaVu Math and Neo
  Euler, rendered to PNG per revision for side-by-side inspection and pixel
  diffs.

How to run everything, from the top of the tree after `make`:

```
make -C tests check-stale
make -C tests TM_TEST_FONT_DIR=/path/to/fonts
TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/render-samples.sh
```

- **Assemblies per specification.** Variant number `nvar + k - 1` of a
  stretchable glyph repeats every extender part `k` times; consecutive parts
  overlap by `minConnectorOverlap`, limited by their connector lengths. The
  definitions are ordinary `glue-above` / `glue*` trees with a negative
  separation (a third argument was added to `glue*`), so both the bitmap and
  the vector drawing paths work. All sizes up to 64 repetitions of a glyph
  are defined at once, so the virtual font holding them is rebuilt once per
  glyph instead of once per size. The overshooting bars of the sample are
  gone.

## 6. Known defects still open

1. The delimiter search still probes `<left-x-N>` for increasing `N` and
   measures; assemblies grow by one extender per step, so the chosen size
   can exceed the request by up to one extender. Parts are glued on their
   ink boxes rather than on their advances.
2. `parse_variant` requires exactly three dash-separated tokens.
3. `ysup_hi_lim` has no MATH counterpart and is set to
   `max (superscriptShiftUp, x-height)`.
4. Script sizes still come from `script ()` (2/3 per level), not from
   `scriptPercentScaleDown`; the environment computes them before the font
   is known.
5. The radical sign of Latin Modern Math shows a gap to its overline, and
   the root index of Asana Math sits too far left: the radical constants
   need a closer look.

## 7. What is still missing

### 7.1 Constants parsed but unused

| Group | Constants | Where they would apply |
|---|---|---|
| Scripts | `superscriptBaselineDropMax`, `subscriptBaselineDropMin`, `subSuperscriptGapMin`, `superscriptBottomMaxWithSubscript`, `spaceAfterScript` | `side_box_rep`: the drop limits for tall bases, the gap between a sub- and a superscript (now `fn->sep`), the space after a script |
| Axis | `mathLeading` | not needed by TeXmacs |
| Accents | `accentBaseHeight`, `flattenedAccentBaseHeight` | `wide_box_rep` accent placement and flattened accent selection |
| Stacks | `stackTopShiftUp`, `stackTopDisplayStyleShiftUp`, `stackBottomShiftDown`, `stackBottomDisplayStyleShiftDown`, `stackGapMin`, `stackDisplayStyleGapMin`, `stretchStack*` | `stack` / `binom` style constructions and `above`/`below` |
| Bars | `overbarVerticalGap`, `overbarRuleThickness`, `overbarExtraAscender`, `underbar*` | `<wide-bar>`, `<wide-underline>` |
| Skewed fractions | `skewedFractionHorizontalGap`, `skewedFractionVerticalGap` | not a TeXmacs primitive today |
| Radicals | `radicalKernBeforeDegree` | commented out in `sqrt_box` |
| Sizes | `scriptPercentScaleDown`, `scriptScriptPercentScaleDown` | `get_script_size` in the environment |
| Operators | `delimitedSubFormulaMinHeight` | minimum delimiter size |

### 7.2 Glyph information not used

- **Top accent attachment**: parsed into `top_accent`, never read. Accents
  are still centered on the ink box with `above_correct` tables.
- **Extended shape coverage**: parsed, never read. The spec uses it to keep
  superscripts on tall delimiters from being raised.
- **Italic correction of assemblies** and the per-variant advance
  measurements (except for display operators) are ignored.
- **Device tables** are not applied.

### 7.3 Variants and assemblies

- Let `get_delimiter` (`text_boxes.cpp`) ask the font for the smallest
  variant or assembly reaching a target height instead of probing
  `<left-x-N>` and measuring; with a target height the connector overlaps
  can be stretched to fit exactly, as the specification intends.
- Route horizontal variants through `wide_box` / `get_wide` for wide
  accents, braces and arrows; today only rubber names reach them.
- `<big-x-N>` for `N > 2`, and the interplay with `supports_big_operators`
  (still name based).

### 7.4 Activation and integration

- `is_math_family` in `smart_font.cpp` is a fixed list (`roman`, `concrete`,
  `Euler`, `ENR`). For any other family, letters in math mode are routed to
  the `fast-italic` text font and Unicode math alphanumerics are rewritten,
  so an OpenType math font never supplies its own italic letters, and their
  italic corrections and cut-in kerns are lost. Fonts with a MATH table
  should be treated as math families, with letters mapped to the plane 1
  code points of the same font.
- Let STIX and TeX Gyre Math use the MATH table, then compare against the
  hand-tuned tables and retire what the MATH data replaces.
- Replace the family-name tests in `poor_rubber.cpp`, `concat_math.cpp` and
  `math_boxes.cpp` with `math_type` checks.
- A preference to enable or disable MATH-table typesetting for comparison.

### 7.5 Beyond the MATH table

- GSUB `ssty` script-style alternates and `dtls`.
- GPOS kerning instead of the legacy `kern` table.
- Bypass the virtual bold and blackboard-bold emulation for fonts with
  complete plane 1 alphabets.
- Verify `<@XXXX>` glyphs and assemblies in PDF, PostScript and SVG export.

### 7.6 Engineering

- Enable dependency tracking in the autotools build, or add a rule that
  invalidates objects on header changes.
- Cache assembled glyphs without rebuilding the virtual font.
- Extend the sample documents (accents, limits in text style, left scripts,
  primes) and keep reference PNGs for the pixel diff.

## 8. Suggested order of the remaining work

1. Direct variant and assembly selection by target height in
   `get_delimiter`, and radical placement.
2. Horizontal variants for wide accents and braces, with top accent
   attachment.
3. Remaining script constants (`subSuperscriptGapMin`, drop limits,
   `spaceAfterScript`) and `scriptPercentScaleDown`.
4. Enable the path for STIX and TeX Gyre Math and compare against the
   hand-tuned output.
5. GSUB `ssty` and GPOS kerning, which require a small OpenType layout
   reader alongside the MATH parser.
