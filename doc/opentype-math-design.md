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

## 4. Status

- The code compiled and ran on 28 December 2024 (binary in `TeXmacs/bin`).
  With Apple clang 17 the tree does not build because of a pre-existing bug in
  `src/Kernel/Containers/hashtree.cpp:97` (`*this->contains (key)` must be
  `(*this)->contains (key)`), identical on `master`. The branch itself has not
  been compiled with the current toolchain.
- Uncommitted changes in the worktree are debugging leftovers and should not
  be committed as is: `font_database.cpp` enables two `cout` prints,
  `find_font.cpp` adds a `debug_fonts` line, `tt_face.cpp` comments one out,
  `smart_font.cpp` makes `get_unicode_range` return an empty string for
  the math alphanumerics block (which changes fallback behaviour for every
  font), and `unicode_font.cpp` only moves a declaration.
- No tests, no documentation and no user-visible switch exist. Activation is
  implicit and depends on the font family not matching an earlier branch of
  the constructor ladder.

## 5. Known defects in the current code

1. `unicode_font.cpp` around line 488: `frac_denom_disp_gap_min` is filled
   from `fractionDenominatorGapMin`; it must use
   `fractionDenomDisplayStyleGapMin`.
2. `unicode_font.cpp` around line 1155: `get_ot_italic_correction` prints to
   `cout` for every glyph with an italic correction.
3. `rubber_unicode_font (base, face)` and `rubber_unicode_font (base)` build
   the same resource name `rubberunicode[<base>]`. Whichever is created first
   is returned for both. Subfont 5 (the fallback) is meant to be the plain
   variant; today it resolves to `poor_rubber_font` because `has_poor_rubber`
   is true, but with that flag off it would resolve to the OpenType rubber
   font itself and recurse.
4. `tt_face_rep` reads the font file from disk a second time to parse the
   MATH table although the bytes are already in `buffer`.
5. The unit conversion through the advance of `m` introduces rounding error
   and depends on the font having an `m`.
6. Cut-in kerning is evaluated at the font ascender or descender rather than
   at the real script position.
7. Assemblies ignore `minConnectorOverlap` and the part connector lengths;
   extenders are tiled by `ver-take` with fixed proportions, so joins may
   overlap or gap depending on the font.
8. The virtual font holding assemblies is destroyed and rebuilt each time a
   new assembly is first used, which also discards all compiled glyphs.
9. The `override` specifier on `~tt_face_rep` was dropped.
10. `parse_variant` requires exactly three dash-separated tokens; a root that
    itself contains a dash would not parse (none exists today, but nothing
    checks).

## 6. What is still missing

### 6.1 Constants that are parsed but unused

| Group | Constants | Where they would apply |
|---|---|---|
| Scripts | `superscriptShiftUp`, `superscriptShiftUpCramped`, `superscriptBottomMin`, `superscriptBaselineDropMax`, `subscriptShiftDown`, `subscriptTopMax`, `subscriptBaselineDropMin`, `subSuperscriptGapMin`, `superscriptBottomMaxWithSubscript`, `spaceAfterScript` | `script_box_rep` in `script_boxes.cpp`; today driven by `ysub_*`, `ysup_*`, `yshift` |
| Axis | `axisHeight`, `mathLeading` | `yfrac` (currently the middle of `-`), centering of delimiters and big operators |
| Accents | `accentBaseHeight`, `flattenedAccentBaseHeight` | `wide_box_rep` accent placement and flattened accent selection |
| Stacks | `stackTopShiftUp`, `stackTopDisplayStyleShiftUp`, `stackBottomShiftDown`, `stackBottomDisplayStyleShiftDown`, `stackGapMin`, `stackDisplayStyleGapMin`, `stretchStack*` | `stack` / `binom` style constructions and `above`/`below` |
| Bars | `overbarVerticalGap`, `overbarRuleThickness`, `overbarExtraAscender`, `underbar*` | `<wide-bar>`, `<wide-underline>` |
| Skewed fractions | `skewedFractionHorizontalGap`, `skewedFractionVerticalGap` | `tfrac`-like slanted fractions (not a TeXmacs primitive today) |
| Radicals | `radicalKernBeforeDegree` | parsed, commented out in `sqrt_box` |
| Sizes | `scriptPercentScaleDown`, `scriptScriptPercentScaleDown` | `script (sz, level)` in `font.cpp` uses a fixed 2/3 |
| Operators | `displayOperatorMinHeight`, `delimitedSubFormulaMinHeight` | choice of `<big-x-1>` versus `<big-x-2>` and minimum delimiter size |
| Fractions | `fractionRuleThickness` | stored as `frac_rule_thickness` but `frac_box` still uses `wline` |

### 6.2 Glyph information not used

- **Top accent attachment**: parsed into `top_accent`, never read. Accents
  are still centered on the ink box with `above_correct` tables.
- **Extended shape coverage**: parsed, never read. The spec uses it to keep
  superscripts on tall delimiters from being raised.
- **Kerning at the right height**: needs the correction API to receive the
  script box (or its baseline offset) instead of guessing with `y1`/`y2`.
- **Italic correction of assemblies** (`GlyphAssembly.italicsCorrection`) and
  the per-variant advance measurements are ignored.
- **Device tables** are not applied. They matter little at high dpi but
  affect on-screen rendering at small sizes.

### 6.3 Variants and assemblies

- Assemblies should be laid out per the specification: compute the number of
  extender repetitions from the target size, overlap connectors by at least
  `minConnectorOverlap`, and respect `startConnectorLength` and
  `endConnectorLength`. This requires a virtual font primitive that receives a
  target length rather than a variant number, or building the glyph directly
  in the rubber font instead of through the virtual font language.
- The delimiter search (`get_delimiter` in `text_boxes.cpp`) still probes
  `<left-(-N>` for increasing `N` and measures the result. With MATH variants
  the font knows the available sizes and the assembly can produce an exact
  height; the search could be replaced by a direct query for fonts of
  `MATH_TYPE_OPENTYPE`.
- Horizontal variants are only reachable through rubber names. Wide accents
  and braces go through `wide_box` / `get_wide` and `wide_box_rep`, which do
  not consult the rubber font unless the font is STIX. The horizontal MATH
  variants (for `<wide-hat>`, `<wide-tilde>`, `<overbrace>`, `<underbrace>`,
  arrows) are therefore unused.
- It should be verified per font whether the first MathGlyphVariantRecord is
  the base glyph itself (common) or the first larger size, since this shifts
  every variant number by one.
- `<big-x-N>` operators for `N > 2` and the interplay with
  `supports_big_operators` (which is still name based) need a rule.

### 6.4 Activation and integration

- **Shipped fonts**: STIX and TeX Gyre Math must be able to use the MATH
  table. That means either removing the hand-tuned branches for those
  families when a MATH table exists, or letting the OpenType data fill the
  gaps that the tables leave. The hand-tuned data was calibrated against the
  old behaviour, so this is a visual regression exercise, not just a code
  change.
- **Family name matching**: `math_type` is set from the name prefix in
  `font_rep::font_rep` and by the Unicode font constructor; `poor_rubber.cpp`,
  `concat_math.cpp` and `math_boxes.cpp` still test the family name for
  `stix` and `agella`. These need to become `math_type` checks so that an
  OpenType font is handled uniformly.
- **Smart font subfonts**: the smart font forwards `make_rubber_font` to its
  main subfont only. Characters resolved from a secondary family (for example
  a `math=` sequence item) get the rubber font of that family, which is
  correct, but constants are copied only from the main font.
- **A user-visible switch** (preference or environment variable) to enable
  or disable MATH-table typesetting would ease comparison and debugging.

### 6.5 Beyond the MATH table

These are not part of MATH but are needed for OpenType math fonts to look
right:

- GSUB `ssty` feature for script-size alternates; GSUB `dtls` for dotless
  variants under accents.
- GPOS kerning (`kern` feature) instead of the legacy `kern` table, which
  most modern math fonts do not include.
- Math alphanumerics: fonts with a MATH table normally have complete plane 1
  alphabets; the smart font's rewriting into `<b-x>`-style names and virtual
  bold/blackboard-bold emulation should be bypassed for them (the uncommitted
  `get_unicode_range` change is a first, too broad, attempt at this).
- Export: verify that `<@XXXX>` glyphs and virtual assemblies round-trip
  through the PDF renderer (which embeds glyphs by `glyph->index`) and the
  PostScript and SVG exporters.

### 6.6 Engineering

- Unit tests for `parse_mathtable` against a known font (the tree ships
  `texgyrepagella-math.otf`) and for `get_kerning`.
- Remove the debug output and the uncommitted experiments; fix the defects
  listed in section 5.
- Parse the MATH table from the in-memory buffer already held by `tt_face`.
- Cache assembled glyphs without rebuilding the virtual font.

## 7. Suggested order of work

1. Fix the build (`hashtree.cpp`), the `frac_denom_disp_gap_min` bug, the
   stray `cout`, and the resource-name collision. Drop the debugging changes.
2. Replace the `m`-based unit conversion with `units_per_EM`.
3. Use the fraction rule thickness, `axisHeight` and the script constants;
   these give the most visible improvement for the least code.
4. Pass script heights to the kerning code.
5. Rework assemblies to follow the specification and query MATH variants
   directly from `get_delimiter` for OpenType fonts.
6. Route horizontal variants through `wide_box`.
7. Enable the path for STIX and TeX Gyre Math and compare against the
   hand-tuned output.
8. Add GSUB `ssty` and GPOS kerning support, which requires a small
   OpenType layout reader alongside the MATH parser.
