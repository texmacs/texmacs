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
hand-tuned tables, which is intended: those tables are crafted against
TeXmacs's layout and take precedence over the font data wherever both
exist. `STIXMath-Regular` does not match the `STIX-` prefix and already
takes the OpenType path.

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

- **Cache invalidation.** When new assemblies are added, the rubber font
  now resets the `font_metric` and `font_glyphs` resources of the virtual
  font as well as the font itself; they share its name and were sized for
  the earlier definitions, which produced wrong glyphs for every glyph after
  the first one. Assemblies start at the number of repetitions that exceeds
  the largest pre-drawn variant, so sizes grow monotonically with the
  variant number, as the delimiter search assumes.

- **Phase 1 of the plan (21 September 2026).** Delimiters are chosen by
  target height through `get_rubber_variant`, with assemblies made to
  measure from the advances of the parts and their measured lengths.
  Scripts follow the specification: standard shifts for ordinary glyphs,
  height-based shifts within the baseline drop limits for boxes and
  extended shapes (`is_extended_shape` hook and `extended_shape` box
  method), `subSuperscriptGapMin` with the TeX resolution of conflicts,
  `spaceAfterScript`, and script sizes from `scriptPercentScaleDown`
  unless `math-font-sizes` is set. Display operators are capped at two em.
  `above` and `below` are limit boxes and already use the limit constants,
  so the stack constants are not needed. All `font_rep` MATH fields are
  zero-initialized; before, they were read uninitialized for fonts without
  a MATH table.

- **Phase 2 of the plan.** Wide accents, braces and long arrows come
  from the horizontal variants and assemblies of the table through
  `get_wide_variant` (names `<wide-name-N>`, `<rubber-name-N>`, with a
  table mapping TeXmacs accent and arrow names to the combining marks and
  base arrows that carry the variants). Accents are attached at the top
  accent attachment points of the base and of the accent (`get_top_accent`
  hook and `top_accent` box method) and raised by the excess of the base
  height over `accentBaseHeight`. The line-drawn shapes remain the fallback
  for fonts without horizontal variants. Native glyphs without advance
  (combining marks) are now accepted by `supports`. Flattened accents
  (`flac`) still need the GSUB reader.

- **Phase 3, first step.** When the main font of a smart font is an
  OpenType math font, letters in math mode are rewritten to its
  mathematical italic alphabet (`REWRITE_MATH_ITALIC`, subfont `ot-italic`
  which is the main font itself) instead of being taken from the text
  italic face, and lowercase Greek uses the font's italic Greek. Italic
  corrections and cut-in kerns therefore apply to letters. The profile
  table, alphabets, GSUB features and menus are still to do.

- **Phase 4 of the plan.** The MATH table is loaded before the per-family
  branches of the Unicode font constructor (`init_ot_math`), and a new
  `font_rep::ot_math` flag says so; `math_type` keeps its tuned value
  (TeX Gyre, STIX) so that every hand-tuned check still fires and the
  correction tables keep precedence, while fractions, radicals, limits,
  script shifts, delimiter variants and assemblies come from the table.
  Wide accents and math letters stay with the tuned tables for tuned fonts.
  The sample diff shows the roman row unchanged and only sizes of nested
  delimiters and gaps changing for TeX Gyre Pagella.

- **GSUB features.** `parse_gsub_feature` reads the single and alternate
  substitutions of one feature (lookup types 1, 3 and 7), cached per face;
  `get_feature_variant (s, feature, alt, r)` exposes them on fonts as
  native glyph names. Used for `dtls` (dotless i and j under accents, in
  `typeset_wide`) and `flac` (flattened accents over bases taller than
  `flattenedAccentBaseHeight`). Narrow accents of untuned OpenType fonts
  are placed by `accentBaseHeight` and the attachment points as well.
  `ssty` is read but not yet applied: the font does not know whether it is
  used at script size, which needs a flag from the environment.

- **Smart font size queries.** Unnumbered rubber names and their
  numbered sizes can resolve to different subfonts of a smart font (the
  radical of the shipped STIX setup is served by a fallback font); the
  size queries now go to the subfont which renders the numbered names.
  The `starts (res_name, "stix-")` checks of `concat_math.cpp` and
  `use_poor_rubber` never matched the capitalized smart font names
  ("Stix-..."), so the hand-tuned `rubber_stix_font` path for radicals and
  large delimiters was dead code; the checks are case-insensitive now and
  the tall STIX radical of the showcase renders correctly.

- **Phase 3, profiles.** `math_font_profiles.cpp` holds the per-font
  profile table, filled at boot from
  `TeXmacs/progs/fonts/fonts-opentype.scm` (twenty fonts). `profile_fix`
  in `smart_font_bis` replaces a text family by its math companion in math
  shapes when the math font is installed, and a math family by its text
  companion otherwise, so `<with|font|Latin Modern Roman>` typesets
  formulas in Latin Modern Math; the `letters` key can keep letters in the
  text italic. The "Mathematical font" menu lists the installed profiled
  fonts. Rubber names `<wide-...>` and `<rubber-...>` are resolved in the
  rubber font before the emulated glyphs for OpenType math fonts, and long
  arrows missing from a font stretch the plain arrow.
  Also: `tests/opentype/samples/math-showcase.tm` tours every feature per
  font; the local font database of the test home had been built only
  partially and was rebuilt (with TeX Live now on the font path, a full
  build takes a long time).

- **Phase 5, partly.** `feature_font (base, feature, alt)` is a font
  decorator which replaces every glyph by the alt-th substitute of a GSUB
  feature; the environment wraps untuned OpenType math fonts in it with
  `ssty` at script levels, so scripts use the script-size alternates.
  `<neg|...>` of a relation which Unicode encodes as a single negated
  symbol typesets that symbol when the (untuned OpenType) font has it, and
  strikes through otherwise. `tests/opentype/check.sh` runs the unit tests
  and both sample renders. Bold mathematics comes for free through the
  font database when the math family has a Bold style (New Computer Modern
  Math, KpMath); it is not yet selected explicitly.

- **Export check.** In the exported PDFs of the samples every TrueType and
  OpenType font, tuned or not, is embedded as a Type 3 bitmap font (the TeX
  fonts as Type 1), so native glyphs and assemblies export like any other
  glyph. Whether this build should embed outlines for OpenType fonts is a
  separate question of the PDF writer, not of the math work.

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
- Let STIX and TeX Gyre Math use the MATH table for what their hand-tuned
  tables do not cover (delimiter variants and assemblies, fraction,
  radical and limit constants). The hand-tuned tables are better than the
  font data and keep precedence: MATH activation must happen before the
  per-family branches of the constructor ladder, and a correction table
  entry must win over the MATH italic correction and kern for that glyph.
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

## 8. Hand-made constructions in the typesetter and their MATH counterparts

TeXmacs draws several parts of formulas itself, with `line_box`, `arc_box`,
magnified glyphs or virtual fonts, because TeX fonts and ordinary text
fonts do not provide them. An OpenType math font provides most of them.
This section lists the hand-made constructions, where they live, and what
the MATH table offers instead. The hand-tuned corrections stay in charge
wherever they exist; what follows is about the constructions that today
are synthesized geometrically.

### 8.1 Wide accents

`compute_wide_accent` in `src/Typeset/Boxes/Composite/math_boxes.cpp`
decides how to render `<wide|x|^>`, `<wide|x|~>`, bars, vectors, checks,
breves and the over- and underbraces:

- narrow bases get the accent glyph, shifted by heuristics on the slope of
  the base (`ref->rsup_correction () + slope * yx / 2`) and clamped
  between `yx/8` and `yx/3` above the base;
- TeX Gyre uses `<wide-hat-N>` rubber names, which `tex_gyre_operators`
  in `unicode_font.cpp` maps by hand to the font's `.h1` to `.h6` glyphs,
  and `get_wide` in `text_boxes.cpp` probes widths;
- STIX uses `wide_stix_box` and `get_wide_stix` with its own names;
- other Unicode fonts magnify the accent glyph horizontally
  (`fn->magnify (sx, sy)`) up to a limit, then fall back to
  `wide_hat_box`, `wide_tilda_box`, `wide_bar_box`, `wide_vect_box`,
  `wide_check_box`, `wide_breve_box`, `wide_squbr_box`, `wide_sqobr_box`
  in `src/Typeset/Boxes/Basic/stretch_boxes.cpp`, which draw the shapes
  from lines and arcs with the pen width `wline`.

The MATH table has all of this: horizontal variants and assemblies for
U+0302 circumflex, U+0303 tilde, U+0305 overline, U+0332 underline,
U+20D7 vector, U+030C check, U+0306 breve, U+0311 inverted breve,
U+23DE / U+23DF over- and underbrace, U+23B4 / U+23B5 square brackets and
U+23DC / U+23DD parentheses (Pagella Math has 86 glyphs with horizontal
variants, 71 with assemblies); the `topAccentAttachment` of both the base
glyph and the accent, which replaces the slope heuristics; the constants
`accentBaseHeight` and `flattenedAccentBaseHeight`, with the GSUB feature
`flac` for the flattened accents used over tall bases; and
`overbarVerticalGap`, `overbarRuleThickness`, `overbarExtraAscender` and
their `underbar` twins for bars.

How to implement:

1. A mapping from TeXmacs accent names to the combining code points
   (`hat` to U+0302, `tilde` to U+0303, `bar` to U+0305, `vect` to U+20D7,
   `check` to U+030C, `breve` to U+0306, `invbreve` to U+0311, `overbrace`
   to U+23DE, `underbrace` to U+23DF, `sqoverbrace` to U+23B4,
   `squnderbrace` to U+23B5, `poverbrace` to U+23DC, `punderbrace` to
   U+23DD). TeXmacs's `<hat>` converts to a spacing modifier letter, not to
   the combining mark that carries the variants, so the table is needed.
2. In `rubber_unicode_font.cpp`, accept `<wide-name-N>` in
   `search_font_sub_opentype` through that mapping; the horizontal variants
   and assemblies are already handled generically (`glue*` with overlaps).
   Better than probing `N`: a new font hook `get_wide_variant (s, width)`
   that returns the smallest variant whose advance measurement reaches the
   width, or the assembly with the right number of repetitions, using the
   advance measurements the parser already stores.
3. A new font hook `get_top_accent (s)` on `font_rep`, defaulting to the
   center of the ink box and implemented from `top_accent` in the Unicode
   font, with the usual delegation through the smart and rubber fonts and a
   `top_accent ()` box method (single glyph boxes return the font value,
   others their center).
4. A `MATH_TYPE_OPENTYPE` branch in `compute_wide_accent`, before the
   generic Unicode branches: pick the variant by width, place it
   horizontally so that the two attachment points coincide, vertically at
   `max (base height, accentBaseHeight)` for narrow bases, and use the
   overbar constants for `<bar>`; apply `flac` when the base is taller than
   `flattenedAccentBaseHeight` (needs a small GSUB single-substitution
   reader). The line-drawn shapes then remain as the fallback for fonts
   without horizontal variants.

### 8.2 Long arrows and wide relations

`typeset_long_arrow` builds `<long-arrow|...>` with `wide_box` on the
arrow's own name, so it depends on `<name-N>` variants that only TeX fonts
and the TeX Gyre tables provide, and stacks the labels with `limit_box`.
Math fonts have horizontal variants and assemblies for the arrows of
U+2190 to U+21FF (in Pagella, `arrowright` has one variant and a
three-part assembly). The same `get_wide_variant` hook serves here, and the
labels above and below should use `stretchStackTopShiftUp`,
`stretchStackBottomShiftDown`, `stretchStackGapAboveMin` and
`stretchStackGapBelowMin`, which exist precisely for this construction.

### 8.3 Radicals

`sqrt_box` combines a `<large-sqrt-N>` delimiter with a `line_box` for
the overline. The delimiter now comes from the MATH variants and assembly,
and the rule thickness, gap, extra ascender and degree placement come from
the radical constants. What is still hand-made is the junction: the rule is
drawn at `sqrtb->y2 + dy` independently of the glyph, which in Latin Modern
leaves a gap because the radical glyph's top does not reach the rule. Per
the specification the rule starts at the top of the radical glyph and has
`radicalRuleThickness`; the box should take the rule's vertical position
from the glyph extents (the assembly's top part is designed to meet the
rule), and `radicalKernBeforeDegree` should be applied.

### 8.4 Fractions and wide fractions

The bar is a `line_box`; it now has the table's thickness and the
numerator and denominator follow the table's shifts and gaps. Wide
fractions (`typeset_wide_frac`) fall back to a slash `<mid-/-N>`, which the
MATH variants of U+2215 provide; `skewedFractionHorizontalGap` and
`skewedFractionVerticalGap` describe how to place numerator and denominator
around it should TeXmacs gain a skewed fraction primitive.

### 8.5 Delimiters, middle bars and brackets

`delimiter_box`, `typeset_wide_middle` and `bracket_box` (line-drawn
brackets for `<left-.>` style TeX cases) all reduce to `<left-x-N>` names,
which the rubber font now serves from variants and assemblies. The
remaining hand-made part is the size search in `get_delimiter`: with a
`get_delimiter_variant (s, height)` hook the typesetter could ask for the
smallest variant or the exact assembly directly, and stretch the connector
overlaps to fit, as the specification intends. `delimitedSubFormulaMinHeight`
gives the minimum size for delimiters around sub-formulas, and the
extended shape coverage tells which delimiters and operators should not
have their superscripts raised.

### 8.6 Stacks: above, below, limits, binomials

`typeset_above`, `typeset_below` and `lim_box` place material above and
below a base with `fn->sep` and `yshift`; limits already use the four
limit constants. The stack constants (`stackTopShiftUp`,
`stackBottomShiftDown`, their display variants and gaps) are the
counterpart for `above`, `below`, `stack` and binomials without a bar.

### 8.7 Negations

`neg_box` strikes a diagonal `line_box` through the box. Unicode has
precomposed negated symbols (U+2260, U+2209, U+2288 and about sixty more,
listed in the `unicode-math` table) which every math font draws better than
a stroke. TeXmacs already has `tradi-negate.vfn` for the reverse direction;
the improvement is to map `<neg|x>` to the precomposed code point when the
font supports it, and keep the stroke as the fallback.

### 8.8 What has no counterpart

`tree_box` (syntax trees), the `syntax` decorations, dotted and dashed
rules, and the emulated bold and blackboard bold letters have nothing in
the MATH table; the alphabets should simply come from the font when it has
them (see the survey document), and the rest stays as it is.

## 9. Plan to complete the OpenType support

Principles: the hand-tuned tables keep precedence everywhere; every step
lands with a unit test or a sample row; the "hand tuned math fonts" switch
is the comparison tool. Sizes are S (a day or less), M (a few days),
L (a week or more). Phases 1 and 2 are independent of 3; phase 4 needs 3.

### Phase 0: groundwork (S)

- Turn dependency tracking on in the autotools build, or add a rule that
  invalidates objects on header changes (`check-stale` is a stopgap).
- Replace the hard-coded TeX Live years in `tt_font_path` by a glob or
  `kpsewhich`, so system math fonts are found.
- Document the family name normalization of `tt_font_name` ("STIX Two
  Math" becomes "Stix Two Math"), which is why the sample uses that
  spelling; no alias mechanism is needed.
- Push the branch; keep `master` merges small.

### Phase 1: finish the table-driven layout (M)

1. `get_delimiter_variant (s, height)` hook on `font_rep`: the rubber font
   returns the smallest pre-drawn variant reaching the height, or the
   assembly with the exact number of repetitions, stretching connector
   overlaps to fit. `get_delimiter` uses it for `MATH_TYPE_OPENTYPE` fonts
   instead of probing; `delimitedSubFormulaMinHeight` gives the minimum.
2. Radical junction: rule position from the radical glyph's top,
   `radicalKernBeforeDegree`; fixes the Latin Modern gap.
3. Remaining script constants: `subSuperscriptGapMin`,
   `superscriptBaselineDropMax`, `subscriptBaselineDropMin`,
   `spaceAfterScript`; `scriptPercentScaleDown` through
   `get_script_size`, which needs the font before the size is computed.
4. Stack constants for `above`, `below`, `stack` and binomials.
5. Extended shape coverage: no raised superscripts on tall delimiters and
   operators.
6. Display operator cap (`display-operator-max`) to tame fonts like IBM
   Plex Math.

Tests: expected heights for `<left-(-N>` at given target heights, radical
rule position, script positions on STIX Two Math and Latin Modern Math;
sample rows checked against LuaLaTeX with `unicode-math` for the same
formulas.

### Phase 2: wide accents and horizontal constructions (M)

1. Table of TeXmacs accent names to combining code points (section 8.1).
2. `get_wide_variant (s, width)` hook, shared by `wide_box`,
   `typeset_long_arrow` and the braces; `<wide-x-N>` in the rubber font
   goes through the horizontal variants and assemblies already
   implemented.
3. `get_top_accent (s)` hook and `top_accent ()` box method; accent
   placement in `compute_wide_accent` from the attachment points,
   `accentBaseHeight`, and the overbar constants for `<bar>`.
4. Small GSUB reader for single substitutions, used for `flac` (flattened
   accents) here and for `ssty` and `dtls` in phase 3.
5. `stretchStack*` constants for the labels of long arrows.

Tests: widths of `<wide-hat-N>` variants and assemblies from the table;
a sample row of accents over letters and over wide bases in every font.

### Phase 3: letters, alphabets and font profiles (L)

1. The math font profile table of the survey document, in Scheme, read at
   boot: family, file, aliases, text companions, bold math variant, letter
   routing, real alphabets, rubber policy, display cap, menu placement,
   quirks.
2. `is_math_family`, `tex_gyre_fix`, `math_fix`, `supports_big_operators`
   and the `font_translate` aliases consult the profiles; letters in math
   mode are rewritten to the plane 1 code points of the math font when the
   profile says so, which makes italic corrections and MathKernInfo
   effective for letters (today they only reach digits and symbols).
3. Real alphabets from the font when present; virtual emulation only for
   the missing ones (script and double-struck are the usual gaps).
4. `ssty` alternates for script sizes and `dtls` under accents through the
   GSUB reader; GPOS pair kerning through a small GPOS reader, since math
   fonts have no legacy `kern` table.
5. Font menus generated from installed profiles; `math-*` legacy values
   kept as aliases.
6. Profiles for Tier 1: Latin Modern, New Computer Modern, TeX Gyre (five),
   STIX Two, XITS, Libertinus, KpMath.

Tests: a profile validation test (files exist, names match, companions and
declared alphabets present); kerning through the smart font on letters,
which the current test could not do.

### Phase 4: the shipped fonts under their hand tuning (M)

1. Reorder the constructor ladder so MATH activation runs first and each
   hand-tuned branch overrides its own fields (`yfrac`, script shifts,
   corrections, integral spacing); a correction table entry wins over the
   table's italic correction and kern for that glyph.
2. TeX Gyre Math and STIX get delimiter variants, assemblies and the
   constants they never had, with their corrections untouched. Compare
   tuned and untuned renders of every sample row; anything the tables tune
   must be pixel-identical to before.
3. Ship STIX Two Math and its text faces, and Latin Modern Math with four
   text faces; route the `stix` family to STIX Two through a profile while
   keeping STIX v1 for old documents.

### Phase 5: polish (M)

- Bold mathematics from real bold math fonts (New Computer Modern, KpMath,
  XITS) through the profile's `bold-math` field.
- Negations mapped to precomposed Unicode symbols when available.
- Verify `<@XXXX>` glyphs and assemblies in PDF, PostScript and SVG
  export; embedded subsets must contain the variant glyphs.
- Cache assembled glyphs without rebuilding the virtual font; measure
  startup and typesetting time with a large document.
- Device tables are deliberately left out.

### Phase 6: tests and documentation (S, continuous)

- Reference PNGs for the pixel diff of the samples, refreshed on purpose.
- A kerning and accents sample next to `math-overview.tm`.
- `make -C tests` and both renders in a script that can run before every
  commit; the design and survey documents updated as steps land.

### Definition of done

A document set in Latin Modern Math, New Computer Modern Math or STIX Two
Math, with letters, scripts, accents, delimiters and operators, renders
with no hand-drawn construction and compares well with LuaLaTeX's
`unicode-math` output of the same source; TeX Gyre and STIX documents
render exactly as before with the switch on; every installed Tier 1 font
has a profile and passes the profile test; all unit tests and both sample
renders pass.
