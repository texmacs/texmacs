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
     |  tt_face_rep ctor (tt_face.cpp): MATH, and on demand GSUB and GPOS
     v
  ot_mathtable  <---- parse_mathtable (tt_tools.cpp)
     |
     |  unicode_font_rep::init_ot_math (unicode_font.cpp), called BEFORE the
     |  per-family branches, which then override what they tune
     v
  unicode_font_rep                         font_rep fields
    ot_math = true                  ---->  frac_*, sqrt_*, *_limit_*,
    math_type = MATH_TYPE_OPENTYPE         stretch_stack_*, *bar_*, script_*
      (unless a tuned branch keeps           (about forty constants)
       its own math_type)
    math_face, math_table
    get_ot_italic_correction ------------> get_right_correction, get_rsup_correction
    get_ot_kerning ----------------------> get_[lr]su[bp]_correction_at (height aware)
    get_top_accent, is_extended_shape ---> top_accent, extended_shape on boxes
    get_feature_variant (GSUB) ----------> dtls, flac, and ssty through feature_font
    make_rubber_font --------------------> rubber_unicode_font (base, face)
                                             |
                                             |  search_font_sub_opentype,
                                             |  get_rubber_variant (by height),
                                             |  get_wide_variant (by width)
                                             v
                                   variants  -> "<@XXXX>" native glyph ids
                                   assembly  -> runtime translator + virtual_font
                                                (glue-above / glue*, ver-take / hor-take)

  Font choice:  smart_font.cpp (profile_fix, math italic letters, ssty)
                <---- math_font_profiles.cpp <---- fonts-opentype.scm

  Typesetter consumers: frac_box, sqrt_box, lim_box (limits and stretch
  stacks), side_box and script placement, wide_box and compute_wide_accent,
  concat_math (delimiters, big operators, long arrows, negations) and
  env_semantics (script sizes, ssty at script levels)
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

`tt_face_rep` keeps the file in memory (`buffer`, `buffer_size`) and holds
three parsed tables: `math_table`, filled by the constructor for every
`.ttf` and `.otf` face right after the `FT_Face` is created, and dumped to
`debug_fonts` in a verbose build; `gsub_features`, a map from feature tag to its substitutions,
filled on demand by `gsub_feature (tag)`; and `gpos_kern_table`, filled on
demand by `gpos_kern ()`. Since `tt_face` instances are cached resources,
each table is parsed once per font file.

`tt_font_metric_rep::kerning` asks the GPOS table first and falls back to the
legacy `kern` table of FreeType. No OpenType math font has a legacy `kern`
table, TeX Gyre Pagella included, so without GPOS they were all set with no
kerning at all.

### 3.3 Font activation (`src/Plugins/Freetype/unicode_font.cpp`)

The `unicode_font_rep` constructor has an `if / else if` ladder on the family
name that installs the hand-made correction tables of STIX, TeX Gyre,
Papyrus, Libertine, Biolinum and Fira. `init_ot_math (face)` runs **before**
that ladder, so a hand-tuned branch overrides the fields it tunes and leaves
the rest to the table. It:

- stores `math_face` and `math_table` on the font and sets the `ot_math`
  flag, which is what the typesetter tests;
- leaves `math_type` alone: the ladder sets it to `MATH_TYPE_OPENTYPE` in
  its final `else`, that is only when no tuned branch claimed the font, so
  every tuned check still fires and the tuned font keeps its own
  `math_type`;
- computes the design unit conversion;
- fills thirty-nine `font_rep` fields from the table: the limit constants
  (`upper_limit_gap_min`, `upper_limit_baseline_rise_min`,
  `lower_limit_gap_min`, `lower_limit_baseline_drop_min`), the stretch stack
  constants (`stretch_stack_top_shift_up`, `stretch_stack_bottom_shift_down`,
  `stretch_stack_gap_above_min`, `stretch_stack_gap_below_min`), the fraction
  constants (`frac_rule_thickness` and the eight numerator and denominator
  shifts and gaps), the radical constants (`sqrt_ver_gap`,
  `sqrt_ver_disp_gap`, `sqrt_rule_thickness`, `sqrt_extra_ascender`,
  `sqrt_degree_rise_percent`, `sqrt_kern_before_degree`,
  `sqrt_kern_after_degree`), the script constants (`sub_sup_gap_min`,
  `sup_drop_max`, `sub_drop_min`, `sup_bottom_max_with_sub`,
  `space_after_script`, `script_percent`, `script_script_percent`), the
  accent constants (`accent_base_height`, `flattened_accent_base_height`) and
  the six bar constants (`overbar_vertical_gap`, `overbar_rule_thickness`,
  `overbar_extra_ascender` and their `underbar` twins);
- sets the classical TeXmacs parameters the table can give: `yfrac` from
  `axisHeight`, `wline` from `fractionRuleThickness`, and the script shifts
  `ysub_lo_base`, `ysub_hi_lim`, `ysup_lo_lim`, `ysup_lo_base` and `yshift`.

All the fields are zero-initialized in `font.hpp`, so a font without a MATH
table reads zeros rather than garbage, and `copy_math_pars` propagates them to
derived fonts (smart fonts, rubber fonts, magnified fonts), so the typesetter
reads them from `env->fn`.

**Unit conversion.** The face is scaled to `size` points at `hdpi` by `vdpi`,
so one design unit measures `size * hpt / units_per_EM` vertically and
`size * wpt / units_per_EM` horizontally. `init_design_unit_factor` computes
both factors and `design_unit_to_metric`, `design_unit_to_metric_x` and
`metric_to_design_unit` apply them with `tm_round`. An earlier version took
the ratio of one glyph advance measured twice, which depended on the rounding
of that glyph.

**Consequence of the ladder position.** Because `init_ot_math` runs first,
the shipped STIX and TeX Gyre math fonts are on the OpenType path for
everything their tuned tables do not cover: they get the delimiter variants,
the assemblies and the constants they never had, while their corrections,
wide accents and integral spacing stay hand-tuned. The switch `(set-hand-tuned-math-fonts #f)` bypasses the tuned branches for
the fonts that have a MATH table, which is how the two can be compared; a
font without a table keeps its hand-made tables either way.

### 3.4 Glyph-level corrections

Native glyph addressing. `read_unicode_char` accepts `<@XXXX>` and returns
`0xc000000 + glyph id`, the same offset the FreeType layer already used for
`native` characters; `decode_index` in `tt_face.cpp` turns such codes into raw
glyph indices. `get_glyphID (s)` maps any TeXmacs character string to a glyph
id, either from the `<@XXXX>` form or through `index_glyph`.

Two different guards appear from here on, and the difference matters:
`font_rep::ot_math` says that the font has a MATH table, whether or not it is
hand-tuned, while `math_type == MATH_TYPE_OPENTYPE` says that no hand-tuned
branch claimed the font, so the table alone drives it. The geometry
constants, the stretchable glyphs, the top accent attachment and the GSUB
features use the first. The per-glyph corrections use the second, because a
hand-tuned font has correction tables of its own that must win; so do the
narrow accents and the precomposed negations.

For a font with `math_type == MATH_TYPE_OPENTYPE`:

- `get_right_correction` returns the MATH italic correction of the last
  glyph when present (`get_ot_italic_correction`).
- `get_lsub_correction`, `get_lsup_correction` return the bottom-left and
  top-left cut-in kern of the first glyph (`get_ot_kerning`).
- `get_rsub_correction`, `get_rsup_correction` combine the bottom-right or
  top-right kern with the italic correction. A subscript gets no italic
  correction at all unless the glyph is an integral (detected by
  `is_ot_integral` via a fixed list of integral names mapped to glyph ids and
  reduced to base glyphs with `get_init_glyphID`), where 60 percent of it is
  applied; a superscript gets the full correction, or 40 percent of it over
  an integral.

The MathKern lookup needs the height at which the script attaches, which the
correction API did not have. The four corrections therefore exist in a second
form, `get_*_correction_at (s, h)`, with `*_correction_at (h)` on boxes;
`side_box_rep` evaluates the base's correction at the facing edge of the
script and the script's correction at the facing edge of the base, which is
what MathKernInfo is for. The height-less versions evaluate at the ascender
or the descender, for callers that have no position, and the smart, rubber
and derived fonts forward both forms.

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
   `head` (`left`, `mid`, `right`, `large`, `big`, `wide`, `rubber`), `root`
   (the delimiter, accent or operator name, which may itself contain dashes)
   and the variant number `N`, the last dash-separated token; `<big-...>`
   numbers are shifted down by one because there is no `<big-x-0>`;
2. converts the root to a code point and then to a glyph id with
   `ft_get_char_index`;
3. if the glyph has vertical or horizontal variants and `N` is within range,
   rewrites the string to `<@XXXX>` for the Nth variant and returns subfont 0.
   The typesetter no longer probes sizes: `get_rubber_variant (s, h)` and
   `get_wide_variant (s, w)` return the smallest variant whose advance
   reaches a target height or width, or the assembly with the number of
   repetitions that does, from the advances the parser stores;
4. otherwise, if it has an assembly, synthesizes on first use one virtual
   glyph definition per size: every part becomes an `@XXXX` leaf, an extender
   part is repeated as many times as that size needs, and the leaves are
   folded with `glue-above` (vertical) or `glue*` (horizontal) with a
   negative separation, the overlap. The first size is the smallest number of
   repetitions whose assembled length exceeds the largest pre-drawn variant,
   so sizes keep growing with the variant number, and the definitions are
   stored in `virt` under their concrete names. Because the virtual font caches compiled definitions,
   adding a glyph evicts the existing instance from `font::instances` and
   also from `font_metric::instances` and `font_glyphs::instances`, which
   share its name and were sized for the earlier definitions; subfont 6 is
   then re-created. All sizes up to 64 repetitions are defined at once, so
   this happens once per glyph rather than once per size. `MAX_ASSEMBLY_REPS`
   is 64, so 64 sizes are defined, from the first useful repetition count
   upwards;
5. otherwise falls back to the legacy `search_font_sub`, and if that yields
   subfont 0 (meaning "not handled") uses subfont 5.

`virtual_font.cpp` gains the `hor-take` primitive (mirror of `ver-take`) in
both the bitmap compiler and the vector `draw_tree` path.

### 3.6 Typesetter changes

Every consumer guards on a MATH table and on the constant it needs being
non-zero, so a font with a degenerate table falls back to the old code.

- **Fractions.** `frac_box` (`math_boxes.cpp`) receives a `disp` flag from
  `typeset_frac` (`concat_math.cpp`) and places the numerator at
  `max (shift_up, bar + gap_min ...)` and the denominator symmetrically,
  from the display or text values of the four shift and gap constants. The
  rule is drawn with `frac_rule_thickness` when the table gives one, and with
  `wline` otherwise.
- **Radicals.** `sqrt_box` draws the rule with `sqrt_rule_thickness`, half a
  thickness below the top of the radical glyph, adds `sqrt_extra_ascender`
  above it, raises the degree by `sqrt_degree_rise_percent` of the sign's
  height, offsets it by `sqrt_kern_after_degree` and takes
  `sqrt_kern_before_degree` off the left of the box. `typeset_sqrt` uses
  `sqrt_ver_gap` or `sqrt_ver_disp_gap` between the radicand and the rule.
- **Limits and stretch stacks.** `lim_box` (`script_boxes.cpp`) uses the four
  limit constants for a big operator and the four `stretchStack*` constants
  when the base is a stretched glyph, which `typeset_long_arrow` signals.
- **Scripts.** `side_box_rep` follows the specification: the standard shifts
  for an ordinary glyph, height-based shifts within `superscriptBaselineDropMax`
  and `subscriptBaselineDropMin` for a box or an extended shape,
  `subSuperscriptGapMin` with the TeX resolution of a conflict, and
  `spaceAfterScript` after the pair. `extended_shape` on boxes asks the font
  whether the base is in the extended shape coverage.
- **Delimiters.** `get_delimiter` (`text_boxes.cpp`) asks the font for the
  variant that reaches the target height, through `get_rubber_variant`,
  instead of probing sizes; only a font without an answer falls back to the
  old search.
- **Big operators.** `concat_math.cpp` builds the `<big-...>` name and the
  rubber font picks the variant: the smallest one that reaches
  `displayOperatorMinHeight`, or the largest one, capped at two em by
  `DISPLAY_OPERATOR_MAX_EM`.
- **Wide accents, bars and braces.** `compute_wide_accent` (`math_boxes.cpp`)
  takes the horizontal variant that reaches the width, places it at the top
  accent attachment points of the base and of the accent, raises it by the
  excess of the base over `accentBaseHeight`, and uses the `flac` feature
  over a base taller than `flattenedAccentBaseHeight`. Over- and underlines
  are rules from the six bar constants, measured from the ink of the base.
- **Script sizes and `ssty`.** `edit_env_rep::update_font`
  (`env_semantics.cpp`) computes the script size from `scriptPercentScaleDown`
  unless the document sets `math-font-sizes`, and wraps an untuned OpenType
  math font in `feature_font (fn, "ssty", level)` at script levels.
- **Negations.** `<neg|x>` typesets the precomposed negated symbol when the
  font has one, and strikes through otherwise.

### 3.7 Font choice: profiles, letters and features

Three pieces sit between the document and the font.

`math_font_profiles.cpp` holds a table keyed by the family name of a math
font, filled at boot from `TeXmacs/progs/fonts/fonts-opentype.scm`, which
records what the MATH table cannot: the text, sans serif and typewriter
companions, whether math letters come from the math font or from the text
italic, a bold math face, a menu label and a group. A companion is named by
its *master*, the way the `font` environment variable names a font.

`profile_fix` in `smart_font.cpp` applies it. In a math shape a text family
is replaced by its math companion when that font is installed, and the
variant picks the companion: sans serif and typewriter mathematics come from
the `sans` and `mono` masters, since a math font has no such face. In a text
shape a math family is replaced by its text companion. Whatever it produces
goes through `font_database_master`, so a profile that names a family instead
of a master still resolves.

`REWRITE_MATH_ITALIC` in the same file takes the letters of a formula from
the mathematical italic alphabet of the math font itself, rather than from
the text italic face; italic corrections and cut-in kerns then apply to
letters. It applies when the main font is an untuned OpenType math font and
the profile does not say `letters text`, so a font with no profile at all
gets it too, and a hand-tuned font never does.

`feature_font (base, feature, alt)` (`feature_font.cpp`) is a font decorator
that replaces every glyph by the alt-th substitute of a GSUB feature. The
environment wraps an untuned OpenType math font in it with `ssty` at script
levels. The other two features are applied at the call site: `dtls` for
dotless letters under an accent, and `flac` for the flattened accent over a
tall base.

## 4. Status (updated 22 September 2026)

This is a log, in the order the work was done. Later entries correct earlier
ones: a sentence saying that something is missing or implicit is history if a
later entry says it was done. Sections 6 and 7 are the current state.

- **Build.** The tree compiles again with Apple clang 17 after fixing a
  pre-existing template bug in `src/Kernel/Containers/hashtree.cpp`. The
  generated `src/makefile` also needed its macOS SDK paths refreshed and the
  removed AGL framework dropped; those are local configuration fixes, not
  source changes. Note that this configuration has no dependency tracking
  (`src/Deps` holds only a stamp): after a header change, objects must be
  removed by hand or they link with a stale vtable and crash. *Later:* the
  `deps` variable of `makefile.in` was never defined, which is why nothing
  was tracked; with it defined, `src/Deps` holds a dependency file per
  object and `make -C tests check-stale` is only a safety net.
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

- **Assemblies per specification.** A stretchable glyph gets one definition
  per size, repeating every extender part `k` times, with `k` starting at the
  first count whose assembled length exceeds the largest pre-drawn variant so
  that sizes keep growing; consecutive parts overlap by `minConnectorOverlap`,
  limited by their connector lengths. The
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

- **Export check.** The first measurement, that every font came out as a
  Type 3 bitmap font, was an artefact of the build: `configure` had
  switched off the native PDF renderer because it did not find `png.h`
  (Homebrew keeps it in `libpng/include/libpng16`, which the bare
  compile of the header check does not see). Reconfigured with
  `CPPFLAGS=-I/opt/homebrew/opt/libpng/include/libpng16 -I/opt/homebrew/include`
  the renderer is compiled in, and the exported samples embed subsets of
  the real fonts: `LatinModernMath-Regular`, `NewCMMath-Regular`,
  `STIXTwoMath-Regular`, `KpSans-Italic` and so on, 78 subsets in the
  showcase. Delimiter variants and assemblies come out of those subsets and
  render correctly. Only thirteen small Type 3 bitmap fonts are left, for
  the glyphs TeXmacs draws itself: emulated blackboard bold and bold, and
  some glued shapes. Note that a reconfigure also rewrites `TeXmacs/SVNREV`
  from `svnversion`, which prints "Unversioned directory" in a git
  checkout; `make SAFE_TEXMACS_REV` puts the expected version back,
  otherwise the binary refuses its own `TEXMACS_PATH`.

- **Scanning and shipping.** Files already recorded in the font database
  (same name and size) are no longer re-read when scanning, and styles
  with known characteristics are not re-analyzed: rescanning a complete
  database went from minutes to about two seconds. Latin Modern Math, New
  Computer Modern Math, STIX Two Math, KpMath and Fira Math are shipped
  with text companions and registered in the global database, so they work
  in a fresh installation without a scan; see section 6 of the survey.

- **GPOS pair kerning.** `parse_gpos_kern` reads the pair adjustments of
  the GPOS `kern` feature, both the explicit pairs of a format 1 subtable
  and the class matrices of a format 2 one, through extension lookups when
  present; `tt_face` caches it and `tt_font_metric_rep::kerning` uses it in
  preference to the legacy `kern` table. None of the OpenType fonts
  TeXmacs ships has a legacy `kern` table, TeX Gyre Pagella included, so
  until now they were all set with no kerning at all. Multi-letter runs in
  text and in formulas are now kerned; single letters in mathematics are
  separate boxes and are unaffected, being governed by the MATH cut-in
  kerning instead.

- **Bars and the radical junction (22 September 2026).** Over- and
  underlines of untuned OpenType math fonts are drawn as rules from
  `overbarRuleThickness` / `underbarRuleThickness`, at
  `overbarVerticalGap` / `underbarVerticalGap` above or below the *ink* of
  the base (so an underline clears a descender instead of touching it),
  with `overbarExtraAscender` / `underbarExtraDescender` added to the
  logical box. In `sqrt_box` the rule now sits half its thickness below the
  top of the radical glyph, which closes the gap Latin Modern Math showed,
  and the degree is placed by `radicalKernAfterDegree` from the left edge
  of the sign with `radicalKernBeforeDegree` taken off the box, so it tucks
  into the notch instead of floating to the left.

- **Cross-check against LuaLaTeX.** `tests/opentype/compare-lualatex.sh`
  typesets the same formulas twice with the same OpenType font, once with
  `unicode-math` under LuaLaTeX and once with TeXmacs, and stacks the two
  renders in one image. Radicals with and without a degree, over- and
  underlines, scripts, fractions, binomials, integrals and big operators
  agree up to the differences TeXmacs makes on purpose (its own spacing
  around relations, upright `d` in `dx`). The formula pairs live in
  `tests/opentype/compare/`.

- **Profiles validated, and the test suite actually runs.** A new test
  reads `TeXmacs/progs/fonts/fonts-opentype.scm` itself and checks every
  profile: known keys, no empty or repeated key, `file`, `menu` and
  `group` present, `letters` either `math` or `text`, the accessors of the
  C++ table returning what the file declares, and, for each font that is
  installed, that it really carries a MATH table and that the family name
  of the profile is the name `tt_font_name` gives the file. The last check
  is the one that matters: a misspelled family is a profile that never
  applies and nothing else says so. It found that Asana Math and TeX Gyre
  Pagella Math both claim the text companion `TeX Gyre Pagella`, where the
  reverse map silently kept the last one; it now keeps the first, so the
  order of the profiles decides and the canonical pairing comes first.
  `make -C tests` was building nothing at all, because the rule that
  regenerates the compiler flags is the first in the makefile and was
  therefore the default goal; with that fixed, `mac_images_test` no longer
  compiles under Qt 6 (`mac_images.h` drops its declarations there) and is
  excluded, and a stale expectation in `analyze_test` came to light:
  `unescape_guile` was asserted to double a backslash, which it has never
  done since it was written in 2012. All eighteen test binaries pass.

- **Math families and bold mathematics.** `math-font-family` set to `ms`
  or `mt` now reaches the `sans` and `mono` companions of the profile:
  `profile_fix` knows the variant and replaces a profiled math family by
  its sans or typewriter companion when that family is installed, since a
  math font has neither face of its own. Before, math sans serif and math
  typewriter of a profiled font fell back to the math font itself or, for
  typewriter, to an unrelated monospaced font.

  `math-font-series` reached nothing at all: the smart font built from the
  math quadruple and the text quadruple used the *text* series and dropped
  the math one, so bold mathematics only followed bold text. A math series
  other than `medium` now overrides the text series, and the font selection
  finds the Bold style of the math family where there is one. With New
  Computer Modern Math the bold radical rule measures 20 pixels at 72 pt
  and 300 dpi against 12 for the regular face, which is the 70 against 40
  design units of the two MATH tables: bold mathematics really comes from
  the bold math font. Families without a bold face are emulated as before.

  The new sample `tests/opentype/samples/math-variants.tm` shows math
  roman, math sans serif, math typewriter and bold mathematics for six
  profiled fonts side by side, with the text set in each profile's
  companion.

- **Stretch stacks.** A label above or below a stretched glyph is not a
  limit of an operator, and the table says so with its own four constants.
  `limit_box` takes a `stretched` flag, which `typeset_long_arrow` sets,
  and then uses `stretchStackGapAboveMin` and `stretchStackTopShiftUp` for
  the label above, `stretchStackGapBelowMin` and
  `stretchStackBottomShiftDown` for the one below, the shifts measured from
  the baseline of the arrow and the gaps from its edges. In Latin Modern
  Math, New Computer Modern Math, KpMath and TeX Gyre DejaVu the four
  values repeat the limit constants, so nothing moves; STIX Two Math asks
  for a shift up of 800 design units against a limit rise of 300 and a gap
  of 68 against 135, and Asana Math for tighter gaps than its limits, so
  those two change. Big operators keep the limit constants.

- **Masters, not families, and a quiet log.** Opening a sample in the
  editor printed thousands of lines: `missing 'Fira Sans' master`,
  `missing 'KpMath' master`, and `glyphCoverageFormat 20 not supported`.
  Three causes, all now fixed.

  The font selection is driven by *masters*, the second field of an entry
  of `font-features.scm` ("Fira" for the family "Fira Sans", "Kepler" for
  "KpRoman"), and the profiles named families. Five groups were wrong:
  DejaVu, Libertinus, Kp, Fira and IBM Plex. Their companions are masters
  now, and `profile_fix` translates whatever it produces through the new
  `font_database_master`, which answers from the features database without
  guessing and without printing, so a profile that names a family still
  resolves. Ten entries were added to `font-features.scm` for profiled math
  fonts it did not know, among them TeX Gyre DejaVu Math and Lete Sans
  Math. The visible effect: the text of a Fira Math document is Fira Sans
  and not the default serif, math typewriter of KpMath is KpMono, and the
  emulated script and blackboard bold alphabets of TeX Gyre DejaVu Math
  are spaced correctly.

  The coverage warnings were a NULL offset of the same kind as the ones
  fixed in `MathGlyphInfo`: `vertGlyphCoverageOffset` and
  `horizGlyphCoverageOffset` may be zero, and the parser then read
  `minConnectorOverlap` as a coverage format — the "format 20" and
  "format 100" of the message were that value. The MathKernInfo and the
  italic correction coverages are guarded the same way, and
  `parse_gsub_subtable` no longer parses a coverage table for a lookup type
  it does not handle. Finally, a missing family or master is reported once
  instead of at every lookup. What is left of the log of a full sample is
  two lines.

- **Type 1 before OpenType (22 September 2026).** XCharter came out as
  currency signs: `a` as a pound sign, `A` as `a`, `1` as `Q`.
  `tt_font_find_sub` tried `.pfb` before the sfnt formats, so for the
  many families a TeX distribution ships in both forms TeXmacs opened the
  Type 1 file, whose builtin encoding is the one the TeX world uses, and
  `XCharter-Roman.pfb` maps code 0x61 to `sterling`. Two consequences: the
  wrong glyphs on screen, since `tt_face_rep` selects that builtin encoding
  with `ft_select_charmap (face, ft_encoding_adobe_custom)`, which succeeds
  for Type 1 and fails for OpenType; and wrong glyphs in exported PDF, since
  `tt_font_glyphs_rep::get` stores the character code in `glyph::index` when
  the selected charmap is not Unicode, and the PDF writer takes that field
  for a glyph index.

  The order is now `.otf`, `.ttf`, `.ttc`, `.pfb`, `.dfont`. On this machine
  1404 font names exist in both forms, all of them inside TeX Live, and 220
  of their Type 1 files put the letters elsewhere; 21 of those are families
  of the shipped database, among them XCharter, ETbb, fbb and AlgolRevived.
  For the others the two files hold the same outlines, so nothing moves, but
  the sfnt file brings its Unicode cmap, which means fewer fallbacks, and
  correct glyph indices in the PDF. Fonts that exist only as `.pfb`, the
  275 shipped with TeXmacs among them, are unaffected. A cold-cache render
  of the overview sample took 58 seconds against 73 before, because the
  `.pfb` lookups go through kpathsea while the sfnt lookups walk the font
  path. An existing `font_cache.scm` keeps pointing at the old files, so it
  has to be cleared once, from Tools.

- **XCharter Math in the database.** The profile named a font the database
  did not know, so `<with|font|XCharter Math>` resolved by feature distance
  to whatever came closest, which was Alegreya. The two faces TeX Live
  ships are now listed in `font-database.scm` and
  `font-characteristics.scm`, with the values TeXmacs's own scanner
  produces, and the bold one is attached to the master `XCharter Math` in
  `font-features.scm`, so a bold formula in that family uses
  `XCharter-Math-Bold.otf` instead of an emulated bold. The upstream name
  table of that face calls its family `XCharter-Math-Bold` rather than
  `XCharter Math` with subfamily `Bold`, which is why the database entry
  has that spelling: it is what a user's own scan produces.

- **The extra symbols are in service (22 September 2026).**
  `tmuniversaltounicode-extra.scm` is loaded beside `tmuniversaltounicode`
  in the seven conversion cases of `converter.cpp` that use it, in both
  directions, so its 200 names are ordinary TeXmacs symbols now:
  `<QED>`, `<increment>`, `<Colon>`, `<intclockwise>`, `<rightangle>` and
  the rest typeset from whatever font serves the formula.
  `tests/opentype/samples/math-symbols-extra.tm`, generated by
  `missing-symbols.py --sample`, shows all of them in tables, so a name the
  tables fail to serve appears as a box rather than a glyph; it is part of
  the sample renders.

  Two things they do not have yet. A class in `std-symbols.scm`, so they
  typeset with no spacing, which the last section of that sample
  demonstrates next to `<oplus>`. And a LaTeX name: export writes
  `\nonconverted{QED}`, because `latex-symbol%` in
  `progs/convert/latex/latex-symbol-drd.scm` does not list them. Adding
  them there is mechanical, since the names come from `unicode-math`, but
  it is also a decision: the exported document then needs that package.

## 5. Tests

### Unit tests

`make -C tests` builds and runs eighteen binaries, 142 test functions in
all, counting the setup and teardown that QtTest reports as tests. Two
sources of the tree are left out: `xml_test`, which includes a file that is
already part of the main build, and `mac_images_test`, whose functions
`mac_images.h` does not declare in a Qt 6 build. Two of the binaries are
this work:

- `tests/Plugins/Freetype/tt_tools_test.cpp`, nine tests of the readers
  against values extracted with fontTools: a font without a MATH table and a
  truncated one, the constants of the shipped `texgyrepagella-math.otf`
  including a negative one, its glyph info, its vertical and horizontal
  variants and assemblies, the absence of MathKernInfo there, the MathKern
  lookup with its height intervals on STIX Two Math, and GPOS pair kerning.
- `tests/Graphics/Fonts/opentype_font_test.cpp`, twenty tests of the font
  level: activation and `math_type` of the shipped fonts, the conversion of
  the constants and its linearity in the size, italic correction, rubber
  variants by number and by target height, assemblies and their monotonicity,
  display operator sizes, kerning at several heights through the Unicode font
  and the smart font, the hand-tuning switch, wide variants, the GSUB feature
  variants and the feature font, the script, bar and radical parameters, the
  stretch stack constants, the bold math face, GPOS kerning through a text
  font, and a validation of every profile of `fonts-opentype.scm` against the
  installed fonts.

Both need OpenType math fonts that TeXmacs does not ship. `TM_TEST_FONT_DIR`
points at a directory that is searched recursively; the tests that need a
missing font skip themselves.

### Renders

`tests/opentype/render-samples.sh` typesets the three documents of
`tests/opentype/samples/` to PDF and to one PNG per page, named after the git
revision, and compares them with a reference directory when one is given:

- `math-overview.tm`, the same formulas in TeX fonts, TeX Gyre Pagella, STIX,
  Latin Modern Math, STIX Two Math, Asana Math, Fira Math, KpMath, TeX Gyre
  DejaVu Math and Neo Euler, one block per font, for side-by-side inspection;
- `math-showcase.tm`, a tour of every MATH feature font by font: alphabets,
  scripts and kerning, fractions, radicals, delimiters, wide accents, big
  operators and arrows with labels;
- `math-variants.tm`, math roman, math sans serif, math typewriter and bold
  mathematics for six profiled fonts, each with its text companion.

`tests/opentype/check.sh` is what to run before a commit: it runs the unit
tests, then the samples twice, once as they are and once with the hand-tuned
customizations switched off. Only the first render is diffed against
`tests/build/ref`; the untuned one is rendered for inspection.

`tests/opentype/compare-lualatex.sh` typesets the formula pairs of
`tests/opentype/compare/` twice with the same OpenType font, once through
`unicode-math` under LuaLaTeX and once through TeXmacs, and stacks the two
renders in one PNG. LuaLaTeX is the reference implementation of the
specification, so a disagreement is worth looking at.

`tests/opentype/font-gallery.sh` renders one specimen per installed math font
into `src/opentype-math/`, which is the gallery of `src/OPENTYPEMATH.md`.
`tests/opentype/survey-math-fonts.py` produces the statistics of
`doc/opentype-math-fonts-survey.md` with fontTools, and
`tests/opentype/missing-symbols.py` those of
`doc/math-symbol-coverage.md`, the mathematical symbols TeXmacs has no
name for. The same script drafts the entries for a family with `--emit`,
the encoding lines and the `std-symbols.scm` group that gives them their
spacing, and checks the tables with `--check`, which `check.sh` runs when
`TM_UNICODE_MATH_TABLE` points at the `unicode-math` list. With `--tables`
it writes the two proposal tables of `TeXmacs/langs/encoding`,
`tmuniversaltounicode-extra.scm` and its candidates file, which wait for a
line in `converter.cpp` to come into service;
`tests/opentype/confirmed-symbols.txt` records the names whose shape was
compared by eye with the glyph of the code point.

### How to run everything

From the top of the tree, after `make`:

```
make -C tests check-stale                 # stale objects after a header change
TM_TEST_FONT_DIR=/path/to/fonts make -C tests
TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/check.sh
```

`tests/README.md` describes the harness itself, including what the pixel diff
does and why the build must have the native PDF renderer for the renders to
be comparable.

## 6. Known defects still open

1. `ysup_hi_lim` has no MATH counterpart and is set to
   `max (superscriptShiftUp, x-height)`.
2. Assembled glyphs are glued on the measured ink of their parts, corrected
   to the advances of the table. The result matches the table within pixel
   rounding, but a font whose parts have unusual side bearings could still
   show a seam.

Two entries of this list were mistakes and are now closed.
`parse_variant` takes the last dash-separated token as the size and
everything between the first and the last as the root, so a root with a
dash parses. And the glue function `font-database-search` does not block:
it takes two arguments, a family and a style, and the four-argument C++
overload is simply not exposed. Calling it with four arguments is an arity
error, after which `texmacs.bin -x ... -q` keeps running instead of
exiting, which is what looked like a hang.

## 7. What is still missing

### 7.1 Kerning: what is left

Pair kerning of the GPOS `kern` feature is implemented (see above). What
remains is the rest of GPOS, none of which TeXmacs needs today: cursive
attachment, mark positioning (`mark`, `mkmk`, used by Libertinus Math and
IBM Plex Math for combining marks), and contextual positioning. Kerning is
also not applied across box boundaries, so two adjacent single letters in
a formula are still unkerned; the MATH cut-in kerning governs the gap
between a base and its scripts, which is the case the specification cares
about.

### 7.2 Constants parsed but still unused

| Group | Constants | Where they would apply |
|---|---|---|
| Stacks | `stackTopShiftUp`, `stackTopDisplayStyleShiftUp`, `stackBottomShiftDown`, `stackBottomDisplayStyleShiftDown`, `stackGapMin`, `stackDisplayStyleGapMin` | `above`, `below` and binomials over an ordinary base, which are limit boxes and use the limit constants; the stack constants would place the top element much higher (444 against 111 design units in Latin Modern Math), so adopting them is a visible change to every `above`, not a gap to fill blindly. The `stretchStack*` four are in use for stretched bases. |
| Fine positioning | device tables | parsed but never applied; they matter only at small sizes on screen |
| Delimiters | `delimitedSubFormulaMinHeight` | deliberately not applied, see below |
| Assemblies | `GlyphAssembly.italicsCorrection` | ignored |
| Axis | `mathLeading` | not needed by TeXmacs |
| Skewed fractions | `skewedFractionHorizontalGap`, `skewedFractionVerticalGap` | no TeXmacs primitive today |

`delimitedSubFormulaMinHeight` is left out on purpose. TeXmacs sizes every
bracket automatically, so the constant would apply to all of them: in Latin
Modern Math it is 1300 design units against a plain parenthesis of 996, so
`(x)` would jump to the third vertical variant, 45 percent taller than the
glyph TeX uses there. TeX and `unicode-math` ignore the constant, and the
samples are compared against them.

### 7.3 Profile keys declared but not consumed

`sans` and `mono` serve math sans serif and math typewriter since
22 September 2026, and bold mathematics reaches a real bold math face
through the font database now that the math series is honored. What is
left:

- `bold-math` is not read. In every profile it names the master whose Bold
  style the database finds anyway, so it only records that a real bold face
  exists. It would be needed for a font whose bold companion is a family of
  its own, and none of the twenty is.
- `group` is not read either. The menus are built from `menu`; `group`
  records the grouping they could have, and the profile test only requires
  it to be there.
- A key for the alphabets a font really provides is still missing, so an
  incomplete alphabet (Latin Modern Math has 18 of 52 script letters) is
  silently mixed with emulated glyphs instead of being declared.

The symbols themselves are a separate gap, on the TeXmacs side rather than
the font side: of the 2435 code points of the `unicode-math` list, 892 have
no TeXmacs name at all, so they can only be typed as `<#XXXX>` and reach
neither the palettes nor the LaTeX conversion.
`doc/math-symbol-coverage.md` lists them, block by block, with the note the
encoding tables already carry for most of them and the number of shipped
math fonts that draw them.

### 7.4 Name-based logic not yet profile-driven

`supports_big_operators` in `poor_rubber.cpp` still matches family names,
but only for fonts without a MATH table: a font that has one is taken to
have its own big operators, as the MATH-aware rubber font already assumed.
The remaining `stix` and `agella` name tests in the typesetter are
deliberate: they guard hand-tuned corrections, which keep precedence.

### 7.5 Testing and export

- Nothing checks the text companions of a profile against the font
  database; the profile test checks the math font itself, its family name
  and its MATH table, but a companion family that is not installed is only
  noticed when a document asks for it.
- The samples are compared against stored reference renders by hand; the
  check script does not fail on a pixel difference, it only reports it.
- The glyphs TeXmacs glues together itself still export as Type 3 bitmap
  fonts (`/ProcSet [ /PDF /ImageB ]`), where the PDF writer could place
  the parts as vectors; `pdf_hummus_renderer.cpp` has a disused path for
  that. It concerns emulated alphabets more than the MATH assemblies,
  which come from the embedded subsets.

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
- any other Unicode font without horizontal variants falls back to the
  line-drawn `wide_hat_box`, `wide_tilda_box`, `wide_bar_box`,
  `wide_vect_box`, `wide_check_box`, `wide_breve_box`, `wide_squbr_box`
  and `wide_sqobr_box` of `src/Typeset/Boxes/Basic/stretch_boxes.cpp`,
  which draw the shapes from lines and arcs with the pen width `wline`.
  There is also a path that magnifies the accent glyph horizontally
  (`fn->magnify (sx, sy)`), but the width heuristic that would select it is
  commented out and `very_wide` is now set unconditionally, so that path is
  not reached any more.

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
their `underbar` twins for bars. The bar constants are in use since
22 September 2026: `<wide-bar>` and `<wide-underline>` of untuned
OpenType math fonts are rules of the table's thickness, set off from the
ink of the base by the table's gap.

What was implemented, on 21 and 22 September 2026:

1. `wide_code_point` in `rubber_unicode_font.cpp` maps the TeXmacs accent
   and arrow names to the combining marks and base arrows that carry the
   variants (`hat` to U+0302, `tilde` to U+0303, `bar` to U+0305,
   `underline` to U+0332, `vect` to U+20D7, `check` to U+030C, `breve` to
   U+0306, `invbreve` to U+0311, the four brace and bracket pairs, and the
   long arrows to U+2190 and its neighbours). It is needed because TeXmacs's
   `<hat>` converts to a spacing modifier letter, not to the combining mark.
2. `get_wide_variant (s, width)` on `font_rep` returns the smallest
   horizontal variant whose advance reaches the width, or the assembly with
   the right number of repetitions; `get_wide` in `text_boxes.cpp` asks it
   before the older tables, and the rubber font resolves the `wide` and
   `rubber` heads through the mapping above.
3. `get_top_accent (s)` on `font_rep` and `top_accent ()` on boxes give the
   attachment point, from the font for a single glyph and from the centre of
   the ink otherwise.
4. `compute_wide_accent` has an OpenType branch before the generic ones: it
   picks the variant by width, places it so that the two attachment points
   coincide, lifts a narrow accent by the excess of the base over
   `accentBaseHeight`, uses `flac` over a base taller than
   `flattenedAccentBaseHeight`, and draws `<bar>` as a rule from the six bar
   constants. The line-drawn shapes remain for fonts with no horizontal
   variants.

### 8.2 Long arrows and wide relations

`typeset_long_arrow` builds `<long-arrow|...>` with `wide_box` on the
arrow's own name and stacks the labels with `limit_box`. It used to depend
on `<name-N>` variants that only TeX fonts and the TeX Gyre tables provide;
now `get_wide` asks `get_wide_variant` first, and the name mapping of
section 8.1 sends the long arrows to U+2190 and its neighbours, so a math
font serves them from its horizontal variants and assemblies (in Pagella,
`arrowright` has one variant and a three-part assembly). Only a font with
no such variants still needs the old tables.

The labels are a stretch stack, not the limits of an operator, and
`limit_box` takes a `stretched` flag that `typeset_long_arrow` sets: the
label above uses `stretchStackGapAboveMin` and `stretchStackTopShiftUp`,
the one below `stretchStackGapBelowMin` and `stretchStackBottomShiftDown`.

### 8.3 Radicals

`sqrt_box` combines a `<large-sqrt-N>` delimiter with a `line_box` for
the overline. The delimiter now comes from the MATH variants and assembly,
and the rule thickness, gap, extra ascender and degree placement come from
the radical constants. The junction follows the specification as well: the
rule has `radicalRuleThickness` and is centred half a thickness below the
top of the radical glyph, where the top part of the assembly is designed to
meet it, and the degree is offset by `radicalKernAfterDegree` with
`radicalKernBeforeDegree` removed from the left of the box. Only the
overline itself is still a `line_box`, which is what the specification
prescribes; nothing of the construction is guesswork any more.

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

This is the plan as it was written on 21 September 2026 and executed over
the two days that followed. It is kept as the record of what was decided;
each phase now carries the state of its items, so the list is not mistaken
for open work. What remains open is section 7.

Principles: the hand-tuned tables keep precedence everywhere; every step
lands with a unit test or a sample row; the "hand tuned math fonts" switch
is the comparison tool. Sizes are S (a day or less), M (a few days),
L (a week or more). Phases 1 and 2 are independent of 3; phase 4 needs 3.

### Phase 0: groundwork (S) — partly done

- Turn dependency tracking on in the autotools build, or add a rule that
  invalidates objects on header changes (`check-stale` is a stopgap).
- Replace the hard-coded TeX Live years in `tt_font_path` by a glob or
  `kpsewhich`, so system math fonts are found. *Done for macOS only:*
  `texlive_font_dirs` scans `/usr/local/texlive`, `/usr/share/texlive`,
  `/opt/texlive` and `$HOME/texlive` for any year; the Linux branch of
  `tt_file.cpp` still lists 2020, 2021 and 2022 by hand.
- Document the family name normalization of `tt_font_name` ("STIX Two
  Math" becomes "Stix Two Math"), which is why the sample uses that
  spelling; no alias mechanism is needed.
- Push the branch; keep `master` merges small.

### Phase 1: finish the table-driven layout (M) — done except item 4

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
4. Stack constants for `above`, `below`, `stack` and binomials. *Not done,
   deliberately:* those are limit boxes and the limit constants suit them;
   the stack constants would move every `above` (444 against 111 design
   units in Latin Modern Math). The `stretchStack*` four are in use for
   labels on stretched arrows.
5. Extended shape coverage: no raised superscripts on tall delimiters and
   operators.
6. Display operator cap to tame fonts like IBM Plex Math. *Done as a
   constant,* `DISPLAY_OPERATOR_MAX_EM` of two em in
   `rubber_unicode_font.cpp`; there is no document variable for it.

Tests: expected heights for `<left-(-N>` at given target heights, radical
rule position, script positions on STIX Two Math and Latin Modern Math;
sample rows checked against LuaLaTeX with `unicode-math` for the same
formulas.

### Phase 2: wide accents and horizontal constructions (M) — done

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

### Phase 3: letters, alphabets and profiles (L) — done but for the alphabet key

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

### Phase 4: the shipped fonts under their hand tuning (M) — done

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

### Phase 5: polish (M) — done except the two items marked below

- Bold mathematics from real bold math fonts (New Computer Modern, KpMath,
  XITS). Done, though not through `bold-math`: making `math-font-series`
  reach the font was enough, the database then finds the Bold style of the
  math family.
- Negations mapped to precomposed Unicode symbols when available. Done.
- Verify `<@XXXX>` glyphs and assemblies in PDF export. Done: with the
  native PDF renderer the variants and assemblies come out of the embedded
  font subsets. *PostScript and SVG export are not checked.*
- Cache assembled glyphs without rebuilding the virtual font; measure
  startup and typesetting time with a large document. *Partly:* all sizes of
  a glyph are defined at once, so the virtual font is rebuilt once per glyph
  rather than once per size, but nothing has been measured.
- Device tables are deliberately left out.

### Phase 6: tests and documentation (S, continuous) — done, and continuing

- Reference PNGs for the pixel diff of the samples, refreshed on purpose.
  Done, in `tests/build/ref`, which is not under version control.
- A kerning and accents sample next to `math-overview.tm`. Done as
  `math-showcase.tm`, which covers both, and `math-variants.tm`.
- `make -C tests` and both renders in a script that can run before every
  commit; the design and survey documents updated as steps land. Done as
  `tests/opentype/check.sh`.

### Definition of done

A document set in Latin Modern Math, New Computer Modern Math or STIX Two
Math, with letters, scripts, accents, delimiters and operators, renders
with no hand-drawn construction and compares well with LuaLaTeX's
`unicode-math` output of the same source; TeX Gyre and STIX documents
render exactly as before with the switch on; every installed Tier 1 font
has a profile and passes the profile test; all unit tests and both sample
renders pass.

### Where this stands, 22 September 2026

Met, with three things worth naming exactly:

- Glyphs still drawn by TeXmacs for an untuned OpenType math font are the
  ones the specification also prescribes as rules (fraction bars, the
  radical overline, over- and underlines) and the alphabets a font does
  not have (blackboard bold and script in most fonts, where the emulation
  is mixed with the letters the font does provide, silently).
- The comparison against `unicode-math` under LuaLaTeX covers radicals,
  bars, scripts, fractions, binomials, integrals and big operators
  (`tests/opentype/compare-lualatex.sh`), not accents, delimiters at every
  size or kerning.
- The profile test checks the math font of every profile, its family name
  and its MATH table, but not that the text, sans and typewriter
  companions it names exist as masters or are installed.

Everything under "What is still missing" is either deliberate
(`delimitedSubFormulaMinHeight`, the device tables, the stack constants
for `above`) or a small, named gap. The twenty-two tests of
`opentype_font_test`, the eleven of `tt_tools_test`, the other sixteen
test binaries and the three samples, tuned and untuned, pass.
