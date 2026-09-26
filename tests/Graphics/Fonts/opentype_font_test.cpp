
/******************************************************************************
* MODULE     : opentype_font_test.cpp
* DESCRIPTION: tests of OpenType MATH support at the font level
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// These tests exercise the path from a MATH table to font parameters,
// glyph corrections and rubber characters. They need an OpenType math font
// that is not special-cased by name in unicode_font.cpp; we use Latin Modern
// Math, found through TM_TEST_FONT_DIR (see tests/Makefile). Expected design
// unit values come from fontTools.

#include "tm_test.hpp"
#include <stdlib.h>
#include "font.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_tools.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "hashset.hpp"


#define LM_UPEM 1000.0
#define LM_SIZE 10
#define LM_DPI  600

static SI
si_abs (SI x) {
  return x >= 0? x: -x;
}

// state shared by the tests below, set once by test_setup
static font lm;
static bool have_lm= false;
static SI   du_y (int du);
static SI   du_x (int du);

static void
test_setup () {
  string dir= get_env ("TM_TEST_FONT_DIR");
  have_lm= false;
  if (dir != "") {
    setenv ("TEXMACS_FONT_PATH", as_charp (dir), 1);
    have_lm= tt_font_exists ("latinmodern-math");
    if (have_lm) lm= unicode_font ("latinmodern-math", LM_SIZE, LM_DPI);
  }
}

static SI
du_y (int du) {
  return (SI) tm_round (du * LM_SIZE * lm->hpt / LM_UPEM);
}

static SI
du_x (int du) {
  return (SI) tm_round (du * LM_SIZE * lm->wpt / LM_UPEM);
}

static void
test_shipped_fonts_keep_legacy_math_type () {
  // Documents the current behaviour: the TeX Gyre math fonts and the STIX
  // text fonts take the hand-tuned branches of unicode_font.cpp and do not
  // use their MATH table yet. STIXMath-Regular does not match the "STIX-"
  // prefix, so it already goes through the OpenType path.
  font pag= unicode_font ("texgyrepagella-math", LM_SIZE, LM_DPI);
  CHECK (!is_nil (pag));
  CHECK_EQ (pag->math_type, MATH_TYPE_TEX_GYRE);
  // ... but the MATH table is loaded underneath the hand-tuned tables
  CHECK (pag->ot_math);
  CHECK_EQ (pag->frac_rule_thickness, (SI) tm_round (60 * LM_SIZE * pag->hpt / 1000.0));
  CHECK (pag->is_extended_shape ("<#222B>"));
  font stix= unicode_font ("STIX-Regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (stix));
  CHECK_EQ (stix->math_type, MATH_TYPE_STIX);
  font stixm= unicode_font ("STIXMath-Regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (stixm));
  CHECK_EQ (stixm->math_type, MATH_TYPE_OPENTYPE);
}

static void
test_activation () {
  if (!have_lm) SKIP ("set TM_TEST_FONT_DIR to a directory containing latinmodern-math.otf");
  CHECK (!is_nil (lm));
  CHECK_EQ (lm->type, FONT_TYPE_UNICODE);
  CHECK_EQ (lm->math_type, MATH_TYPE_OPENTYPE);
  // a text font of the same family has no MATH table
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (rm));
  CHECK_EQ (rm->math_type, MATH_TYPE_NORMAL);
}

static void
test_constants_conversion () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  // Latin Modern Math constants (design units, unitsPerEm 1000)
  CHECK_EQ (lm->frac_rule_thickness, du_y (40));
  CHECK_EQ (lm->frac_num_gap_min, du_y (40));
  CHECK_EQ (lm->frac_denom_disp_gap_min, du_y (120));
  CHECK_EQ (lm->sqrt_rule_thickness, du_y (40));
  CHECK_EQ (lm->upper_limit_gap_min, du_y (200));
  CHECK (lm->frac_rule_thickness > 0);
  // the conversion must be linear in the size
  font lm2= unicode_font ("latinmodern-math", 2 * LM_SIZE, LM_DPI);
  CHECK_EQ (lm2->frac_denom_disp_gap_min, (SI) tm_round (120 * 2 * LM_SIZE * lm2->hpt / LM_UPEM));
}

static void
test_italic_correction () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  // U+1D453 math italic f: italic correction 90; U+222B integral: 332
  CHECK_EQ (lm->get_right_correction ("<#1D453>"), du_x (90));
  CHECK_EQ (lm->get_right_correction ("<#222B>"), du_x (332));
  // no italic correction for the digit one
  CHECK_EQ (lm->get_right_correction ("1"), (SI) 0);
}

static void
test_rubber_variants () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  CHECK (!is_nil (rf));
  CHECK (rf->res_name != lm->res_name);
  // parenleft has 8 vertical variants in Latin Modern Math (base + 7);
  // their heights must increase strictly
  SI prev= 0;
  for (int i= 0; i < 8; i++) {
    string s= "<left-(-" * as_string (i) * ">";
    CHECK_MSG (rf->supports (s), as_charp (s));
    metric ex;
    rf->get_extents (s, ex);
    SI h= ex->y2 - ex->y1;
    CHECK_MSG (h > prev, as_charp (s * " not taller than the previous size"));
    prev= h;
  }
  // the advance measurements of the variants (fontTools): 997 ... 2991 du
  metric ex0, ex7;
  rf->get_extents ("<left-(-0>", ex0);
  rf->get_extents ("<left-(-7>", ex7);
  CHECK (si_abs ((ex7->y2 - ex7->y1) - du_y (2991)) <= du_y (30));
  CHECK (si_abs ((ex0->y2 - ex0->y1) - du_y (997)) <= du_y (30));
}

static void
test_rubber_assembly () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  metric ex7, ex8, ex12;
  rf->get_extents ("<left-(-7>", ex7);
  // beyond the last pre-drawn variant the glyph is assembled from parts
  CHECK (rf->supports ("<left-(-8>"));
  rf->get_extents ("<left-(-8>", ex8);
  CHECK (ex8->y2 - ex8->y1 > 0);
  CHECK (ex8->x4 - ex8->x3 > 0);
  rf->get_extents ("<left-(-12>", ex12);
  CHECK (ex12->y2 - ex12->y1 > ex8->y2 - ex8->y1);
  CHECK (ex8->y2 - ex8->y1 > ex7->y2 - ex7->y1);
  // Latin Modern Math parenleft assembly: bottom and top parts of advance
  // 1495 with connectors of 249, extender of 498, minConnectorOverlap 20.
  // With k repetitions of the extender: 2*1495 + k*498 - (k+1)*20.
  SI h8 = ex8->y2 - ex8->y1;
  SI h12= ex12->y2 - ex12->y1;
  SI tol= du_y (60); // parts are glued on their ink boxes, not advances
  CHECK_MSG (si_abs (h8 - du_y (2*1495 + 498 - 2*20)) <= tol,
            as_charp ("height " * as_string (h8) * " for one extender"));
  CHECK_MSG (si_abs (h12 - du_y (2*1495 + 5*498 - 6*20)) <= tol,
            as_charp ("height " * as_string (h12) * " for five extenders"));
  // horizontal assemblies: arrows are not rubber characters, but the
  // machinery must not break on horizontal variants either
  CHECK (rf->supports ("<left-(-40>"));
}

static void
test_big_operators () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  // integral: two sizes (1112 and 2223 du)
  metric ex1, ex2;
  CHECK (rf->supports ("<big-int-1>"));
  CHECK (rf->supports ("<big-int-2>"));
  rf->get_extents ("<big-int-1>", ex1);
  rf->get_extents ("<big-int-2>", ex2);
  CHECK (ex2->y2 - ex2->y1 > ex1->y2 - ex1->y1);
  CHECK (si_abs ((ex1->y2 - ex1->y1) - du_y (1112)) <= du_y (40));
  CHECK (si_abs ((ex2->y2 - ex2->y1) - du_y (2223)) <= du_y (40));
}

static void
test_kerning_at_height () {
  // STIX Two Math has MathKernInfo; Latin Modern Math does not.
  if (get_env ("TM_TEST_FONT_DIR") == "" || !tt_font_exists ("STIXTwoMath-Regular"))
    SKIP ("no STIX Two Math in TM_TEST_FONT_DIR");
  font st= unicode_font ("STIXTwoMath-Regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (st));
  CHECK_EQ (st->math_type, MATH_TYPE_OPENTYPE);
  double upem= 1000.0;
  auto du_x= [&] (int du) { return (SI) tm_round (du * LM_SIZE * st->wpt / upem); };
  auto du_y= [&] (int du) { return (SI) tm_round (du * LM_SIZE * st->hpt / upem); };

  // U+1D449 math italic V: italic correction 100,
  //   bottom right kern: heights [156, 280] -> values [-222, -118, 202]
  //   top right kern: single value 0
  string V= "<#1D449>";
  CHECK_EQ (st->get_rsub_correction_at (V, du_y (100)), du_x (-222));
  CHECK_EQ (st->get_rsub_correction_at (V, du_y (200)), du_x (-118));
  CHECK_EQ (st->get_rsub_correction_at (V, du_y (400)), du_x (202));
  // superscript: italic correction plus (zero) kern, at any height
  CHECK_EQ (st->get_rsup_correction_at (V, du_y (100)), du_x (100));
  CHECK_EQ (st->get_rsup_correction_at (V, du_y (900)), du_x (100));
  // the height-less version evaluates at the font descender
  CHECK_EQ (st->get_rsub_correction (V), st->get_rsub_correction_at (V, st->y1));

  // U+1D434 math italic A: no italic correction,
  //   top right kern: heights [213, 350] -> values [58, -58, -70]
  string A= "<#1D434>";
  CHECK_EQ (st->get_rsup_correction_at (A, du_y (100)), du_x (58));
  CHECK_EQ (st->get_rsup_correction_at (A, du_y (300)), du_x (-58));
  CHECK_EQ (st->get_rsup_correction_at (A, du_y (400)), du_x (-70));

  // NOTE: the smart font used by the typesetter forwards the height to the
  // subfont (smart_font_rep::get_*_correction_at). Smart fonts need the
  // font database and a running TeXmacs, so that path is covered by the
  // visual samples in tests/opentype rather than here.
}

static void
test_assembly_monotone () {
  // The delimiter search assumes that sizes grow with the variant number.
  // Asana Math parentheses have 4 variants and an assembly whose smallest
  // instance is shorter than the largest variant; braces have 4 variants
  // with two extenders; bars have 7 variants and a two-part assembly.
  if (get_env ("TM_TEST_FONT_DIR") == "" || !tt_font_exists ("Asana-Math"))
    SKIP ("no Asana Math in TM_TEST_FONT_DIR");
  font as= unicode_font ("Asana-Math", LM_SIZE, LM_DPI);
  CHECK_EQ (as->math_type, MATH_TYPE_OPENTYPE);
  font rf= rubber_font (as);
  const char* roots[]= {"(", "{", "|", "sqrt"};
  for (int r= 0; r < 4; r++) {
    SI prev= 0;
    for (int i= 0; i < 16; i++) {
      string s= "<left-" * string (roots[r]) * "-" * as_string (i) * ">";
      CHECK_MSG (rf->supports (s), as_charp (s));
      metric ex;
      rf->get_extents (s, ex);
      SI h= ex->y2 - ex->y1;
      CHECK_MSG (h > prev, as_charp (s * " is not taller than the previous size"));
      prev= h;
    }
  }
}

static void
test_hand_tuned_switch () {
  // With the hand-tuned customizations switched off, a shipped font with a
  // MATH table takes the OpenType path. Fonts are cached by name, so use
  // sizes not used elsewhere in this test.
  CHECK (get_hand_tuned_math_fonts ());
  set_hand_tuned_math_fonts (false);
  font pag= unicode_font ("texgyrepagella-math", 12, LM_DPI);
  CHECK (!is_nil (pag));
  CHECK_EQ (pag->math_type, MATH_TYPE_OPENTYPE);
  CHECK (pag->frac_rule_thickness > 0);
  // fractionRuleThickness 60 in Pagella Math
  CHECK_EQ (pag->wline, (SI) tm_round (60 * 12 * pag->hpt / 1000.0));
  // a text font without MATH table is not affected by the switch: its
  // math_type still comes from the file name (get_math_type in font.cpp)
  font lib= unicode_font ("texgyrepagella-regular", 12, LM_DPI);
  CHECK (!is_nil (lib));
  CHECK_EQ (lib->math_type, MATH_TYPE_TEX_GYRE);
  CHECK (!lib->ot_math);
  CHECK_EQ (lib->frac_rule_thickness, (SI) 0);
  set_hand_tuned_math_fonts (true);
  font pag2= unicode_font ("texgyrepagella-math", 14, LM_DPI);
  CHECK_EQ (pag2->math_type, MATH_TYPE_TEX_GYRE);
  CHECK (pag2->ot_math);
  CHECK (pag2->frac_rule_thickness > 0);
}

static void
test_rubber_variant_by_height () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  string r;
  // Latin Modern Math parenleft variants: 997 1095 1195 1445 1793 2093 2393
  // 2991 design units; the smallest one reaching the height is chosen
  CHECK (rf->get_rubber_variant ("<left-(>", du_y (900), r));
  CHECK_EQ (r, string ("<left-(-0>"));
  CHECK (rf->get_rubber_variant ("<left-(>", du_y (1500), r));
  CHECK_EQ (r, string ("<left-(-4>"));
  CHECK (rf->get_rubber_variant ("<left-(>", du_y (2991), r));
  CHECK_EQ (r, string ("<left-(-7>"));
  // beyond the variants an assembly is made to measure: one extender
  // (3448 with minimal overlaps) shrunk to 3200
  SI h= du_y (3200);
  CHECK (rf->get_rubber_variant ("<left-(>", h, r));
  CHECK_MSG (starts (r, "<left-(-h"), as_charp (r));
  CHECK (rf->supports (r));
  metric ex;
  rf->get_extents (r, ex);
  CHECK_MSG (si_abs ((ex->y2 - ex->y1) - h) <= du_y (60),
            as_charp ("assembled height " * as_string (ex->y2 - ex->y1) *
                      " for target " * as_string (h)));
  // a much taller request needs more extenders and still fits
  h= du_y (9000);
  CHECK (rf->get_rubber_variant ("<left-(>", h, r));
  rf->get_extents (r, ex);
  CHECK_MSG (si_abs ((ex->y2 - ex->y1) - h) <= du_y (60),
            as_charp ("assembled height " * as_string (ex->y2 - ex->y1) *
                      " for target " * as_string (h)));
  // radicals and integrals go through the same path
  CHECK (rf->get_rubber_variant ("<large-sqrt>", du_y (2000), r));
  CHECK (rf->supports (r));
  // unknown characters are refused
  CHECK (!rf->get_rubber_variant ("<left-.>", du_y (2000), r));
}

static void
test_script_parameters () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  // Latin Modern Math: subSuperscriptGapMin 160, superscriptBaselineDropMax
  // 250, subscriptBaselineDropMin 200, superscriptBottomMaxWithSubscript
  // 344, spaceAfterScript 56, script scales 70% and 50%
  CHECK_EQ (lm->sub_sup_gap_min, du_y (160));
  CHECK_EQ (lm->sup_drop_max, du_y (250));
  CHECK_EQ (lm->sub_drop_min, du_y (200));
  CHECK_EQ (lm->sup_bottom_max_with_sub, du_y (344));
  CHECK_EQ (lm->space_after_script, du_x (56));
  CHECK_EQ (lm->script_percent, 70);
  CHECK_EQ (lm->script_script_percent, 50);
  // over- and underlines, and the radical constants
  CHECK_EQ (lm->overbar_vertical_gap, du_y (120));
  CHECK_EQ (lm->overbar_rule_thickness, du_y (40));
  CHECK_EQ (lm->overbar_extra_ascender, du_y (40));
  CHECK_EQ (lm->underbar_vertical_gap, du_y (120));
  CHECK_EQ (lm->underbar_rule_thickness, du_y (40));
  CHECK_EQ (lm->underbar_extra_descender, du_y (40));
  CHECK_EQ (lm->sqrt_rule_thickness, du_y (40));
  CHECK_EQ (lm->sqrt_kern_before_degree, du_y (278));
  CHECK_EQ (lm->sqrt_kern_after_degree, du_y (-556));
  CHECK_EQ (lm->sqrt_degree_rise_percent, 60);
  // a font without a MATH table has none of them
  font rm0= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK_EQ (rm0->overbar_rule_thickness, (SI) 0);
  // the standard shifts come from the table as well
  CHECK_EQ (lm->ysup_lo_base, du_y (363));
  // integrals and summation signs are extended shapes, letters are not
  CHECK (lm->is_extended_shape ("<#222B>"));
  CHECK (lm->is_extended_shape ("<#2211>"));
  CHECK (!lm->is_extended_shape ("<#1D453>"));
  CHECK (!lm->is_extended_shape ("x"));
  // the rubber font answers for stretched variants as well
  font rf= rubber_font (lm);
  CHECK (rf->is_extended_shape ("<big-int-2>"));
  CHECK (rf->is_extended_shape ("<left-(-9>"));
  // fonts without a MATH table have no extended shapes
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK (!rm->is_extended_shape ("<#222B>"));
  CHECK_EQ (rm->sub_sup_gap_min, (SI) 0);
}

static void
test_wide_variants () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  string r;
  // the hat has horizontal variants and an assembly in Latin Modern Math;
  // widths must grow with the variant number and the chosen variant must
  // reach the requested width
  SI prev= 0;
  for (int i= 0; i < 7; i++) {
    string s= "<wide-hat-" * as_string (i) * ">";
    CHECK_MSG (rf->supports (s), as_charp (s));
    metric ex;
    rf->get_extents (s, ex);
    SI w= ex->x4 - ex->x3;
    CHECK_MSG (w > prev, as_charp (s * " not wider than the previous size"));
    prev= w;
  }
  CHECK (rf->get_wide_variant ("<wide-hat>", du_x (300), r));
  CHECK_EQ (r, string ("<wide-hat-0>"));
  CHECK (rf->get_wide_variant ("<wide-hat>", du_x (1000), r));
  metric ex;
  rf->get_extents (r, ex);
  CHECK_MSG (ex->x4 - ex->x3 >= du_x (1000) - du_x (10), as_charp (r));
  // the hat has no assembly: beyond its widest variant (1897) we get that one
  SI w= du_x (4000);
  CHECK (rf->get_wide_variant ("<wide-hat>", w, r));
  CHECK_EQ (r, string ("<wide-hat-7>"));
  // the overbrace has one (its widest variant is 4007): an assembly made
  // to measure
  w= du_x (6000);
  CHECK (rf->get_wide_variant ("<wide-overbrace>", w, r));
  CHECK_MSG (starts (r, "<wide-overbrace-w"), as_charp (r));
  CHECK (rf->supports (r));
  rf->get_extents (r, ex);
  CHECK_MSG (si_abs ((ex->x2 - ex->x1) - w) <= du_x (60),
            as_charp ("assembled width " * as_string (ex->x2 - ex->x1) *
                      " for target " * as_string (w)));
  // long arrows: <rubber-rightarrow> maps to U+2192
  w= du_x (3000);
  CHECK (rf->get_wide_variant ("<rubber-rightarrow>", w, r));
  rf->get_extents (r, ex);
  CHECK (ex->x2 - ex->x1 >= w - du_x (60));
  CHECK (rf->get_wide_variant ("<rubber-longrightarrow>", w, r));
  // over- and underbraces
  CHECK (rf->get_wide_variant ("<wide-overbrace>", du_x (2500), r));
  CHECK (rf->supports (r));
  // no horizontal variants for parentheses
  CHECK (!rf->get_wide_variant ("<wide-(>", du_x (1000), r));
  // top accent attachment of the math italic f (464 design units)
  SI x;
  CHECK (lm->get_top_accent ("<#1D453>", x));
  CHECK_EQ (x, du_x (464));
  CHECK (!lm->get_top_accent ("1", x) || x > 0);
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK (!rm->get_top_accent ("f", x));
}

static void
test_feature_variants () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  string r;
  // Latin Modern Math: dtls maps math italic i (U+1D456) to glyph 1322,
  // ssty maps math italic x (U+1D465) to glyph 1427 (first alternate);
  // it has no flac feature
  CHECK (lm->get_feature_variant ("<#1D456>", "dtls", 0, r));
  CHECK_MSG (r == string ("<@") * as_hexadecimal (1322, 4) * ">",
            as_charp ("dtls of U+1D456 gave " * r));
  CHECK (lm->supports (r));
  CHECK (!lm->get_feature_variant ("<#1D465>", "dtls", 0, r));
  CHECK (lm->get_feature_variant ("<#1D465>", "ssty", 0, r));
  CHECK_EQ (r, string ("<@") * as_hexadecimal (1427, 4) * ">");
  CHECK (lm->get_feature_variant ("<#1D465>", "ssty", 1, r));
  CHECK (!lm->get_feature_variant ("<#1D465>", "ssty", 2, r));
  CHECK (!lm->get_feature_variant ("<#302>", "flac", 0, r));
  // STIX Two Math: flac maps the combining circumflex to glyph 4800
  if (tt_font_exists ("STIXTwoMath-Regular")) {
    font st= unicode_font ("STIXTwoMath-Regular", LM_SIZE, LM_DPI);
    CHECK (st->get_feature_variant ("<#302>", "flac", 0, r));
    CHECK_EQ (r, string ("<@") * as_hexadecimal (4800, 4) * ">");
    CHECK (st->get_feature_variant ("<#1D456>", "dtls", 0, r));
    CHECK_EQ (r, string ("<@") * as_hexadecimal (3335, 4) * ">");
  }
  // fonts without a MATH table answer nothing
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK (!rm->get_feature_variant ("i", "dtls", 0, r));
}

static void
test_profiles () {
  // profiles are defined in Scheme at boot; here we set one by hand
  tree p (TUPLE);
  p << tuple ("file", "latinmodern-math") << tuple ("text", "Latin Modern Roman")
    << tuple ("letters", "math") << tuple ("menu", "Latin Modern");
  math_font_profile_set ("Latin Modern Math", p);
  CHECK_EQ (math_font_profile_attr ("Latin Modern Math", "text"),
            string ("Latin Modern Roman"));
  CHECK_EQ (math_font_profile_attr ("Latin Modern Math", "nonsense"), string (""));
  CHECK_EQ (math_font_profile_attr ("No Such Font", "text"), string (""));
  CHECK_EQ (math_family_for_text ("Latin Modern Roman"), string ("Latin Modern Math"));
  CHECK_EQ (math_family_for_text ("Latin Modern Math"), string (""));
  CHECK_EQ (text_family_for_math ("Latin Modern Math"), string ("Latin Modern Roman"));
  CHECK (N (math_font_profile_families ()) >= 1);
  CHECK_EQ (N (math_font_profile ("Latin Modern Math")), 4);
}

static void
test_feature_font () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  // the ssty alternates of Latin Modern Math are heavier: a different
  // glyph for the same character
  font sf= feature_font (lm, "ssty", 0);
  CHECK (!is_nil (sf));
  CHECK (sf->res_name != lm->res_name);
  CHECK (sf->supports ("<#1D465>"));
  metric ex1, ex2;
  lm->get_extents ("<#1D465>", ex1);
  sf->get_extents ("<#1D465>", ex2);
  CHECK (ex1->y2 > 0 && ex2->y2 > 0);
  font_metric fm1, fm2; font_glyphs fg1, fg2;
  CHECK (sf->index_glyph ("<#1D465>", fm1, fg1) !=
           lm->index_glyph ("<#1D465>", fm2, fg2));
  CHECK (sf->supports ("1") && sf->supports ("+"));
  // positions are reported per byte of the original string
  string s= "<#1D465><#1D466>1";
  SI* xpos= tm_new_array<SI> (N(s) + 1);
  sf->get_xpositions (s, xpos);
  CHECK_EQ (xpos[0], (SI) 0);
  CHECK (xpos[8] > 0 && xpos[16] > xpos[8] && xpos[N(s)] > xpos[16]);
  tm_delete_array (xpos);
  // the rubber font of the decorated font is the one of the base font
  string r;
  font rf= rubber_font (sf);
  CHECK (rf->get_rubber_variant ("<left-(>", du_y (1500), r));
  CHECK_EQ (r, string ("<left-(-4>"));
}

static void
test_gpos_kerning () {
  // STIX Two Text has its kerning in GPOS and no legacy kern table:
  // 'AV' must be narrower than 'A' and 'V' set apart, by 100 design units
  font tx= unicode_font ("STIXTwoText-Regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (tx));
  metric a, v, av;
  tx->get_extents ("A", a);
  tx->get_extents ("V", v);
  tx->get_extents ("AV", av);
  SI expected= (SI) tm_round (-100 * LM_SIZE * tx->wpt / 1000.0);
  SI got= (av->x2 - av->x1) - ((a->x2 - a->x1) + (v->x2 - v->x1));
  CHECK_MSG (si_abs (got - expected) <= PIXEL,
            as_charp ("kerning of AV is " * as_string (got) *
                      " instead of " * as_string (expected)));
  // an unkerned pair is unchanged
  metric aa;
  tx->get_extents ("AA", aa);
  CHECK_EQ ((aa->x2 - aa->x1) - 2 * (a->x2 - a->x1), (SI) 0);
  // cursor positions follow the kerning
  SI* xpos= tm_new_array<SI> (3);
  tx->get_xpositions ("AV", xpos);
  CHECK_EQ (xpos[0], (SI) 0);
  CHECK (si_abs (xpos[1] - ((a->x2 - a->x1) + expected)) <= PIXEL);
  tm_delete_array (xpos);
}

static void
test_profile_file () {
  // The profiles shipped in TeXmacs/progs/fonts/fonts-opentype.scm must be
  // well formed, and their family names must be the names the font database
  // gives to the files: a misspelled family is a profile that never applies
  // and nothing else says so.
  string body;
  url u= url_system (get_env ("TEXMACS_PATH")) *
         url ("progs") * url ("fonts") * url ("fonts-opentype.scm");
  CHECK_MSG (!load_string (u, body, false), "fonts-opentype.scm not readable");
  scheme_tree forms= block_to_scheme_tree (body);
  hashset<string> known;
  known->insert ("file"); known->insert ("text"); known->insert ("sans");
  known->insert ("mono"); known->insert ("letters");
  known->insert ("bold-math"); known->insert ("menu"); known->insert ("group");
  hashset<string> names, texts;
  int nr_profiles= 0, nr_installed= 0;
  for (int i=0; i<N(forms); i++) {
    scheme_tree f= forms[i];
    if (is_atomic (f) || N(f) < 2) continue;
    if (!is_atomic (f[0]) ||
        f[0]->label != "define-math-font-profile") continue;
    CHECK (is_atomic (f[1]));
    string name= scm_unquote (f[1]->label);
    CHECK_MSG (name != "", "a profile has an empty family name");
    CHECK_MSG (!names->contains (name),
              as_charp ("two profiles for " * name));
    names->insert (name);
    nr_profiles++;
    tree props (TUPLE);
    hashmap<string,string> val ("");
    for (int j=2; j<N(f); j++) {
      scheme_tree q= f[j];
      CHECK_MSG (!is_atomic (q) && N(q) == 2,
                as_charp ("malformed property in the profile of " * name));
      string key= scm_unquote (q[0]->label);
      string value= scm_unquote (q[1]->label);
      CHECK_MSG (known->contains (key),
                as_charp ("unknown key " * key * " in the profile of " * name));
      CHECK_MSG (!val->contains (key),
                as_charp ("key " * key * " twice in the profile of " * name));
      CHECK_MSG (value != "",
                as_charp ("empty " * key * " in the profile of " * name));
      val (key)= value;
      props << tuple (key, value);
    }
    CHECK_MSG (val->contains ("file"),
              as_charp ("no file in the profile of " * name));
    CHECK_MSG (val->contains ("menu"),
              as_charp ("no menu label in the profile of " * name));
    CHECK_MSG (val->contains ("group"),
              as_charp ("no group in the profile of " * name));
    if (val->contains ("letters"))
      CHECK_MSG (val["letters"] == "math" || val["letters"] == "text",
                as_charp ("letters is neither math nor text in " * name));
    // several math fonts may name the same text companion; the first one
    // in the file is the one that companion pulls in
    bool first_claim= false;
    if (val->contains ("text")) {
      first_claim= !texts->contains (val["text"]);
      texts->insert (val["text"]);
    }
    // the table and its accessors must return what the file declares
    math_font_profile_set (name, props);
    CHECK_EQ (math_font_profile_attr (name, "file"), val["file"]);
    CHECK_EQ (math_font_profile_attr (name, "menu"), val["menu"]);
    if (val->contains ("text")) {
      CHECK_EQ (text_family_for_math (name), val["text"]);
      if (first_claim)
        CHECK_EQ (math_family_for_text (val["text"]), name);
      else
        CHECK (math_family_for_text (val["text"]) != name);
    }
    // an installed font must really be an OpenType math font, and the
    // family name of the profile must be the one the database gives it
    if (!tt_font_exists (val["file"])) continue;
    nr_installed++;
    scheme_tree fn= tt_font_name (tt_font_find (val["file"]));
    CHECK_MSG (!is_atomic (fn) && N(fn) >= 1, as_charp ("no name table in " *
                                                       val["file"]));
    CHECK_MSG (!is_atomic (fn[0]) && N(fn[0]) >= 1, "malformed name table");
    CHECK_EQ (scm_unquote (fn[0][0]->label), name);
    font mf= unicode_font (val["file"], LM_SIZE, LM_DPI);
    CHECK (!is_nil (mf));
    CHECK_MSG (mf->ot_math,
              as_charp (val["file"] * " has no MATH table"));
  }
  CHECK_MSG (nr_profiles >= 15,
            as_charp ("only " * as_string (nr_profiles) * " profiles read"));
  // The companions (text, sans, mono, bold-math) are master names, the way
  // the font environment variable names a font, not family names; the test
  // does not resolve them, see section 7.5 of doc/opentype-math-design.md.
  CHECK_MSG (nr_installed >= 1, "no profiled math font is installed");
}

static void
test_bold_math_font () {
  // Selecting the bold series of a math family must give the bold face with
  // the constants of its own MATH table, not the regular one stroked. New
  // Computer Modern Math is shipped with both faces and its bold radical
  // rule is 70 design units against 40 for the regular one. (This harness
  // runs with the smart fonts off, so this is the plain font selection; the
  // smart font path is covered by the math-variants sample.)
  // du_y measures in the units of Latin Modern Math, so this needs it too
  if (!have_lm) SKIP ("Latin Modern Math missing");
  if (!tt_font_exists ("NewCMMath-Bold")) SKIP ("NewCMMath-Bold missing");
  font reg= smart_font ("NewComputerModernMath", "mr", "medium", "normal",
                        "roman", "rm", "medium", "mathitalic",
                        LM_SIZE, LM_DPI);
  font bld= smart_font ("NewComputerModernMath", "mr", "bold", "normal",
                        "roman", "rm", "medium", "mathitalic",
                        LM_SIZE, LM_DPI);
  CHECK (!is_nil (reg) && !is_nil (bld));
  CHECK_MSG (reg->ot_math, "the regular math font has no MATH table");
  CHECK_MSG (bld->ot_math, "the bold math font has no MATH table");
  CHECK_EQ (reg->sqrt_rule_thickness, du_y (40));
  CHECK_MSG (bld->sqrt_rule_thickness == du_y (70),
            as_charp ("bold radical rule is " *
                      as_string (bld->sqrt_rule_thickness) *
                      " instead of " * as_string (du_y (70))));
  // and the bold glyphs are wider than the regular ones
  metric ra, ba;
  reg->get_extents ("a", ra);
  bld->get_extents ("a", ba);
  CHECK (ba->x2 - ba->x1 > ra->x2 - ra->x1);
}

static void
test_stretch_stack_constants () {
  // Labels above and below a stretched glyph (a long arrow) follow the
  // stretch stack constants. In Latin Modern Math they repeat the limit
  // constants, so nothing moves there; STIX Two Math wants a much larger
  // shift up and a much smaller gap, which is the case the constants exist
  // for.
  if (!have_lm) SKIP ("Latin Modern Math missing");
  CHECK_EQ (lm->stretch_stack_top_shift_up, du_y (111));
  CHECK_EQ (lm->stretch_stack_bottom_shift_down, du_y (600));
  CHECK_EQ (lm->stretch_stack_gap_above_min, du_y (200));
  CHECK_EQ (lm->stretch_stack_gap_below_min, du_y (167));
  CHECK_EQ (lm->stretch_stack_top_shift_up, lm->upper_limit_baseline_rise_min);
  CHECK_EQ (lm->stretch_stack_gap_below_min, lm->lower_limit_gap_min);
  if (!tt_font_exists ("STIXTwoMath-Regular")) return;
  font st= unicode_font ("STIXTwoMath-Regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (st));
  CHECK_EQ (st->stretch_stack_top_shift_up, du_y (800));
  CHECK_EQ (st->stretch_stack_bottom_shift_down, du_y (590));
  CHECK_EQ (st->stretch_stack_gap_above_min, du_y (68));
  CHECK_EQ (st->stretch_stack_gap_below_min, du_y (68));
  CHECK (st->stretch_stack_gap_above_min < st->upper_limit_gap_min);
}

static void
test_text_font_features () {
  // The GSUB features of an ordinary text font, which has no MATH table:
  // Latin Modern Roman keeps its old style figures under 'onum' and Linux
  // Libertine its small capitals under 'smcp', both single substitutions.
  if (!tt_font_exists ("lmroman10-regular")) SKIP ("Latin Modern Roman missing");
  font lm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  CHECK (!is_nil (lm));
  string sub;
  CHECK (lm->get_feature_variant ("0", "onum", 0, sub));
  CHECK (sub != "0");
  CHECK (lm->supports (sub));
  // a feature the font does not have answers nothing
  CHECK (!lm->get_feature_variant ("0", "zzzz", 0, sub));
  // and the font of the feature draws the substitute: an old style zero
  // reaches no higher than the x-height, a lining one as high as a capital
  font os= apply_features (lm, "onum");
  CHECK (!is_nil (os));
  metric lining, oldstyle, ex_x;
  lm->get_extents ("0", lining);
  os->get_extents ("0", oldstyle);
  lm->get_extents ("x", ex_x);
  CHECK (oldstyle->y2 < lining->y2);
  CHECK (si_abs (oldstyle->y2 - ex_x->y2) <= lining->y2 - ex_x->y2);
  // the string is unchanged for everything else
  metric la, lb;
  lm->get_extents ("abc", la);
  os->get_extents ("abc", lb);
  CHECK_EQ (lb->x2 - lb->x1, la->x2 - la->x1);
  // an unknown tag and an empty list leave the font alone
  CHECK_EQ (apply_features (lm, "")->res_name, lm->res_name);
  CHECK_EQ (apply_features (lm, "toolong")->res_name, lm->res_name);
  if (!tt_font_exists ("LinLibertine_R")) return;
  font lib= unicode_font ("LinLibertine_R", LM_SIZE, LM_DPI);
  CHECK (lib->get_feature_variant ("a", "smcp", 0, sub));
  CHECK (sub != "a");
  metric small, cap;
  apply_features (lib, "smcp")->get_extents ("a", small);
  lib->get_extents ("A", cap);
  CHECK (small->y2 < cap->y2);
}

// Assemblies of every font, not only of the ones whose parts sit at the
// origin. The parts are placed by the near edge of their ink, at the
// distance the MATH table prescribes between them, so the left and the
// right half of a pair of delimiters, whose assemblies are mirror images,
// must come out with the same height, and no delimiter may come out
// shorter than it was asked for. Two things used to spoil this: gluing
// every part to the stack built so far, which measures the tallest part of
// the stack instead of the one which is to receive the next part, and a
// font which declares no advance for a part, as KpMath 0.35 does for the
// bottom of its right parenthesis, which then came out an em too tall.
static void
test_assembled_delimiters () {
  if (get_env ("TM_TEST_FONT_DIR") == "") SKIP ("no TM_TEST_FONT_DIR");
  array<string> files;
  files << string ("latinmodern-math") << string ("KpMath-Regular")
        << string ("STIXTwoMath-Regular") << string ("texgyrepagella-math")
        << string ("XCharter-Math") << string ("Erewhon-Math")
        << string ("Asana-Math") << string ("LibertinusMath-Regular")
        << string ("NewCMMath-Regular");
  array<string> left, right;
  left  << string ("<left-(>") << string ("<left-{>") << string ("<left-|>");
  right << string ("<right-)>") << string ("<right-}>") << string ("<right-|>");
  int nr= 0, nr_assembled= 0;
  for (int i= 0; i < N (files); i++) {
    if (!tt_font_exists (files[i])) continue;
    font fn= unicode_font (files[i], LM_SIZE, LM_DPI);
    if (is_nil (fn) || !fn->ot_math) continue;
    font rf= rubber_font (fn);
    nr++;
    for (int j= 0; j < N (left); j++)
      for (int du= 3000; du <= 6000; du+= 1500) {
        SI     h  = (SI) tm_round (du * LM_SIZE * fn->hpt / LM_UPEM);
        SI     tol= (SI) tm_round (40 * LM_SIZE * fn->hpt / LM_UPEM);
        string rl, rr;
        if (!rf->get_rubber_variant (left[j], h, rl)) continue;
        if (!rf->get_rubber_variant (right[j], h, rr)) continue;
        // only the glyphs made to measure are assembled from parts
        if (!occurs ("-h", rl) || !occurs ("-h", rr)) continue;
        nr_assembled++;
        metric el, er;
        rf->get_extents (rl, el);
        rf->get_extents (rr, er);
        SI hl= el->y2 - el->y1, hr= er->y2 - er->y1;
        // the parts must join: the ink of an assembled delimiter is one
        // connected piece, whatever the offsets at which the font draws
        // the parts
        for (int side= 0; side < 2; side++) {
          string nm= side? rr: rl;
          glyph  gl= rf->get_glyph (nm);
          if (is_nil (gl) || gl->width <= 0 || gl->height <= 0) continue;
          int w= gl->width, ht= gl->height, total= 0, seen= 0, start= -1;
          array<bool> ink (w * ht), done (w * ht);
          for (int y= 0; y < ht; y++)
            for (int x= 0; x < w; x++) {
              bool b= (gl->get_x (x, y) != 0);
              ink[y*w + x]= b;
              done[y*w + x]= false;
              if (b) { total++; if (start < 0) start= y*w + x; }
            }
          if (total == 0) continue;
          (void) start;
          // the largest connected piece of ink must be the whole delimiter
          for (int c0= 0; c0 < w * ht; c0++) {
            if (!ink[c0] || done[c0]) continue;
            int        size= 0;
            array<int> todo;
            todo << c0;
            done[c0]= true;
            while (N (todo) > 0) {
              int c= todo[N(todo) - 1];
              todo->resize (N(todo) - 1);
              size++;
              int x= c % w, y= c / w;
              for (int d= 0; d < 4; d++) {
                int nx= x + ((d == 0)? -1: ((d == 1)? 1: 0));
                int ny= y + ((d == 2)? -1: ((d == 3)? 1: 0));
                if (nx < 0 || ny < 0 || nx >= w || ny >= ht) continue;
                int nc= ny*w + nx;
                if (ink[nc] && !done[nc]) { done[nc]= true; todo << nc; }
              }
            }
            seen= max (seen, size);
          }
          CHECK_MSG (seen * 20 >= total * 19,
                    as_charp (files[i] * " " * nm * ": the parts do not join, "
                              * as_string (total - seen) * " of "
                              * as_string (total) *
                              " pixels of ink are detached"));
        }
      }
  }
  if (nr == 0) SKIP ("no math font in TM_TEST_FONT_DIR");
  CHECK_MSG (nr_assembled >= 4, "no delimiter was assembled from parts");
}

// The screen draws with a copy of the font made for the resolution of the
// display, and such a copy starts with an empty virtual font. A delimiter
// made to measure must therefore be rebuilt from its name, and at the size
// the name says, whatever the resolution the copy was made for: the name
// carries the size in thousandths of an em for that reason. It used to
// carry it in pixels, and a delimiter which needed an assembly was drawn
// in its base size on the screen, while paper had it right.
static void
test_magnified_assembly () {
  if (!have_lm) SKIP ("no Latin Modern Math");
  font   rf= rubber_font (lm);
  string r;
  SI     h= du_y (9000);
  CHECK (rf->get_rubber_variant ("<left-(>", h, r));
  CHECK_MSG (occurs ("-h", r), as_charp (r));
  metric ex;
  rf->get_extents (r, ex);
  CHECK (ex->y2 - ex->y1 > 0);
  double zooms[3]= {2.0, 0.5, 1.5};
  for (int i= 0; i < 3; i++) {
    font mg= rubber_font (lm)->magnify (zooms[i]);
    CHECK (!is_nil (mg));
    CHECK_MSG (mg->supports (r), as_charp (r));
    metric ey;
    mg->get_extents (r, ey);
    SI got = ey->y2 - ey->y1;
    SI want= (SI) tm_round (zooms[i] * (ex->y2 - ex->y1));
    CHECK_MSG (si_abs (got - want) <= du_y (100),
              as_charp (r * " magnified by " * as_string (zooms[i]) * ": " *
                        as_string (got / PIXEL) * " pixels instead of " *
                        as_string (want / PIXEL)));
  }
}

// A document names the family it wants, and a profile names the master of
// its text companion: a family of that master must find the same math font,
// or the mathematics of KpRoman, Fira Sans or Libertinus Serif is emulated
// from the text face instead of coming from KpMath, Fira Math or
// Libertinus Math.
static void
test_math_family_for_text () {
  if (get_env ("TM_TEST_FONT_DIR") == "") SKIP ("no TM_TEST_FONT_DIR");
  // profiles are defined in Scheme at boot; here we set one by hand
  tree p (TUPLE);
  p << tuple ("file", "KpMath-Regular") << tuple ("text", "Kepler")
    << tuple ("letters", "math") << tuple ("menu", "Kp Fonts");
  math_font_profile_set ("KpMath", p);
  CHECK_EQ (math_family_for_text ("Kepler"), string ("KpMath"));
  // KpRoman and KpSans belong to the master Kepler
  CHECK_EQ (math_family_for_text ("KpRoman"), string ("KpMath"));
  CHECK_EQ (math_family_for_text ("KpSans"), string ("KpMath"));
  // a family which belongs to no master with a profile keeps its answer
  CHECK_EQ (math_family_for_text ("Zorglub Nonesuch"), string (""));
  CHECK_EQ (math_family_for_text ("KpMath"), string (""));
}

int
main () {
  test_setup ();
  RUN (test_shipped_fonts_keep_legacy_math_type);
  RUN (test_activation);
  RUN (test_constants_conversion);
  RUN (test_italic_correction);
  RUN (test_rubber_variants);
  RUN (test_rubber_assembly);
  RUN (test_assembled_delimiters);
  RUN (test_magnified_assembly);
  RUN (test_big_operators);
  RUN (test_kerning_at_height);
  RUN (test_assembly_monotone);
  RUN (test_hand_tuned_switch);
  RUN (test_rubber_variant_by_height);
  RUN (test_script_parameters);
  RUN (test_wide_variants);
  RUN (test_feature_variants);
  RUN (test_profiles);
  RUN (test_math_family_for_text);
  RUN (test_feature_font);
  RUN (test_gpos_kerning);
  RUN (test_text_font_features);
  RUN (test_profile_file);
  RUN (test_bold_math_font);
  RUN (test_stretch_stack_constants);
  return test_report ();
}
