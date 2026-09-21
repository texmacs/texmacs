
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

#include <QtTest/QtTest>
#include <stdlib.h>
#include "font.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "Freetype/tt_file.hpp"

#define LM_UPEM 1000.0
#define LM_SIZE 10
#define LM_DPI  600

class TestOpenTypeFont: public QObject {
  Q_OBJECT

  font lm;
  bool have_lm;
  SI   du_y (int du);
  SI   du_x (int du);

private slots:
  void initTestCase ();
  void test_shipped_fonts_keep_legacy_math_type ();
  void test_activation ();
  void test_constants_conversion ();
  void test_italic_correction ();
  void test_rubber_variants ();
  void test_rubber_assembly ();
  void test_big_operators ();
  void test_kerning_at_height ();
  void test_assembly_monotone ();
  void test_hand_tuned_switch ();
  void test_rubber_variant_by_height ();
  void test_script_parameters ();
  void test_wide_variants ();
  void test_feature_variants ();
  void test_profiles ();
};

void
TestOpenTypeFont::initTestCase () {
  string dir= get_env ("TM_TEST_FONT_DIR");
  have_lm= false;
  if (dir != "") {
    setenv ("TEXMACS_FONT_PATH", as_charp (dir), 1);
    have_lm= tt_font_exists ("latinmodern-math");
    if (have_lm) lm= unicode_font ("latinmodern-math", LM_SIZE, LM_DPI);
  }
}

SI
TestOpenTypeFont::du_y (int du) {
  return (SI) tm_round (du * LM_SIZE * lm->hpt / LM_UPEM);
}

SI
TestOpenTypeFont::du_x (int du) {
  return (SI) tm_round (du * LM_SIZE * lm->wpt / LM_UPEM);
}

void
TestOpenTypeFont::test_shipped_fonts_keep_legacy_math_type () {
  // Documents the current behaviour: the TeX Gyre math fonts and the STIX
  // text fonts take the hand-tuned branches of unicode_font.cpp and do not
  // use their MATH table yet. STIXMath-Regular does not match the "STIX-"
  // prefix, so it already goes through the OpenType path.
  font pag= unicode_font ("texgyrepagella-math", LM_SIZE, LM_DPI);
  QVERIFY (!is_nil (pag));
  QCOMPARE (pag->math_type, MATH_TYPE_TEX_GYRE);
  // ... but the MATH table is loaded underneath the hand-tuned tables
  QVERIFY (pag->ot_math);
  QCOMPARE (pag->frac_rule_thickness, (SI) tm_round (60 * LM_SIZE * pag->hpt / 1000.0));
  QVERIFY (pag->is_extended_shape ("<#222B>"));
  font stix= unicode_font ("STIX-Regular", LM_SIZE, LM_DPI);
  QVERIFY (!is_nil (stix));
  QCOMPARE (stix->math_type, MATH_TYPE_STIX);
  font stixm= unicode_font ("STIXMath-Regular", LM_SIZE, LM_DPI);
  QVERIFY (!is_nil (stixm));
  QCOMPARE (stixm->math_type, MATH_TYPE_OPENTYPE);
}

void
TestOpenTypeFont::test_activation () {
  if (!have_lm) QSKIP ("set TM_TEST_FONT_DIR to a directory containing latinmodern-math.otf");
  QVERIFY (!is_nil (lm));
  QCOMPARE (lm->type, FONT_TYPE_UNICODE);
  QCOMPARE (lm->math_type, MATH_TYPE_OPENTYPE);
  // a text font of the same family has no MATH table
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  QVERIFY (!is_nil (rm));
  QCOMPARE (rm->math_type, MATH_TYPE_NORMAL);
}

void
TestOpenTypeFont::test_constants_conversion () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  // Latin Modern Math constants (design units, unitsPerEm 1000)
  QCOMPARE (lm->frac_rule_thickness, du_y (40));
  QCOMPARE (lm->frac_num_gap_min, du_y (40));
  QCOMPARE (lm->frac_denom_disp_gap_min, du_y (120));
  QCOMPARE (lm->sqrt_rule_thickness, du_y (40));
  QCOMPARE (lm->upper_limit_gap_min, du_y (200));
  QVERIFY (lm->frac_rule_thickness > 0);
  // the conversion must be linear in the size
  font lm2= unicode_font ("latinmodern-math", 2 * LM_SIZE, LM_DPI);
  QCOMPARE (lm2->frac_denom_disp_gap_min, (SI) tm_round (120 * 2 * LM_SIZE * lm2->hpt / LM_UPEM));
}

void
TestOpenTypeFont::test_italic_correction () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  // U+1D453 math italic f: italic correction 90; U+222B integral: 332
  QCOMPARE (lm->get_right_correction ("<#1D453>"), du_x (90));
  QCOMPARE (lm->get_right_correction ("<#222B>"), du_x (332));
  // no italic correction for the digit one
  QCOMPARE (lm->get_right_correction ("1"), (SI) 0);
}

void
TestOpenTypeFont::test_rubber_variants () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  QVERIFY (!is_nil (rf));
  QVERIFY (rf->res_name != lm->res_name);
  // parenleft has 8 vertical variants in Latin Modern Math (base + 7);
  // their heights must increase strictly
  SI prev= 0;
  for (int i= 0; i < 8; i++) {
    string s= "<left-(-" * as_string (i) * ">";
    QVERIFY2 (rf->supports (s), as_charp (s));
    metric ex;
    rf->get_extents (s, ex);
    SI h= ex->y2 - ex->y1;
    QVERIFY2 (h > prev, as_charp (s * " not taller than the previous size"));
    prev= h;
  }
  // the advance measurements of the variants (fontTools): 997 ... 2991 du
  metric ex0, ex7;
  rf->get_extents ("<left-(-0>", ex0);
  rf->get_extents ("<left-(-7>", ex7);
  QVERIFY (qAbs ((ex7->y2 - ex7->y1) - du_y (2991)) <= du_y (30));
  QVERIFY (qAbs ((ex0->y2 - ex0->y1) - du_y (997)) <= du_y (30));
}

void
TestOpenTypeFont::test_rubber_assembly () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  metric ex7, ex8, ex12;
  rf->get_extents ("<left-(-7>", ex7);
  // beyond the last pre-drawn variant the glyph is assembled from parts
  QVERIFY (rf->supports ("<left-(-8>"));
  rf->get_extents ("<left-(-8>", ex8);
  QVERIFY (ex8->y2 - ex8->y1 > 0);
  QVERIFY (ex8->x4 - ex8->x3 > 0);
  rf->get_extents ("<left-(-12>", ex12);
  QVERIFY (ex12->y2 - ex12->y1 > ex8->y2 - ex8->y1);
  QVERIFY (ex8->y2 - ex8->y1 > ex7->y2 - ex7->y1);
  // Latin Modern Math parenleft assembly: bottom and top parts of advance
  // 1495 with connectors of 249, extender of 498, minConnectorOverlap 20.
  // With k repetitions of the extender: 2*1495 + k*498 - (k+1)*20.
  SI h8 = ex8->y2 - ex8->y1;
  SI h12= ex12->y2 - ex12->y1;
  SI tol= du_y (60); // parts are glued on their ink boxes, not advances
  QVERIFY2 (qAbs (h8 - du_y (2*1495 + 498 - 2*20)) <= tol,
            as_charp ("height " * as_string (h8) * " for one extender"));
  QVERIFY2 (qAbs (h12 - du_y (2*1495 + 5*498 - 6*20)) <= tol,
            as_charp ("height " * as_string (h12) * " for five extenders"));
  // horizontal assemblies: arrows are not rubber characters, but the
  // machinery must not break on horizontal variants either
  QVERIFY (rf->supports ("<left-(-40>"));
}

void
TestOpenTypeFont::test_big_operators () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  // integral: two sizes (1112 and 2223 du)
  metric ex1, ex2;
  QVERIFY (rf->supports ("<big-int-1>"));
  QVERIFY (rf->supports ("<big-int-2>"));
  rf->get_extents ("<big-int-1>", ex1);
  rf->get_extents ("<big-int-2>", ex2);
  QVERIFY (ex2->y2 - ex2->y1 > ex1->y2 - ex1->y1);
  QVERIFY (qAbs ((ex1->y2 - ex1->y1) - du_y (1112)) <= du_y (40));
  QVERIFY (qAbs ((ex2->y2 - ex2->y1) - du_y (2223)) <= du_y (40));
}

void
TestOpenTypeFont::test_kerning_at_height () {
  // STIX Two Math has MathKernInfo; Latin Modern Math does not.
  if (get_env ("TM_TEST_FONT_DIR") == "" || !tt_font_exists ("STIXTwoMath-Regular"))
    QSKIP ("no STIX Two Math in TM_TEST_FONT_DIR");
  font st= unicode_font ("STIXTwoMath-Regular", LM_SIZE, LM_DPI);
  QVERIFY (!is_nil (st));
  QCOMPARE (st->math_type, MATH_TYPE_OPENTYPE);
  double upem= 1000.0;
  auto du_x= [&] (int du) { return (SI) tm_round (du * LM_SIZE * st->wpt / upem); };
  auto du_y= [&] (int du) { return (SI) tm_round (du * LM_SIZE * st->hpt / upem); };

  // U+1D449 math italic V: italic correction 100,
  //   bottom right kern: heights [156, 280] -> values [-222, -118, 202]
  //   top right kern: single value 0
  string V= "<#1D449>";
  QCOMPARE (st->get_rsub_correction_at (V, du_y (100)), du_x (-222));
  QCOMPARE (st->get_rsub_correction_at (V, du_y (200)), du_x (-118));
  QCOMPARE (st->get_rsub_correction_at (V, du_y (400)), du_x (202));
  // superscript: italic correction plus (zero) kern, at any height
  QCOMPARE (st->get_rsup_correction_at (V, du_y (100)), du_x (100));
  QCOMPARE (st->get_rsup_correction_at (V, du_y (900)), du_x (100));
  // the height-less version evaluates at the font descender
  QCOMPARE (st->get_rsub_correction (V), st->get_rsub_correction_at (V, st->y1));

  // U+1D434 math italic A: no italic correction,
  //   top right kern: heights [213, 350] -> values [58, -58, -70]
  string A= "<#1D434>";
  QCOMPARE (st->get_rsup_correction_at (A, du_y (100)), du_x (58));
  QCOMPARE (st->get_rsup_correction_at (A, du_y (300)), du_x (-58));
  QCOMPARE (st->get_rsup_correction_at (A, du_y (400)), du_x (-70));

  // NOTE: the smart font used by the typesetter forwards the height to the
  // subfont (smart_font_rep::get_*_correction_at). Smart fonts need the
  // font database and a running TeXmacs, so that path is covered by the
  // visual samples in tests/opentype rather than here.
}

void
TestOpenTypeFont::test_assembly_monotone () {
  // The delimiter search assumes that sizes grow with the variant number.
  // Asana Math parentheses have 4 variants and an assembly whose smallest
  // instance is shorter than the largest variant; braces have 4 variants
  // with two extenders; bars have 7 variants and a two-part assembly.
  if (get_env ("TM_TEST_FONT_DIR") == "" || !tt_font_exists ("Asana-Math"))
    QSKIP ("no Asana Math in TM_TEST_FONT_DIR");
  font as= unicode_font ("Asana-Math", LM_SIZE, LM_DPI);
  QCOMPARE (as->math_type, MATH_TYPE_OPENTYPE);
  font rf= rubber_font (as);
  const char* roots[]= {"(", "{", "|", "sqrt"};
  for (int r= 0; r < 4; r++) {
    SI prev= 0;
    for (int i= 0; i < 16; i++) {
      string s= "<left-" * string (roots[r]) * "-" * as_string (i) * ">";
      QVERIFY2 (rf->supports (s), as_charp (s));
      metric ex;
      rf->get_extents (s, ex);
      SI h= ex->y2 - ex->y1;
      QVERIFY2 (h > prev, as_charp (s * " is not taller than the previous size"));
      prev= h;
    }
  }
}

void
TestOpenTypeFont::test_hand_tuned_switch () {
  // With the hand-tuned customizations switched off, a shipped font with a
  // MATH table takes the OpenType path. Fonts are cached by name, so use
  // sizes not used elsewhere in this test.
  QVERIFY (get_hand_tuned_math_fonts ());
  set_hand_tuned_math_fonts (false);
  font pag= unicode_font ("texgyrepagella-math", 12, LM_DPI);
  QVERIFY (!is_nil (pag));
  QCOMPARE (pag->math_type, MATH_TYPE_OPENTYPE);
  QVERIFY (pag->frac_rule_thickness > 0);
  // fractionRuleThickness 60 in Pagella Math
  QCOMPARE (pag->wline, (SI) tm_round (60 * 12 * pag->hpt / 1000.0));
  // a text font without MATH table is not affected by the switch: its
  // math_type still comes from the file name (get_math_type in font.cpp)
  font lib= unicode_font ("texgyrepagella-regular", 12, LM_DPI);
  QVERIFY (!is_nil (lib));
  QCOMPARE (lib->math_type, MATH_TYPE_TEX_GYRE);
  QVERIFY (!lib->ot_math);
  QCOMPARE (lib->frac_rule_thickness, (SI) 0);
  set_hand_tuned_math_fonts (true);
  font pag2= unicode_font ("texgyrepagella-math", 14, LM_DPI);
  QCOMPARE (pag2->math_type, MATH_TYPE_TEX_GYRE);
  QVERIFY (pag2->ot_math);
  QVERIFY (pag2->frac_rule_thickness > 0);
}

void
TestOpenTypeFont::test_rubber_variant_by_height () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  string r;
  // Latin Modern Math parenleft variants: 997 1095 1195 1445 1793 2093 2393
  // 2991 design units; the smallest one reaching the height is chosen
  QVERIFY (rf->get_rubber_variant ("<left-(>", du_y (900), r));
  QCOMPARE (r, string ("<left-(-0>"));
  QVERIFY (rf->get_rubber_variant ("<left-(>", du_y (1500), r));
  QCOMPARE (r, string ("<left-(-4>"));
  QVERIFY (rf->get_rubber_variant ("<left-(>", du_y (2991), r));
  QCOMPARE (r, string ("<left-(-7>"));
  // beyond the variants an assembly is made to measure: one extender
  // (3448 with minimal overlaps) shrunk to 3200
  SI h= du_y (3200);
  QVERIFY (rf->get_rubber_variant ("<left-(>", h, r));
  QVERIFY2 (starts (r, "<left-(-h"), as_charp (r));
  QVERIFY (rf->supports (r));
  metric ex;
  rf->get_extents (r, ex);
  QVERIFY2 (qAbs ((ex->y2 - ex->y1) - h) <= du_y (60),
            as_charp ("assembled height " * as_string (ex->y2 - ex->y1) *
                      " for target " * as_string (h)));
  // a much taller request needs more extenders and still fits
  h= du_y (9000);
  QVERIFY (rf->get_rubber_variant ("<left-(>", h, r));
  rf->get_extents (r, ex);
  QVERIFY2 (qAbs ((ex->y2 - ex->y1) - h) <= du_y (60),
            as_charp ("assembled height " * as_string (ex->y2 - ex->y1) *
                      " for target " * as_string (h)));
  // radicals and integrals go through the same path
  QVERIFY (rf->get_rubber_variant ("<large-sqrt>", du_y (2000), r));
  QVERIFY (rf->supports (r));
  // unknown characters are refused
  QVERIFY (!rf->get_rubber_variant ("<left-.>", du_y (2000), r));
}

void
TestOpenTypeFont::test_script_parameters () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  // Latin Modern Math: subSuperscriptGapMin 160, superscriptBaselineDropMax
  // 250, subscriptBaselineDropMin 200, superscriptBottomMaxWithSubscript
  // 344, spaceAfterScript 56, script scales 70% and 50%
  QCOMPARE (lm->sub_sup_gap_min, du_y (160));
  QCOMPARE (lm->sup_drop_max, du_y (250));
  QCOMPARE (lm->sub_drop_min, du_y (200));
  QCOMPARE (lm->sup_bottom_max_with_sub, du_y (344));
  QCOMPARE (lm->space_after_script, du_x (56));
  QCOMPARE (lm->script_percent, 70);
  QCOMPARE (lm->script_script_percent, 50);
  // the standard shifts come from the table as well
  QCOMPARE (lm->ysup_lo_base, du_y (363));
  // integrals and summation signs are extended shapes, letters are not
  QVERIFY (lm->is_extended_shape ("<#222B>"));
  QVERIFY (lm->is_extended_shape ("<#2211>"));
  QVERIFY (!lm->is_extended_shape ("<#1D453>"));
  QVERIFY (!lm->is_extended_shape ("x"));
  // the rubber font answers for stretched variants as well
  font rf= rubber_font (lm);
  QVERIFY (rf->is_extended_shape ("<big-int-2>"));
  QVERIFY (rf->is_extended_shape ("<left-(-9>"));
  // fonts without a MATH table have no extended shapes
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  QVERIFY (!rm->is_extended_shape ("<#222B>"));
  QCOMPARE (rm->sub_sup_gap_min, (SI) 0);
}

void
TestOpenTypeFont::test_wide_variants () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  font rf= rubber_font (lm);
  string r;
  // the hat has horizontal variants and an assembly in Latin Modern Math;
  // widths must grow with the variant number and the chosen variant must
  // reach the requested width
  SI prev= 0;
  for (int i= 0; i < 7; i++) {
    string s= "<wide-hat-" * as_string (i) * ">";
    QVERIFY2 (rf->supports (s), as_charp (s));
    metric ex;
    rf->get_extents (s, ex);
    SI w= ex->x4 - ex->x3;
    QVERIFY2 (w > prev, as_charp (s * " not wider than the previous size"));
    prev= w;
  }
  QVERIFY (rf->get_wide_variant ("<wide-hat>", du_x (300), r));
  QCOMPARE (r, string ("<wide-hat-0>"));
  QVERIFY (rf->get_wide_variant ("<wide-hat>", du_x (1000), r));
  metric ex;
  rf->get_extents (r, ex);
  QVERIFY2 (ex->x4 - ex->x3 >= du_x (1000) - du_x (10), as_charp (r));
  // the hat has no assembly: beyond its widest variant (1897) we get that one
  SI w= du_x (4000);
  QVERIFY (rf->get_wide_variant ("<wide-hat>", w, r));
  QCOMPARE (r, string ("<wide-hat-7>"));
  // the overbrace has one (its widest variant is 4007): an assembly made
  // to measure
  w= du_x (6000);
  QVERIFY (rf->get_wide_variant ("<wide-overbrace>", w, r));
  QVERIFY2 (starts (r, "<wide-overbrace-w"), as_charp (r));
  QVERIFY (rf->supports (r));
  rf->get_extents (r, ex);
  QVERIFY2 (qAbs ((ex->x2 - ex->x1) - w) <= du_x (60),
            as_charp ("assembled width " * as_string (ex->x2 - ex->x1) *
                      " for target " * as_string (w)));
  // long arrows: <rubber-rightarrow> maps to U+2192
  w= du_x (3000);
  QVERIFY (rf->get_wide_variant ("<rubber-rightarrow>", w, r));
  rf->get_extents (r, ex);
  QVERIFY (ex->x2 - ex->x1 >= w - du_x (60));
  QVERIFY (rf->get_wide_variant ("<rubber-longrightarrow>", w, r));
  // over- and underbraces
  QVERIFY (rf->get_wide_variant ("<wide-overbrace>", du_x (2500), r));
  QVERIFY (rf->supports (r));
  // no horizontal variants for parentheses
  QVERIFY (!rf->get_wide_variant ("<wide-(>", du_x (1000), r));
  // top accent attachment of the math italic f (464 design units)
  SI x;
  QVERIFY (lm->get_top_accent ("<#1D453>", x));
  QCOMPARE (x, du_x (464));
  QVERIFY (!lm->get_top_accent ("1", x) || x > 0);
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  QVERIFY (!rm->get_top_accent ("f", x));
}

void
TestOpenTypeFont::test_feature_variants () {
  if (!have_lm) QSKIP ("no Latin Modern Math");
  string r;
  // Latin Modern Math: dtls maps math italic i (U+1D456) to glyph 1322,
  // ssty maps math italic x (U+1D465) to glyph 1427 (first alternate);
  // it has no flac feature
  QVERIFY (lm->get_feature_variant ("<#1D456>", "dtls", 0, r));
  QVERIFY2 (r == string ("<@") * as_hexadecimal (1322, 4) * ">",
            as_charp ("dtls of U+1D456 gave " * r));
  QVERIFY (lm->supports (r));
  QVERIFY (!lm->get_feature_variant ("<#1D465>", "dtls", 0, r));
  QVERIFY (lm->get_feature_variant ("<#1D465>", "ssty", 0, r));
  QCOMPARE (r, string ("<@") * as_hexadecimal (1427, 4) * ">");
  QVERIFY (lm->get_feature_variant ("<#1D465>", "ssty", 1, r));
  QVERIFY (!lm->get_feature_variant ("<#1D465>", "ssty", 2, r));
  QVERIFY (!lm->get_feature_variant ("<#302>", "flac", 0, r));
  // STIX Two Math: flac maps the combining circumflex to glyph 4800
  if (tt_font_exists ("STIXTwoMath-Regular")) {
    font st= unicode_font ("STIXTwoMath-Regular", LM_SIZE, LM_DPI);
    QVERIFY (st->get_feature_variant ("<#302>", "flac", 0, r));
    QCOMPARE (r, string ("<@") * as_hexadecimal (4800, 4) * ">");
    QVERIFY (st->get_feature_variant ("<#1D456>", "dtls", 0, r));
    QCOMPARE (r, string ("<@") * as_hexadecimal (3335, 4) * ">");
  }
  // fonts without a MATH table answer nothing
  font rm= unicode_font ("lmroman10-regular", LM_SIZE, LM_DPI);
  QVERIFY (!rm->get_feature_variant ("i", "dtls", 0, r));
}

void
TestOpenTypeFont::test_profiles () {
  // profiles are defined in Scheme at boot; here we set one by hand
  tree p (TUPLE);
  p << tuple ("file", "latinmodern-math") << tuple ("text", "Latin Modern Roman")
    << tuple ("letters", "math") << tuple ("menu", "Latin Modern");
  math_font_profile_set ("Latin Modern Math", p);
  QCOMPARE (math_font_profile_attr ("Latin Modern Math", "text"),
            string ("Latin Modern Roman"));
  QCOMPARE (math_font_profile_attr ("Latin Modern Math", "nonsense"), string (""));
  QCOMPARE (math_font_profile_attr ("No Such Font", "text"), string (""));
  QCOMPARE (math_family_for_text ("Latin Modern Roman"), string ("Latin Modern Math"));
  QCOMPARE (math_family_for_text ("Latin Modern Math"), string (""));
  QCOMPARE (text_family_for_math ("Latin Modern Math"), string ("Latin Modern Roman"));
  QVERIFY (N (math_font_profile_families ()) >= 1);
  QCOMPARE (N (math_font_profile ("Latin Modern Math")), 4);
}

QTEST_GUILESS_MAIN(TestOpenTypeFont)
#include "opentype_font_test.moc"
