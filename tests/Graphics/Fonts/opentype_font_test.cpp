
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

QTEST_GUILESS_MAIN(TestOpenTypeFont)
#include "opentype_font_test.moc"
