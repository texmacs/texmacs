
/******************************************************************************
* MODULE     : tt_tools_test.cpp
* DESCRIPTION: tests of the OpenType MATH table parser
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// The expected values below were extracted independently with fontTools
// (python3 -m fontTools.ttx or a small script over font['MATH']).
// Glyph ids refer to the fonts shipped in TeXmacs/fonts/truetype.

#include <QtTest/QtTest>
#include "Freetype/tt_tools.hpp"
#include "sys_utils.hpp"
#include "file.hpp"

static url
shipped_font (string name) {
  return url ("$TEXMACS_PATH/fonts/truetype") * url (name);
}

// optional directory with additional fonts, see tests/Makefile
static url
extra_font (string name) {
  string dir= get_env ("TM_TEST_FONT_DIR");
  if (dir == "") return url_none ();
  url u= complete (search_sub_dirs (url_system (dir)) * url_wildcard (name), "fr");
  while (is_or (u)) u= u[1];
  return u;
}

class TestTTTools: public QObject {
  Q_OBJECT

private slots:
  void test_no_math_table ();
  void test_garbage ();
  void test_pagella_constants ();
  void test_pagella_glyph_info ();
  void test_pagella_vertical_variants ();
  void test_pagella_horizontal_variants ();
  void test_pagella_no_kerning ();
  void test_stixtwo_kerning ();
  void test_gpos_kern ();
};

void
TestTTTools::test_no_math_table () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-regular.otf"));
  QVERIFY (is_nil (t));
}

void
TestTTTools::test_garbage () {
  QVERIFY (is_nil (parse_mathtable (string (""))));
  QVERIFY (is_nil (parse_mathtable (string ("this is not a font"))));
  QVERIFY (is_nil (parse_mathtable (url ("$TEXMACS_PATH/fonts/no-such-font.otf"))));
}

void
TestTTTools::test_pagella_constants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  QVERIFY (!is_nil (t));
  QCOMPARE ((int) t->majorVersion, 1);
  QCOMPARE ((int) t->minorVersion, 0);
  MathConstantsTable& c= t->constants_table;
  QCOMPARE (c[scriptPercentScaleDown], 74);
  QCOMPARE (c[scriptScriptPercentScaleDown], 55);
  QCOMPARE (c[delimitedSubFormulaMinHeight], 1500);
  QCOMPARE (c[displayOperatorMinHeight], 1500);
  QCOMPARE (c[radicalDegreeBottomRaisePercent], 55);
  QCOMPARE (c[mathLeading], 160);
  QCOMPARE (c[axisHeight], 250);
  QCOMPARE (c[accentBaseHeight], 500);
  QCOMPARE (c[flattenedAccentBaseHeight], 670);
  QCOMPARE (c[subscriptShiftDown], 232);
  QCOMPARE (c[superscriptShiftUp], 354);
  QCOMPARE (c[superscriptShiftUpCramped], 296);
  QCOMPARE (c[spaceAfterScript], 40);
  QCOMPARE (c[upperLimitGapMin], 120);
  QCOMPARE (c[upperLimitBaselineRiseMin], 120);
  QCOMPARE (c[lowerLimitGapMin], 120);
  QCOMPARE (c[lowerLimitBaselineDropMin], 468);
  QCOMPARE (c[stackTopDisplayStyleShiftUp], 720);
  QCOMPARE (c[fractionNumeratorShiftUp], 469);
  QCOMPARE (c[fractionNumeratorDisplayStyleShiftUp], 720);
  QCOMPARE (c[fractionDenominatorShiftDown], 311);
  QCOMPARE (c[fractionDenominatorDisplayStyleShiftDown], 666);
  QCOMPARE (c[fractionNumeratorGapMin], 120);
  QCOMPARE (c[fractionNumDisplayStyleGapMin], 160);
  QCOMPARE (c[fractionRuleThickness], 60);
  QCOMPARE (c[fractionDenominatorGapMin], 120);
  QCOMPARE (c[fractionDenomDisplayStyleGapMin], 160);
  QCOMPARE (c[skewedFractionHorizontalGap], 350);
  QCOMPARE (c[overbarExtraAscender], 40);
  QCOMPARE (c[underbarExtraDescender], 40);
  QCOMPARE (c[radicalVerticalGap], 120);
  QCOMPARE (c[radicalDisplayStyleVerticalGap], 120);
  QCOMPARE (c[radicalRuleThickness], 60);
  QCOMPARE (c[radicalExtraAscender], 120);
  QCOMPARE (c[radicalKernBeforeDegree], 40);
  // negative value: checks signed 16 bit decoding
  QCOMPARE (c[radicalKernAfterDegree], -344);
}

void
TestTTTools::test_pagella_glyph_info () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  QVERIFY (!is_nil (t));
  QCOMPARE (N (t->italics_correction), 1253);
  QCOMPARE (N (t->top_accent), 1941);
  QCOMPARE (N (t->extended_shape_coverage), 290);

  // U+222B integral, glyph 4113
  QVERIFY (t->italics_correction->contains (4113));
  QCOMPARE (t->italics_correction[4113].value, 189);
  QVERIFY (t->extended_shape_coverage->contains (4113));
  // U+2211 summation, glyph 4214
  QVERIFY (t->extended_shape_coverage->contains (4214));
  QVERIFY (!t->italics_correction->contains (4214));
  // U+1D453 math italic f, glyph 474
  QCOMPARE (t->italics_correction[474].value, 163);
  QCOMPARE (t->top_accent[474].value, 356);
  // U+1D434 math italic A, glyph 443: top accent only
  QVERIFY (!t->italics_correction->contains (443));
  QCOMPARE (t->top_accent[443].value, 485);
  // U+0302 combining circumflex, glyph 3374: negative top accent
  QCOMPARE (t->top_accent[3374].value, -250);
  // 'm', glyph 78
  QCOMPARE (t->italics_correction[78].value, 14);
}

void
TestTTTools::test_pagella_vertical_variants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  QVERIFY (!is_nil (t));
  QCOMPARE ((int) t->minConnectorOverlap, 30);
  QCOMPARE (N (t->ver_glyph_variants), 95);

  // U+0028 parenleft, glyph 9: the first variant is the base glyph itself
  QVERIFY (t->ver_glyph_variants->contains (9));
  array<unsigned int> v= t->ver_glyph_variants[9];
  array<unsigned int> a= t->ver_glyph_variants_adv[9];
  QCOMPARE (N (v), 7);
  QCOMPARE (N (a), 7);
  unsigned int ev[]= {9, 3461, 3483, 3505, 3527, 3549, 3571};
  unsigned int ea[]= {829, 989, 1181, 1411, 1687, 2019, 2417};
  for (int i= 0; i < 7; i++) {
    QCOMPARE (v[i], ev[i]);
    QCOMPARE (a[i], ea[i]);
  }
  // variants map back to their base glyph
  QCOMPARE (t->get_init_glyphID (3483), 9u);
  QCOMPARE (t->get_init_glyphID (3571), 9u);
  QCOMPARE (t->get_init_glyphID (9), 9u);
  QCOMPARE (t->get_init_glyphID (4214), 4214u); // no variants: itself

  // assembly of parenleft: bottom, extender, top
  QVERIFY (t->ver_glyph_assembly->contains (9));
  GlyphAssembly g= t->ver_glyph_assembly[9];
  QCOMPARE (g.partCount, 3);
  QCOMPARE (N (g.partRecords), 3);
  QCOMPARE (g.italicsCorrection.value, 0);
  QCOMPARE (g.partRecords[0].glyphID, 3575u);
  QCOMPARE (g.partRecords[0].startConnectorLength, 0u);
  QCOMPARE (g.partRecords[0].endConnectorLength, 198u);
  QCOMPARE (g.partRecords[0].fullAdvance, 1208u);
  QCOMPARE (g.partRecords[0].partFlags, 0u);
  QCOMPARE (g.partRecords[1].glyphID, 3576u);
  QCOMPARE (g.partRecords[1].startConnectorLength, 396u);
  QCOMPARE (g.partRecords[1].endConnectorLength, 396u);
  QCOMPARE (g.partRecords[1].fullAdvance, 396u);
  QCOMPARE (g.partRecords[1].partFlags, 1u); // extender
  QCOMPARE (g.partRecords[2].glyphID, 3577u);
  QCOMPARE (g.partRecords[2].partFlags, 0u);

  // braceleft, glyph 92: five parts with two extenders
  GlyphAssembly b= t->ver_glyph_assembly[92];
  QCOMPARE (b.partCount, 5);
  QCOMPARE (b.partRecords[1].glyphID, 3590u);
  QCOMPARE (b.partRecords[3].glyphID, 3590u);
  QCOMPARE (b.partRecords[1].partFlags, 1u);
  QCOMPARE (b.partRecords[3].partFlags, 1u);
  QCOMPARE (b.partRecords[2].fullAdvance, 1194u);

  // summation, glyph 4214: two sizes, no assembly
  QCOMPARE (N (t->ver_glyph_variants[4214]), 2);
  QCOMPARE (t->ver_glyph_variants[4214][1], 4217u);
  QVERIFY (!t->ver_glyph_assembly->contains (4214));

  // integral, glyph 4113: seven sizes and an assembly
  QCOMPARE (N (t->ver_glyph_variants[4113]), 7);
  QCOMPARE (t->ver_glyph_variants[4113][6], 4185u);
  QCOMPARE (t->ver_glyph_variants_adv[4113][6], 2903u);
  QVERIFY (t->ver_glyph_assembly->contains (4113));
}

void
TestTTTools::test_pagella_horizontal_variants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  QVERIFY (!is_nil (t));
  QCOMPARE (N (t->hor_glyph_variants), 86);

  // U+2192 arrowright, glyph 2962
  QVERIFY (t->hor_glyph_variants->contains (2962));
  QVERIFY (!t->ver_glyph_variants->contains (2962));
  array<unsigned int> v= t->hor_glyph_variants[2962];
  QCOMPARE (N (v), 2);
  QCOMPARE (v[0], 2962u);
  QCOMPARE (v[1], 2964u);
  QCOMPARE (t->hor_glyph_variants_adv[2962][1], 1211u);
  QCOMPARE (t->get_init_glyphID (2964), 2962u);
  GlyphAssembly g= t->hor_glyph_assembly[2962];
  QCOMPARE (g.partCount, 3);
  QCOMPARE (g.partRecords[0].glyphID, 2968u);
  QCOMPARE (g.partRecords[1].glyphID, 2969u);
  QCOMPARE (g.partRecords[1].partFlags, 1u);
  QCOMPARE (g.partRecords[2].glyphID, 2970u);

  // U+0302 circumflex, glyph 3374: seven widths, no assembly
  QCOMPARE (N (t->hor_glyph_variants[3374]), 7);
  QCOMPARE (t->hor_glyph_variants[3374][6], 3434u);
  QVERIFY (!t->hor_glyph_assembly->contains (3374));

  // U+23DE overbrace, glyph 3453: five parts with a middle piece
  GlyphAssembly b= t->hor_glyph_assembly[3453];
  QCOMPARE (b.partCount, 5);
  QCOMPARE (b.partRecords[2].glyphID, 3621u);
  QCOMPARE (b.partRecords[2].fullAdvance, 1769u);
}

void
TestTTTools::test_pagella_no_kerning () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  QVERIFY (!is_nil (t));
  QCOMPARE (N (t->math_kern_info), 0);
  QVERIFY (!t->has_kerning (474, true, false));
  QVERIFY (!t->has_kerning (474, false, true));
}

void
TestTTTools::test_stixtwo_kerning () {
  url u= extra_font ("STIXTwoMath-Regular.otf");
  if (is_none (u)) QSKIP ("set TM_TEST_FONT_DIR to a directory containing STIXTwoMath-Regular.otf");
  ot_mathtable t= parse_mathtable (u);
  QVERIFY (!is_nil (t));
  QCOMPARE (N (t->math_kern_info), 233);
  QCOMPARE ((int) t->minConnectorOverlap, 100);

  // U+1D449 math italic V, glyph 3321:
  //   top right:    no heights, single value 0
  //   bottom right: heights [156, 280], values [-222, -118, 202]
  QVERIFY (t->has_kerning (3321, true, false));
  QVERIFY (t->has_kerning (3321, false, false));
  QVERIFY (!t->has_kerning (3321, true, true));
  QVERIFY (!t->has_kerning (3321, false, true));
  QCOMPARE (t->get_kerning (3321, -1000, true, false), 0);
  QCOMPARE (t->get_kerning (3321, 1000, true, false), 0);
  QCOMPARE (t->get_kerning (3321, 0, false, false), -222);
  QCOMPARE (t->get_kerning (3321, 155, false, false), -222);
  QCOMPARE (t->get_kerning (3321, 156, false, false), -118);
  QCOMPARE (t->get_kerning (3321, 200, false, false), -118);
  QCOMPARE (t->get_kerning (3321, 279, false, false), -118);
  QCOMPARE (t->get_kerning (3321, 280, false, false), 202);
  QCOMPARE (t->get_kerning (3321, 5000, false, false), 202);

  // U+1D434 math italic A, glyph 3300: top right [213, 350] -> [58, -58, -70]
  QVERIFY (t->has_kerning (3300, true, false));
  QCOMPARE (t->get_kerning (3300, 100, true, false), 58);
  QCOMPARE (t->get_kerning (3300, 300, true, false), -58);
  QCOMPARE (t->get_kerning (3300, 400, true, false), -70);
}

void
TestTTTools::test_gpos_kern () {
  // Pair kerning of the GPOS 'kern' feature; expected values and glyph ids
  // extracted with fontTools. These fonts have no legacy 'kern' table.
  string buf;
  QVERIFY (!load_string (shipped_font ("stix2/STIXTwoText-Regular.otf"),
                         buf, false));
  ot_gpos_kern k= parse_gpos_kern (buf);
  QVERIFY (!is_nil (k));
  QVERIFY (!k->empty ());
  QCOMPARE (k->get (3, 24), -100);   // A V
  QCOMPARE (k->get (24, 3), -105);   // V A
  QCOMPARE (k->get (22, 270), -70);  // T o
  QCOMPARE (k->get (25, 255), -60);  // W a
  QCOMPARE (k->get (260, 263), 10);  // f i, a positive adjustment
  QCOMPARE (k->get (273, 1829), -65);// r .
  QCOMPARE (k->get (18, 255), -25);  // P a
  QCOMPARE (k->get (3, 3), 0);       // A A is not kerned

  string buf2;
  QVERIFY (!load_string (shipped_font ("newcm/NewCM10-Regular.otf"),
                         buf2, false));
  ot_gpos_kern k2= parse_gpos_kern (buf2);
  QVERIFY (!is_nil (k2) && !k2->empty ());
  QCOMPARE (k2->get (34, 55), -111); // A V
  QCOMPARE (k2->get (53, 80), -83);  // T o
  QCOMPARE (k2->get (49, 66), -28);  // P a

  // a font without GPOS kerning gives an empty table
  string buf3;
  QVERIFY (!load_string (shipped_font ("lm/latinmodern-math.otf"),
                         buf3, false));
  ot_gpos_kern k3= parse_gpos_kern (buf3);
  QVERIFY (is_nil (k3) || k3->empty ());
}

QTEST_GUILESS_MAIN(TestTTTools)
#include "tt_tools_test.moc"
