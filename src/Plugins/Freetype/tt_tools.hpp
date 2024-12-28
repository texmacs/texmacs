
/******************************************************************************
* MODULE     : tt_tools.hpp
* DESCRIPTION: Direct access of True Type font (independent from FreeType)
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TT_TOOLS_H
#define TT_TOOLS_H
#include "url.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

void tt_dump (url u);
scheme_tree tt_font_name (url u);
url tt_unpack (string name);

string find_attribute_value (array<string> a, string s);
array<string> tt_analyze (string family);
double characteristic_distance (array<string> a1, array<string> a2);
double trace_distance (string v1, string v2, double m);

// quantities with respect to ex height
double get_M_width       (array<string> a);
double get_lo_pen_width  (array<string> a);
double get_lo_pen_height (array<string> a);
double get_up_pen_width  (array<string> a);
double get_up_pen_height (array<string> a);


/******************************************************************************
 * OpenType MATH table
 ******************************************************************************/
// e OpenType MATH table is not implemented.
// see https://docs.microsoft.com/en-gb/typography/opentype/spec/math

// index of the MathConstantsTable.records
enum MathConstantRecordEnum {
  mathLeading,
  axisHeight,
  accentBaseHeight,
  flattenedAccentBaseHeight,
  subscriptShiftDown,
  subscriptTopMax,
  subscriptBaselineDropMin,
  superscriptShiftUp,
  superscriptShiftUpCramped,
  superscriptBottomMin,
  superscriptBaselineDropMax,
  subSuperscriptGapMin,
  superscriptBottomMaxWithSubscript,
  spaceAfterScript,
  upperLimitGapMin,
  upperLimitBaselineRiseMin,
  lowerLimitGapMin,
  lowerLimitBaselineDropMin,
  stackTopShiftUp,
  stackTopDisplayStyleShiftUp,
  stackBottomShiftDown,
  stackBottomDisplayStyleShiftDown,
  stackGapMin,
  stackDisplayStyleGapMin,
  stretchStackTopShiftUp,
  stretchStackBottomShiftDown,
  stretchStackGapAboveMin,
  stretchStackGapBelowMin,
  fractionNumeratorShiftUp,
  fractionNumeratorDisplayStyleShiftUp,
  fractionDenominatorShiftDown,
  fractionDenominatorDisplayStyleShiftDown,
  fractionNumeratorGapMin,
  fractionNumDisplayStyleGapMin,
  fractionRuleThickness,
  fractionDenominatorGapMin,
  fractionDenomDisplayStyleGapMin,
  skewedFractionHorizontalGap,
  skewedFractionVerticalGap,
  overbarVerticalGap,
  overbarRuleThickness,
  overbarExtraAscender,
  underbarVerticalGap,
  underbarRuleThickness,
  underbarExtraDescender,
  radicalVerticalGap,
  radicalDisplayStyleVerticalGap,
  radicalRuleThickness,
  radicalExtraAscender,
  radicalKernBeforeDegree,
  radicalKernAfterDegree,
  otmathConstantsRecordsEnd, // count the number of records
  scriptPercentScaleDown,
  scriptScriptPercentScaleDown,
  delimitedSubFormulaMinHeight,
  displayOperatorMinHeight,
  radicalDegreeBottomRaisePercent
};

struct DeviceTable {
  unsigned int startSize;
  unsigned int endSize;
  unsigned int deltaFormat;
  unsigned int deltaValues;
};

struct MathValueRecord {
  int         value;
  bool        hasDevice;
  DeviceTable deviceTable;
  MathValueRecord () : hasDevice (false) {}

  // cast to int
  operator int () const { return value; }
};

struct MathConstantsTable {
  int                    scriptPercentScaleDown;
  int                    scriptScriptPercentScaleDown;
  unsigned int           delimitedSubFormulaMinHeight;
  unsigned int           displayOperatorMinHeight;
  array<MathValueRecord> records;
  int                    radicalDegreeBottomRaisePercent;
  MathConstantsTable ()
      : records (MathConstantRecordEnum::otmathConstantsRecordsEnd){};

  int operator[] (int i) {
    if (i >= 0 && i < MathConstantRecordEnum::otmathConstantsRecordsEnd)
      return records[i];
    switch (i) {
    case MathConstantRecordEnum::scriptPercentScaleDown:
      return scriptPercentScaleDown;
    case MathConstantRecordEnum::scriptScriptPercentScaleDown:
      return scriptScriptPercentScaleDown;
    case MathConstantRecordEnum::delimitedSubFormulaMinHeight:
      return delimitedSubFormulaMinHeight;
    case MathConstantRecordEnum::displayOperatorMinHeight:
      return displayOperatorMinHeight;
    case MathConstantRecordEnum::radicalDegreeBottomRaisePercent:
      return radicalDegreeBottomRaisePercent;
    }
    FAILED ("MathConstantsTable: index out of range");
    return 0; // should never reach here
  }
};

struct MathKernTable {
  unsigned int           heightCount;
  array<MathValueRecord> correctionHeight;
  array<MathValueRecord> kernValues;
  MathKernTable ()= default;
  MathKernTable (unsigned int h)
      : heightCount (h), correctionHeight (h), kernValues (h + 1) {}
};

struct MathKernInfoRecord {
  MathKernTable topRight;
  MathKernTable topLeft;
  MathKernTable bottomRight;
  MathKernTable bottomLeft;
  bool          hasTopRight;
  bool          hasTopLeft;
  bool          hasBottomRight;
  bool          hasBottomLeft;
  MathKernInfoRecord ()
      : hasTopRight (false), hasTopLeft (false), hasBottomRight (false),
        hasBottomLeft (false) {}

  bool has_kerning (bool top, bool left);
  int  get_kerning (int height, bool top, bool left);
};

struct GlyphPartRecord {
  unsigned int glyphID;
  unsigned int startConnectorLength;
  unsigned int endConnectorLength;
  unsigned int fullAdvance;
  unsigned int partFlags;
};

struct GlyphAssembly {
  MathValueRecord        italicsCorrection;
  array<GlyphPartRecord> partRecords;
  int                    partCount;

  const GlyphPartRecord& operator[] (int i) { return partRecords[i]; }
};

struct ot_mathtable_rep : concrete_struct {
  unsigned int                               majorVersion, minorVersion;
  MathConstantsTable                         constants_table;
  unsigned int                               minConnectorOverlap;
  hashmap<unsigned int, MathValueRecord>     italics_correction;
  hashmap<unsigned int, MathValueRecord>     top_accent;
  hashset<unsigned int>                      extended_shape_coverage;
  hashmap<unsigned int, MathKernInfoRecord>  math_kern_info;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants_adv;
  hashmap<unsigned int, GlyphAssembly>       ver_glyph_assembly;
  hashmap<unsigned int, GlyphAssembly>       hor_glyph_assembly;

  // helper functions and data
  hashmap<unsigned int, unsigned int> get_init_glyphID_cache;
  // for variant glyph, get the glyphID of the base glyph
  unsigned int get_init_glyphID (unsigned int glyphID);

  bool has_kerning (unsigned int glyphID, bool top, bool left);
  int  get_kerning (unsigned int glyphID, int height, bool top, bool left);
};

struct ot_mathtable {
  CONCRETE_NULL (ot_mathtable);
  ot_mathtable (ot_mathtable_rep* rep2) : rep (rep2) {}
};
CONCRETE_NULL_CODE (ot_mathtable);

ot_mathtable parse_mathtable (const string& buf);
ot_mathtable parse_mathtable (url u);

#endif // TT_TOOLS_H
