
/******************************************************************************
* MODULE     : tt_tools.cpp
* DESCRIPTION: Direct access of True Type font (independent from FreeType)
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tt_tools.hpp"
#include "tt_file.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "iterator.hpp"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#define U8  uint8_t
#define U16 uint16_t
#define U32 uint32_t
#define U64 uint64_t
#define S16 int16_t
#else
#define U8  unsigned char
#define U16 unsigned short
#define U32 unsigned int
#define U64 unsigned long long int
#define S16 short
#endif

string strip_suffix (string name);

/******************************************************************************
* Data access
******************************************************************************/

string
get_sub (string s, int i, int j) {
  i= min (max (i, 0), N(s));
  j= min (max (j, 0), N(s));
  return s (i, j);
}

string
get_tag (string s, int i) {
  return get_sub (s, i, i+4);
}

U8
get_U8 (string s, int i) {
  return *((U8*) ((void*) (&s[i])));
}

U16
get_U16 (string s, int i) {
  return (((U16) get_U8 (s, i)) << 8) + get_U8 (s, i+1);
}

U32
get_U32 (string s, int i) {
  return (((U32) get_U16 (s, i)) << 16) + get_U16 (s, i+2);
}

S16
get_S16 (string s, int i) {
  return (S16) get_U16 (s, i);
}

string
pack_U32 (U32 i) {
  string r;
  r << ((char) ( i >> 24));
  r << ((char) ((i >> 16) & 255));
  r << ((char) ((i >>  8) & 255));
  r << ((char) ( i        & 255));
  return r;
}

/******************************************************************************
* True Type font collections
******************************************************************************/

bool
tt_is_collection (string tt) {
  return get_tag (tt, 0) == "ttcf";
}

int
tt_nr_fonts (string tt) {
  if (!tt_is_collection (tt)) return 1;
  else return get_U32 (tt, 8);
}

int
tt_header_index (string tt, int i) {
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return 0;
  else return get_U32 (tt, 12 + 4*i);
}

/******************************************************************************
* Font table management
******************************************************************************/

bool
tt_correct_version (string tt, int i) {
  int h= tt_header_index (tt, i);
  return
    get_U32 (tt, h) == 0x00010000 ||
    get_tag (tt, h) == "OTTO" ||
    get_tag (tt, h) == "true" ||
    get_tag (tt, h) == "typ1";
}

int
tt_nr_tables (string tt, int i) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  int h= tt_header_index (tt, i);
  return get_U16 (tt, h + 4);
}

string
tt_table_tag (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_tag (tt, h + 12 + 16 * k);
}

U32
tt_table_start (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_U32 (tt, h + 20 + 16 * k);
}

string
tt_table (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  int start= get_U32 (tt, h + 20 + 16 * k);
  int len  = get_U32 (tt, h + 24 + 16 * k);
  return get_sub (tt, start, start + len);
}

string
tt_table (string tt, int i, string tag) {
  for (int k=0; k<tt_nr_tables (tt, i); k++)
    if (tt_table_tag (tt, i, k) == tag)
      return tt_table (tt, i, k);
  return "";
}

/******************************************************************************
* Extracting a subfont
******************************************************************************/

string
tt_extract_subfont (string tt, int i) {
  if (i < 0 || i >= tt_nr_fonts (tt))
    cout << "TeXmacs] error, invalid TrueType subfont " << i
         << " out of " << tt_nr_fonts (tt) << "\n";
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return tt;
  string r;
  int h= tt_header_index (tt, i);
  r << get_sub (tt, h, h + 12);
  int nr_tabs= tt_nr_tables (tt, i);
  int offset= 12 + 16 * nr_tabs;
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    r << get_sub (tt, taboff, taboff + 8);
    r << pack_U32 (offset);
    r << get_sub (tt, taboff + 12, taboff + 16);
    int len= get_U32 (tt, taboff + 12);
    offset += (((len + 3) >> 2) << 2);
  }
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    int start= get_U32 (tt, taboff + 8);
    int len  = get_U32 (tt, taboff + 12);
    int plen = (((len + 3) >> 2) << 2);
    r << get_sub (tt, start, start + plen);
  }
  return r;
}

/******************************************************************************
* Name table
******************************************************************************/

int
name_format (string nt) {
  return get_U16 (nt, 0);
}

int
name_nr_records (string nt) {
  return get_U16 (nt, 2);
}

int
name_storage_offset (string nt) {
  return get_U16 (nt, 4);
}

string
name_record (string nt, int i) {
  ASSERT (i >= 0 && i < name_nr_records (nt), "index out of range");
  return get_sub (nt, 6 + 12*i, 18 + 12*i);
}

int
name_record_platform_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 0);
}

int
name_record_encoding_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 2);
}

int
name_record_language_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 4);
}

int
name_record_name_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 6);
}

string
name_record_string (string nt, int i) {
  int off= name_storage_offset (nt);
  string rec= name_record (nt, i);
  int start= get_U16 (rec, 10) + off;
  int len= get_U16 (rec, 8);
  return get_sub (nt, start, start + len);
}

string
filter_english (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] >= ' ' && (((unsigned int) s[i]) <= 127))
      r << s[i];
  return r;
}

string
name_record_english_string (string nt, int name_id) {
  for (int i=0; i < name_nr_records (nt); i++)
    if (name_record_name_id (nt, i) == name_id) {
      if ((name_record_platform_id (nt, i) == 1 &&
           name_record_language_id (nt, i) == 0) ||
          (name_record_platform_id (nt, i) == 3 &&
           name_record_language_id (nt, i) == 0x0409))
        return filter_english (name_record_string (nt, i));
    }
  return "";
}

string
name_record_family (string nt) {
  return name_record_english_string (nt, 1);
}

string
name_record_shape (string nt) {
  return name_record_english_string (nt, 2);
}

/******************************************************************************
* Dump all font info to the screen
******************************************************************************/

void
tt_dump (string tt) {
  for (int i=0; i<tt_nr_fonts (tt); i++) {
    cout << HRULE << "Font " << i << LF << HRULE;

    cout << "Font tables:";
    for (int k=0; k<tt_nr_tables (tt, i); k++)
      cout << " " << tt_table_tag (tt, i, k);
    cout << LF;

    string nt= tt_table (tt, i, "name");
    cout << "Font family: " << name_record_family (nt) << "\n";
    cout << "Font shape: " << name_record_shape (nt) << "\n";
    cout << "Name table" << LF;
    for (int k=0; k<name_nr_records (nt); k++)
      cout << "  " << name_record_name_id (nt, k)
           << " [" << name_record_language_id (nt, k)
           << ", " << name_record_platform_id (nt, k) << "]"
           << " -> " << name_record_string (nt, k) << LF;
  }
  dump_mathtable (cout, parse_mathtable (tt));
}

void
tt_dump (url u) {
  string tt;
  if (!load_string (u, tt, false))
    tt_dump (tt);
  else cout << "file not found";
}

/******************************************************************************
* Get the font family and available shapes
******************************************************************************/

void
move_to_shape (string& fam, string& shape, string what, string by) {
  int pos= search_forwards (" " * what, 0, fam);
  if (pos < 0) return;
  fam  = fam (0, pos) * fam (pos + N(what) + 1, N(fam));
  if (N(by) == 0) return;
  if (shape == "Regular") shape= by;
  else shape= by * " " * shape;
}

scheme_tree
tt_font_name (url u) {
  string tt;
  tree r (TUPLE);
  if (load_string (u, tt, false)) return r;
  for (int i=0; i < tt_nr_fonts (tt); i++) {
    if (!tt_correct_version (tt, i)) return tree (TUPLE);
    string nt = tt_table (tt, i, "name");
    string fam= name_record_family (nt);
    string sh = name_record_shape (nt);

    // Some basic normalization of family name
    move_to_shape (fam, sh, "Narrow", "Narrow");
    move_to_shape (fam, sh, "Condensed", "Condensed");
    move_to_shape (fam, sh, "Extended", "Extended");
    move_to_shape (fam, sh, "Wide", "Wide");
    move_to_shape (fam, sh, "Caption", "Caption");
    move_to_shape (fam, sh, "Semilight", "SemiLight");
    move_to_shape (fam, sh, "SemiLight", "SemiLight");
    move_to_shape (fam, sh, "Semi Light", "SemiLight");
    move_to_shape (fam, sh, "Ultralight", "Thin");
    move_to_shape (fam, sh, "UltraLight", "Thin");
    move_to_shape (fam, sh, "Ultra Light", "Thin");
    move_to_shape (fam, sh, "Light", "Light");
    move_to_shape (fam, sh, "Medium", "");
    move_to_shape (fam, sh, "Semibold", "SemiBold");
    move_to_shape (fam, sh, "SemiBold", "SemiBold");
    move_to_shape (fam, sh, "Semi Bold", "SemiBold");
    move_to_shape (fam, sh, "Demibold", "DemiBold");
    move_to_shape (fam, sh, "DemiBold", "DemiBold");
    move_to_shape (fam, sh, "Demi Bold", "DemiBold");
    move_to_shape (fam, sh, "Bold", "Bold");
    move_to_shape (fam, sh, "Extrabold", "ExtraBold");
    move_to_shape (fam, sh, "ExtraBold", "ExtraBold");
    move_to_shape (fam, sh, "Extra Bold", "ExtraBold");
    move_to_shape (fam, sh, "Heavy", "Heavy");
    move_to_shape (fam, sh, "Black", "Black");
    move_to_shape (fam, sh, "Italic", "Italic");
    move_to_shape (fam, sh, "Oblique", "Oblique");

    while (fam != "" && !is_alpha (fam[0])) fam= fam (1, N(fam));
    if (upcase_all (fam) == fam) fam= locase_all (fam);
    fam= upcase_first (fam);
    if (starts (fam, "STIX")) fam= "Stix" * fam (4, N(fam));
    // End normalization of family name
    
    r << tuple (fam, sh);
  }
  return r;
}

url
tt_unpack (string s) {
  if (!is_int (suffix (url (s)))) return url_none ();
  url dir= url ("$TEXMACS_HOME_PATH/fonts/unpacked");
  if (!exists (dir)) mkdir (dir);
  url name= dir * url (s * ".ttf");
  if (exists (name)) return name;
  //cout << "Extracting " << name << "\n";
  int i= as_int (suffix (url (s)));
  s= strip_suffix (s);
  url u= tt_font_find (s);
  if (is_none (u)) return url_none ();
  string ttc;
  if (load_string (u, ttc, false)) return url_none ();
  string tt= tt_extract_subfont (ttc, i);
  if (save_string (name, tt, false)) return url_none ();
  return name;
}

/******************************************************************************
 * OpenType MATH table
 ******************************************************************************/

unsigned int
ot_mathtable_rep::get_init_glyphID (unsigned int glyphID) {
  // init cache
  if (N (get_init_glyphID_cache) == 0) {
    auto it= iterate (ver_glyph_variants);
    while (it->busy ()) {
      unsigned int        gid= it->next ();
      array<unsigned int> v  = ver_glyph_variants (gid);
      for (int i=0; i < N(v); i++) {
        get_init_glyphID_cache (v[i])= gid;
      }
    }
    it= iterate (hor_glyph_variants);
    while (it->busy ()) {
      unsigned int        gid= it->next ();
      array<unsigned int> v  = hor_glyph_variants (gid);
      for (int i=0; i < N(v); i++) {
        get_init_glyphID_cache (v[i])= gid;
      }
    }
  }

  // look up cache
  if (get_init_glyphID_cache->contains (glyphID)) {
    return get_init_glyphID_cache (glyphID);
  }

  return glyphID;
}

bool
MathKernInfoRecord::has_kerning (bool top, bool left) {
  return (top ? (left ? hasTopLeft : hasTopRight)
              : (left ? hasBottomLeft : hasBottomRight));
}

// get the kerning value of a glyphID with a given height
// should be called after has_kerning
int
MathKernInfoRecord::get_kerning (int height, bool top, bool left) {
  MathKernTable& kt=
      top ? (left ? topLeft : topRight) : (left ? bottomLeft : bottomRight);

  int idx= 0, n= (int) kt.heightCount;

  // only one kerning value
  if (n == 0) {
    return kt.kernValues[0];
  }

  // find the kerning value
  if (height < kt.correctionHeight[0]) {
    idx= 0; // below the first height entry
  }
  else if (height >= kt.correctionHeight[n - 1]) {
    idx= n; // above the last height entry
  }
  else {
    for (idx= 1; idx <= n - 1; idx++) {
      if (height >= kt.correctionHeight[idx - 1] &&
          height < kt.correctionHeight[idx]) {
        break;
      }
    }
  }
  // cout << "get_kerning : height " << height << " -> " << idx << " -> "
  //  << kt.kernValues[idx] << LF;
  return kt.kernValues[idx];
}

bool
ot_mathtable_rep::has_kerning (unsigned int glyphID, bool top, bool left) {
  return math_kern_info->contains (glyphID) &&
         math_kern_info (glyphID).has_kerning (top, left);
}

int
ot_mathtable_rep::get_kerning (unsigned int glyphID, int height, bool top,
                               bool left) {
  // should be called after has_kerning
  auto& record= math_kern_info (glyphID);
  return record.get_kerning (height, top, left);
}

// a helper function to parse the coverage table
// return an array of glyphID
static array<unsigned int>
parse_coverage_table (const string& tt, int offset) {
  int format= get_U16 (tt, offset);
  array<unsigned int> coverage;
  if (format == 1) {
    unsigned int glyphCount= get_U16 (tt, offset + 2);
    for (unsigned int i= 0; i < glyphCount; i++) {
      unsigned int glyphID= get_U16 (tt, offset + 4 + 2 * i);
      coverage << glyphID;
    }
  }
  else if (format == 2) {
    unsigned int rangeCount= get_U16 (tt, offset + 2);
    for (unsigned int i= 0; i < rangeCount; i++) {
      unsigned int startGlyphID= get_U16 (tt, offset + 4 + 6 * i);
      unsigned int endGlyphID= get_U16 (tt, offset + 4 + 6 * i + 2);
      for (unsigned int glyphID= startGlyphID; glyphID <= endGlyphID;
           glyphID++) {
        coverage << glyphID;
      }
    }
  }
  else {
    cout << "parse_mathtable : glyphCoverageFormat " << format
         << " not supported." << LF;
  }
  return coverage;
}

// a helper function to parse the MathValueRecord
static MathValueRecord
parse_math_record (const string& tt, int parent_table_offset,
                   int record_offset) {
  MathValueRecord record;
  int value= get_S16 (tt, parent_table_offset + record_offset);
  unsigned int deviceOffset=
      get_U16 (tt, parent_table_offset + record_offset + 2);
  if (deviceOffset > 0) {
    int deviceAbsOffset= parent_table_offset + deviceOffset;
    unsigned int startSize= get_U16 (tt, deviceAbsOffset);
    unsigned int endSize= get_U16 (tt, deviceAbsOffset + 2);
    unsigned int deltaFormat= get_U16 (tt, deviceAbsOffset + 4);
    unsigned int deltaValues= get_U16 (tt, deviceAbsOffset + 6);
    record.hasDevice= true;
    record.deviceTable= {startSize, endSize, deltaFormat, deltaValues};
  }
  record.value= value;
  return record;
}

// a helper function to parse the MathGlyphConstruction table
// return true if the sub table GlyphAssembly is not NULL
static bool
parse_construction (const string& tt, unsigned int construction_offset,
                    array<unsigned int>& variantGlyph,
                    array<unsigned int>& advanceMeasurement,
                    GlyphAssembly& assembly) {
  unsigned int glyphAssemblyOffset= get_U16 (tt, construction_offset);
  unsigned int variantCount= get_U16 (tt, construction_offset + 2);

  // MathGlyphVariantRecord
  for (unsigned int i= 0; i < variantCount; i++) {
    unsigned int mathGlyphVariantGlyph=
        get_U16 (tt, construction_offset + 4 + 4*i);
    unsigned int mathGlyphVariantAdvanceMeasurement=
        get_U16 (tt, construction_offset + 4 + 4*i + 2);
    variantGlyph << mathGlyphVariantGlyph;
    advanceMeasurement << mathGlyphVariantAdvanceMeasurement;
  }

  // GlyphAssembly table, may be NULL
  if (glyphAssemblyOffset > 0) {
    // TODO
    int glyphAssemblyAbsOffset= construction_offset + glyphAssemblyOffset;
    assembly.italicsCorrection=
        parse_math_record (tt, glyphAssemblyAbsOffset, 0);
    unsigned int partCount= get_U16 (tt, glyphAssemblyAbsOffset + 4);
    // GlyphPart records
    for (int j= 0, offset= 6; j < partCount; j++) {
      array<unsigned int> part (5);
      for (int k= 0; k < 5; k++, offset+= 2) {
        part[k]=
            get_U16 (tt, construction_offset + glyphAssemblyOffset + offset);
      }
      assembly.partRecords << GlyphPartRecord{part[0], part[1], part[2],
                                              part[3], part[4]};
    }
    assembly.partCount= partCount;
  }
  return (glyphAssemblyOffset > 0);
}

// a helper function to parse the MathVariants table
static void
parse_variants (const string& tt, int var_offset, int coverage_offset,
                int construction_offset,
                hashmap<unsigned int, array<unsigned int>>& glyph_variants,
                hashmap<unsigned int, array<unsigned int>>& glyph_variants_adv,
                hashmap<unsigned int, GlyphAssembly>& glyph_assembly) {

  auto coverage= parse_coverage_table (tt, coverage_offset);
  int  coverage_N= N (coverage);
  for (unsigned int i= 0; i < coverage_N; i++) {
    unsigned int glyph= coverage[i];
    unsigned int glyphConstructionOffset=
        get_U16 (tt, construction_offset + 2 * i);
    array<unsigned int> variants;
    array<unsigned int> adv;
    GlyphAssembly assembly;
    bool has_assemply= parse_construction (tt, 
      var_offset + glyphConstructionOffset, variants, adv, assembly);
    glyph_variants (glyph)= variants;
    glyph_variants_adv (glyph)= adv;
    if (has_assemply) {
      glyph_assembly (glyph)= assembly;
    }
  }
}

// a helper function to parse the MathKern table
static MathKernTable
parse_math_kern_table (const string& tt, int offset) {
  unsigned int  heightCount= get_U16 (tt, offset);
  MathKernTable kern (heightCount);
  for (unsigned int i= 0; i < heightCount; i++) {
    kern.correctionHeight[i]= parse_math_record (tt, offset, 2 + 4 * i);
  }
  for (unsigned int i= 0; i < heightCount + 1; i++) {
    kern.kernValues[i]=
        parse_math_record (tt, offset, 2 + 4 * heightCount + 4 * i);
  }
  return kern;
}

// a helper function to parse the MathKernInfo table
static void
parse_math_kern_info_table (const string& tt, int offset,
                            hashmap<unsigned int, MathKernInfoRecord>& table) {
  unsigned int mathKernCoverageOffset= get_U16 (tt, offset);
  // unsigned int mathKernCount          = get_U16 (tt, offset + 2);
  auto coverage= parse_coverage_table (tt, offset + mathKernCoverageOffset);
  int  coverage_N= N (coverage);
  for (unsigned int i= 0; i < coverage_N; i++) {
    unsigned int glyphID= coverage[i];
    MathKernInfoRecord record;
    unsigned int topRightMathKernOffset= get_U16 (tt, offset + 4 + 8 * i);
    unsigned int topLeftMathKernOffset= get_U16 (tt, offset + 4 + 8 * i + 2);
    unsigned int bottomRightMathKernOffset=
        get_U16 (tt, offset + 4 + 8 * i + 4);
    unsigned int bottomLeftMathKernOffset= get_U16 (tt, offset + 4 + 8 * i + 6);
    if (topRightMathKernOffset > 0) {
      record.topRight=
          parse_math_kern_table (tt, offset + topRightMathKernOffset);
      record.hasTopRight= true;
    }
    if (topLeftMathKernOffset > 0) {
      record.topLeft=
          parse_math_kern_table (tt, offset + topLeftMathKernOffset);
      record.hasTopLeft= true;
    }
    if (bottomRightMathKernOffset > 0) {
      record.bottomRight=
          parse_math_kern_table (tt, offset + bottomRightMathKernOffset);
      record.hasBottomRight= true;
    }
    if (bottomLeftMathKernOffset > 0) {
      record.bottomLeft=
          parse_math_kern_table (tt, offset + bottomLeftMathKernOffset);
      record.hasBottomLeft= true;
    }
    table (glyphID)= record;
  }
}

// a helper function to parse the MathValueRecords with coverage table,
// used in parsing MathItalicsCorrectionInfo and MathTopAccentAttachment
static void
parse_record_with_coverage (
    const string& tt, int parent_table_offset, int coverage_offset,
    int record_offset, hashmap<unsigned int, MathValueRecord>& record_map) {
  int coverage_abs_offset = parent_table_offset + coverage_offset;
  array<unsigned int> coverage= parse_coverage_table (tt, coverage_abs_offset);
  int record_abs_offset= parent_table_offset + record_offset;
  int coverage_N= N (coverage);
  for (unsigned int i= 0; i < coverage_N; i++) {
    unsigned int glyphID= coverage[i];
    record_map (glyphID)=
        parse_math_record (tt, parent_table_offset, record_offset + 4 * i);
  }
  (void) record_abs_offset; // avoid unused warning
}

// parse the OpenType MATH constants table.
static void
parse_constant (const string& tt, int offset, MathConstantsTable& table) {
  int scriptPercentScaleDown= get_S16 (tt, offset);
  int scriptScriptPercentScaleDown= get_S16 (tt, offset + 2);
  unsigned int delimitedSubFormulaMinHeight= get_U16 (tt, offset + 4);
  unsigned int displayOperatorMinHeight= get_U16 (tt, offset + 6);
  for (int i= 0; i < otmathConstantsRecordsEnd; i++) {
    table.records[i]= parse_math_record (tt, offset, 8 + 4 * i);
  }
  int radicalDegreeBottomRaisePercent=
      get_S16 (tt, offset + 8 + 4 * otmathConstantsRecordsEnd);
  table.scriptPercentScaleDown= scriptPercentScaleDown;
  table.scriptScriptPercentScaleDown= scriptScriptPercentScaleDown;
  table.delimitedSubFormulaMinHeight= delimitedSubFormulaMinHeight;
  table.displayOperatorMinHeight= displayOperatorMinHeight;
  table.radicalDegreeBottomRaisePercent= radicalDegreeBottomRaisePercent;
}

// parse the OpenType MATH table
// buf is a .otf file content
// tt is a buffer of the MATH table
ot_mathtable
parse_mathtable (const string& buf) {
  if ((N (buf) == 0) || (!tt_correct_version (buf, 0))) return {};
  string tt= tt_table (buf, 0, "MATH");

  if (N (tt) == 0) return {};
  ot_mathtable table (tm_new<ot_mathtable_rep> ());

  // MATH Header
  table->majorVersion= get_U16 (tt, 0);
  table->minorVersion= get_U16 (tt, 2);
  int mathConstantsOffset= get_U16 (tt, 4);
  int mathGlyphInfoOffset= get_U16 (tt, 6);
  int mathVariantsOffset= get_U16 (tt, 8);

  // version check
  if ((table->majorVersion != 1) || (table->minorVersion != 0)) return {};

  // parse MathConstants table
  parse_constant (tt, mathConstantsOffset, table->constants_table);

  // MathGlyphInfo table
  int mathItalicsCorrectionInfoOffset= get_U16 (tt, mathGlyphInfoOffset + 0);
  int mathTopAccentAttachmentOffset= get_U16 (tt, mathGlyphInfoOffset + 2);
  int extendedShapeCoverageOffset= get_U16 (tt, mathGlyphInfoOffset + 4);
  int mathKernInfoOffset= get_U16 (tt, mathGlyphInfoOffset + 6);

  // MathItalicsCorrectionInfo table
  //cout << "parse MathItalicsCorrectionInfo\n";
  int mathItalicsCorrectionInfoAbsOffset=
      mathGlyphInfoOffset + mathItalicsCorrectionInfoOffset;
  int italicsCorrectionCoverageOffset=
      get_U16 (tt, mathItalicsCorrectionInfoAbsOffset + 0);
  // int italicsCorrectionCount= get_U16 (tt, mathItalicsCorrectionInfoOffset +
  // 2); parse MathItalicsCorrection Coverage table
  parse_record_with_coverage (tt, mathItalicsCorrectionInfoAbsOffset,
                              italicsCorrectionCoverageOffset, 4,
                              table->italics_correction);

  // MathTopAccentAttachment table
  //cout << "parse MathTopAccentAttachment\n";
  int mathTopAccentAttachmentAbsOffset=
      mathGlyphInfoOffset + mathTopAccentAttachmentOffset;
  int topAccentCoverageOffset=
      get_U16 (tt, mathTopAccentAttachmentAbsOffset + 0);
  // int topAccentAttachmentCount= get_U16 (tt, mathTopAccentAttachmentOffset +
  // 2)
  parse_record_with_coverage (tt, mathTopAccentAttachmentAbsOffset,
                              topAccentCoverageOffset, 4, table->top_accent);

  // ExtendedShapeCoverage table (may be NULL)
  //cout << "parse ExtendedShapeCoverage\n";
  if (extendedShapeCoverageOffset > 0) {
    auto extendedShapeCoverage= parse_coverage_table (
        tt, mathGlyphInfoOffset + extendedShapeCoverageOffset);
    for (int i= 0;  i< N(extendedShapeCoverage); i++) {
      table->extended_shape_coverage->insert (extendedShapeCoverage[i]);
    }
  }


  // MathKernInfo table
  //cout << "parse MathKernInfo\n";
  parse_math_kern_info_table (tt, mathGlyphInfoOffset + mathKernInfoOffset,
                              table->math_kern_info);

  // math variants
  table->minConnectorOverlap= get_U16 (tt, mathVariantsOffset + 0);
  int vertGlyphCoverageOffset=
      mathVariantsOffset + get_U16 (tt, mathVariantsOffset + 2);
  int horizGlyphCoverageOffset=
      mathVariantsOffset + get_U16 (tt, mathVariantsOffset + 4);
  int vertGlyphCount= get_U16 (tt, mathVariantsOffset + 6);
  //int horizGlyphCount= get_U16 (tt, mathVariantsOffset + 8); // unused

  // (tt-dump "/Users/mgubi/t/svn-src/TeXmacs/fonts/truetype/texgyre/texgyrepagella-math.otf")


  // parse vertical variants
  // cout << "parse vertical variants\n";
  parse_variants (tt, mathVariantsOffset, vertGlyphCoverageOffset,
                  mathVariantsOffset + 10, table->ver_glyph_variants,
                  table->ver_glyph_variants_adv, table->ver_glyph_assembly);

  // parse horizontal variants
  // cout << "parse horizontal variants\n";
  parse_variants (tt, mathVariantsOffset, horizGlyphCoverageOffset,
                  mathVariantsOffset + 10 + 2 * vertGlyphCount,
                  table->hor_glyph_variants, table->hor_glyph_variants_adv,
                  table->hor_glyph_assembly);

  return table;
}

ot_mathtable
parse_mathtable (url u) {
  string tt;
  if (!load_string (u, tt, false)) return parse_mathtable (tt);
  else return {};
}

array<string>
as_hexadecimal (array<unsigned int> a) {
  array<string> as = array<string> ();
  for (int i=0; i<N(a); i++) {
    as << as_hexadecimal (a [i]);
  }
  return as;
}

void
dump_mathtable (tm_ostream& str, ot_mathtable table) {
  if (is_nil(table)) return;
  
  str << "MATH table " << table->majorVersion << " "
       << table->minorVersion << LF;
  {
    str << "Vertical variants" << LF;
    iterator<unsigned int> it= iterate (table->ver_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->ver_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->ver_glyph_variants[glyph]) << LF;
    }
    str << "Vertical assembly" << LF;
    it= iterate (table->ver_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << table->ver_glyph_assembly[glyph].partCount
           << ") : ";
      auto v= table->ver_glyph_assembly[glyph].partRecords;
      for (int j= 0; j<N(v); j++) {
        str << as_hexadecimal (v[j].glyphID) << " ";
      }
      str << LF;
    }
  }
  {
    str << "Horizontal variants" << LF;
    iterator<unsigned int> it= iterate (table->hor_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->hor_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->hor_glyph_variants[glyph]) << LF;
    }
    str << "Horizontal assembly" << LF;
    it= iterate (table->hor_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << table->hor_glyph_assembly[glyph].partCount
           << ") : ";
      auto v= table->hor_glyph_assembly[glyph].partRecords;
      for (int j= 0; j<N(v); j++) {
        str << as_hexadecimal (v[j].glyphID) << " ";
      }
      str << LF;
    }
  }
}
