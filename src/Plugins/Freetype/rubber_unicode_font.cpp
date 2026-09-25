
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Rubber unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "converter.hpp"
#include "bitmap_font.hpp"
#include "Freetype/tt_face.hpp"
#include "Freetype/tt_tools.hpp"
#include "translator.hpp"
#include "convert.hpp" // string_to_scheme_tree

#ifdef USE_FREETYPE

bool supports_big_operators (string res_name); // from poor_rubber.cpp

// largest number of repetitions of the extenders of an assembled glyph
#define MAX_ASSEMBLY_REPS 64
// display operators are never required to be taller than this (in em):
// some fonts declare very large displayOperatorMinHeight values
#define DISPLAY_OPERATOR_MAX_EM 2.0
font rubber_unicode_font (font base, tt_face face);

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_unicode_font_rep: font_rep {
  font base;
  bool big_flag;
  array<bool> initialized;
  array<font> subfn;
  bool big_sums;

  // for opentype math font
  translator virt;
  tt_face    math_face;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  rubber_unicode_font_rep (string name, font base, tt_face face= nullptr);
  font   get_font (int nr);
  array<GlyphPartRecord> part_records (GlyphAssembly gass, bool ver,
                                       double du, int min_overlap,
                                       array<SI>& ink);
  int    search_font_sub (string s, string& rew);
  int    search_font_sub_opentype (string s, string& rew);
  bool   get_rubber_variant (string s, SI height, string& r);
  bool   make_measured (string s);
  bool   get_wide_variant (string s, SI width, string& r);
  bool   get_top_accent (string s, SI& x);
  bool   is_extended_shape (string s);
  bool   variant_glyph (string head, string root, unsigned int& glyphID);
  void   add_virtual_glyph (string name, string def);
  int    search_font_cached (string s, string& rew);
  font   search_font (string& s);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_lsub_correction_at (string s, SI h);
  SI     get_lsup_correction_at (string s, SI h);
  SI     get_rsub_correction_at (string s, SI h);
  SI     get_rsup_correction_at (string s, SI h);
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

rubber_unicode_font_rep::rubber_unicode_font_rep (string name, font base2,  
                                                  tt_face face):
  font_rep (name, base2), base (base2),
  big_flag (supports_big_operators (base2->res_name)),
  math_face (face)
{
  this->copy_math_pars (base);
  big_sums= false;
  if (base->supports ("<sum>")) {
    metric ex;
    base->get_extents ("<sum>", ex);
    //cout << base->res_name << " -> "
    //<< ((double) (ex->y2-ex->y1)) / base->yx << LF;
    if ((((double) (ex->y2-ex->y1)) / base->yx) >= 1.55) big_sums= true;
  }
  // number of subfonts 7, see get_font(int) for details
  int nr_subfonts= 7;
  for (int i=0; i<nr_subfonts; i++) {
    initialized << false;
    subfn << base;
  }
  if (base->ot_math) {
    big_flag    = true;
    big_sums    = true;
    string vname= "opentype_virtual[" * base->res_name * "]";
    virt        = tm_new<translator_rep> (vname);
    // virt->virt_def= array<tree> ();
    // virt->virt_def << tree (); // fill out the 0 glyph
  }

}

font
rubber_unicode_font_rep::get_font (int nr) {
  ASSERT (nr < N(subfn), "wrong font number");
  if (initialized[nr]) return subfn[nr];
  initialized[nr]= true;
  switch (nr) {
  case 0:
    break;
  case 1:
    subfn[nr]= base->magnify (sqrt (0.5));
    break;
  case 2:
    subfn[nr]= base->magnify (sqrt (2.0));
    break;
  case 3:
    subfn[nr]= base->magnify (2.0);
    break;
  case 4:
    subfn[nr]= rubber_assemble_font (base);
    break;
  case 5:
    // if opentype math font fails, use default rubber font
    subfn[nr]= font_rep::make_rubber_font (base);
    break;
  case 6:
    int hdpi= (72 * base->wpt + (PIXEL / 2)) / PIXEL;
    int vdpi= (72 * base->hpt + (PIXEL / 2)) / PIXEL;
    subfn[nr]=
        virtual_font (base, virt->res_name, base->size, hdpi, vdpi, false);
    break;
  }
  return subfn[nr];
}

/******************************************************************************
* Find the font
******************************************************************************/

// The code point carrying the horizontal variants of a wide accent or
// stretchable relation, for the TeXmacs names used in <wide-name-N> and
// <rubber-name-N>. TeXmacs's own translation of these names gives spacing
// modifier letters or the long forms, which fonts do not stretch.
static uint32_t
wide_code_point (string root) {
  static hashmap<string,int> t (0);
  if (N(t) == 0) {
    t ("hat")= 0x302;          t ("^")= 0x302;
    t ("tilde")= 0x303;        t ("~")= 0x303;
    t ("bar")= 0x305;          t ("overline")= 0x305;
    t ("underline")= 0x332;    t ("underbar")= 0x332;
    t ("vect")= 0x20D7;        t ("check")= 0x30C;
    t ("breve")= 0x306;        t ("invbreve")= 0x311;
    t ("acute")= 0x301;        t ("grave")= 0x300;
    t ("dot")= 0x307;          t ("ddot")= 0x308;
    t ("dddot")= 0x20DB;       t ("abovering")= 0x30A;
    t ("overbrace")= 0x23DE;   t ("underbrace")= 0x23DF;
    t ("overbrace*")= 0x23DE;  t ("underbrace*")= 0x23DF;
    t ("sqoverbrace")= 0x23B4; t ("squnderbrace")= 0x23B5;
    t ("sqoverbrace*")= 0x23B4; t ("squnderbrace*")= 0x23B5;
    t ("poverbrace")= 0x23DC;  t ("punderbrace")= 0x23DD;
    t ("poverbrace*")= 0x23DC; t ("punderbrace*")= 0x23DD;
    t ("longrightarrow")= 0x2192;     t ("longleftarrow")= 0x2190;
    t ("longleftrightarrow")= 0x2194; t ("longmapsto")= 0x21A6;
    t ("longhookrightarrow")= 0x21AA; t ("longhookleftarrow")= 0x21A9;
    t ("Longrightarrow")= 0x21D2;     t ("Longleftarrow")= 0x21D0;
    t ("Longleftrightarrow")= 0x21D4;
  }
  if (t->contains (root)) return (uint32_t) t[root];
  string uu= N (root) > 1 ? strict_cork_to_utf8 ("<" * root * ">") : root;
  int j= 0;
  return decode_from_utf8 (uu, j);
}

int
parse_variant (string s, string& head, string& root) {
  // cout << "parse_variant for " << s << LF;
  int var= 0;
  if (!starts (s, "<") || !ends (s, ">") || N (s) < 3) return 0;
  root = s (1, N(s) - 1);
  array<string> v= tokenize (root, "-");
  // the last token is the size, the first the kind, everything in between
  // is the root: a root may contain dashes, as in <wide-var-rightarrow-2>
  if (N (v) >= 3 && is_int (v[N(v) - 1])) {
    var = as_int (v[N(v) - 1]);
    head= v[0];
    root= recompose (range (v, 1, N(v) - 1), "-");
  }
  return var;
}

// The size of a made to measure glyph, in thousandths of an em, which is
// what its name carries: a name must mean the same thing to every copy of
// the font, and the pixels in which the typesetter measures depend on the
// resolution the copy was made for.
static int
per_em (SI len, double em) {
  if (em <= 0.0) return 0;
  return (int) tm_round ((1000.0 * len) / em);
}

// Length (in design units) of an assembly whose extenders are repeated
// 'reps' times, following the connector arithmetic of the specification.
static int
assembled_length (array<GlyphPartRecord> prs, int reps, int min_overlap) {
  int total= 0, prev_end= -1;
  for (int i= 0; i < N (prs); i++) {
    GlyphPartRecord pr= prs[i];
    int n= ((pr.partFlags & 1) != 0)? reps: 1;
    for (int k= 0; k < n; k++) {
      total += (int) pr.fullAdvance;
      if (prev_end >= 0)
        total -= min (min_overlap, min (prev_end, (int) pr.startConnectorLength));
      prev_end= (int) pr.endConnectorLength;
    }
  }
  return total;
}

// Build the definition of an assembled glyph according to the OpenType MATH
// specification: every extender part is repeated 'reps' times and
// consecutive parts overlap by at least minConnectorOverlap, limited by the
// connector lengths of the two parts. When target > 0, the overlaps are
// enlarged uniformly so that the assembly shrinks towards the target length
// (in design units). Parts are stacked bottom to top or left to right.
//
// The parts are placed one by one, at the distance the table prescribes
// between the near edges of their ink (du is the size in SI of a design
// unit, em the size of the em, ink the measured bottom or left of every
// part record in the rendered font), and joined. Gluing them to each other
// instead, as this routine used to do, measures the ink of the whole stack
// built so far, and answers the ink of its tallest part rather than of the
// part which is to receive the next one: the parts of the Latin Modern,
// TeX Gyre, DejaVu and Fira families all sit at the origin, so the two
// agree there, but KpMath, XCharter, Old Standard, Concrete, Euler,
// Erewhon, Garamond, Asana, STIX, XITS and New Computer Modern draw their
// parts at offsets which differ from one part to the next, by more than an
// em in some of them, and their assembled delimiters came out broken.
static string
assemble (array<GlyphPartRecord> prs, array<SI> ink, int reps, int min_overlap,
          int target, double du, double em, bool ver) {
  array<GlyphPartRecord> parts;
  array<SI>              base;
  for (int i= 0; i < N (prs); i++) {
    GlyphPartRecord pr= prs[i];
    int n= ((pr.partFlags & 1) != 0)? reps: 1;
    for (int k= 0; k < n; k++) { parts << pr; base << ink[i]; }
  }
  int n= N (parts);
  if (n == 0) return "";
  array<int> max_o (max (n - 1, 0));
  int total= 0;
  for (int i= 0; i < n; i++) {
    total += (int) parts[i].fullAdvance;
    if (i + 1 < n) {
      max_o[i]= min ((int) parts[i].endConnectorLength,
                     (int) parts[i+1].startConnectorLength);
      total -= min (min_overlap, max_o[i]);
    }
  }
  // extra overlap per joint needed to reach the target
  int extra= 0;
  if (target > 0 && n > 1 && total > target)
    extra= (total - target + n - 2) / (n - 1);
  if (em <= 0.0) em= 1.0;
  string r= "(join";
  double off= 0.0; // where the origin of the current part goes, in SI
  for (int i= 0; i < n; i++) {
    string d= as_string (off / em);
    r << " (" << (ver? string ("0"): d) << " " << (ver? d: string ("0"))
      << " @" << as_hexadecimal (parts[i].glyphID, 4) << ")";
    if (i + 1 < n) {
      int o= min (min_overlap + extra, max_o[i]);
      off += ((double) ((int) parts[i].fullAdvance - o)) * du +
             ((double) (base[i] - base[i+1]));
    }
  }
  r << ")";
  return r;
}

int
rubber_unicode_font_rep::search_font_sub_opentype (string s, string& rew) {
  string root, head;
  int    var           = 0;
  bool   using_vertical= true; // verizontal or vertical, default is vertical
  rew                  = s;

  var= parse_variant (s, head, root);

  // there is no <big-xxx-0>
  if (starts (s, "<big-")) {
    var= max (0, var - 1);
  }

  if (root == "") return search_font_sub (s, rew);

  unsigned int glyphID= 0;
  (void) variant_glyph (head, root, glyphID);

  // cout << "unicode " << uu << " -> " << lolly::data::to_hex (u) << LF;
  // cout << "search_font_sub_opentype for " << s << " -> " << glyphID << LF;

  bool         has_variants= false;
  bool         has_assembly= false;
  ot_mathtable math_table  = math_face->math_table;

  // a glyph can not be both vertical and horizontal
  if (math_table->ver_glyph_variants->contains (glyphID)) {
    using_vertical= true;
    has_variants  = true;
    // if a glyph has assembly, it must be contained in the variants table
    has_assembly= math_table->ver_glyph_assembly->contains (glyphID);
  }
  else if (math_table->hor_glyph_variants->contains (glyphID)) {
    has_variants  = true;
    using_vertical= false;
    has_assembly  = math_table->hor_glyph_assembly->contains (glyphID);
  }

  auto glyph_variants= using_vertical
                           ? math_face->math_table->ver_glyph_variants
                           : math_face->math_table->hor_glyph_variants;

  auto glyph_assembly= using_vertical
                           ? math_face->math_table->ver_glyph_assembly
                           : math_face->math_table->hor_glyph_assembly;

  // names of made to measure assemblies (see make_measured), which a
  // magnified copy of this font has to build again
  if (make_measured (s)) return 6;

  // turn a number to a 4-digit hexadecimal string "@XXXX"
  auto hex4= [] (int x) { return "@" * as_hexadecimal (x, 4); };

  if (has_variants) {
    auto& gv= glyph_variants (glyphID);
    // display style big operators: the smallest variant which is at least
    // displayOperatorMinHeight tall (or the largest one)
    if (starts (s, "<big-") && var == 1 && using_vertical) {
      auto& adv= math_table->ver_glyph_variants_adv (glyphID);
      int   min_h= math_table->constants_table[displayOperatorMinHeight];
      double upem= (double) math_face->ft_face->units_per_EM;
      if (upem <= 0.0) upem= 1000.0;
      min_h= min (min_h, (int) (DISPLAY_OPERATOR_MAX_EM * upem));
      int   i= N (gv) - 1;
      for (int j= 1; j < N (gv); j++)
        if ((int) adv[j] >= min_h) { i= j; break; }
      var= max (i, 1);
    }
    if (var < N (gv)) {
      int res= gv[var];
      // use <@XXXX> for native glyph id
      rew= "<" * hex4 (res) * ">";
      // the unicode font itself has the variant glyph
      return 0;
    }
  }

  if (has_assembly) {
    // Variant numbers beyond the pre-drawn variants are assembled from
    // parts; number nvar + k - 1 repeats every extender k times. We define
    // all the sizes of a glyph at once, so that the virtual font holding
    // them has to be rebuilt only once per glyph.
    int nvar= has_variants? N (glyph_variants (glyphID)): 0;
    if (var < nvar + MAX_ASSEMBLY_REPS) {
      if (!virt->dict->contains (s)) {
        GlyphAssembly gass= glyph_assembly (glyphID);
        int    min_overlap= (int) math_table->minConnectorOverlap;
        double upem= (double) math_face->ft_face->units_per_EM;
        if (upem <= 0.0) upem= 1000.0;
        string prefix= "<" * head * "-" * root * "-";
        int    shift = starts (s, "<big-")? 1: 0; // there is no <big-x-0>
        // sizes must grow with the variant number: start with the smallest
        // number of repetitions which exceeds the largest pre-drawn variant
        double pt= using_vertical? (double) base->hpt: (double) base->wpt;
        double du= ((double) base->size * pt) / upem;
        double em= (double) base->size * pt;
        array<SI> ink;
        array<GlyphPartRecord> prs=
          part_records (gass, using_vertical, du, min_overlap, ink);
        int k0= 1;
        if (nvar > 0) {
          auto& adv= using_vertical? math_table->ver_glyph_variants_adv (glyphID)
                                   : math_table->hor_glyph_variants_adv (glyphID);
          int largest= (N (adv) == nvar)? (int) adv[nvar - 1]: 0;
          while (k0 < MAX_ASSEMBLY_REPS &&
                 assembled_length (prs, k0, min_overlap) <= largest) k0++;
        }
        for (int k= k0; k < k0 + MAX_ASSEMBLY_REPS; k++) {
          string name= prefix * as_string (nvar + k - k0 + shift) * ">";
          if (virt->dict->contains (name)) continue;
          add_virtual_glyph (name, assemble (prs, ink, k, min_overlap, 0,
                                             du, em, using_vertical));
        }
      }
      return 6;
    }
  }
  // cout << "No opentype variant for " << uu << " -> " << glyphID << LF;

  // try to use subfont
  int nr= search_font_sub (s, rew);
  // if nr == 0, failed to find the sub font from subfn[1:4]
  // use default rubber font subfn[5]
  return nr == 0 ? 5 : nr;
}

// Glyph of the base character of a rubber or wide character name
bool
rubber_unicode_font_rep::variant_glyph (string head, string root,
                                        unsigned int& glyphID) {
  if (is_nil (math_face) || root == "" || root == ".") return false;
  uint32_t u;
  if (head == "wide" || head == "rubber") u= wide_code_point (root);
  else {
    string uu= N (root) > 1 ? strict_cork_to_utf8 ("<" * root * ">") : root;
    int j= 0;
    u= decode_from_utf8 (uu, j);
  }
  glyphID= ft_get_char_index (math_face->ft_face, u);
  return glyphID != 0;
}

// Answer the width search of the typesetter for wide accents, braces and
// long arrows from the horizontal variants and assemblies of the MATH
// table; made to measure assemblies are named <head-root-wN> with N the
// width in pixels.
bool
rubber_unicode_font_rep::get_wide_variant (string s, SI width, string& r) {
  if (is_nil (math_face) || is_nil (math_face->math_table)) return false;
  if (!starts (s, "<") || !ends (s, ">") || N(s) < 3) return false;
  array<string> v= tokenize (s (1, N(s) - 1), "-");
  if (N(v) != 2) return false;
  string head= v[0], root= v[1];
  unsigned int glyphID;
  if (!variant_glyph (head, root, glyphID)) return false;
  ot_mathtable mt= math_face->math_table;
  if (!mt->hor_glyph_variants->contains (glyphID)) return false;

  double upem= (double) math_face->ft_face->units_per_EM;
  if (upem <= 0.0) upem= 1000.0;
  double du= ((double) base->size * (double) base->wpt) / upem;
  int target= (int) ceil (width / du);
  string prefix= "<" * head * "-" * root * "-";

  array<unsigned int> gv = mt->hor_glyph_variants (glyphID);
  array<unsigned int> adv= mt->hor_glyph_variants_adv (glyphID);
  for (int i= 0; i < N(gv) && i < N(adv); i++)
    if ((int) adv[i] >= target) {
      r= prefix * as_string (i) * ">";
      return true;
    }
  if (!mt->hor_glyph_assembly->contains (glyphID)) {
    r= prefix * as_string (N(gv) - 1) * ">";
    return N(gv) > 0;
  }
  double em= (double) base->size * (double) base->wpt;
  r= prefix * "w" * as_string (per_em (width, em)) * ">";
  return make_measured (r);
}

bool
rubber_unicode_font_rep::get_top_accent (string s, SI& x) {
  string rew;
  int nr= search_font_cached (s, rew);
  if (nr == 6) return false;
  return get_font (nr)->get_top_accent (rew, x);
}

bool
rubber_unicode_font_rep::is_extended_shape (string s) {
  // every stretched delimiter or operator is an extended shape; plain
  // glyphs ask the base font
  string rew;
  int nr= search_font_cached (s, rew);
  if (nr == 6 || starts (rew, "<@")) return true;
  return get_font (nr)->is_extended_shape (rew);
}

// The part records of an assembly, together with the near edge of the ink
// of every part in the base font (the bottom of a vertical part, the left
// of a horizontal one, relative to the origin of the part).
//
// A part whose full advance the font leaves at zero is repaired with the
// length of its ink: KpMath 0.35, which TeXmacs distributes, declares no
// advance and no connectors for the bottom part of its right parenthesis,
// and every right parenthesis tall enough to be assembled came out with a
// detached foot.
array<GlyphPartRecord>
rubber_unicode_font_rep::part_records (GlyphAssembly gass, bool ver, double du,
                                       int min_overlap, array<SI>& ink) {
  // the connector to lend to a part which declares none: the shortest one
  // of the parts which are properly declared
  int conn= 0;
  for (int i= 0; i < N (gass.partRecords); i++) {
    GlyphPartRecord pr= gass.partRecords[i];
    if (pr.fullAdvance == 0) continue;
    int c= min ((int) pr.startConnectorLength, (int) pr.endConnectorLength);
    conn= (conn == 0)? c: min (conn, c);
  }
  conn= max (conn, min_overlap);
  array<GlyphPartRecord> r;
  ink= array<SI> ();
  for (int i= 0; i < N (gass.partRecords); i++) {
    GlyphPartRecord pr= gass.partRecords[i];
    metric ex;
    base->get_extents ("<@" * as_hexadecimal (pr.glyphID, 4) * ">", ex);
    ink << (ver? ex->y1: ex->x1);
    if (pr.fullAdvance == 0 && du > 0.0) {
      SI len= ver? (ex->y2 - ex->y1): (ex->x2 - ex->x1);
      pr.fullAdvance= (unsigned short) max (0.0, tm_round (len / du));
      if (pr.startConnectorLength == 0)
        pr.startConnectorLength= (unsigned short) conn;
      if (pr.endConnectorLength == 0)
        pr.endConnectorLength= (unsigned short) conn;
    }
    r << pr;
  }
  return r;
}

// Add a glyph definition to the virtual font of the assembled glyphs.
// subfn[6] caches the definitions, so it must be recreated when new ones
// are added; its metric and glyph caches are resources with the same name
// and are sized after the number of definitions, so reset them as well.
void
rubber_unicode_font_rep::add_virtual_glyph (string name, string def) {
  virt->dict (name)= N (virt->virt_def);
  virt->virt_def << string_to_scheme_tree (def);
  if (initialized[6]) {
    string vname= subfn[6]->res_name;
    font::instances->reset (vname);
    font_metric::instances->reset (vname);
    font_glyphs::instances->reset (vname);
    initialized[6]= false;
  }
}

// Answer the delimiter search of the typesetter directly from the MATH
// table: the smallest pre-drawn variant whose advance reaches the height,
// or an assembly made to measure, named <head-root-hN> with N the height
// in pixels.
bool
rubber_unicode_font_rep::get_rubber_variant (string s, SI height, string& r) {
  if (is_nil (math_face) || is_nil (math_face->math_table)) return false;
  // <left-(> style names, without a variant number
  if (!starts (s, "<") || !ends (s, ">") || N(s) < 3) return false;
  array<string> v= tokenize (s (1, N(s) - 1), "-");
  if (N(v) != 2) return false;
  string head= v[0], root= v[1];
  if (root == "." || root == "") return false;
  string uu= N (root) > 1 ? strict_cork_to_utf8 ("<" * root * ">") : root;
  int      j= 0;
  uint32_t u= decode_from_utf8 (uu, j);
  unsigned int glyphID= ft_get_char_index (math_face->ft_face, u);
  ot_mathtable mt= math_face->math_table;
  if (!mt->ver_glyph_variants->contains (glyphID)) return false;

  double upem= (double) math_face->ft_face->units_per_EM;
  if (upem <= 0.0) upem= 1000.0;
  // size in SI of one design unit (vertical) and of the em
  double du= ((double) base->size * (double) base->hpt) / upem;
  double em= (double) base->size * (double) base->hpt;
  int target= (int) ceil (height / du);
  string prefix= "<" * head * "-" * root * "-";

  array<unsigned int> gv = mt->ver_glyph_variants (glyphID);
  array<unsigned int> adv= mt->ver_glyph_variants_adv (glyphID);
  for (int i= 0; i < N(gv) && i < N(adv); i++)
    if ((int) adv[i] >= target) {
      r= prefix * as_string (i) * ">";
      return true;
    }
  if (!mt->ver_glyph_assembly->contains (glyphID)) {
    // no assembly: the largest variant will have to do
    r= prefix * as_string (N(gv) - 1) * ">";
    return N(gv) > 0;
  }
  r= prefix * "h" * as_string (per_em (height, em)) * ">";
  return make_measured (r);
}

// Build the assembly named <head-root-hN> (a height of N pixels) or
// <head-root-wN> (a width of N pixels), if it is not in the virtual font
// of this rubber font yet.
//
// The name carries the size because the definition has to be rebuilt on
// demand: a magnified copy of this font, which is what the screen draws
// with, starts with an empty virtual font, and the typesetter asks it for
// the name which the unmagnified font answered. Without this the delimiter
// was drawn in its base size on the screen, and correctly on paper.
bool
rubber_unicode_font_rep::make_measured (string s) {
  if (virt->dict->contains (s)) return true;
  if (is_nil (math_face) || is_nil (math_face->math_table)) return false;
  if (!starts (s, "<") || !ends (s, ">") || N(s) < 3) return false;
  array<string> v= tokenize (s (1, N(s) - 1), "-");
  if (N(v) < 3) return false;
  string last= v[N(v) - 1];
  if (N(last) < 2 || (last[0] != 'h' && last[0] != 'w')) return false;
  if (!is_int (last (1, N(last)))) return false;
  bool   ver= (last[0] == 'h');
  int    mil= as_int (last (1, N(last)));
  string head= v[0];
  string root= recompose (range (v, 1, N(v) - 1), "-");
  unsigned int glyphID;
  if (!variant_glyph (head, root, glyphID)) return false;
  ot_mathtable mt= math_face->math_table;
  if (ver? !mt->ver_glyph_assembly->contains (glyphID)
         : !mt->hor_glyph_assembly->contains (glyphID)) return false;
  double upem= (double) math_face->ft_face->units_per_EM;
  if (upem <= 0.0) upem= 1000.0;
  double pt= ver? (double) base->hpt: (double) base->wpt;
  double du= ((double) base->size * pt) / upem;
  double em= (double) base->size * pt;
  if (du <= 0.0) return false;
  int target= (int) tm_round ((mil * upem) / 1000.0);
  GlyphAssembly gass= ver? mt->ver_glyph_assembly (glyphID)
                         : mt->hor_glyph_assembly (glyphID);
  int min_overlap= (int) mt->minConnectorOverlap;
  array<SI> ink;
  array<GlyphPartRecord> prs= part_records (gass, ver, du, min_overlap, ink);
  int k= 1;
  while (k < MAX_ASSEMBLY_REPS &&
         assembled_length (prs, k, min_overlap) < target) k++;
  add_virtual_glyph (s, assemble (prs, ink, k, min_overlap, target,
                                  du, em, ver));
  return true;
}

int
rubber_unicode_font_rep::search_font_sub (string s, string& rew) {
  if (starts (s, "<big-") && ends (s, "-1>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 0;
      return 2;
    }
  }
  if (starts (s, "<big-") && ends (s, "-2>")) {
    if (big_flag && base->supports (s)) {
      rew= s;
      return 0;
    }
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (big_flag && base->supports ("<big-" * r * "-1>")) {
      rew= "<big-" * r * "-1>";
      return 2;
    }
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 2;
      return 3;
    }
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") { rew= ""; return 0; }
      if ((r == "(" && base->supports ("<#239C>")) ||
          (r == ")" && base->supports ("<#239F>")) ||
          (r == "[" && base->supports ("<#23A2>")) ||
          (r == "]" && base->supports ("<#23A5>")) ||
          ((r == "{" || r == "}") && base->supports ("<#23AA>")) ||
          (r == "sqrt" && base->supports ("<#23B7>"))) {
        rew= s;
        return 4;
      }
      rew= r;
      if (N(rew) > 1) rew= "<" * rew * ">";
      if (ends (s, "-0>")) return 0;
      return 0;
    }
  }
  rew= s;
  return 0;
}

int
rubber_unicode_font_rep::search_font_cached (string s, string& rew) {
  // cout << "search_font_cached for " << s << LF;
  if (mapper->contains (s)) {
    rew= rewriter[s];
    return mapper[s];
  }
  int nr= 0;
  if (!is_nil (math_face) && !is_nil (math_face->math_table)) {
    nr= search_font_sub_opentype (s, rew);
  }
  else {
    nr= search_font_sub (s, rew);
  }
  mapper(s)= nr;
  rewriter(s)= rew;
  //cout << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_unicode_font_rep::search_font (string& s) {
  string rew;
  int nr= search_font_cached (s, rew);
  s= rew;
  return get_font (nr);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_unicode_font_rep::supports (string s) {
  // fonts with a MATH table: whatever the variant search resolves
  if (!is_nil (math_face) && !is_nil (math_face->math_table)) {
    string rew;
    int nr= search_font_cached (s, rew);
    if (nr == 6) return true;
    if (nr != 0 || rew != s) return get_font (nr)->supports (rew);
  }
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (N(r) > 1) r= "<" * r * ">";
    return base->supports (r);
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") return true;
      if (r == "sqrt") return base->supports ("<#23B7>");
      if (N(r) > 1) r= "<" * r * ">";
      if (!base->supports (r)) return false;
      if (ends (s, "-0>")) return true;
      if (r == "(") return base->supports ("<#239C>");
      if (r == ")") return base->supports ("<#239F>");
      if (r == "[") return base->supports ("<#23A2>");
      if (r == "]") return base->supports ("<#23A5>");
      if (r == "{" || r == "}") return base->supports ("<#23AA>");
      return true;
    }
  }
  return base->supports (s);
}

void
rubber_unicode_font_rep::get_extents (string s, metric& ex) {
  font fn= search_font (s);
  fn->get_extents (s, ex);
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos);
  else {
    int i, n=N(s);
    for (i=1; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1);
  }
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos, xk);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos, xk);
  else {
    int i, n=N(s);
    for (i=0; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1, xk);
  }
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y);
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y, xk);
}

font
rubber_unicode_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_unicode_font (base->magnify (zoomx, zoomy), math_face);
}

glyph
rubber_unicode_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

int
rubber_unicode_font_rep::index_glyph (string s, font_metric& fnm,
                                                font_glyphs& fng) {
  font fn= search_font (s);
  return fn->index_glyph (s, fnm, fng);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
rubber_unicode_font_rep::get_left_slope  (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
rubber_unicode_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
rubber_unicode_font_rep::get_left_correction  (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
rubber_unicode_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

SI
rubber_unicode_font_rep::get_lsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
rubber_unicode_font_rep::get_lsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
rubber_unicode_font_rep::get_rsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
rubber_unicode_font_rep::get_rsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
}

SI
rubber_unicode_font_rep::get_lsub_correction_at (string s, SI h) {
  font fn= search_font (s);
  return fn->get_lsub_correction_at (s, h);
}

SI
rubber_unicode_font_rep::get_lsup_correction_at (string s, SI h) {
  font fn= search_font (s);
  return fn->get_lsup_correction_at (s, h);
}

SI
rubber_unicode_font_rep::get_rsub_correction_at (string s, SI h) {
  font fn= search_font (s);
  return fn->get_rsub_correction_at (s, h);
}

SI
rubber_unicode_font_rep::get_rsup_correction_at (string s, SI h) {
  font fn= search_font (s);
  return fn->get_rsup_correction_at (s, h);
}

SI
rubber_unicode_font_rep::get_wide_correction  (string s, int mode) {
  font fn= search_font (s);
  return fn->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base));
}

font
rubber_unicode_font (font base, tt_face face) {
  // NOTE: the name must differ from the one of rubber_unicode_font (base),
  // which serves as fallback (subfont 5) inside this font
  string name= "rubberunicode-ot[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base, face));
}

#else

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

font
rubber_unicode_font (font base, tt_face face) {
  return rubber_unicode_font (base);
}


#endif
