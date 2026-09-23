
/******************************************************************************
* MODULE     : feature_font.cpp
* DESCRIPTION: a font whose glyphs are replaced by the substitutes of an
*              OpenType feature, e.g. the script size alternates (ssty)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"

/******************************************************************************
* The feature font class
******************************************************************************/

struct feature_font_rep: font_rep {
  font   base;
  string feature;
  int    alt;
  hashmap<string,string> cache;

  feature_font_rep (string name, font base, string feature, int alt);
  string rewrite (string s);
  string rewrite (string s, array<int>& map);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lig);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos, bool ligf);
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
  font   make_rubber_font (font fn);
  bool   get_rubber_variant (string s, SI height, string& r);
  bool   get_wide_variant (string s, SI width, string& r);
  bool   is_extended_shape (string s);
  bool   get_top_accent (string s, SI& x);
  bool   get_feature_variant (string s, string f, int a, string& r);
};

feature_font_rep::feature_font_rep (string name, font b, string f, int a):
  font_rep (name, b), base (b), feature (f), alt (a), cache ("")
{
  this->copy_math_pars (base);
}

/******************************************************************************
* Rewriting strings glyph by glyph
******************************************************************************/

// map[i] is the position in the rewritten string of the byte i of s
string
feature_font_rep::rewrite (string s, array<int>& map) {
  string r;
  int i= 0, n= N(s);
  map= array<int> (n + 1);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    string c= s (start, i);
    string sub;
    if (cache->contains (c)) sub= cache[c];
    else {
      if (!base->get_feature_variant (c, feature, alt, sub)) sub= c;
      cache (c)= sub;
    }
    for (int j= start; j < i; j++) map[j]= N(r);
    r << sub;
  }
  map[n]= N(r);
  return r;
}

string
feature_font_rep::rewrite (string s) {
  array<int> map;
  return rewrite (s, map);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
feature_font_rep::supports (string s) {
  return base->supports (rewrite (s));
}

void
feature_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (rewrite (s), ex);
}

void
feature_font_rep::get_xpositions (string s, SI* xpos) {
  array<int> map;
  string r= rewrite (s, map);
  SI* rpos= tm_new_array<SI> (N(r) + 1);
  base->get_xpositions (r, rpos);
  for (int i= 0; i <= N(s); i++) xpos[i]= rpos[map[i]];
  tm_delete_array (rpos);
}

void
feature_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  (void) lig;
  get_xpositions (s, xpos);
}

void
feature_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  array<int> map;
  string r= rewrite (s, map);
  SI* rpos= tm_new_array<SI> (N(r) + 1);
  base->get_xpositions (r, rpos, xk);
  for (int i= 0; i <= N(s); i++) xpos[i]= rpos[map[i]];
  tm_delete_array (rpos);
}

void
feature_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  base->draw_fixed (ren, rewrite (s), x, y);
}

void
feature_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool lf) {
  base->draw_fixed (ren, rewrite (s), x, y, lf);
}

void
feature_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  base->draw_fixed (ren, rewrite (s), x, y, xk);
}

font
feature_font_rep::magnify (double zoomx, double zoomy) {
  return feature_font (base->magnify (zoomx, zoomy), feature, alt);
}

void
feature_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  (void) ligf;
  tm_char_forwards (s, pos);
}

glyph
feature_font_rep::get_glyph (string s) {
  return base->get_glyph (rewrite (s));
}

int
feature_font_rep::index_glyph (string s, font_metric& fnm, font_glyphs& fng) {
  return base->index_glyph (rewrite (s), fnm, fng);
}

/******************************************************************************
* Metric properties and hooks
******************************************************************************/

double feature_font_rep::get_left_slope (string s) {
  return base->get_left_slope (rewrite (s)); }
double feature_font_rep::get_right_slope (string s) {
  return base->get_right_slope (rewrite (s)); }
SI feature_font_rep::get_left_correction (string s) {
  return base->get_left_correction (rewrite (s)); }
SI feature_font_rep::get_right_correction (string s) {
  return base->get_right_correction (rewrite (s)); }
SI feature_font_rep::get_lsub_correction (string s) {
  return base->get_lsub_correction (rewrite (s)); }
SI feature_font_rep::get_lsup_correction (string s) {
  return base->get_lsup_correction (rewrite (s)); }
SI feature_font_rep::get_rsub_correction (string s) {
  return base->get_rsub_correction (rewrite (s)); }
SI feature_font_rep::get_rsup_correction (string s) {
  return base->get_rsup_correction (rewrite (s)); }
SI feature_font_rep::get_lsub_correction_at (string s, SI h) {
  return base->get_lsub_correction_at (rewrite (s), h); }
SI feature_font_rep::get_lsup_correction_at (string s, SI h) {
  return base->get_lsup_correction_at (rewrite (s), h); }
SI feature_font_rep::get_rsub_correction_at (string s, SI h) {
  return base->get_rsub_correction_at (rewrite (s), h); }
SI feature_font_rep::get_rsup_correction_at (string s, SI h) {
  return base->get_rsup_correction_at (rewrite (s), h); }
SI feature_font_rep::get_wide_correction (string s, int mode) {
  return base->get_wide_correction (rewrite (s), mode); }
font feature_font_rep::make_rubber_font (font fn) {
  (void) fn; return rubber_font (base); }
bool feature_font_rep::get_rubber_variant (string s, SI height, string& r) {
  return base->get_rubber_variant (s, height, r); }
bool feature_font_rep::get_wide_variant (string s, SI width, string& r) {
  return base->get_wide_variant (s, width, r); }
bool feature_font_rep::is_extended_shape (string s) {
  return base->is_extended_shape (rewrite (s)); }
bool feature_font_rep::get_top_accent (string s, SI& x) {
  return base->get_top_accent (rewrite (s), x); }
bool feature_font_rep::get_feature_variant (string s, string f, int a, string& r) {
  return base->get_feature_variant (rewrite (s), f, a, r); }

/******************************************************************************
* Interface
******************************************************************************/

font
feature_font (font base, string feature, int alt) {
  string name= base->res_name * "#" * feature * as_string (alt);
  return make (font, name,
               tm_new<feature_font_rep> (name, base, feature, alt));
}

// The value of the font-features environment variable: a comma separated
// list of OpenType feature tags, each of them optionally followed by the
// number of the alternate to take, as in "onum,ss01=1". Tags are applied
// from left to right, so a later one sees the glyphs the earlier ones chose.
font
apply_features (font fn, string features) {
  array<string> a= trim_spaces (tokenize (features, ","));
  for (int i=0; i<N(a); i++) {
    if (N(a[i]) == 0) continue;
    array<string> b= trim_spaces (tokenize (a[i], "="));
    string tag= b[0];
    int    alt= 0;
    if (N(b) >= 2 && is_int (b[1])) alt= as_int (b[1]);
    if (N(tag) != 4) continue;  // an OpenType tag is four characters long
    fn= feature_font (fn, tag, alt);
  }
  return fn;
}
