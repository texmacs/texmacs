
/******************************************************************************
* MODULE     : poor_distorted.cpp
* DESCRIPTION: Font distortion based on random noise
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"
#include "frame.hpp"

/******************************************************************************
* True Type fonts
******************************************************************************/

struct poor_distorted_font_rep: font_rep {
  font base;
  tree kind;

  poor_distorted_font_rep (string name, font base, tree kind);

  bool   supports (string c);
  SI     get (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lig);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool lig);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction  (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

poor_distorted_font_rep::poor_distorted_font_rep (string name, font b, tree k):
  font_rep (name, b), base (b), kind (k)
{
  this->copy_math_pars (base);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_distorted_font_rep::supports (string s) {
  return base->supports (s);
}

void
poor_distorted_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (s, ex);
}

void
poor_distorted_font_rep::get_xpositions (string s, SI* xpos) {
  base->get_xpositions (s, xpos);
}

void
poor_distorted_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  base->get_xpositions (s, xpos, lig);
}

void
poor_distorted_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  base->get_xpositions (s, xpos, xk);
}

void
poor_distorted_font_rep::draw_fixed (renderer ren, string s,
                                  SI x, SI y, SI* xpos) {
  int i=0;
  while (i < N(s)) {
    int start= i;
    base->advance_glyph (s, i);
    string ss= s (start, i);
    font_metric fnm;
    font_glyphs fng;
    int c= index_glyph (ss, fnm, fng);
    if (c >= 0) ren->draw (c, fng, start==0? x: x + xpos[start], y);
  }
}

void
poor_distorted_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_distorted_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool l) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, l);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_distorted_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

font
poor_distorted_font_rep::magnify (double zoomx, double zoomy) {
  return poor_distorted_font (base->magnify (zoomx, zoomy), kind);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
poor_distorted_font_rep::advance_glyph (string s, int& pos) {
  base->advance_glyph (s, pos);
}

glyph
poor_distorted_font_rep::get_glyph (string s) {
  font_metric fnm;
  font_glyphs fng;
  int c= index_glyph (s, fnm, fng);
  return fng->get (c);
}

int
poor_distorted_font_rep::index_glyph (string s, font_metric& fnm,
                                                font_glyphs& fng) {
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return c;
  //fnm= distorted (fnm, kind);
  fng= distorted (fng, kind, wfn);
  return c;
}

/******************************************************************************
* Distorted correction
******************************************************************************/

double
poor_distorted_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s);
}

double
poor_distorted_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s);
}

SI
poor_distorted_font_rep::get_left_correction (string s) {
  return base->get_left_correction (s);
}

SI
poor_distorted_font_rep::get_right_correction (string s) {
  return base->get_right_correction (s);
}

SI
poor_distorted_font_rep::get_lsub_correction (string s) {
  return base->get_lsub_correction (s);
}

SI
poor_distorted_font_rep::get_lsup_correction (string s) {
  return base->get_lsup_correction (s);
}

SI
poor_distorted_font_rep::get_rsub_correction (string s) {
  return base->get_rsub_correction (s);
}

SI
poor_distorted_font_rep::get_rsup_correction (string s) {
  return base->get_rsup_correction (s);
}

SI
poor_distorted_font_rep::get_wide_correction (string s, int mode) {
  return base->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

string functional_to_string (tree t);

font
poor_distorted_font (font base, tree kind) {
  string name= "poordistorted[" * base->res_name;
  name << "," << functional_to_string (kind) << "]";
  return make (font, name, tm_new<poor_distorted_font_rep> (name, base, kind));
}
