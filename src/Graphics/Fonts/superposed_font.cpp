
/******************************************************************************
* MODULE     : superposed_font.cpp
* DESCRIPTION: Superpose various fonts
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"
#include "frame.hpp"
#include "colors.hpp"

/******************************************************************************
* True Type fonts
******************************************************************************/

struct superposed_font_rep: font_rep {
  array<font> fns;
  int ref;
  
  superposed_font_rep (string name, array<font> fns, int ref);

  bool   supports (string c);
  SI     get (string c);
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

superposed_font_rep::superposed_font_rep
  (string name, array<font> fns2, int ref2):
    font_rep (name, fns2[ref2]), fns (fns2), ref (ref2)
{
  this->copy_math_pars (fns[ref]);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
superposed_font_rep::supports (string s) {
  for (int i=0; i<N(fns); i++)
    if (!fns[i]->supports (s)) return false;
  return true;
}

void
superposed_font_rep::get_extents (string s, metric& ex) {
  fns[0]->get_extents (s, ex);
  for (int i=1; i<N(fns); i++) {
    metric ey;
    fns[i]->get_extents (s, ey);
    ex->x1= min (ex->x1, ey->x1);
    ex->y1= min (ex->y1, ey->y1);
    ex->x2= max (ex->x2, ey->x2);
    ex->y2= max (ex->y2, ey->y2);
    ex->x3= min (ex->x3, ey->x3);
    ex->y3= min (ex->y3, ey->y3);
    ex->x4= max (ex->x4, ey->x4);
    ex->y4= max (ex->y4, ey->y4);
  }
}

void
superposed_font_rep::get_xpositions (string s, SI* xpos) {
  fns[ref]->get_xpositions (s, xpos);
}

void
superposed_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  fns[ref]->get_xpositions (s, xpos, lig);
}

void
superposed_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  fns[ref]->get_xpositions (s, xpos, xk);
}

void
superposed_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  for (int i=0; i<N(fns); i++)
    fns[i]->draw_fixed (ren, s, x, y);
}

void
superposed_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool lf) {
  for (int i=0; i<N(fns); i++)
    fns[i]->draw_fixed (ren, s, x, y, lf);
}

void
superposed_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  for (int i=0; i<N(fns); i++)
    fns[i]->draw_fixed (ren, s, x, y, xk);
}

font
superposed_font_rep::magnify (double zoomx, double zoomy) {
  array<font> mfns;
  for (int i=0; i<N(fns); i++)
    mfns << fns[i]->magnify (zoomx, zoomy);
  return superposed_font (mfns, ref);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
superposed_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  fns[ref]->advance_glyph (s, pos, ligf);
}

glyph
superposed_font_rep::get_glyph (string s) {
  // TODO: implement superposition of glyphs
  return fns[ref]->get_glyph (s);
}

int
superposed_font_rep::index_glyph (string s, font_metric& fnm,
                                 font_glyphs& fng) {
  // TODO: implement superposition of bitmap fonts
  return fns[ref]->index_glyph (s, fnm, fng);
}

/******************************************************************************
* Microtypographical corrections
******************************************************************************/

double
superposed_font_rep::get_left_slope (string s) {
  return fns[ref]->get_left_slope (s);
}

double
superposed_font_rep::get_right_slope (string s) {
  return fns[ref]->get_right_slope (s);
}

SI
superposed_font_rep::get_left_correction (string s) {
  return fns[ref]->get_left_correction (s);
}

SI
superposed_font_rep::get_right_correction (string s) {
  return fns[ref]->get_right_correction (s);
}

SI
superposed_font_rep::get_lsub_correction (string s) {
  return fns[ref]->get_lsub_correction (s);
}

SI
superposed_font_rep::get_lsup_correction (string s) {
  return fns[ref]->get_lsup_correction (s);
}

SI
superposed_font_rep::get_rsub_correction (string s) {
  return fns[ref]->get_rsub_correction (s);
}

SI
superposed_font_rep::get_rsup_correction (string s) {
  return fns[ref]->get_rsup_correction (s);
}

SI
superposed_font_rep::get_wide_correction (string s, int mode) {
  return fns[ref]->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
superposed_font (array<font> fns, int ref) {
  string name= "superposed[" * fns[0]->res_name;
  for (int i=1; i<N(fns); i++) name << "," << fns[i]->res_name;
  name << "," << as_string (ref) << "]";
  return make (font, name, tm_new<superposed_font_rep> (name, fns, ref));
}
