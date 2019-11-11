
/******************************************************************************
* MODULE     : recolored_font.cpp
* DESCRIPTION: Change the color of letters in a font
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

struct recolored_font_rep: font_rep {
  font base;
  tree kind;
  
  recolored_font_rep (string name, font base, tree kind);
  void change_pencil (renderer ren);

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

recolored_font_rep::recolored_font_rep (string name, font b, tree k):
  font_rep (name, b), base (b), kind (k)
{
  this->copy_math_pars (base);
}

void
recolored_font_rep::change_pencil (renderer ren) {
  ren->set_pencil (named_color (as_string (kind)));
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
recolored_font_rep::supports (string s) {
  return base->supports (s);
}

void
recolored_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (s, ex);
}

void
recolored_font_rep::get_xpositions (string s, SI* xpos) {
  base->get_xpositions (s, xpos);
}

void
recolored_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  base->get_xpositions (s, xpos, lig);
}

void
recolored_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  base->get_xpositions (s, xpos, xk);
}

void
recolored_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  pencil saved_pen= ren->get_pencil ();
  change_pencil (ren);
  base->draw_fixed (ren, s, x, y);
  ren->set_pencil (saved_pen);
}

void
recolored_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool lf) {
  pencil saved_pen= ren->get_pencil ();
  change_pencil (ren);
  base->draw_fixed (ren, s, x, y, lf);
  ren->set_pencil (saved_pen);
}

void
recolored_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  pencil saved_pen= ren->get_pencil ();
  change_pencil (ren);
  base->draw_fixed (ren, s, x, y, xk);
  ren->set_pencil (saved_pen);
}

font
recolored_font_rep::magnify (double zoomx, double zoomy) {
  return recolored_font (base->magnify (zoomx, zoomy), kind);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
recolored_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  base->advance_glyph (s, pos, ligf);
}

glyph
recolored_font_rep::get_glyph (string s) {
  return base->get_glyph (s);
}

int
recolored_font_rep::index_glyph (string s, font_metric& fnm,
                                 font_glyphs& fng) {
  return base->index_glyph (s, fnm, fng);
}

/******************************************************************************
* Microtypographical corrections
******************************************************************************/

double
recolored_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s);
}

double
recolored_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s);
}

SI
recolored_font_rep::get_left_correction (string s) {
  return base->get_left_correction (s);
}

SI
recolored_font_rep::get_right_correction (string s) {
  return base->get_right_correction (s);
}

SI
recolored_font_rep::get_lsub_correction (string s) {
  return base->get_lsub_correction (s);
}

SI
recolored_font_rep::get_lsup_correction (string s) {
  return base->get_lsup_correction (s);
}

SI
recolored_font_rep::get_rsub_correction (string s) {
  return base->get_rsub_correction (s);
}

SI
recolored_font_rep::get_rsup_correction (string s) {
  return base->get_rsup_correction (s);
}

SI
recolored_font_rep::get_wide_correction (string s, int mode) {
  return base->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

string functional_to_string (tree t);

font
recolored_font (font base, tree kind) {
  string name= "recolored[" * base->res_name;
  name << "," << functional_to_string (kind) << "]";
  return make (font, name, tm_new<recolored_font_rep> (name, base, kind));
}
