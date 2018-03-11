
/******************************************************************************
* MODULE     : virtual_enhance.cpp
* DESCRIPTION: enhance font with extra virtual character definitions
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "universal.hpp"

/******************************************************************************
* The poor smallcaps font class
******************************************************************************/

struct virtual_enhance_font_rep: font_rep {
  font   base;
  font   extra;
  string virt;

  virtual_enhance_font_rep (string name, font base, string virt);
  bool   supports (string c);
  bool   is_basic (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
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
  SI     get_wide_correction  (string s, int mode);
};

virtual_enhance_font_rep::virtual_enhance_font_rep (
  string name, font base2, string virt2):
    font_rep (name, base2), base (base2), virt (virt2)
{
  this->copy_math_pars (base);
  int hdpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  int vdpi= (72 * base->hpt + (PIXEL/2)) / PIXEL;
  extra= virtual_font (base, virt, base->size, hdpi, vdpi, false);
}

bool
virtual_enhance_font_rep::supports (string c) {
  if (base->supports (c)) return true;
  return N(c) >= 2 && c[0] == '<' && c[N(c)-1] == '>' && extra->supports (c);
}

bool
virtual_enhance_font_rep::is_basic (string c) {
  if (base->supports (c)) return true;
  return N(c) < 2 || c[0] != '<' || c[N(c)-1] != '>';
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

void
virtual_enhance_font_rep::get_extents (string s, metric& ex) {
  if (is_basic (s)) base->get_extents (s, ex);
  else extra->get_extents (s, ex);
}

void
virtual_enhance_font_rep::get_xpositions (string s, SI* xpos) {
  if (is_basic (s)) base->get_xpositions (s, xpos);
  else extra->get_xpositions (s, xpos);
}

void
virtual_enhance_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (is_basic (s)) base->get_xpositions (s, xpos, xk);
  else extra->get_xpositions (s, xpos, xk);
}

void
virtual_enhance_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  if (is_basic (s)) base->draw_fixed (ren, s, x, y);
  else extra->draw_fixed (ren, s, x, y);
}

void
virtual_enhance_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  if (is_basic (s)) base->draw_fixed (ren, s, x, y, xk);
  else extra->draw_fixed (ren, s, x, y, xk);
}

font
virtual_enhance_font_rep::magnify (double zoomx, double zoomy) {
  return virtual_enhance_font (base->magnify (zoomx, zoomy), virt);
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

void
virtual_enhance_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  if (is_basic (s)) base->advance_glyph (s, pos, ligf);
  else extra->advance_glyph (s, pos, ligf);
}

glyph
virtual_enhance_font_rep::get_glyph (string s) {
  if (is_basic (s)) return base->get_glyph (s);
  else return extra->get_glyph (s);
}

int
virtual_enhance_font_rep::index_glyph (string s, font_metric& fnm,
                                                 font_glyphs& fng) {
  if (is_basic (s)) return base->index_glyph (s, fnm, fng);
  else return extra->index_glyph (s, fnm, fng);
}

double
virtual_enhance_font_rep::get_left_slope  (string s) {
  if (is_basic (s)) return base->get_left_slope (s);
  else return extra->get_left_slope (s);
}

double
virtual_enhance_font_rep::get_right_slope (string s) {
  if (is_basic (s)) return base->get_right_slope (s);
  else return extra->get_right_slope (s);
}

SI
virtual_enhance_font_rep::get_left_correction  (string s) {
  if (is_basic (s)) return base->get_left_correction (s);
  else return extra->get_left_correction (s);
}

SI
virtual_enhance_font_rep::get_right_correction (string s) {
  if (is_basic (s)) return base->get_right_correction (s);
  else return extra->get_right_correction (s);
}

SI
virtual_enhance_font_rep::get_lsub_correction  (string s) {
  if (is_basic (s)) return base->get_lsub_correction (s);
  else return extra->get_lsub_correction (s);
}

SI
virtual_enhance_font_rep::get_lsup_correction  (string s) {
  if (is_basic (s)) return base->get_lsup_correction (s);
  else return extra->get_lsup_correction (s);
}

SI
virtual_enhance_font_rep::get_rsub_correction  (string s) {
  if (is_basic (s)) return base->get_rsub_correction (s);
  else return extra->get_rsub_correction (s);
}

SI
virtual_enhance_font_rep::get_rsup_correction  (string s) {
  if (is_basic (s)) return base->get_rsup_correction (s);
  else return extra->get_rsup_correction (s);
}

SI
virtual_enhance_font_rep::get_wide_correction  (string s, int mode) {
  if (is_basic (s)) return base->get_wide_correction (s, mode);
  else return extra->get_wide_correction (s, mode);
}

/******************************************************************************
* User interface
******************************************************************************/

font
virtual_enhance_font (font base, string virt) {
  string name= base->res_name * "+" * virt;
  if (font::instances->contains (name)) return font (name);
  return make (font, name, tm_new<virtual_enhance_font_rep> (name, base, virt));
}
