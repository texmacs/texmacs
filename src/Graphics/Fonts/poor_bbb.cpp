
/******************************************************************************
* MODULE     : poor_bbb.cpp
* DESCRIPTION: Emulation of backboard bold fonts
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"
#include "universal.hpp"

/******************************************************************************
* True Type fonts
******************************************************************************/

struct poor_bbb_font_rep: font_rep {
  font   base;
  double penw;
  double penh;
  double fatw;
  SI     wpen;
  SI     hpen;
  SI     fat;

  poor_bbb_font_rep (string name, font base,
		     double penw, double penh, double fatw);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   adjust_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lig);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool lig);
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

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

poor_bbb_font_rep::poor_bbb_font_rep (string name, font b,
				      double pw, double ph, double d):
  font_rep (name, b), base (b), penw (pw), penh (ph), fatw (d)
{
  this->copy_math_pars (base);
  wpen= (SI) (penw * wfn);
  hpen= (SI) (penh * wfn);
  fat = (SI) (fatw * wfn);

  this->spc    = this->spc + space (fat >> 1);
  this->wquad += fat;
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_bbb_font_rep::supports (string s) {
  return base->supports (s);
}

void
poor_bbb_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (s, ex);
  if (N(s) == 0) return;
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  ex->x4 += xpos[N(s)] - ex->x2;
  ex->x2= xpos[N(s)];
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bbb_font_rep::adjust_xpositions (string s, SI* xpos) {
  SI dx= 0;
  int i=0;
  while (i < N(s)) {
    int start= i;
    tm_char_forwards (s, i);
    dx += fat;
    for (int k=start+1; k<=i; k++)
      xpos[k] += dx;
  }
}

void
poor_bbb_font_rep::get_xpositions (string s, SI* xpos) {
  base->get_xpositions (s, xpos, false);
  adjust_xpositions (s, xpos);
}

void
poor_bbb_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  (void) lig;
  get_xpositions (s, xpos);
}

void
poor_bbb_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  base->get_xpositions (s, xpos, xk);
  adjust_xpositions (s, xpos);
}

void
poor_bbb_font_rep::draw_fixed (renderer ren, string s,
                                SI x, SI y, SI* xpos) {
  int i=0;
  while (i < N(s)) {
    int start= i;
    tm_char_forwards (s, i);
    string ss= s (start, i);
    font_metric fnm;
    font_glyphs fng;
    int c= index_glyph (ss, fnm, fng);
    if (c >= 0) ren->draw (c, fng, start==0? x: x + xpos[start], y);
  }
}

void
poor_bbb_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bbb_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool l) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, l);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bbb_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

font
poor_bbb_font_rep::magnify (double zoomx, double zoomy) {
  return poor_bbb_font (base->magnify (zoomx, zoomy), penw, penh, fatw);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
poor_bbb_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  base->advance_glyph (s, pos, ligf);
}

glyph
poor_bbb_font_rep::get_glyph (string s) {
  glyph gl= base->get_glyph (s);
  if (is_nil (gl)) return gl;
  font_metric fnm;
  font_glyphs fng;
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return glyph ();
  return make_bbb (gl, c, wpen, hpen, fat);
}

int
poor_bbb_font_rep::index_glyph (string s, font_metric& fnm,
                                          font_glyphs& fng) {
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return c;
  fnm= bolden   (fnm, fat, 0);
  fng= make_bbb (fng, wpen, hpen, fat);
  return c;
}

/******************************************************************************
* Bbb correction
******************************************************************************/

double
poor_bbb_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s);
}

double
poor_bbb_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s);
}

SI
poor_bbb_font_rep::get_left_correction (string s) {
  return base->get_left_correction (s);
}

SI
poor_bbb_font_rep::get_right_correction (string s) {
  return base->get_right_correction (s);
}

SI
poor_bbb_font_rep::get_lsub_correction (string s) {
  return base->get_lsub_correction (s);
}

SI
poor_bbb_font_rep::get_lsup_correction (string s) {
  return base->get_lsup_correction (s);
}

SI
poor_bbb_font_rep::get_rsub_correction (string s) {
  SI r= base->get_rsub_correction (s);
  if (N(s) == 1) {
    char c= s[0];
    if (c == 'T') r -= (SI) (0.04 * wfn);
    if (c == 'P') r -= (SI) (0.08 * wfn);
    if (c == 'F') r -= (SI) (0.12 * wfn);
  }
  return r;
}

SI
poor_bbb_font_rep::get_rsup_correction (string s) {
  SI r= base->get_rsup_correction (s);
  if (N(s) == 1) {
    char c= s[0];
    if (c == 'A') r -= (SI) (0.04 * wfn);
    if (c == 'L') r -= (SI) (0.08 * wfn);
  }
  return r;
}

SI
poor_bbb_font_rep::get_wide_correction (string s, int mode) {
  SI r= base->get_wide_correction (s, mode);
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_bbb_font (font base, double penw, double penh, double fatw) {
  string name= "poorbbb[" * base->res_name;
  name << "," << as_string (penw);
  name << "," << as_string (penh);
  name << "," << as_string (fatw) << "]";
  return make (font, name,
	       tm_new<poor_bbb_font_rep> (name, base, penw, penh, fatw));
}

font
poor_bbb_font (font base) {
  double penw= 0.6 * ((double) base->wline) / ((double) base->wfn);
  return poor_bbb_font (base, penw, penw, 1.5 * penw);
}
