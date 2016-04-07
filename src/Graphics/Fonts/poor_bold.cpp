
/******************************************************************************
* MODULE     : poor_bold.cpp
* DESCRIPTION: Emulation of bold fonts
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

struct poor_bold_font_rep: font_rep {
  font   base;
  double lofat;
  double upfat;
  SI     dlo;
  SI     dup;

  poor_bold_font_rep (string name, font base, double lofat, double upfat);

  void   fatten (string c, SI& dpen, SI& dtot);
  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   adjust_xpositions (string s, SI* xpos, bool lig);
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
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

poor_bold_font_rep::poor_bold_font_rep (string name, font b,
                                        double l, double h):
  font_rep (name, b), base (b), lofat (l), upfat (h)
{
  this->copy_math_pars (base);
  dlo= (SI) (lofat * wfn);
  dup= (SI) (upfat * wfn);

  this->spc    = this->spc + space (dlo >> 1);
  this->wquad += dup;
}

void
poor_bold_font_rep::fatten (string c, SI& dpen, SI& dtot) {
  // FIXME: a future improvement would be to allow the total increase 'dtot'
  // of width to be higher than the thickening 'dpen'.  For instance,
  // for the character 'i', we should have dtot = dpen, but for 'n'
  // and 'fi', we should rather have dtot = 2 dpen; for 'm', we should
  // even have dtot = 3 pen.  This requires horizontal font stretching.
  if (is_uni_upcase_char (c)) {
    dpen= dup;
    dtot= dup;
  }
  else {
    dpen= dlo;
    dtot= dlo;
  }
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_bold_font_rep::supports (string s) {
  return base->supports (s);
}

void
poor_bold_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (s, ex);
  if (N(s) == 0) return;
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  ex->x4 += xpos[N(s)] - ex->x2;
  ex->x2= xpos[N(s)];
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bold_font_rep::adjust_xpositions (string s, SI* xpos, bool lig) {
  SI dx= 0;
  int i=0;
  while (i < N(s)) {
    int start= i;
    if (lig) base->advance_glyph (s, i);
    else tm_char_forwards (s, i);
    int j= start;
    SI delta= 0;
    while (j < i) {
      int prev= j;
      tm_char_forwards (s, j);
      SI dpen, dtot;
      fatten (s (start, j), dpen, dtot);
      delta= dtot;
      for (int k=prev+1; k<=j; k++)
        xpos[k] += dx + delta;
    }
    dx += delta;
  }
}

void
poor_bold_font_rep::get_xpositions (string s, SI* xpos) {
  base->get_xpositions (s, xpos);
  adjust_xpositions (s, xpos, true);
}

void
poor_bold_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  base->get_xpositions (s, xpos, lig);
  adjust_xpositions (s, xpos, lig);
}

void
poor_bold_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  base->get_xpositions (s, xpos, xk);
  adjust_xpositions (s, xpos, false);
}

void
poor_bold_font_rep::draw_fixed (renderer ren, string s,
                                SI x, SI y, SI* xpos) {
  int i=0;
  while (i < N(s)) {
    int start= i;
    base->advance_glyph (s, i);
    string ss= s (start, i);
    if (ren->is_screen) {
      font_metric fnm;
      font_glyphs fng;
      int c= index_glyph (ss, fnm, fng);
      if (c >= 0) ren->draw (c, fng, start==0? x: x + xpos[start], y);
    }
    else {
      SI dpen, dtot;
      fatten (ss, dpen, dtot);
      for (int k=0; k<=8; k++) {
        SI dx= (k*dpen) / 8;
        base->draw (ren, ss, x + dx + (start==0? 0: xpos[start]), y);
      }
    }
  }
}

void
poor_bold_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bold_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool l) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, l);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_bold_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

font
poor_bold_font_rep::magnify (double zoomx, double zoomy) {
  return poor_bold_font (base->magnify (zoomx, zoomy), lofat, upfat);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
poor_bold_font_rep::advance_glyph (string s, int& pos) {
  base->advance_glyph (s, pos);
}

glyph
poor_bold_font_rep::get_glyph (string s) {
  glyph gl= base->get_glyph (s);
  if (is_nil (gl)) return gl;
  SI dpen, dtot;
  fatten (s, dpen, dtot);
  return bolden (gl, dpen);
}

int
poor_bold_font_rep::index_glyph (string s, font_metric& fnm,
                                           font_glyphs& fng) {
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return c;
  SI dpen, dtot;
  fatten (s, dpen, dtot);
  fnm= bolden (fnm, dtot);
  fng= bolden (fng, dpen);
  return c;
}

/******************************************************************************
* Bold correction
******************************************************************************/

double
poor_bold_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s);
}

double
poor_bold_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s);
}

SI
poor_bold_font_rep::get_left_correction (string s) {
  return base->get_left_correction (s);
}

SI
poor_bold_font_rep::get_right_correction (string s) {
  return base->get_right_correction (s);
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_bold_font (font base, double lofat, double upfat) {
  string name= "poorbold[" * base->res_name * "," * as_string (lofat);
  if (upfat != lofat) name << "," << as_string (upfat);
  name << "]";
  return make (font, name,
    tm_new<poor_bold_font_rep> (name, base, lofat, upfat));
}

font
poor_bold_font (font base) {
  double lofat= ((double) base->wline) / ((double) base->wfn);
  return poor_bold_font (base, lofat, lofat);
}
