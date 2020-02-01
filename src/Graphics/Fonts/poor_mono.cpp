
/******************************************************************************
* MODULE     : poor_mono.cpp
* DESCRIPTION: Emulation of monospaced fonts
* COPYRIGHT  : (C) 2020  Joris van der Hoeven
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

struct poor_mono_font_rep: font_rep {
  font   base;
  double lw;    // logical width factor
  double phw;   // maximal physical logical width factor
  SI     mquad;

  poor_mono_font_rep (string name, font base, double lw, double phw);

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
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

poor_mono_font_rep::poor_mono_font_rep (
  string name, font b, double l, double ph):
    font_rep (name, b), base (b), lw (l), phw (ph)
{
  this->copy_math_pars (base);
  wquad= (SI) (lw * base->wquad);
  mquad= (SI) (phw * base->wquad);
  spc  = space (wquad);
  extra= space (0);
  mspc = space (wquad);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_mono_font_rep::supports (string s) {
  return base->supports (s);
}

void
poor_mono_font_rep::get_extents (string s, metric& ex) {
  SI  x= 0;
  int i= 0;
  metric ey;
  ex->x1= ex->x3= ex->x2= ex->x4=0;
  ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  while (i < N(s)) {
    int prev= i;
    tm_char_forwards (s, i);
    base->get_extents (s (prev, i), ey);
    SI w= ey->x2 - ey->x1;
    if (w <= mquad) {
      SI dx= x + ((wquad - w) >> 1);
      ex->x1= min (ex->x1, ey->x1 + dx);
      ex->x2= max (ex->x2, ey->x2 + dx);
      ex->x3= min (ex->x3, ey->x3 + dx);
      ex->x4= max (ex->x4, ey->x4 + dx);
    }
    else {
      double f= ((double) mquad) / ((double) w);
      f= max (floor (f * 16.0) / 16.0, 0.01); // limit the number of factors
      SI dx= x + ((wquad - mquad) >> 1);
      ex->x1= min (ex->x1, ((SI) (f * ey->x1)) + dx);
      ex->x2= max (ex->x2, ((SI) (f * ey->x2)) + dx);
      ex->x3= min (ex->x3, ((SI) (f * ey->x3)) + dx);
      ex->x4= max (ex->x4, ((SI) (f * ey->x4)) + dx);
    }
    ex->y1= min (ex->y1, ey->y1);
    ex->y2= max (ex->y2, ey->y2);
    ex->y3= min (ex->y3, ey->y3);
    ex->y4= max (ex->y4, ey->y4);
    x += wquad;
  }
}

void
poor_mono_font_rep::get_xpositions (string s, SI* xpos) {
  get_xpositions (s, xpos, (SI) 0);
}

void
poor_mono_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  (void) lig;
  get_xpositions (s, xpos, (SI) 0);
}

void
poor_mono_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI  w= wquad + 2*xk;
  SI  x= 0;
  int i= 0;
  while (i < N(s)) {
    int prev= i;
    tm_char_forwards (s, i);
    x += w;
    for (int k=prev+1; k<=i; k++) xpos[k]= x;
  }
}

void
poor_mono_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  draw_fixed (ren, s, x, y, (SI) 0);
}

void
poor_mono_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool ligf) {
  (void) ligf;
  draw_fixed (ren, s, x, y, (SI) 0);
}

void
poor_mono_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int i= 0;
  metric ex;
  while (i < N(s)) {
    int prev= i;
    tm_char_forwards (s, i);
    string ss= s (prev, i);
    base->get_extents (ss, ex);
    SI w= ex->x2 - ex->x1;
    if (w <= mquad) {
      SI dx= x + ((wquad - w) >> 1) + xk;
      base->draw_fixed (ren, ss, dx, y);
    }
    else {
      double f= ((double) mquad) / ((double) w);
      f= max (floor (f * 16.0) / 16.0, 0.01); // limit the number of factors
      SI dx= x + ((wquad - mquad) >> 1) + xk;
      ren->set_transformation (scaling (point (f, 1.0), point (dx, 0.0)));
      base->draw_fixed (ren, ss, 0, y);
      ren->reset_transformation ();
    }
    x += wquad + (2*xk);
  }
}

font
poor_mono_font_rep::magnify (double zoomx, double zoomy) {
  return poor_mono_font (base->magnify (zoomx, zoomy), lw, phw);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
poor_mono_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  (void) ligf;
  tm_char_forwards (s, pos);
}

glyph
poor_mono_font_rep::get_glyph (string s) {
  glyph gl= base->get_glyph (s);
  return mono (gl, wquad, mquad);
}

int
poor_mono_font_rep::index_glyph (string s, font_metric& fnm,
                                 font_glyphs& fng) {
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return c;
  fnm= mono (fnm, wquad, mquad);
  fng= mono (fng, wquad, mquad);
  return c;
}

/******************************************************************************
* Mono correction
******************************************************************************/

double
poor_mono_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s);
}

double
poor_mono_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s);
}

SI
poor_mono_font_rep::get_left_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_right_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_lsub_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_lsup_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_rsub_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_rsup_correction (string s) {
  (void) s; return 0;
}

SI
poor_mono_font_rep::get_wide_correction (string s, int mode) {
  return base->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_mono_font (font base, double lw, double phw) {
  string name=
    "poormono[" * base->res_name *
    "," * as_string (lw) * "," * as_string (phw) * "]";
  return make (font, name, tm_new<poor_mono_font_rep> (name, base, lw, phw));
}
